(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020-2021  Bogdan-Cristian Tataroiu

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

open! Core
open! Async
open! Import

let read_commit_object object_store commit_sha1 =
  let commit = Set_once.create () in
  let%map () =
    Object_store.Packed.read_commit
      object_store
      commit_sha1
      ~on_commit:(Set_once.set_exn commit [%here])
  in
  Set_once.get_exn commit [%here]
;;

module Timestamp_tree = struct
  type t =
    { files : Time_ns.t String.Table.t
    ; directories : t String.Table.t
    ; mutable timestamp : Time_ns.t
    }

  let create () =
    { files = String.Table.create ()
    ; directories = String.Table.create ()
    ; timestamp = Time_ns.epoch
    }
  ;;

  let rec get_exn t = function
    | [] -> t.timestamp
    | entry :: directories ->
      (match Hashtbl.find_exn t.directories entry with
       | exception exn ->
         (match directories with
          | [] -> Hashtbl.find_exn t.files entry
          | _ :: _ -> raise exn)
       | node -> get_exn node directories)
  ;;

  let get_exn t path =
    match get_exn t path with
    | exception _ -> raise_s [%message "File not found" (path : string list)]
    | result -> result
  ;;

  let rec update_exn t path time =
    match path with
    | [] -> failwith "Path not pointing to a file"
    | [ file ] ->
      (match Hashtbl.find_exn t.directories file with
       | exception _ ->
         Hashtbl.set t.files ~key:file ~data:time;
         t.timestamp <- Time_ns.max t.timestamp time
       | (_ : t) -> failwith "Path not pointing to a file")
    | directory :: directories ->
      (match Hashtbl.find_exn t.files directory with
       | exception _ ->
         let node = Hashtbl.find_or_add t.directories directory ~default:create in
         update_exn node directories time;
         t.timestamp <- Time_ns.max t.timestamp time
       | (_ : Time_ns.t) -> failwith "Path not pointing to a file")
  ;;

  let update_exn t path time =
    match update_exn t path time with
    | exception _ ->
      raise_s
        [%message "Path not pointing to a file" (path : string list) (time : Time_ns.t)]
    | result -> result
  ;;

  let rec update_directory_exn t path time =
    match path with
    | [] -> t.timestamp <- time
    | directory :: directories ->
      (match Hashtbl.find_exn t.files directory with
       | exception _ ->
         let node = Hashtbl.find_or_add t.directories directory ~default:create in
         update_directory_exn node directories time;
         t.timestamp <- Time_ns.max t.timestamp time
       | (_ : Time_ns.t) -> failwith "Path not pointing to a directory")
  ;;

  let update_directory_exn t path time =
    match update_directory_exn t path time with
    | exception _ ->
      raise_s
        [%message
          "Path not pointing to a directory" (path : string list) (time : Time_ns.t)]
    | result -> result
  ;;
end

type t =
  { object_store : Object_store.Packed.t
  ; tree_cache : Tree_cache.t
  ; tree_history : (Tree_cache.Node.t * Time_ns.t) array
  ; last_change_time : Timestamp_tree.t
  }

let rec walk_commit ~acc object_store commit_sha1 =
  let%bind commit = read_commit_object object_store commit_sha1 in
  let time = commit.author.timestamp in
  let parent_sha1 =
    match commit.parents with
    | [] -> None
    | parent_sha1 :: _ -> Some parent_sha1
  in
  match parent_sha1 with
  | None -> return (Array.of_list acc)
  | Some parent_sha1 ->
    walk_commit
      ~acc:((Tree_cache.Node.of_disk_hash commit.tree, time) :: acc)
      object_store
      parent_sha1
;;

let create object_store tree_cache ~current_commit =
  let%map tree_history =
    match current_commit with
    | None -> Deferred.return [||]
    | Some current_commit -> walk_commit ~acc:[] object_store current_commit
  in
  { object_store; tree_cache; tree_history; last_change_time = Timestamp_tree.create () }
;;

let update_exn t ~path time = Timestamp_tree.update_exn t.last_change_time path time

let last_change_time_exn t ~path ~current_sha1 =
  match Timestamp_tree.get_exn t.last_change_time path with
  | timestamp -> return timestamp
  | exception _ ->
    let length = Array.length t.tree_history in
    let step = ref 1 in
    while !step <= length do
      step := !step lsl 1
    done;
    let pos = ref (-1) in
    let in_tree tree =
      match%bind Tree_cache.Node.get_file t.tree_cache tree ~path with
      | Some { sha1 = tree_sha1; _ }
        when [%compare.equal: Sha1.Hex.t] tree_sha1 current_sha1 ->
        return `Present_as_file
      | Some _ -> return `Wrong_sha1
      | None ->
        (match%bind Tree_cache.Node.get_node t.tree_cache tree ~path with
         | Some node ->
           (match%bind Tree_cache.Node.persist t.tree_cache node with
            | tree_sha1 when [%compare.equal: Sha1.Hex.t] tree_sha1 current_sha1 ->
              return `Present_as_directory
            | _ -> return `Wrong_sha1)
         | None -> return `Not_found)
    in
    let rec loop () =
      step := !step lsr 1;
      if !step = 0
      then return (!pos + 1)
      else if !pos + !step < length
      then (
        let tree, _ = t.tree_history.(!pos + !step) in
        match%bind in_tree tree with
        | `Present_as_file | `Present_as_directory -> loop ()
        | `Wrong_sha1 | `Not_found ->
          pos := !pos + !step;
          loop ())
      else loop ()
    in
    let%bind pos = loop () in
    if pos >= length
    then raise_s [%message "Unable to find path in tree" (path : string list)];
    let tree, time = t.tree_history.(pos) in
    (match%bind in_tree tree with
     | `Wrong_sha1 | `Not_found ->
       raise_s
         [%message
           "Unable to find path in tree with the provided SHA1"
             (path : string list)
             (current_sha1 : Sha1.Hex.t)]
     | `Present_as_file ->
       Timestamp_tree.update_exn t.last_change_time path time;
       return time
     | `Present_as_directory ->
       Timestamp_tree.update_directory_exn t.last_change_time path time;
       return time)
;;
