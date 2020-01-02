(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2020  Bogdan-Cristian Tataroiu

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

open Core
open Async

type t =
  { object_directory : string
  ; packs : (string * Git_pack_reader.t) list
  ; sha1_buf : Sha1.Raw.Volatile.t
  ; read_throttle : Git_object_reader.t Throttle.t
  }

let unexpected_blob_size (_ : int) = failwith "Unexpected blob object"

let unexpected_blob_chunk (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) =
  failwith "Unexpected blob object"
;;

let unexpected_commit (_ : Commit.t) = failwith "Unexpected commit object"

let unexpected_tree_line (_ : File_mode.t) (_ : Sha1.Raw.Volatile.t) ~name:(_ : string) =
  failwith "Unexpected tree object"
;;

let unexpected_tag (_ : Tag.t) = failwith "Unexpected tag object"

let create ~object_directory ~max_concurrent_reads =
  let open Deferred.Or_error.Let_syntax in
  let%bind packs =
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      let open Deferred.Let_syntax in
      let%bind () = Unix.mkdir ~p:() (object_directory ^/ "pack") in
      Sys.readdir (object_directory ^/ "pack"))
  in
  let%map packs =
    Array.to_list packs
    |> List.filter ~f:(fun pack_file -> String.is_suffix ~suffix:".pack" pack_file)
    |> Deferred.Or_error.List.map ~how:`Sequential ~f:(fun pack_file ->
      let pack_file = object_directory ^/ "pack" ^/ pack_file in
      let%map pack_reader = Git_pack_reader.create ~pack_file in
      pack_file, pack_reader)
  in
  let read_throttle =
    Throttle.create_with
      ~continue_on_error:true
      (List.init max_concurrent_reads ~f:(fun (_ : int) ->
         Git_object_reader.create
           ~on_blob_size:unexpected_blob_size
           ~on_blob_chunk:unexpected_blob_chunk
           ~on_commit:unexpected_commit
           ~on_tree_line:unexpected_tree_line
           ~on_tag:unexpected_tag
           ~on_error:Error.raise))
  in
  { object_directory; packs; sha1_buf = Sha1.Raw.Volatile.create (); read_throttle }
;;

let object_directory t = t.object_directory

let unpacked_disk_path t sha1 =
  let sha1_string = Sha1.Hex.to_string sha1 in
  Filename.concat
    (Filename.concat t.object_directory (String.sub sha1_string ~pos:0 ~len:2))
    (String.sub sha1_string ~pos:2 ~len:(Sha1.Hex.length - 2))
;;

let read_object =
  let rec loop_packs
            t
            sha1
            ~on_blob_size
            ~on_blob_chunk
            ~on_commit
            ~on_tree_line
            ~on_tag
            ~push_back
            packs
    =
    match packs with
    | [] ->
      Throttle.enqueue t.read_throttle (fun git_object_reader ->
        Git_object_reader.set_on_blob
          git_object_reader
          ~on_size:on_blob_size
          ~on_chunk:on_blob_chunk;
        Git_object_reader.set_on_commit git_object_reader on_commit;
        Git_object_reader.set_on_tree_line git_object_reader on_tree_line;
        Git_object_reader.set_on_tag git_object_reader on_tag;
        let path = unpacked_disk_path t sha1 in
        Git_object_reader.read_file' git_object_reader ~file:path ~push_back)
    | (_, pack) :: packs ->
      (match Git_pack_reader.find_sha1_index' pack t.sha1_buf with
       | None ->
         loop_packs
           t
           sha1
           ~on_blob_size
           ~on_blob_chunk
           ~on_commit
           ~on_tree_line
           ~on_tag
           ~push_back
           packs
       | Some { index } ->
         Git_pack_reader.read_object
           pack
           ~index
           ~on_blob_size
           ~on_blob_chunk
           ~on_commit
           ~on_tree_line
           ~on_tag;
         Deferred.unit)
  in
  fun t sha1 ~on_blob_size ~on_blob_chunk ~on_commit ~on_tree_line ~on_tag ~push_back ->
    Sha1.Raw.Volatile.of_hex sha1 t.sha1_buf;
    loop_packs
      t
      sha1
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      ~push_back
      t.packs
;;

let read_blob t sha1 ~on_size ~on_chunk ~push_back =
  read_object
    t
    sha1
    ~on_blob_size:on_size
    ~on_blob_chunk:on_chunk
    ~on_commit:unexpected_commit
    ~on_tree_line:unexpected_tree_line
    ~on_tag:unexpected_tag
    ~push_back
;;

let read_commit t sha1 ~on_commit =
  read_object
    t
    sha1
    ~on_blob_size:unexpected_blob_size
    ~on_blob_chunk:unexpected_blob_chunk
    ~on_commit
    ~on_tree_line:unexpected_tree_line
    ~on_tag:unexpected_tag
    ~push_back:(Fn.const (return `Ok))
;;

let read_tree t sha1 ~on_tree_line =
  read_object
    t
    sha1
    ~on_blob_size:unexpected_blob_size
    ~on_blob_chunk:unexpected_blob_chunk
    ~on_commit:unexpected_commit
    ~on_tree_line
    ~on_tag:unexpected_tag
    ~push_back:(Fn.const (return `Ok))
;;

let read_tag t sha1 ~on_tag =
  read_object
    t
    sha1
    ~on_blob_size:unexpected_blob_size
    ~on_blob_chunk:unexpected_blob_chunk
    ~on_commit:unexpected_commit
    ~on_tree_line:unexpected_tree_line
    ~on_tag
    ~push_back:(Fn.const (return `Ok))
;;

let with_on_disk_file t sha1 ~f =
  let path = unpacked_disk_path t sha1 in
  match%bind Sys.is_file_exn path with
  | true -> f path
  | false ->
    Sha1.Raw.Volatile.of_hex sha1 t.sha1_buf;
    let pack_and_index =
      List.find_map t.packs ~f:(fun (_, pack) ->
        match Git_pack_reader.find_sha1_index' pack t.sha1_buf with
        | None -> None
        | Some { index } -> Some (pack, index))
    in
    (match pack_and_index with
     | None -> raise_s [%message "Object not found" (sha1 : Sha1.Hex.t)]
     | Some (pack, index) ->
       let object_type_and_length = Set_once.create () in
       let data = Set_once.create () in
       Git_pack_reader.read_raw_object
         pack
         ~index
         ~on_header:(fun object_type ~size ->
           Set_once.set_exn object_type_and_length [%here] (object_type, size))
         ~on_payload:(fun buf ~pos ~len ->
           Set_once.set_exn data [%here] (Bigstring.sub buf ~pos ~len));
       let object_type, length = Set_once.get_exn object_type_and_length [%here] in
       let writer =
         Git_object_writer.With_header.Known_size.create_uninitialised
           ~object_directory:t.object_directory
       in
       let%bind () =
         Git_object_writer.With_header.Known_size.init_or_reset writer object_type ~length
       in
       let data = Set_once.get_exn data [%here] in
       Git_object_writer.With_header.Known_size.append_data
         writer
         data
         ~pos:0
         ~len:(Bigstring.length data);
       let%bind saved_sha1 =
         Git_object_writer.With_header.Known_size.finalise_exn writer
       in
       let saved_sha1 = Sha1.Raw.to_hex saved_sha1 in
       assert ([%compare.equal: Sha1.Hex.t] sha1 saved_sha1);
       Monitor.protect (fun () -> f path) ~finally:(fun () -> Unix.unlink path))
;;

module Object_location = struct
  type t =
    | Unpacked_file of string
    | In_pack_file of
        { pack_file : string
        ; index : int
        }
  [@@deriving sexp_of]
end

let all_objects_in_store t =
  let result = Sha1.Hex.Table.create () in
  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false
  in
  let%bind () =
    let%bind first_level = Sys.readdir t.object_directory in
    Deferred.Array.iter first_level ~f:(fun first_child ->
      let path = Filename.concat t.object_directory first_child in
      if String.( = ) first_child "info" || String.( = ) first_child "pack"
      then Deferred.unit
      else if Int.( = ) (String.length first_child) 2
           && String.for_all ~f:is_hex first_child
      then (
        let%bind second_level = Sys.readdir path in
        Deferred.Array.iter second_level ~f:(fun file ->
          let path = Filename.concat path file in
          match%map Sys.is_file_exn path with
          | false -> Log.Global.info "Unexpected directory: %s" path
          | true ->
            if Int.( = ) (String.length file) 38 && String.for_all ~f:is_hex file
            then
              Hashtbl.add_multi
                result
                ~key:(Sha1.Hex.of_string (first_child ^ file))
                ~data:(Object_location.Unpacked_file path)
            else Log.Global.info "Unexpected file: %s" path))
      else (
        Log.Global.info "Unexpected file or directory: %s" path;
        Deferred.unit))
  in
  List.iter t.packs ~f:(fun (pack_file, pack_reader) ->
    let items_in_pack = Git_pack_reader.items_in_pack pack_reader in
    for index = 0 to items_in_pack - 1 do
      Hashtbl.add_multi
        result
        ~key:(Sha1.Raw.Volatile.to_hex (Git_pack_reader.sha1 pack_reader ~index))
        ~data:(Object_location.In_pack_file { pack_file; index })
    done);
  return result
;;
