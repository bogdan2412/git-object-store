(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020-2025  Bogdan-Cristian Tataroiu

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

module File = struct
  module Kind = struct
    type t =
      | Regular_file
      | Executable_file
      | Link
    [@@deriving compare, sexp_of]
  end

  type t =
    { sha1 : Sha1.Hex.t
    ; kind : Kind.t
    }
  [@@deriving compare, sexp_of]
end

module Node0 = struct
  type _ state =
    | Not_loaded : Sha1.Hex.t -> [ `Not_loaded ] state
    | Loading : Sha1.Hex.t * [ `Loaded ] state Deferred.t -> [ `Not_loaded ] state
    | Loaded_and_persisted :
        { sha1 : Sha1.Hex.t
        ; directories : t String.Map.t
        ; files : File.t String.Map.t
        ; submodules : Sha1.Hex.t String.Map.t
        }
        -> [ `Loaded ] state
    | Loaded_and_not_persisted :
        { directories : t String.Map.t
        ; files : File.t String.Map.t
        ; submodules : Sha1.Hex.t String.Map.t
        }
        -> [ `Loaded ] state

  and packed_state = T : _ state -> packed_state
  and t = { mutable state : packed_state }

  let empty () =
    { state =
        T
          (Loaded_and_not_persisted
             { directories = String.Map.empty
             ; files = String.Map.empty
             ; submodules = String.Map.empty
             })
    }
  ;;

  let empty_node_sha1 =
    (* Validated in expect test below. *)
    Sha1.Hex.of_string "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  ;;

  let is_empty t =
    match t.state with
    | T (Not_loaded sha1) | T (Loading (sha1, _)) ->
      [%compare.equal: Sha1.Hex.t] sha1 empty_node_sha1
    | T (Loaded_and_persisted { sha1 = _; directories; files; submodules })
    | T (Loaded_and_not_persisted { directories; files; submodules }) ->
      Map.is_empty directories && Map.is_empty files && Map.is_empty submodules
  ;;

  let of_disk_hash sha1 = { state = T (Not_loaded sha1) }

  let directories = function
    | Loaded_and_persisted { directories; _ } -> directories
    | Loaded_and_not_persisted { directories; _ } -> directories
  ;;

  let files = function
    | Loaded_and_persisted { files; _ } -> files
    | Loaded_and_not_persisted { files; _ } -> files
  ;;

  let submodules = function
    | Loaded_and_persisted { submodules; _ } -> submodules
    | Loaded_and_not_persisted { submodules; _ } -> submodules
  ;;

  let sha1 t =
    match t.state with
    | T (Not_loaded sha1) -> Some sha1
    | T (Loading (sha1, _)) -> Some sha1
    | T (Loaded_and_persisted { sha1; _ }) -> Some sha1
    | T (Loaded_and_not_persisted _) -> None
  ;;
end

module Sequencer_witness : sig
  type t

  val create : unit -> t
end = struct
  type t = [ `Mutation_sequencer ]

  let create () : t = `Mutation_sequencer
end

type t =
  { object_store : Object_store.Packed.t
  ; mutable root : Node0.t
  ; loaded : [ `Loaded ] Node0.state Deferred.t Sha1.Hex.Table.t
  ; mutation_sequencer : Sequencer_witness.t Sequencer.t
  ; git_tree_writer : Tree_writer.t
  ; git_tree_writer_mode : Git_object_files.Writer.Mode.t
  ; git_tree_writer_volatile_sha1_raw : Sha1.Raw.Volatile.t
  }

let create object_store ~root =
  { object_store
  ; root
  ; loaded = Sha1.Hex.Table.create ()
  ; mutation_sequencer =
      Sequencer.create ~continue_on_error:false (Sequencer_witness.create ())
  ; git_tree_writer =
      Tree_writer.create_uninitialised
        ~object_directory:(Object_store.Packed.object_directory object_store)
  ; git_tree_writer_mode =
      Write
        { should_discard =
            (fun sha1_raw ->
              match Object_store.Packed.find_object' object_store sha1_raw with
              | In_pack_file _ -> true
              | Unpacked_file_if_exists _ -> false)
        }
  ; git_tree_writer_volatile_sha1_raw = Sha1.Raw.Volatile.create ()
  }
;;

let root t = t.root

module Node = struct
  include Node0

  module Entry = struct
    type t =
      | File of File.t
      | Directory of Node0.t
      | Submodule of Sha1.Hex.t
  end

  let ensure_loaded t node : [ `Loaded ] state Deferred.t =
    match node.state with
    | T (Loaded_and_persisted _ as loaded) -> return loaded
    | T (Loaded_and_not_persisted _ as loaded) -> return loaded
    | T (Loading (_, loaded)) -> loaded
    | T (Not_loaded sha1) ->
      let loaded_deferred =
        Hashtbl.find_or_add t.loaded sha1 ~default:(fun () ->
          let entry_names = ref String.Set.empty in
          let files = ref String.Map.empty in
          let directories = ref String.Map.empty in
          let submodules = ref String.Map.empty in
          let%map () =
            Object_store.Packed.read_tree
              t.object_store
              sha1
              ~on_tree_line:(fun mode entry_sha1 ~name ->
                let entry_sha1 = Sha1.Raw.Volatile.to_hex entry_sha1 in
                (match Set.mem !entry_names name with
                 | true ->
                   raise_s
                     [%message
                       "Naming conflict"
                         ~tree_sha1:(sha1 : Sha1.Hex.t)
                         ~duplicate_entry_name:(name : string)
                         ~duplicate_entry_sha1:(entry_sha1 : Sha1.Hex.t)
                         ~duplicate_entry_type:(mode : Git_core_types.File_mode.t)]
                 | false -> entry_names := Set.add !entry_names name);
                match mode with
                | Non_executable_file | Executable_file | Link ->
                  let file =
                    { File.sha1 = entry_sha1
                    ; kind =
                        (match mode with
                         | Non_executable_file -> Regular_file
                         | Executable_file -> Executable_file
                         | Link -> Link
                         | Directory | Git_submodule ->
                           raise_s
                             [%message "Compiler BUG - impossible match variant" [%here]])
                    }
                  in
                  files := Map.add_exn !files ~key:name ~data:file
                | Directory ->
                  directories
                  := Map.add_exn
                       !directories
                       ~key:name
                       ~data:{ state = T (Not_loaded entry_sha1) }
                | Git_submodule ->
                  submodules := Map.add_exn !submodules ~key:name ~data:entry_sha1)
          in
          Loaded_and_persisted
            { sha1; files = !files; directories = !directories; submodules = !submodules })
      in
      node.state <- T (Loading (sha1, loaded_deferred));
      upon loaded_deferred (fun loaded -> node.state <- T loaded);
      loaded_deferred
  ;;

  let is_persisted node =
    match node.state with
    | T (Not_loaded _) -> true
    | T (Loading _) -> true
    | T (Loaded_and_persisted _) -> true
    | T (Loaded_and_not_persisted _) -> false
  ;;

  let rec persist' t node (witness : Sequencer_witness.t) =
    match node.state with
    | T (Not_loaded sha1) -> return sha1
    | T (Loaded_and_persisted { sha1; _ }) -> return sha1
    | T (Loading (sha1, _)) -> return sha1
    | T (Loaded_and_not_persisted { files; directories; submodules }) ->
      let%bind directory_lines =
        Deferred.Map.mapi directories ~how:`Sequential ~f:(fun ~key:name ~data:node ->
          match is_empty node with
          | true ->
            raise_s
              [%message "trees cannot contain empty trees as children" (name : string)]
          | false ->
            let%map sha1 = persist' t node witness in
            sha1, Git_core_types.File_mode.Directory)
      in
      let file_lines =
        Map.map files ~f:(fun { sha1; kind } ->
          ( sha1
          , match kind with
            | Regular_file -> Git_core_types.File_mode.Non_executable_file
            | Executable_file -> Executable_file
            | Link -> Link ))
      in
      let submodule_lines =
        Map.map submodules ~f:(fun sha1 -> sha1, Git_core_types.File_mode.Git_submodule)
      in
      let all_lines =
        Map.merge directory_lines file_lines ~f:(fun ~key:name -> function
          | `Both (dir, file) ->
            raise_s
              [%message
                "naming conflict"
                  (name : string)
                  (dir : Sha1.Hex.t * Git_core_types.File_mode.t)
                  (file : Sha1.Hex.t * Git_core_types.File_mode.t)]
          | `Left value | `Right value -> Some value)
        |> Map.merge submodule_lines ~f:(fun ~key:name -> function
          | `Both (dir_or_file, submodule) ->
            raise_s
              [%message
                "naming conflict"
                  (name : string)
                  (dir_or_file : Sha1.Hex.t * Git_core_types.File_mode.t)
                  (submodule : Sha1.Hex.t * Git_core_types.File_mode.t)]
          | `Left value | `Right value -> Some value)
      in
      let%bind () = Tree_writer.init_or_reset t.git_tree_writer t.git_tree_writer_mode in
      Map.iteri all_lines ~f:(fun ~key:name ~data:(sha1, kind) ->
        Sha1.Raw.Volatile.of_hex sha1 t.git_tree_writer_volatile_sha1_raw;
        Tree_writer.write_tree_line'
          t.git_tree_writer
          kind
          t.git_tree_writer_volatile_sha1_raw
          ~name);
      let%bind sha1_raw = Tree_writer.finalise t.git_tree_writer in
      let sha1 = Sha1.Raw.to_hex sha1_raw in
      let%map state =
        Hashtbl.find_or_add t.loaded sha1 ~default:(fun () ->
          return (Loaded_and_persisted { sha1; files; directories; submodules }))
      in
      node.state <- T state;
      sha1
  ;;

  let persist t node = Throttle.enqueue t.mutation_sequencer (persist' t node)

  let rec get_entry_map t node ~path ~f =
    let%bind state = ensure_loaded t node in
    match path with
    | [] -> return None
    | [ entry ] -> return (f state entry)
    | directory :: rest ->
      (match Map.find (directories state) directory with
       | Some node -> get_entry_map t node ~path:rest ~f
       | None -> return None)
  ;;

  let get_entry t node ~path =
    get_entry_map t node ~path ~f:(fun state entry : Entry.t option ->
      match Map.find (directories state) entry with
      | Some node -> Some (Directory node)
      | None ->
        (match Map.find (files state) entry with
         | Some file -> Some (File file)
         | None ->
           (match Map.find (submodules state) entry with
            | Some sha1 -> Some (Submodule sha1)
            | None -> None)))
  ;;

  let get_file t node ~path =
    get_entry_map t node ~path ~f:(fun state entry -> Map.find (files state) entry)
  ;;

  let get_submodule t node ~path =
    get_entry_map t node ~path ~f:(fun state entry -> Map.find (submodules state) entry)
  ;;

  let get_node t node ~path =
    get_entry_map t node ~path ~f:(fun state entry -> Map.find (directories state) entry)
  ;;

  let add_entry_to_directory t node ~name entry =
    let%bind state = ensure_loaded t node in
    let unchanged =
      match (entry : Entry.t) with
      | Directory new_node ->
        (match is_empty new_node with
         | true ->
           not
             (Map.mem (directories state) name
              || Map.mem (files state) name
              || Map.mem (submodules state) name)
         | false ->
           let old_directory_sha1 =
             Map.find (directories state) name |> Option.bind ~f:sha1
           in
           let new_directory_sha1 = sha1 new_node in
           (match old_directory_sha1, new_directory_sha1 with
            | Some old_sha1, Some new_sha1
              when [%compare.equal: Sha1.Hex.t] old_sha1 new_sha1 -> true
            | _ -> false))
      | File { File.sha1 = new_sha1; _ } ->
        (match Map.find (files state) name with
         | Some { sha1 = old_sha1; _ } when [%compare.equal: Sha1.Hex.t] old_sha1 new_sha1
           -> true
         | _ -> false)
      | Submodule new_sha1 ->
        (match Map.find (submodules state) name with
         | Some old_sha1 when [%compare.equal: Sha1.Hex.t] old_sha1 new_sha1 -> true
         | _ -> false)
    in
    match unchanged with
    | true -> return node
    | false ->
      let files = Map.remove (files state) name in
      let directories = Map.remove (directories state) name in
      let submodules = Map.remove (submodules state) name in
      let state =
        Loaded_and_not_persisted
          { files =
              (match entry with
               | File data -> Map.add_exn files ~key:name ~data
               | _ -> files)
          ; directories =
              (match entry with
               | Directory data when not (is_empty data) ->
                 Map.add_exn directories ~key:name ~data
               | _ -> directories)
          ; submodules =
              (match entry with
               | Submodule data -> Map.add_exn submodules ~key:name ~data
               | _ -> submodules)
          }
      in
      return { state = T state }
  ;;

  let rec set_path_to_entry t node ~path entry (witness : Sequencer_witness.t) =
    match path with
    | [] -> failwith "Empty path"
    | [ name ] -> add_entry_to_directory t node ~name entry
    | directory :: drest ->
      let%bind state = ensure_loaded t node in
      let%bind new_directory =
        match Map.find (directories state) directory with
        | Some node -> set_path_to_entry t node ~path:drest entry witness
        | None -> set_path_to_entry t (empty ()) ~path:drest entry witness
      in
      (match is_empty new_directory || is_persisted new_directory with
       | true -> (* Child directory was not changed *) return node
       | false ->
         let state =
           Loaded_and_not_persisted
             { files = files state
             ; directories =
                 Map.set (directories state) ~key:directory ~data:new_directory
             ; submodules = submodules state
             }
         in
         return { state = T state })
  ;;

  let add_entry t node ~path entry =
    Throttle.enqueue t.mutation_sequencer (fun witness ->
      set_path_to_entry t node ~path entry witness)
  ;;

  let add_file' t node ~path sha1 kind witness =
    set_path_to_entry t node ~path (File { File.sha1; kind }) witness
  ;;

  let add_file t node ~path sha1 kind =
    Throttle.enqueue t.mutation_sequencer (fun witness ->
      add_file' t node ~path sha1 kind witness)
  ;;

  let add_node' t node ~path new_node witness =
    set_path_to_entry t node ~path (Directory new_node) witness
  ;;

  let add_node t node ~path new_node =
    Throttle.enqueue t.mutation_sequencer (fun witness ->
      add_node' t node ~path new_node witness)
  ;;

  let add_submodule' t node ~path sha1 witness =
    set_path_to_entry t node ~path (Submodule sha1) witness
  ;;

  let add_submodule t node ~path sha1 =
    Throttle.enqueue t.mutation_sequencer (fun witness ->
      add_submodule' t node ~path sha1 witness)
  ;;

  let rec remove_path' t node ~path (witness : Sequencer_witness.t) =
    match path with
    | [] -> return None
    | [ leaf ] ->
      let%bind state = ensure_loaded t node in
      (match
         ( Map.find (files state) leaf
         , Map.find (directories state) leaf
         , Map.find (submodules state) leaf )
       with
       | None, None, None ->
         (* Path does not exist, node remains unchanged. *) return (Some node)
       | _ ->
         let state =
           Loaded_and_not_persisted
             { files = Map.remove (files state) leaf
             ; directories = Map.remove (directories state) leaf
             ; submodules = Map.remove (submodules state) leaf
             }
         in
         (match is_empty { state = T state } with
          | true -> return None
          | false -> return (Some { state = T state })))
    | directory :: drest ->
      let%bind state = ensure_loaded t node in
      (match Map.find (directories state) directory with
       | None -> return (Some node)
       | Some directory_node ->
         let%bind new_directory = remove_path' t directory_node ~path:drest witness in
         (match new_directory with
          | Some new_directory when is_persisted new_directory ->
            (* Child directory was not changed *)
            return (Some node)
          | None | Some _ ->
            let state =
              Loaded_and_not_persisted
                { files = files state
                ; directories =
                    Map.change (directories state) directory ~f:(Fn.const new_directory)
                ; submodules = submodules state
                }
            in
            (match is_empty { state = T state } with
             | true -> return None
             | false -> return (Some { state = T state }))))
  ;;

  let remove_path t node ~path =
    Throttle.enqueue t.mutation_sequencer (fun witness ->
      remove_path' t node ~path witness)
  ;;
end

let get_entry t ~path = Node.get_entry t t.root ~path
let get_file t ~path = Node.get_file t t.root ~path
let get_node t ~path = Node.get_node t t.root ~path
let get_submodule t ~path = Node.get_submodule t t.root ~path

let add_entry t ~path entry =
  Throttle.enqueue t.mutation_sequencer (fun witness ->
    let%map root = Node.set_path_to_entry t t.root ~path entry witness in
    t.root <- root)
;;

let add_file t ~path sha1 kind =
  Throttle.enqueue t.mutation_sequencer (fun witness ->
    let%map root = Node.add_file' t t.root ~path sha1 kind witness in
    t.root <- root)
;;

let add_node t ~path node =
  Throttle.enqueue t.mutation_sequencer (fun witness ->
    let%map root = Node.add_node' t t.root ~path node witness in
    t.root <- root)
;;

let add_submodule t ~path sha1 =
  Throttle.enqueue t.mutation_sequencer (fun witness ->
    let%map root = Node.add_submodule' t t.root ~path sha1 witness in
    t.root <- root)
;;

let remove_path t ~path =
  Throttle.enqueue t.mutation_sequencer (fun witness ->
    let%map root = Node.remove_path' t t.root ~path witness in
    match root with
    | None -> raise_s [%message "Can not remove root node" (path : string list)]
    | Some root -> t.root <- root)
;;

let is_persisted t = Node.is_persisted t.root
let persist t = Node.persist t t.root

module%test _ = struct
  let gen_sha1 seed =
    String.init Sha1.Hex.length ~f:(fun (_ : int) ->
      let value = Random.State.int seed 16 in
      if value <= 9
      then Char.of_int_exn (Char.to_int '0' + value)
      else Char.of_int_exn (Char.to_int 'a' + value - 10))
    |> Sha1.Hex.of_string
  ;;

  let%expect_test "Empty tree SHA1" =
    Expect_test_helpers_async.with_temp_dir (fun object_directory ->
      let%bind object_store =
        Object_store.Packed.create ~object_directory ~max_concurrent_reads:4 Validate_sha1
        >>| ok_exn
      in
      let t = create object_store ~root:(Node.empty ()) in
      let%bind sha1 = persist t in
      print_s [%sexp (sha1 : Sha1.Hex.t)];
      [%expect {| 4b825dc642cb6eb9a060e54bf8d69288fbee4904 |}];
      [%test_result: Sha1.Hex.t] ~expect:Node.empty_node_sha1 sha1;
      Deferred.unit)
  ;;

  let%expect_test "Randomised test" =
    Expect_test_helpers_async.with_temp_dir (fun object_directory ->
      let%bind object_store =
        Object_store.Packed.create ~object_directory ~max_concurrent_reads:4 Validate_sha1
        >>| ok_exn
      in
      let seed = Random.State.make [| 1337 |] in
      let t = create object_store ~root:(Node.empty ()) in
      let added = String.Table.create () in
      let random_add t =
        let length = Random.State.int seed 5 + 1 in
        let path =
          List.init length ~f:(fun index ->
            let name =
              String.init
                (Random.State.int seed 2 + 1)
                ~f:(fun (_ : int) ->
                  Char.of_int_exn (Char.to_int 'a' + Random.State.int seed 3))
            in
            if index = length - 1 then "F" ^ name else "D" ^ name)
        in
        let sha1 = gen_sha1 seed in
        let kind : File.Kind.t =
          match Random.State.int seed 3 with
          | 0 -> Regular_file
          | 1 -> Executable_file
          | 2 -> Link
          | _ -> assert false
        in
        Hashtbl.set added ~key:(String.concat path ~sep:"/") ~data:{ File.sha1; kind };
        add_file t ~path sha1 kind
      in
      let rec random_adds t = function
        | 0 -> Deferred.unit
        | n ->
          let%bind () = random_add t in
          random_adds t (n - 1)
      in
      let random_remove t =
        let index = Random.State.int seed (Hashtbl.length added) in
        let key = List.nth_exn (Hashtbl.keys added) index in
        let path = String.split ~on:'/' key in
        Hashtbl.remove added key;
        remove_path t ~path
      in
      let rec random_removes t = function
        | 0 -> Deferred.unit
        | n ->
          let%bind () = random_remove t in
          random_removes t (n - 1)
      in
      let%bind () = random_adds t 100 in
      let%bind sha1 = persist t in
      printf !"%{Sha1.Hex}" sha1;
      [%expect {| 02e2ce6b94b54cfc4a1ca1b9b7e2bf9b26628ff7 |}];
      let t = create object_store ~root:(Node.of_disk_hash sha1) in
      let%bind () = random_adds t 100 in
      let%bind sha1 = persist t in
      printf !"%{Sha1.Hex}" sha1;
      [%expect {| 402371cc128563c93d4add4e93145aa184f7086d |}];
      let t = create object_store ~root:(Node.of_disk_hash sha1) in
      let%bind () = random_adds t 100 in
      let%bind sha1 = persist t in
      printf !"%{Sha1.Hex}" sha1;
      [%expect {| 5a3941929695b776327f7ff9ee0dcc9d6173d43b |}];
      let%bind () = random_removes t 50 in
      let%bind () = random_adds t 100 in
      let%bind () = random_removes t 20 in
      let%bind sha1 = persist t in
      printf !"%{Sha1.Hex}" sha1;
      [%expect {| 9fc3fbcc17e7ecb2fac140bf98fb21f2fdd6cc86 |}];
      let%bind () =
        Deferred.List.iter
          (Hashtbl.to_alist added)
          ~how:`Sequential
          ~f:(fun (path, expected) ->
            let path = String.split ~on:'/' path in
            let%map result = get_file t ~path in
            if not ([%compare.equal: File.t option] result (Some expected))
            then
              raise_s
                [%message
                  "Path missing or incorrectly mapped"
                    (path : string list)
                    (expected : File.t)
                    (result : File.t option)])
      in
      return ())
  ;;

  let%expect_test "is_persisted is maintained correctly" =
    Expect_test_helpers_async.with_temp_dir (fun object_directory ->
      let%bind object_store =
        Object_store.Packed.create ~object_directory ~max_concurrent_reads:4 Validate_sha1
        >>| ok_exn
      in
      let t = create object_store ~root:(Node.empty ()) in
      let pr () = printf "is_persisted %b\n" (is_persisted t) in
      let sha1_file = Sha1.Hex.of_string "62e79c807f27a8a3fbf315e252ea20720c9bc3f5" in
      let%bind () = add_file t ~path:[ "a"; "a" ] sha1_file Regular_file in
      pr ();
      [%expect {| is_persisted false |}];
      let%bind (_ : Sha1.Hex.t) = persist t in
      pr ();
      [%expect {| is_persisted true |}];
      let%bind () = add_file t ~path:[ "a"; "a" ] sha1_file Regular_file in
      pr ();
      [%expect {| is_persisted true |}];
      let%bind (_ : Sha1.Hex.t) = persist t in
      let%bind () =
        add_node
          t
          ~path:[ "a" ]
          (Node.of_disk_hash
             (Sha1.Hex.of_string "bb80b732a47e282685796d55bdf2034becf13ed7"))
      in
      pr ();
      [%expect {| is_persisted true |}];
      let%bind (_ : Sha1.Hex.t) = persist t in
      let%bind () = remove_path t ~path:[ "b" ] in
      pr ();
      [%expect {| is_persisted true |}];
      let%bind (_ : Sha1.Hex.t) = persist t in
      return ())
  ;;
end
