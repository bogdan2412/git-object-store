(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020  Bogdan-Cristian Tataroiu

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

module Node0 = struct
  type _ state =
    | Not_loaded : Sha1.Hex.t -> [ `Not_loaded ] state
    | Loading : Sha1.Hex.t * [ `Loaded ] state Deferred.t -> [ `Not_loaded ] state
    | Loaded_and_persisted :
        { sha1 : Sha1.Hex.t
        ; directories : t String.Map.t
        ; files : Sha1.Hex.t String.Map.t
        }
        -> [ `Loaded ] state
    | Loaded_and_not_persisted :
        { directories : t String.Map.t
        ; files : Sha1.Hex.t String.Map.t
        }
        -> [ `Loaded ] state

  and packed_state = T : _ state -> packed_state

  and t = { mutable state : packed_state }

  let empty () =
    { state =
        T
          (Loaded_and_not_persisted
             { directories = String.Map.empty; files = String.Map.empty })
    }
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
  }

let create object_store ~root =
  { object_store
  ; root
  ; loaded = Sha1.Hex.Table.create ()
  ; mutation_sequencer =
      Sequencer.create ~continue_on_error:false (Sequencer_witness.create ())
  }
;;

let root t = t.root

module Node = struct
  include Node0

  let ensure_loaded t node : [ `Loaded ] state Deferred.t =
    match node.state with
    | T (Loaded_and_persisted _ as loaded) -> return loaded
    | T (Loaded_and_not_persisted _ as loaded) -> return loaded
    | T (Loading (_, loaded)) -> loaded
    | T (Not_loaded sha1) ->
      let loaded_deferred =
        Hashtbl.find_or_add t.loaded sha1 ~default:(fun () ->
          let files = ref String.Map.empty in
          let directories = ref String.Map.empty in
          let%map () =
            Object_store.Packed.read_tree
              t.object_store
              sha1
              ~on_tree_line:(fun mode sha1 ~name ->
                match mode with
                | Non_executable_file ->
                  files
                  := Map.add_exn
                       !files
                       ~key:name
                       ~data:(Sha1.Raw.Volatile.to_hex sha1)
                | Directory ->
                  directories
                  := Map.add_exn
                       !directories
                       ~key:name
                       ~data:
                         { state = T (Not_loaded (Sha1.Raw.Volatile.to_hex sha1)) }
                | Executable_file | Link | Git_submodule ->
                  failwith "Read tree line with unexpected file mode")
          in
          Loaded_and_persisted { sha1; files = !files; directories = !directories })
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
    | T (Loaded_and_not_persisted { files; directories }) ->
      let%bind directory_sha1s =
        Map.map directories ~f:(fun node -> persist' t node witness)
        |> Map.to_alist
        |> List.map ~f:(fun (name, def) ->
          let%map def = def in
          name, def)
        |> Deferred.all
      in
      let git_tree_writer =
        Tree_writer.create_uninitialised
          ~object_directory:(Object_store.Packed.object_directory t.object_store)
      in
      let%bind () = Tree_writer.init_or_reset git_tree_writer in
      let sha1_raw = Sha1.Raw.Volatile.create () in
      List.iter directory_sha1s ~f:(fun (directory, sha1) ->
        Sha1.Raw.Volatile.of_hex sha1 sha1_raw;
        Tree_writer.write_tree_line' git_tree_writer Directory sha1_raw ~name:directory);
      Map.iteri files ~f:(fun ~key:file ~data:sha1 ->
        Sha1.Raw.Volatile.of_hex sha1 sha1_raw;
        Tree_writer.write_tree_line'
          git_tree_writer
          Non_executable_file
          sha1_raw
          ~name:file);
      let%bind sha1_raw = Tree_writer.finalise git_tree_writer in
      let sha1 = Sha1.Raw.to_hex sha1_raw in
      let state = Loaded_and_persisted { sha1; files; directories } in
      let%map state =
        Hashtbl.find_or_add t.loaded sha1 ~default:(fun () -> return state)
      in
      node.state <- T state;
      sha1
  ;;

  let persist t node = Throttle.enqueue t.mutation_sequencer (persist' t node)

  let rec get_file t node ~path =
    let%bind state = ensure_loaded t node in
    match path with
    | [] -> return None
    | [ file ] ->
      (match Map.find (files state) file with
       | Some sha1 -> return (Some sha1)
       | None -> return None)
    | directory :: rest ->
      (match Map.find (directories state) directory with
       | Some node -> get_file t node ~path:rest
       | None -> return None)
  ;;

  let rec get_node t node ~path =
    let%bind state = ensure_loaded t node in
    match path with
    | [] -> return (Some node)
    | directory :: rest ->
      (match Map.find (directories state) directory with
       | Some node -> get_node t node ~path:rest
       | None -> return None)
  ;;

  let rec add_file' t node ~path sha1 (witness : Sequencer_witness.t) =
    match path with
    | [] -> failwith "Empty path"
    | [ file ] ->
      let%bind state = ensure_loaded t node in
      (match Map.find (files state) file with
       | Some old_sha1 when [%compare.equal: Sha1.Hex.t] old_sha1 sha1 ->
         (* File already exists and is not being changed. *)
         return node
       | None | Some _ ->
         let state =
           Loaded_and_not_persisted
             { files = Map.set (files state) ~key:file ~data:sha1
             ; directories = directories state
             }
         in
         return { state = T state })
    | directory :: drest ->
      let%bind state = ensure_loaded t node in
      let%bind new_directory =
        match Map.find (directories state) directory with
        | Some node -> add_file' t node ~path:drest sha1 witness
        | None -> add_file' t (empty ()) ~path:drest sha1 witness
      in
      (match is_persisted new_directory with
       | true -> (* Child directory was not changed *) return node
       | false ->
         let state =
           Loaded_and_not_persisted
             { files = files state
             ; directories = Map.set (directories state) ~key:directory ~data:new_directory
             }
         in
         return { state = T state })
  ;;

  let add_file t node ~path sha1 =
    Throttle.enqueue t.mutation_sequencer (fun witness ->
      add_file' t node ~path sha1 witness)
  ;;

  let rec add_node' t node ~path new_node (witness : Sequencer_witness.t) =
    match path with
    | [] -> failwith "Empty path"
    | [ directory ] ->
      let%bind state = ensure_loaded t node in
      let old_directory_sha1 =
        Map.find (directories state) directory |> Option.bind ~f:sha1
      in
      let new_directory_sha1 = sha1 new_node in
      (match old_directory_sha1, new_directory_sha1 with
       | Some old_sha1, Some new_sha1 when [%compare.equal: Sha1.Hex.t] old_sha1 new_sha1
         ->
         (* Directory already exists and is not being changed. *)
         return node
       | _ ->
         let state =
           Loaded_and_not_persisted
             { files = files state
             ; directories = Map.set (directories state) ~key:directory ~data:new_node
             }
         in
         return { state = T state })
    | directory :: drest ->
      let%bind state = ensure_loaded t node in
      let%bind new_directory =
        match Map.find (directories state) directory with
        | Some node -> add_node' t node ~path:drest new_node witness
        | None -> add_node' t (empty ()) ~path:drest new_node witness
      in
      (match is_persisted new_directory with
       | true ->
         (* Child directory was not changed *)
         return node
       | false ->
         let state =
           Loaded_and_not_persisted
             { files = files state
             ; directories = Map.set (directories state) ~key:directory ~data:new_directory
             }
         in
         return { state = T state })
  ;;

  let add_node t node ~path new_node =
    Throttle.enqueue t.mutation_sequencer (fun witness ->
      add_node' t node ~path new_node witness)
  ;;

  let rec remove_path' t node ~path (witness : Sequencer_witness.t) =
    match path with
    | [] -> return None
    | [ leaf ] ->
      let%bind state = ensure_loaded t node in
      (match Map.find (files state) leaf, Map.find (directories state) leaf with
       | None, None ->
         (* Path does not exist, node remains unchanged. *) return (Some node)
       | _ ->
         let state =
           Loaded_and_not_persisted
             { files = Map.change (files state) leaf ~f:(Fn.const None)
             ; directories = Map.change (directories state) leaf ~f:(Fn.const None)
             }
         in
         return (Some { state = T state }))
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
                }
            in
            return (Some { state = T state })))
  ;;

  let remove_path t node ~path =
    Throttle.enqueue t.mutation_sequencer (fun witness ->
      remove_path' t node ~path witness)
  ;;
end

let get_file t ~path = Node.get_file t t.root ~path
let get_node t ~path = Node.get_node t t.root ~path

let add_file t ~path sha1 =
  Throttle.enqueue t.mutation_sequencer (fun witness ->
    let%map root = Node.add_file' t t.root ~path sha1 witness in
    t.root <- root)
;;

let add_node t ~path node =
  Throttle.enqueue t.mutation_sequencer (fun witness ->
    let%map root = Node.add_node' t t.root ~path node witness in
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

let%test_module _ =
  (module struct
    let gen_sha1 seed =
      String.init Sha1.Hex.length ~f:(fun (_ : int) ->
        let value = Random.State.int seed 16 in
        if value <= 9
        then Char.of_int_exn (Char.to_int '0' + value)
        else Char.of_int_exn (Char.to_int 'a' + value - 10))
      |> Sha1.Hex.of_string
    ;;

    let%expect_test "Randomised test" =
      Expect_test_helpers.with_temp_dir (fun object_directory ->
        let%bind object_store =
          Object_store.Packed.create
            ~object_directory
            ~max_concurrent_reads:4
            Validate_sha1
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
          Hashtbl.set added ~key:(String.concat path ~sep:"/") ~data:sha1;
          add_file t ~path sha1
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
        let%bind () = [%expect {| 95d55928fcc1829e6174e5ed798c34771f326b5d |}] in
        let t = create object_store ~root:(Node.of_disk_hash sha1) in
        let%bind () = random_adds t 100 in
        let%bind sha1 = persist t in
        printf !"%{Sha1.Hex}" sha1;
        let%bind () = [%expect {| b0d688b87e5954d9fa8a1a1d1b0e44453c692357 |}] in
        let t = create object_store ~root:(Node.of_disk_hash sha1) in
        let%bind () = random_adds t 100 in
        let%bind sha1 = persist t in
        printf !"%{Sha1.Hex}" sha1;
        let%bind () = [%expect {| 39215c913b9a336b8da9a6e34d45dc78204dc366 |}] in
        let%bind () = random_removes t 50 in
        let%bind () = random_adds t 100 in
        let%bind () = random_removes t 20 in
        let%bind sha1 = persist t in
        printf !"%{Sha1.Hex}" sha1;
        let%bind () = [%expect {| be5dc333d0bdd304208b2a02def6977554bc9fc3 |}] in
        let%bind () =
          Deferred.List.iter (Hashtbl.to_alist added) ~f:(fun (path, expected_sha1) ->
            let path = String.split ~on:'/' path in
            let%map result = get_file t ~path in
            if not ([%compare.equal: Sha1.Hex.t option] result (Some expected_sha1))
            then
              raise_s
                [%message
                  "Path missing or incorrectly mapped"
                    (path : string list)
                    (expected_sha1 : Sha1.Hex.t)
                    (result : Sha1.Hex.t option)])
        in
        return ())
    ;;

    let%expect_test "is_persisted is maintained correctly" =
      Expect_test_helpers.with_temp_dir (fun object_directory ->
        let%bind object_store =
          Object_store.Packed.create
            ~object_directory
            ~max_concurrent_reads:4
            Validate_sha1
          >>| ok_exn
        in
        let t = create object_store ~root:(Node.empty ()) in
        let pr () = printf "is_persisted %b\n" (is_persisted t) in
        let sha1_file = Sha1.Hex.of_string "62e79c807f27a8a3fbf315e252ea20720c9bc3f5" in
        let%bind () = add_file t ~path:[ "a"; "a" ] sha1_file in
        pr ();
        let%bind () = [%expect {| is_persisted false |}] in
        let%bind (_ : Sha1.Hex.t) = persist t in
        pr ();
        let%bind () = [%expect {| is_persisted true |}] in
        let%bind () = add_file t ~path:[ "a"; "a" ] sha1_file in
        pr ();
        let%bind () = [%expect {| is_persisted true |}] in
        let%bind (_ : Sha1.Hex.t) = persist t in
        let%bind () =
          add_node
            t
            ~path:[ "a" ]
            (Node.of_disk_hash
               (Sha1.Hex.of_string "bb80b732a47e282685796d55bdf2034becf13ed7"))
        in
        pr ();
        let%bind () = [%expect {| is_persisted true |}] in
        let%bind (_ : Sha1.Hex.t) = persist t in
        let%bind () = remove_path t ~path:[ "b" ] in
        pr ();
        let%bind () = [%expect {| is_persisted true |}] in
        let%bind (_ : Sha1.Hex.t) = persist t in
        return ())
    ;;
  end)
;;
