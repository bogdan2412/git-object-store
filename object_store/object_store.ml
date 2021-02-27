(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2021  Bogdan-Cristian Tataroiu

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

type 'Sha1_validation t =
  { object_directory : string
  ; packs : (string * 'Sha1_validation Pack_reader.t) list
  ; sha1_buf : Sha1.Raw.Volatile.t
  ; read_throttle :
      ('Sha1_validation Object_reader.t * 'Sha1_validation Object_reader.Raw.t) Throttle.t
  ; sha1_validation : 'Sha1_validation Sha1_validation.t
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

let unexpected_raw_header (_ : Object_type.t) ~size:(_ : int) =
  failwith "Unexpected raw header"
;;

let unexpected_raw_payload (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) =
  failwith "Unexpected raw payload chunk"
;;

let create ~object_directory ~max_concurrent_reads sha1_validation =
  let open Deferred.Or_error.Let_syntax in
  let%bind packs =
    Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
      let open Deferred.Let_syntax in
      let%bind () = Unix.mkdir ~p:() (object_directory ^/ "pack") in
      Sys.readdir (object_directory ^/ "pack"))
  in
  let%map packs =
    Array.to_list packs
    |> List.filter ~f:(fun pack_file -> String.is_suffix ~suffix:".pack" pack_file)
    |> Deferred.Or_error.List.map ~how:`Sequential ~f:(fun pack_file ->
      let pack_file = object_directory ^/ "pack" ^/ pack_file in
      let%map pack_reader = Pack_reader.create ~pack_file sha1_validation in
      pack_file, pack_reader)
  in
  let read_throttle =
    Throttle.create_with
      ~continue_on_error:true
      (List.init max_concurrent_reads ~f:(fun (_ : int) ->
         ( Object_reader.create
             ~on_blob_size:unexpected_blob_size
             ~on_blob_chunk:unexpected_blob_chunk
             ~on_commit:unexpected_commit
             ~on_tree_line:unexpected_tree_line
             ~on_tag:unexpected_tag
             ~on_error:Error.raise
             sha1_validation
         , Object_reader.Raw.create
             ~on_header:unexpected_raw_header
             ~on_payload:unexpected_raw_payload
             ~on_error:Error.raise
             sha1_validation )))
  in
  { object_directory
  ; packs
  ; sha1_buf = Sha1.Raw.Volatile.create ()
  ; read_throttle
  ; sha1_validation
  }
;;

let object_directory t = t.object_directory

module Find_result : sig
  module Volatile : sig
    (** Do not keep references to instances of this type as they will be mutated
        on every call to [find_object]. *)
    type 'Sha1_validation t = private
      | In_pack_file of
          { mutable pack : 'Sha1_validation Pack_reader.t
          ; mutable index : int
          }
      | Unpacked_file_if_exists of { mutable path : string }

    val in_pack_file
      :  'Sha1_validation Sha1_validation.t
      -> 'Sha1_validation Pack_reader.t
      -> index:int
      -> 'Sha1_validation t

    val unpacked_file_if_exists
      :  'Sha1_validation Sha1_validation.t
      -> string
      -> 'Sha1_validation t
  end
end = struct
  module Volatile = struct
    type 'Sha1_validation t =
      | In_pack_file of
          { mutable pack : 'Sha1_validation Pack_reader.t
          ; mutable index : int
          }
      | Unpacked_file_if_exists of { mutable path : string }

    let unit_in_pack_file : unit Pack_reader.t -> index:int -> unit t =
      let value = In_pack_file { pack = Obj.magic (); index = 0 } in
      fun pack ~index ->
        (match value with
         | In_pack_file record ->
           record.pack <- pack;
           record.index <- index
         | _ -> assert false);
        value
    ;;

    let sha1_in_pack_file : Sha1.Hex.t Pack_reader.t -> index:int -> Sha1.Hex.t t =
      let value = In_pack_file { pack = Obj.magic (); index = 0 } in
      fun pack ~index ->
        (match value with
         | In_pack_file record ->
           record.pack <- pack;
           record.index <- index
         | _ -> assert false);
        value
    ;;

    let in_pack_file
          (type a)
          (sha1_validation : a Sha1_validation.t)
          (pack : a Pack_reader.t)
          ~index
      : a t
      =
      match sha1_validation with
      | Do_not_validate_sha1 -> unit_in_pack_file pack ~index
      | Validate_sha1 -> sha1_in_pack_file pack ~index
    ;;

    let unit_unpacked_file_if_exists : string -> unit t =
      let value = Unpacked_file_if_exists { path = "" } in
      fun path ->
        (match value with
         | Unpacked_file_if_exists record -> record.path <- path
         | _ -> assert false);
        value
    ;;

    let sha1_unpacked_file_if_exists : string -> Sha1.Hex.t t =
      let value = Unpacked_file_if_exists { path = "" } in
      fun path ->
        (match value with
         | Unpacked_file_if_exists record -> record.path <- path
         | _ -> assert false);
        value
    ;;

    let unpacked_file_if_exists (type a) (sha1_validation : a Sha1_validation.t) path
      : a t
      =
      match sha1_validation with
      | Do_not_validate_sha1 -> unit_unpacked_file_if_exists path
      | Validate_sha1 -> sha1_unpacked_file_if_exists path
    ;;
  end
end

let unpacked_disk_path t sha1 =
  let sha1_string = Sha1.Hex.to_string sha1 in
  Filename.concat
    (Filename.concat t.object_directory (String.sub sha1_string ~pos:0 ~len:2))
    (String.sub sha1_string ~pos:2 ~len:(Sha1.Hex.length - 2))
;;

let find_object =
  let rec loop_packs t sha1 packs =
    match packs with
    | [] ->
      let path = unpacked_disk_path t sha1 in
      Find_result.Volatile.unpacked_file_if_exists t.sha1_validation path
    | (_, pack) :: packs ->
      (match Pack_reader.find_sha1_index' pack t.sha1_buf with
       | None -> loop_packs t sha1 packs
       | Some { index } -> Find_result.Volatile.in_pack_file t.sha1_validation pack ~index)
  in
  fun t sha1 ->
    Sha1.Raw.Volatile.of_hex sha1 t.sha1_buf;
    loop_packs t sha1 t.packs
;;

let read_object_from_unpacked_file
      (type a)
      (t : a t)
      (sha1 : Sha1.Hex.t)
      path
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      ~push_back
  =
  Throttle.enqueue t.read_throttle (fun (object_reader, _) ->
    Object_reader.set_on_blob
      object_reader
      ~on_size:on_blob_size
      ~on_chunk:on_blob_chunk;
    Object_reader.set_on_commit object_reader on_commit;
    Object_reader.set_on_tree_line object_reader on_tree_line;
    Object_reader.set_on_tag object_reader on_tag;
    Object_reader.read_file'
      object_reader
      ~file:path
      ~push_back
      (match t.sha1_validation with
       | Do_not_validate_sha1 -> ()
       | Validate_sha1 -> sha1))
;;

let read_object
      (type a)
      (t : a t)
      sha1
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      ~push_back
  =
  match find_object t sha1 with
  | In_pack_file { pack; index } ->
    Pack_reader.read_object
      pack
      ~index
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag;
    Deferred.unit
  | Unpacked_file_if_exists { path } ->
    (match%bind Sys.is_file_exn path with
     | false -> raise_s [%message "Object does not exist" (sha1 : Sha1.Hex.t)]
     | true ->
       read_object_from_unpacked_file
         t
         sha1
         path
         ~on_blob_size
         ~on_blob_chunk
         ~on_commit
         ~on_tree_line
         ~on_tag
         ~push_back)
;;

let read_raw_object_from_unpacked_file
      (type a)
      (t : a t)
      (sha1 : Sha1.Hex.t)
      path
      ~on_header
      ~on_payload
      ~push_back
  =
  Throttle.enqueue t.read_throttle (fun (_, object_raw_reader) ->
    Object_reader.Raw.set_on_header object_raw_reader on_header;
    Object_reader.Raw.set_on_payload object_raw_reader on_payload;
    Object_reader.Raw.read_file'
      object_raw_reader
      ~file:path
      ~push_back
      (match t.sha1_validation with
       | Do_not_validate_sha1 -> ()
       | Validate_sha1 -> sha1))
;;

let read_raw_object (type a) (t : a t) sha1 ~on_header ~on_payload ~push_back =
  match find_object t sha1 with
  | In_pack_file { pack; index } ->
    Pack_reader.read_raw_object pack ~index ~on_header ~on_payload;
    Deferred.unit
  | Unpacked_file_if_exists { path } ->
    (match%bind Sys.is_file_exn path with
     | false -> raise_s [%message "Object does not exist" (sha1 : Sha1.Hex.t)]
     | true ->
       read_raw_object_from_unpacked_file t sha1 path ~on_header ~on_payload ~push_back)
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

let size t sha1 =
  match find_object t sha1 with
  | In_pack_file { pack; index } ->
    let { Pack_reader.Size.Volatile.size; delta_size = _; pack_size = _ } =
      Pack_reader.size pack ~index
    in
    return size
  | Unpacked_file_if_exists { path } ->
    let received_size = ref false in
    let returned_size = ref (-1) in
    let%bind () =
      read_raw_object_from_unpacked_file
        t
        sha1
        path
        ~on_header:(fun (_ : Object_type.t) ~size ->
          received_size := true;
          returned_size := size)
        ~on_payload:(fun (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) -> ())
        ~push_back:(fun () ->
          if !received_size then return `Reader_closed else return `Ok)
    in
    if !returned_size <= -1
    then
      raise_s
        [%message
          "BUG - negative size returned" (sha1 : Sha1.Hex.t) ~size:(!returned_size : int)]
    else return !returned_size
;;

let with_on_disk_file t sha1 ~f =
  let path = unpacked_disk_path t sha1 in
  match%bind Sys.is_file_exn path with
  | true -> f path
  | false ->
    Sha1.Raw.Volatile.of_hex sha1 t.sha1_buf;
    let pack_and_index =
      List.find_map t.packs ~f:(fun (_, pack) ->
        match Pack_reader.find_sha1_index' pack t.sha1_buf with
        | None -> None
        | Some { index } -> Some (pack, index))
    in
    (match pack_and_index with
     | None -> raise_s [%message "Object not found" (sha1 : Sha1.Hex.t)]
     | Some (pack, index) ->
       let object_type_and_length = Set_once.create () in
       let data = Set_once.create () in
       Pack_reader.read_raw_object
         pack
         ~index
         ~on_header:(fun object_type ~size ->
           Set_once.set_exn object_type_and_length [%here] (object_type, size))
         ~on_payload:(fun buf ~pos ~len ->
           Set_once.set_exn data [%here] (Bigstring.sub buf ~pos ~len));
       let object_type, length = Set_once.get_exn object_type_and_length [%here] in
       let writer =
         Object_writer.With_header.Known_size.create_uninitialised
           ~object_directory:t.object_directory
       in
       let%bind () =
         Object_writer.With_header.Known_size.init_or_reset
           writer
           object_type
           ~length
           ~dry_run:false
       in
       let data = Set_once.get_exn data [%here] in
       Object_writer.With_header.Known_size.append_data
         writer
         data
         ~pos:0
         ~len:(Bigstring.length data);
       let%bind saved_sha1 = Object_writer.With_header.Known_size.finalise_exn writer in
       let saved_sha1 = Sha1.Raw.to_hex saved_sha1 in
       assert ([%compare.equal: Sha1.Hex.t] sha1 saved_sha1);
       Monitor.protect
         ~rest:`Raise
         ~run:`Now
         (fun () -> f path)
         ~finally:(fun () -> Unix.unlink path))
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

let all_pack_files_in_store t = t.packs

let all_unpacked_objects_in_store t =
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
              Hashtbl.add_exn
                result
                ~key:(Sha1.Hex.of_string (first_child ^ file))
                ~data:path
            else Log.Global.info "Unexpected file: %s" path))
      else (
        Log.Global.info "Unexpected file or directory: %s" path;
        Deferred.unit))
  in
  return result
;;

let all_objects_in_store t =
  let%bind result = all_unpacked_objects_in_store t in
  let result =
    Hashtbl.map result ~f:(fun path -> [ Object_location.Unpacked_file path ])
  in
  List.iter t.packs ~f:(fun (pack_file, pack_reader) ->
    let items_in_pack = Pack_reader.items_in_pack pack_reader in
    for index = 0 to items_in_pack - 1 do
      Hashtbl.add_multi
        result
        ~key:(Sha1.Raw.Volatile.to_hex (Pack_reader.sha1 pack_reader ~index))
        ~data:(Object_location.In_pack_file { pack_file; index })
    done);
  return result
;;

module Packed = struct
  type 'a non_packed = 'a t
  type t = T : _ non_packed -> t

  let create ~object_directory ~max_concurrent_reads sha1_validation =
    create ~object_directory ~max_concurrent_reads sha1_validation
    >>|? fun object_store -> T object_store
  ;;

  let object_directory (T object_store) = object_directory object_store

  let read_object
        (T object_store)
        sha1
        ~on_blob_size
        ~on_blob_chunk
        ~on_commit
        ~on_tree_line
        ~on_tag
        ~push_back
    =
    read_object
      object_store
      sha1
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      ~push_back
  ;;

  let read_raw_object (T object_store) sha1 ~on_header ~on_payload ~push_back =
    read_raw_object object_store sha1 ~on_header ~on_payload ~push_back
  ;;

  let read_blob (T object_store) sha1 ~on_size ~on_chunk ~push_back =
    read_blob object_store sha1 ~on_size ~on_chunk ~push_back
  ;;

  let read_commit (T object_store) sha1 ~on_commit =
    read_commit object_store sha1 ~on_commit
  ;;

  let read_tree (T object_store) sha1 ~on_tree_line =
    read_tree object_store sha1 ~on_tree_line
  ;;

  let read_tag (T object_store) sha1 ~on_tag = read_tag object_store sha1 ~on_tag
  let size (T object_store) sha1 = size object_store sha1
  let with_on_disk_file (T object_store) sha1 ~f = with_on_disk_file object_store sha1 ~f

  module Object_location = Object_location

  let all_unpacked_objects_in_store (T object_store) =
    all_unpacked_objects_in_store object_store
  ;;

  let all_objects_in_store (T object_store) = all_objects_in_store object_store
end
