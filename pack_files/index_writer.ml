(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021  Bogdan-Cristian Tataroiu

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

module Object = struct
  type t =
    { pack_pos : int
    ; pack_data_start_pos : int
    ; pack_data_length : int
    ; object_type : Object_type.t
    ; object_length : int
    ; crc32 : int
    ; sha1 : Sha1.Raw.t Set_once.t
    ; delta_parent : t option
    ; mutable delta_children : t list
    ; mutable contents : Bigstring.t option
    }

  [@@@ocaml.warning "-32"]

  let sexp_of_t
        { pack_pos
        ; pack_data_start_pos
        ; pack_data_length
        ; object_type
        ; object_length
        ; crc32
        ; sha1
        ; delta_parent
        ; delta_children
        ; contents
        }
    =
    let sha1 =
      match Set_once.get sha1 with
      | None -> Sexp.Atom "unset"
      | Some sha1 -> [%sexp_of: Sha1.Hex.t] (Sha1.Raw.to_hex sha1)
    in
    [%sexp
      { pack_pos : int
      ; pack_data_start_pos : int
      ; pack_data_length : int
      ; object_type : Object_type.t
      ; object_length : int
      ; crc32 : int
      ; sha1 : Sexp.t
      ; delta_parent =
          (Option.map delta_parent ~f:(fun delta_parent -> delta_parent.pack_pos) : int
                                                                                      option)
      ; delta_children =
          (List.map delta_children ~f:(fun delta_child -> delta_child.pack_pos) : int list)
      ; contents : (Bigstring.t option[@sexp.option])
      }]
  ;;

  [@@@ocaml.warning "+32"]
end

let index_objects =
  let rec index_objects_loop ~objects ~zlib_inflate ~items_left buf ~pos =
    if items_left = 0
    then pos
    else (
      let (T pack_object_type) = Low_level_reader.object_type buf ~pos in
      let object_length = Low_level_reader.object_length buf ~pos in
      let data_start_pos = Low_level_reader.skip_variable_length_integer buf ~pos in
      let data_start_pos, (object_type : Object_type.t), delta_parent =
        match pack_object_type with
        | Commit -> data_start_pos, Commit, None
        | Tree -> data_start_pos, Tree, None
        | Blob -> data_start_pos, Blob, None
        | Tag -> data_start_pos, Tag, None
        | Ofs_delta ->
          let rel_offset =
            Low_level_reader.read_variable_length_relative_offset buf ~pos:data_start_pos
          in
          let delta_parent_pos = pos - rel_offset in
          let (delta_parent_object : Object.t) =
            Hashtbl.find_exn objects delta_parent_pos
          in
          let object_type = delta_parent_object.object_type in
          ( Low_level_reader.skip_variable_length_integer buf ~pos:data_start_pos
          , object_type
          , Some delta_parent_object )
        | Ref_delta -> failwith "Cannot re-index packs with Ref_delta objects"
      in
      Zlib.Inflate.init_or_reset zlib_inflate;
      let data_length =
        Util.feed_parser_data
          zlib_inflate
          ~process:Zlib.Inflate.process
          buf
          ~pos:data_start_pos
      in
      Zlib.Inflate.finalise zlib_inflate;
      let object_ =
        { Object.pack_pos = pos
        ; pack_data_start_pos = data_start_pos
        ; pack_data_length = data_length
        ; object_type
        ; object_length
        ; crc32 =
            Crc32.finalise
              (Crc32.process
                 Crc32.init
                 buf
                 ~pos
                 ~len:(data_start_pos - pos + data_length))
        ; sha1 = Set_once.create ()
        ; delta_parent
        ; delta_children = []
        ; contents = None
        }
      in
      Hashtbl.add_exn objects ~key:pos ~data:object_;
      Option.iter delta_parent ~f:(fun delta_parent ->
        delta_parent.delta_children <- object_ :: delta_parent.delta_children);
      index_objects_loop
        ~objects
        ~zlib_inflate
        ~items_left:(items_left - 1)
        buf
        ~pos:(data_start_pos + data_length))
  in
  fun ~pack_file_mmap ~items_in_pack ->
    let objects = Int.Table.create () in
    let zlib_inflate =
      Zlib.Inflate.create_uninitialised
        ~on_data_chunk:(fun (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) -> ())
    in
    let pos =
      index_objects_loop
        ~objects
        ~zlib_inflate
        ~items_left:items_in_pack
        pack_file_mmap
        ~pos:12
    in
    assert (pos = Bigstring.length pack_file_mmap - Sha1.Raw.length);
    objects
;;

let compute_resulting_delta_object
      (type kind)
      (delta_object_parser :
         (unit, [ `No_base ], [ `No_delta ], [ `No_result ]) Delta_object_parser.t)
      (pack_object_type : kind Pack_object_type.t)
      ~expected_length
      buf
      ~pos
  : (unit, [ `No_base ], [ `No_delta ], [ `Have_result of kind ]) Delta_object_parser.t
  =
  let delta_object_parser =
    Delta_object_parser.begin_zlib_inflate_into_result
      delta_object_parser
      pack_object_type
      ~expected_length
  in
  let (_ : int) =
    Util.feed_parser_data
      delta_object_parser
      ~process:Delta_object_parser.feed_result_zlib_inflate
      buf
      ~pos
  in
  Delta_object_parser.finalise_result_zlib_inflate_exn delta_object_parser
;;

let rec dfs =
  let object_type_buf = Bigstring.create 32 in
  fun delta_object_parser sha1_compute_uninit (object_ : Object.t) buf ->
    let pos = object_.pack_data_start_pos in
    let object_type = Pack_object_type.of_object_type object_.object_type in
    let (T delta_object_parser) =
      match object_.delta_parent with
      | None ->
        let delta_object_parser =
          compute_resulting_delta_object
            delta_object_parser
            object_type
            ~expected_length:object_.object_length
            buf
            ~pos
        in
        Delta_object_parser.T delta_object_parser
      | Some delta_parent ->
        let delta_object_parser =
          compute_resulting_delta_object
            delta_object_parser
            Ref_delta
            ~expected_length:object_.object_length
            buf
            ~pos
        in
        let parent_contents = Option.value_exn delta_parent.contents in
        let delta_object_parser =
          Delta_object_parser.set_result_as_delta delta_object_parser
        in
        let delta_object_parser =
          Delta_object_parser.with_base_buffer_and_length
            delta_object_parser
            object_type
            parent_contents
            ~length:(Bigstring.length parent_contents)
            (* The method below validates that the length of the result is correct. *)
            Delta_object_parser.compute_result
        in
        T delta_object_parser
    in
    let sha1_compute = Sha1.Compute.init_or_reset sha1_compute_uninit in
    let header_len =
      Object_header_writer.write_from_left
        object_type_buf
        ~pos:0
        object_.object_type
        ~object_length:(Delta_object_parser.result_len delta_object_parser)
    in
    Sha1.Compute.process sha1_compute object_type_buf ~pos:0 ~len:header_len;
    Sha1.Compute.process
      sha1_compute
      (Delta_object_parser.result_buf delta_object_parser)
      ~pos:0
      ~len:(Delta_object_parser.result_len delta_object_parser);
    let sha1_compute = Sha1.Compute.finalise sha1_compute in
    let sha1_raw = Sha1.Raw.Volatile.non_volatile (Sha1.Compute.get_raw sha1_compute) in
    Set_once.set_exn object_.sha1 [%here] sha1_raw;
    match object_.delta_children with
    | [] -> ()
    | children ->
      object_.contents
      <- Some
           (Bigstring.sub
              (Delta_object_parser.result_buf delta_object_parser)
              ~pos:0
              ~len:(Delta_object_parser.result_len delta_object_parser));
      let delta_object_parser = Delta_object_parser.reset delta_object_parser in
      List.iter children ~f:(fun child ->
        dfs delta_object_parser sha1_compute_uninit child buf);
      object_.contents <- None
;;

let write_index_file ~index_file ~(objects_in_sha1_order : Object.t array) ~pack_sha1 =
  let items_in_pack = Array.length objects_in_sha1_order in
  let offsets = Index_offsets.create ~items_in_pack in
  let max_uint32_offset = (1 lsl 31) - 1 in
  let uint64_offsets, _ =
    Array.foldi objects_in_sha1_order ~init:(Int.Map.empty, 0) ~f:(fun key acc object_ ->
      if object_.pack_pos <= max_uint32_offset
      then acc
      else (
        let map, cnt = acc in
        Map.add_exn map ~key ~data:cnt, cnt + 1))
  in
  let index_file_size =
    Index_offsets.uint64_offset offsets (Map.length uint64_offsets) + (Sha1.Raw.length * 2)
  in
  let%bind fd = Unix.openfile ~mode:[ `Rdwr; `Creat; `Trunc ] index_file in
  let%map file_mmap =
    Fd.syscall_in_thread_exn ~name:"git-pack-mmap-file" fd (fun file_descr ->
      Bigstring_unix.map_file ~shared:true file_descr index_file_size)
  in
  Bigstring.unsafe_set_uint32_le file_mmap ~pos:0 1666151679;
  Bigstring.unsafe_set_uint32_be file_mmap ~pos:4 2;
  Array.iter objects_in_sha1_order ~f:(fun object_ ->
    let first_byte = (Sha1.Raw.to_string (Set_once.get_exn object_.sha1 [%here])).[0] in
    let pos = Index_offsets.fanout offsets first_byte in
    Bigstring.set_uint32_be_exn
      file_mmap
      ~pos
      (Bigstring.get_uint32_be file_mmap ~pos + 1));
  for idx = 1 to 255 do
    let pos = Index_offsets.fanout offsets (Char.of_int_exn idx) in
    Bigstring.set_uint32_be_exn
      file_mmap
      ~pos
      (Bigstring.get_uint32_be file_mmap ~pos:(pos - 4)
       + Bigstring.get_uint32_be file_mmap ~pos)
  done;
  Array.iteri objects_in_sha1_order ~f:(fun idx object_ ->
    let pos = Index_offsets.sha1 offsets idx in
    Bigstring.From_string.blit
      ~src:(Sha1.Raw.to_string (Set_once.get_exn object_.sha1 [%here]))
      ~src_pos:0
      ~dst:file_mmap
      ~dst_pos:pos
      ~len:Sha1.Raw.length);
  Array.iteri objects_in_sha1_order ~f:(fun idx object_ ->
    let pos = Index_offsets.crc32 offsets idx in
    Bigstring.set_uint32_be_exn file_mmap ~pos object_.crc32);
  Array.iteri objects_in_sha1_order ~f:(fun idx object_ ->
    let pos = Index_offsets.offset offsets idx in
    let offset_value =
      if object_.pack_pos <= max_uint32_offset
      then object_.pack_pos
      else (
        let big_offset = Map.find_exn uint64_offsets idx in
        Bigstring.set_uint64_be_exn
          file_mmap
          ~pos:(Index_offsets.uint64_offset offsets big_offset)
          object_.pack_pos;
        big_offset lor (1 lsl 31))
    in
    Bigstring.unsafe_set_uint32_be file_mmap ~pos offset_value);
  Bigstring.From_string.blit
    ~src:(Sha1.Raw.to_string pack_sha1)
    ~src_pos:0
    ~dst:file_mmap
    ~dst_pos:(index_file_size - (Sha1.Raw.length * 2))
    ~len:Sha1.Raw.length;
  let sha1_compute = Sha1.Compute.create_uninitialised () |> Sha1.Compute.init_or_reset in
  Sha1.Compute.process
    sha1_compute
    file_mmap
    ~pos:0
    ~len:(index_file_size - Sha1.Raw.length);
  let sha1_compute = Sha1.Compute.finalise sha1_compute in
  Bigstring.From_bytes.blit
    ~src:(Sha1.Raw.Volatile.bytes (Sha1.Compute.get_raw sha1_compute))
    ~src_pos:0
    ~dst:file_mmap
    ~dst_pos:(index_file_size - Sha1.Raw.length)
    ~len:Sha1.Raw.length
;;

let index_pack ~pack_file ~pack_file_mmap ~items_in_pack =
  let index_file = String.chop_suffix_exn ~suffix:".pack" pack_file ^ ".idx" in
  let delta_object_parser = Delta_object_parser.create Do_not_validate_sha1 in
  let sha1_compute = Sha1.Compute.create_uninitialised () in
  let objects = index_objects ~pack_file_mmap ~items_in_pack in
  let objects_in_pack_order = Array.of_list (Hashtbl.data objects) in
  Array.sort
    objects_in_pack_order
    ~compare:(Comparable.lift Int.compare ~f:(fun object_ -> object_.Object.pack_pos));
  Array.iter objects_in_pack_order ~f:(fun object_ ->
    match object_.delta_parent with
    | Some _ -> ()
    | None -> dfs delta_object_parser sha1_compute object_ pack_file_mmap);
  let objects_in_sha1_order = Array.of_list (Hashtbl.data objects) in
  Array.sort
    objects_in_sha1_order
    ~compare:
      (Comparable.lift Sha1.Raw.compare ~f:(fun object_ ->
         Set_once.get_exn object_.Object.sha1 [%here]));
  let pack_sha1 =
    Sha1.Raw.of_string
      (Bigstring.To_string.sub
         pack_file_mmap
         ~pos:(Bigstring.length pack_file_mmap - Sha1.Raw.length)
         ~len:Sha1.Raw.length)
  in
  write_index_file ~index_file ~objects_in_sha1_order ~pack_sha1
;;