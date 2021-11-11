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
  { pack_file : string
  ; pack_fd : Fd.t
  ; pack_file_size : int
  ; pack_file_mmap : Bigstring.t
  ; items_in_pack : int
  ; base_object_parser : 'Sha1_validation Base_object_parser.t
  ; delta_object_parser :
      ( 'Sha1_validation
      , [ `No_base ]
      , [ `No_delta ]
      , [ `No_result ] )
        Delta_object_parser.t
  ; sha1_validation : 'Sha1_validation Sha1_validation.t
  ; index : Index_reader.t
  }

let create ~pack_file sha1_validation =
  let pack_file =
    if String.is_suffix ~suffix:".pack" pack_file then pack_file else pack_file ^ ".pack"
  in
  Util.with_file pack_file ~f:(fun pack_fd pack_file_size pack_file_mmap ->
    let open Deferred.Or_error.Let_syntax in
    let items_in_pack = Bigstring.get_uint32_be pack_file_mmap ~pos:8 in
    let%bind index =
      Index_reader.open_existing
        ~pack_file
        ~pack_file_mmap
        ~pack_file_size
        ~items_in_pack
    in
    let result =
      let open Or_error.Let_syntax in
      let%bind () =
        (* At least 4 for signature, 4 for size, 4 for number of items and one raw SHA1 *)
        if pack_file_size < 12 + Sha1.Raw.length
        then Or_error.error_s [%sexp "Pack file impossibly small"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint32_be pack_file_mmap ~pos:0 <> 0x5041434b
        then Or_error.error_s [%sexp "Expected pack signature"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint32_be pack_file_mmap ~pos:4 <> 2
        then Or_error.error_s [%sexp "Expected pack version number 2"]
        else Ok ()
      in
      return
        { pack_file
        ; pack_fd
        ; pack_file_size
        ; pack_file_mmap
        ; items_in_pack
        ; base_object_parser = Base_object_parser.create sha1_validation
        ; delta_object_parser = Delta_object_parser.create sha1_validation
        ; sha1_validation
        ; index
        }
    in
    Deferred.return result)
;;

let write_pack_index ~pack_file =
  let pack_file =
    if String.is_suffix ~suffix:".pack" pack_file then pack_file else pack_file ^ ".pack"
  in
  Util.with_file pack_file ~f:(fun (_ : Fd.t) (_pack_file_size : int) pack_file_mmap ->
    let items_in_pack = Bigstring.get_uint32_be pack_file_mmap ~pos:8 in
    Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
      Index_writer.write_index ~pack_file ~pack_file_mmap ~items_in_pack))
;;

let write_pack_reverse_index ~pack_file =
  let pack_file =
    if String.is_suffix ~suffix:".pack" pack_file then pack_file else pack_file ^ ".pack"
  in
  Util.with_file pack_file ~f:(fun (_ : Fd.t) pack_file_size pack_file_mmap ->
    let items_in_pack = Bigstring.get_uint32_be pack_file_mmap ~pos:8 in
    let%bind.Deferred.Or_error index =
      Index_reader.open_existing
        ~pack_file
        ~pack_file_mmap
        ~pack_file_size
        ~items_in_pack
    in
    Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
      Reverse_index_writer.write_reverse_index index))
;;

let items_in_pack t = t.items_in_pack
let sha1 t ~index = Index_reader.sha1 t.index ~index
let pack_file_object_offset t ~index = Index_reader.pack_file_offset t.index ~index

let object_type t ~index =
  let pos = pack_file_object_offset t ~index in
  Low_level_reader.object_type t.pack_file_mmap ~pos
;;

let object_length t ~index =
  let pos = pack_file_object_offset t ~index in
  Low_level_reader.object_length t.pack_file_mmap ~pos
;;

let find_sha1_index t sha1 = Index_reader.find_sha1_index t.index sha1
let find_sha1_index' t sha1 = Index_reader.find_sha1_index' t.index sha1

module Read_raw_delta_object_function = struct
  let feed_parser_data delta_object_parser object_type ~expected_length buf pos =
    let delta_object_parser =
      Delta_object_parser.begin_zlib_inflate_into_result
        delta_object_parser
        object_type
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

  let sha1 = Sha1.Raw.Volatile.create ()

  let delta_object_base_pos t ~pos ~data_start_pos object_type =
    match (object_type : [ `Delta ] Pack_object_type.t) with
    | Ofs_delta ->
      let rel_offset =
        Low_level_reader.read_variable_length_relative_offset
          t.pack_file_mmap
          ~pos:data_start_pos
      in
      pos - rel_offset
    | Ref_delta ->
      Bigstring.To_bytes.blit
        ~src:t.pack_file_mmap
        ~src_pos:data_start_pos
        ~dst:(Sha1.Raw.Volatile.bytes sha1)
        ~dst_pos:0
        ~len:Sha1.Raw.length;
      let index = Find_result.Volatile.index_exn (find_sha1_index' t sha1) in
      pack_file_object_offset t ~index
  ;;

  let rec impl t ~pos =
    let data_start_pos =
      Low_level_reader.skip_variable_length_integer t.pack_file_mmap ~pos
    in
    match Low_level_reader.object_type t.pack_file_mmap ~pos with
    | T ((Commit | Tree | Blob | Tag) as object_type) ->
      let delta_object_parser =
        feed_parser_data
          t.delta_object_parser
          object_type
          ~expected_length:(Low_level_reader.object_length t.pack_file_mmap ~pos)
          t.pack_file_mmap
          data_start_pos
      in
      Delta_object_parser.T delta_object_parser
    | T ((Ofs_delta | Ref_delta) as delta_object_type) ->
      let base_pos = delta_object_base_pos t ~pos ~data_start_pos delta_object_type in
      let data_start_pos =
        match delta_object_type with
        | Ofs_delta ->
          Low_level_reader.skip_variable_length_integer
            t.pack_file_mmap
            ~pos:data_start_pos
        | Ref_delta -> data_start_pos + Sha1.Raw.length
      in
      let (T delta_object_parser) = impl t ~pos:base_pos in
      let delta_object_parser =
        Delta_object_parser.set_result_as_base delta_object_parser
      in
      let delta_object_parser =
        feed_parser_data
          delta_object_parser
          delta_object_type
          ~expected_length:(Low_level_reader.object_length t.pack_file_mmap ~pos)
          t.pack_file_mmap
          data_start_pos
      in
      let delta_object_parser =
        Delta_object_parser.set_result_as_delta delta_object_parser
      in
      T (Delta_object_parser.compute_result delta_object_parser)
  ;;
end

let read_raw_delta_object = Read_raw_delta_object_function.impl

module Read_raw_object_function = struct
  let sha1_context = Sha1.Compute.create_uninitialised ()
  let buf = Bigstring.create 32

  let impl (type a) (t : a t) ~pack_offset ~on_header ~on_payload (sha1_validation : a) =
    let (T delta_object_parser) = read_raw_delta_object t ~pos:pack_offset in
    let object_type = Delta_object_parser.result_object_type delta_object_parser in
    let len = Delta_object_parser.result_len delta_object_parser in
    on_header object_type ~size:len;
    on_payload (Delta_object_parser.result_buf delta_object_parser) ~pos:0 ~len;
    match t.sha1_validation with
    | Do_not_validate_sha1 -> ()
    | Validate_sha1 ->
      let sha1_context = Sha1.Compute.init_or_reset sha1_context in
      let header_len =
        Object_header_writer.write_from_left buf ~pos:0 object_type ~object_length:len
      in
      Sha1.Compute.process sha1_context buf ~pos:0 ~len:header_len;
      Sha1.Compute.process
        sha1_context
        (Delta_object_parser.result_buf delta_object_parser)
        ~pos:0
        ~len;
      let sha1_context = Sha1.Compute.finalise sha1_context in
      let actual_sha1 =
        Sha1.Hex.Volatile.non_volatile (Sha1.Compute.get_hex sha1_context)
      in
      if [%compare.equal: Sha1.Hex.t] actual_sha1 sha1_validation
      then ()
      else
        raise_s
          [%message
            "Unexpected_sha1"
              (actual_sha1 : Sha1.Hex.t)
              ~expected_sha1:(sha1_validation : Sha1.Hex.t)]
  ;;
end

let read_raw_object' = Read_raw_object_function.impl

let read_raw_object (type a) (t : a t) ~index ~on_header ~on_payload =
  let pack_offset = pack_file_object_offset t ~index in
  read_raw_object'
    t
    ~pack_offset
    ~on_header
    ~on_payload
    (match t.sha1_validation with
     | Do_not_validate_sha1 -> ()
     | Validate_sha1 -> Sha1.Raw.Volatile.to_hex (sha1 t ~index))
;;

module Read_object_function = struct
  let feed_parser_data t pos =
    let (_ : int) =
      Util.feed_parser_data
        t.base_object_parser
        ~process:Base_object_parser.process
        t.pack_file_mmap
        ~pos
    in
    Base_object_parser.finalise t.base_object_parser
  ;;

  let impl
        (type a)
        (t : a t)
        ~pack_offset
        ~on_blob_size
        ~on_blob_chunk
        ~on_commit
        ~on_tree_line
        ~on_tag
        sha1_validation
    =
    let payload_length =
      Low_level_reader.object_length t.pack_file_mmap ~pos:pack_offset
    in
    let data_start_pos =
      Low_level_reader.skip_variable_length_integer t.pack_file_mmap ~pos:pack_offset
    in
    match Low_level_reader.object_type t.pack_file_mmap ~pos:pack_offset with
    | T ((Commit | Tree | Blob | Tag) as object_type) ->
      Base_object_parser.reset_for_reading
        t.base_object_parser
        (Pack_object_type.to_object_type object_type)
        ~payload_length
        ~on_blob_size
        ~on_blob_chunk
        ~on_commit
        ~on_tree_line
        ~on_tag
        sha1_validation;
      feed_parser_data t data_start_pos
    | T (Ofs_delta | Ref_delta) ->
      let (T delta_object_parser) = read_raw_delta_object t ~pos:pack_offset in
      Delta_object_parser.parse_result
        delta_object_parser
        ~on_blob_size
        ~on_blob_chunk
        ~on_commit
        ~on_tree_line
        ~on_tag
        sha1_validation
  ;;
end

let read_object' = Read_object_function.impl

let read_object
      (type a)
      (t : a t)
      ~index
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
  =
  let pack_offset = pack_file_object_offset t ~index in
  read_object'
    t
    ~pack_offset
    ~on_blob_size
    ~on_blob_chunk
    ~on_commit
    ~on_tree_line
    ~on_tag
    (match t.sha1_validation with
     | Do_not_validate_sha1 -> ()
     | Validate_sha1 -> Sha1.Raw.Volatile.to_hex (sha1 t ~index))
;;

module Size = struct
  module Volatile = struct
    type t =
      { mutable size : int
      ; mutable delta_size : int
      ; mutable pack_size : int
      }
    [@@deriving fields]

    let return =
      let value = { size = 0; pack_size = 0; delta_size = 0 } in
      fun ~size ~pack_size ~delta_size ->
        Fields.Direct.set_all_mutable_fields value ~size ~pack_size ~delta_size;
        value
    ;;
  end

  let impl t ~pack_offset =
    let (T pack_object_type) =
      Low_level_reader.object_type t.pack_file_mmap ~pos:pack_offset
    in
    let data_start_pos =
      Low_level_reader.skip_variable_length_integer t.pack_file_mmap ~pos:pack_offset
    in
    let data_start_pos =
      match pack_object_type with
      | Commit | Tree | Blob | Tag -> data_start_pos
      | Ofs_delta ->
        Low_level_reader.skip_variable_length_integer t.pack_file_mmap ~pos:data_start_pos
      | Ref_delta -> data_start_pos + Sha1.Raw.length
    in
    let delta_size = Low_level_reader.object_length t.pack_file_mmap ~pos:pack_offset in
    let delta_object_parser =
      Delta_object_parser.begin_zlib_inflate_into_result
        t.delta_object_parser
        pack_object_type
        ~expected_length:delta_size
    in
    let data_length =
      Util.feed_parser_data
        delta_object_parser
        ~process:Delta_object_parser.feed_result_zlib_inflate
        t.pack_file_mmap
        ~pos:data_start_pos
    in
    let delta_object_parser =
      Delta_object_parser.finalise_result_zlib_inflate_exn delta_object_parser
    in
    let pack_size = data_start_pos + data_length - pack_offset in
    let size =
      match pack_object_type with
      | Commit | Tree | Blob | Tag -> delta_size
      | Ofs_delta | Ref_delta ->
        let result_buf = Delta_object_parser.result_buf delta_object_parser in
        (* The uncompressed delta starts with the length of the expected base object
           and the length of the expected undeltified result encoded as variable length
           integers before the list of delta operations.

           Skip over the first length and return the second one. *)
        let result_pos =
          Low_level_reader.skip_variable_length_integer result_buf ~pos:0
        in
        Low_level_reader.read_variable_length_integer result_buf ~pos:result_pos
    in
    Volatile.return ~size ~delta_size ~pack_size
  ;;
end

let size' = Size.impl

let size t ~index =
  let pack_offset = pack_file_object_offset t ~index in
  size' t ~pack_offset
;;

module Low_level = struct
  let index t = t.index
  let read_object = read_object'
  let read_raw_object = read_raw_object'
  let size = size'
end

module For_testing = struct
  let print_out_pack_file pack_file =
    let%bind t = create ~pack_file Validate_sha1 >>| ok_exn in
    printf "items in pack: %d\n" t.items_in_pack;
    printf "idx | %40s | pack file offset | object length | object type\n" "sha1";
    for index = 0 to t.items_in_pack - 1 do
      printf
        !"%3d | %{Sha1.Hex} | %16d | %13d | %{sexp: Pack_object_type.Flat.t}\n"
        (Find_result.Volatile.index_exn (find_sha1_index' t (sha1 t ~index)))
        (Sha1.Raw.Volatile.to_hex (sha1 t ~index))
        (pack_file_object_offset t ~index)
        (object_length t ~index)
        (object_type t ~index |> Pack_object_type.packed_to_flat)
    done;
    let%map reverse_index = Reverse_index_reader.open_existing t.index >>| ok_exn in
    printf "pack order | index | pack file offset\n";
    for pack_order = 0 to t.items_in_pack - 1 do
      let index = Reverse_index_reader.index_of_pack_order reverse_index ~pack_order in
      let pack_file_offset =
        Reverse_index_reader.pack_file_offset_of_pack_order reverse_index ~pack_order
      in
      let pack_order_index_round_trip =
        Reverse_index_reader.pack_order_of_index reverse_index ~index
      in
      let pack_order_offset_round_trip =
        Reverse_index_reader.pack_order_of_pack_file_offset
          reverse_index
          ~pack_file_offset
      in
      let index_offset_round_trip =
        Reverse_index_reader.index_of_pack_file_offset reverse_index ~pack_file_offset
      in
      printf
        "%4d %2d %2d | %2d %2d | %16d \n"
        pack_order
        pack_order_index_round_trip
        pack_order_offset_round_trip
        index
        index_offset_round_trip
        pack_file_offset
    done;
    for index = 0 to t.items_in_pack - 1 do
      printf !"\n%{Sha1.Hex}\n" (Sha1.Raw.Volatile.to_hex (sha1 t ~index));
      read_raw_object
        t
        ~index
        ~on_header:(fun object_type ~size ->
          printf !"Header data: %{Object_type} size %d\n" object_type size)
        ~on_payload:(fun (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) -> ());
      read_object
        t
        ~index
        ~on_blob_size:(fun size -> printf "Blob size: %d\n" size)
        ~on_blob_chunk:(fun buf ~pos ~len ->
          printf "Blob chunk: %S\n" (Bigstring.To_string.sub buf ~pos ~len))
        ~on_commit:(printf !"%{sexp: Commit.t}\n")
        ~on_tree_line:(fun file_mode sha1 ~name ->
          printf
            !"Tree line: %{sexp: File_mode.t} %{Sha1.Hex} %s\n"
            file_mode
            (Sha1.Raw.Volatile.to_hex sha1)
            name)
        ~on_tag:(printf !"%{sexp: Tag.t}\n")
    done
  ;;
end

let%expect_test "read pack" =
  let pack_contents =
    "PACK\000\000\000\002\000\000\000\005\198\tx\156\029\204A\n\
     \1940\016\133\225}N1{Q\210i\146\018\144\"z\005/0I\166%b\155\146\142\162\183\183\246\173~x\240\149\240\224(0$\029P\251\198z\199\206\248\193E2\022\027t\024[\210\145R\208I\027d%\223\133!\150i\202\162\132Fx\011\175{\141\\\225Z\198D\243\241V\243*\153f\184\147P-\249\005\231\176?\023\254\208\180<\249\180\001=4\214t\173G\139\029\028\2446\165\254\022l\150\250\001\017\151/U\154\012x\156\165\140A\n\
     \1940\016\000\239y\197\222EI\218$\018\144\"\250\005?\176\155l4`\026I\183\224\243-\250\004\2318\003#\157\025h\012\t9b\014\020\173v\137\025\143C\2066EJ\142\200xoF&\147\021\174\242h\029.\237\158p\222_{Y\164\224\0127\020\236\173\172p\162o9\243\027\235\235\201\135\216\234\004\198Y\239\131\025\180\133\157\222P\155\173E\132\255\030)\225E\224\183S\031\135HA\131;x\156K\203,*.QH\203\204I\229\002\000\025\199\003\243<x\156+NM\206\207KQH\203\204I\229\002\000\0302\004G\163\007x\156340031QHd0\176\255\217x\164c\135\2086\197\248\218\237\147x\223n\222:w\150!D2\137A&\210\169\234\142\183B\148:o\127#\179\128\165g\210\243Y\221&\006@\160\144\204pa\147{nT\254\241=o\253\243~8n\127\206\164\191\150e\178\161\017X2\133!c\239\235\213\203\191H\253_\184j\207\211\224\2273$f86\174\004\000\188\152.\205\184\131\178>\246Ls\189Y\t\202\243Ya)\203\b\022\240}"
  in
  let index_contents =
    "\255tOc\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\028YBz\220K \
     Z'\r\143\129\003\0169Ib\231\154\1390?\249\129\196\136\184\018\182!_}\183\146\r\237\179\181\157\154\172_6\128\023\231<\172Y\156}\253w\1896\218+\129n\175\179\157\174\202\249\188@]\238\167/\244\220\189[\177f\019\235\031\210\239\140q\004\022\243\139\223n\132\135c\004\134\131\014\220l\127\137\244h\223\248\191\197\209\135\177\130\255\231\214\159\229\193_\201O\000\000\001,\000\000\001\024\000\000\000\012\000\000\001A\000\000\000\150\184\131\178>\246Ls\189Y\t\202\243Ya)\203\b\022\240}|\216\027\2209\007\228\001\230\145\203R\132\166\189w\220\235\1864"
  in
  let reverse_index_contents =
    "RIDX\000\000\000\001\000\000\000\001\000\000\000\002\000\000\000\004\000\000\000\001\000\000\000\000\000\000\000\003\184\131\178>\246Ls\189Y\t\202\243Ya)\203\b\022\240}\205\167'\231y\0184\129\215;\ta\154k\227\149\255}\219\017"
  in
  let pack_name = "pack-b883b23ef64c73bd5909caf3596129cb0816f07d" in
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    Expect_test_time_zone.with_fixed_time_zone_async (fun () ->
      let pack_file = dir ^/ pack_name ^ ".pack" in
      let index_file = dir ^/ pack_name ^ ".idx" in
      let reverse_index_file = dir ^/ pack_name ^ ".rev" in
      let%bind () = Writer.save pack_file ~contents:pack_contents in
      let%bind () = write_pack_index ~pack_file >>| ok_exn in
      let%bind () = write_pack_reverse_index ~pack_file >>| ok_exn in
      let%bind generated_index_contents = Reader.file_contents index_file in
      let%bind generated_reverse_index_contents =
        Reader.file_contents reverse_index_file
      in
      if String.( <> ) index_contents generated_index_contents
      then
        printf
          "Generated index files differ:\n\nExpected:\n%S\n\nActual:\n%S\n\n"
          index_contents
          generated_index_contents;
      if String.( <> ) reverse_index_contents generated_reverse_index_contents
      then
        printf
          "Generated reverse index files differ:\n\nExpected:\n%S\n\nActual:\n%S\n\n"
          reverse_index_contents
          generated_reverse_index_contents;
      [%expect {||}];
      let%bind () = For_testing.print_out_pack_file (dir ^/ pack_name) in
      [%expect
        {|
        items in pack: 5
        idx |                                     sha1 | pack file offset | object length | object type
          0 | 1c59427adc4b205a270d8f810310394962e79a8b |              300 |            12 | Blob
          1 | 303ff981c488b812b6215f7db7920dedb3b59d9a |              280 |            11 | Blob
          2 | ac5f368017e73cac599c7dfd77bd36da2b816eaf |               12 |           150 | Tag
          3 | b39daecaf9bc405deea72ff4dcbd5bb16613eb1f |              321 |           115 | Tree
          4 | d2ef8c710416f38bdf6e8487630486830edc6c7f |              150 |           202 | Commit
        pack order | index | pack file offset
           0  0  0 |  2  2 |               12
           1  1  1 |  4  4 |              150
           2  2  2 |  1  1 |              280
           3  3  3 |  0  0 |              300
           4  4  4 |  3  3 |              321

        1c59427adc4b205a270d8f810310394962e79a8b
        Header data: Blob size 12
        Blob size: 12
        Blob chunk: "second file\n"

        303ff981c488b812b6215f7db7920dedb3b59d9a
        Header data: Blob size 11
        Blob size: 11
        Blob chunk: "first file\n"

        ac5f368017e73cac599c7dfd77bd36da2b816eaf
        Header data: Tag size 150
        ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
         (tag vtest)
         (tagger
          (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
            (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC+0))))
         (description "test tag\n"))

        b39daecaf9bc405deea72ff4dcbd5bb16613eb1f
        Header data: Tree size 115
        Tree line: Non_executable_file 303ff981c488b812b6215f7db7920dedb3b59d9a a
        Tree line: Non_executable_file 1c59427adc4b205a270d8f810310394962e79a8b b
        Tree line: Directory d0b2476d5a6fc7bced4f6ef841b7e7022fad0493 c
        Tree line: Link 68bdebaba7f41affa1aabce553c79818984181a9 d

        d2ef8c710416f38bdf6e8487630486830edc6c7f
        Header data: Commit size 202
        ((tree b39daecaf9bc405deea72ff4dcbd5bb16613eb1f) (parents ())
         (author
          ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
           (timestamp (2019-01-05 07:26:44.000000000-05:00)) (zone UTC+0)))
         (committer
          ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
           (timestamp (2019-01-05 07:26:44.000000000-05:00)) (zone UTC+0)))
         (encoding ()) (merge_tags ()) (gpg_signature ())
         (description "test commit\n")) |}];
      Deferred.unit))
;;
