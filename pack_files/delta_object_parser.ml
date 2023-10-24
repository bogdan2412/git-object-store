(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021-2023  Bogdan-Cristian Tataroiu

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

type ('Sha1_validation, _, _, _) t =
  { mutable base_buf : Bigstring.t
  ; mutable base_len : int
  ; mutable delta_buf : Bigstring.t
  ; mutable delta_len : int
  ; mutable result_buf : Bigstring.t
  ; mutable result_len : int
  ; mutable result_object_type : Object_type.t
  ; mutable zlib_result_expected_length : int
  ; result_zlib_inflate : Zlib.Inflate.t
  ; object_parser : 'Sha1_validation Object_parser.t
  }
[@@deriving fields]

type 'Sha1_validation packed_result =
  | T :
      ( 'Sha1_validation
      , [< `No_base | `Have_base ]
      , [< `No_delta | `Have_delta ]
      , [ `Have_result of [ `Object ] ] )
        t
      -> 'Sha1_validation packed_result

let blit_into_result_buf t buf ~pos ~len =
  while t.result_len + len > Bigstring.length t.result_buf do
    let new_buf = Bigstring.create (2 * Bigstring.length t.result_buf) in
    Bigstring.blit ~src:t.result_buf ~src_pos:0 ~dst:new_buf ~dst_pos:0 ~len:t.result_len;
    t.result_buf <- new_buf
  done;
  Bigstring.blit ~src:buf ~src_pos:pos ~dst:t.result_buf ~dst_pos:t.result_len ~len;
  t.result_len <- t.result_len + len
;;

let rec compute_result t ~delta_pos =
  if delta_pos >= t.delta_len
  then ()
  else (
    let first_byte = Bigstring.get_uint8 t.delta_buf ~pos:delta_pos in
    if first_byte land 128 = 0
    then (
      (* Instruction to add new data. *)
      let length = first_byte in
      if length = 0 then raise_s [%message "Unsupported reserved delta instruction"];
      if delta_pos + 1 + length > t.delta_len
      then raise_s [%message "Delta instruction to add new data out of bounds"];
      blit_into_result_buf t t.delta_buf ~pos:(delta_pos + 1) ~len:length;
      compute_result t ~delta_pos:(delta_pos + 1 + length))
    else (
      (* Instruction to copy from base object. *)
      let offset, delta_pos = 0, delta_pos + 1 in
      let offset, delta_pos =
        if first_byte land 1 = 0
        then offset, delta_pos
        else offset lor Bigstring.get_uint8 t.delta_buf ~pos:delta_pos, delta_pos + 1
      in
      let offset, delta_pos =
        if first_byte land 2 = 0
        then offset, delta_pos
        else
          offset lor (Bigstring.get_uint8 t.delta_buf ~pos:delta_pos lsl 8), delta_pos + 1
      in
      let offset, delta_pos =
        if first_byte land 4 = 0
        then offset, delta_pos
        else
          ( offset lor (Bigstring.get_uint8 t.delta_buf ~pos:delta_pos lsl 16)
          , delta_pos + 1 )
      in
      let offset, delta_pos =
        if first_byte land 8 = 0
        then offset, delta_pos
        else
          ( offset lor (Bigstring.get_uint8 t.delta_buf ~pos:delta_pos lsl 24)
          , delta_pos + 1 )
      in
      let size = 0 in
      let size, delta_pos =
        if first_byte land 16 = 0
        then size, delta_pos
        else size lor Bigstring.get_uint8 t.delta_buf ~pos:delta_pos, delta_pos + 1
      in
      let size, delta_pos =
        if first_byte land 32 = 0
        then size, delta_pos
        else
          size lor (Bigstring.get_uint8 t.delta_buf ~pos:delta_pos lsl 8), delta_pos + 1
      in
      assert (first_byte land 64 = 0);
      let size = if size = 0 then 0x10000 else size in
      blit_into_result_buf t t.base_buf ~pos:offset ~len:size;
      compute_result t ~delta_pos))
;;

let create sha1_validation =
  let object_parser =
    Object_parser.create
      ~on_blob_size:(fun (_ : int) -> ())
      ~on_blob_chunk:(fun (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) -> ())
      ~on_commit:(fun (_ : Commit.t) -> ())
      ~on_tree_line:(fun (_ : File_mode.t) (_ : Sha1.Raw.Volatile.t) ~name:(_ : string) ->
        ())
      ~on_tag:(fun (_ : Tag.t) -> ())
      ~on_error:Error.raise
      sha1_validation
  in
  let rec t =
    lazy
      { base_buf = Bigstring.create (if Core.am_running_test then 1 else 1 lsl 15)
      ; base_len = 0
      ; delta_buf = Bigstring.create (if Core.am_running_test then 1 else 1 lsl 15)
      ; delta_len = 0
      ; result_buf = Bigstring.create (if Core.am_running_test then 1 else 1 lsl 15)
      ; result_len = 0
      ; result_object_type = Commit
      ; zlib_result_expected_length = 0
      ; result_zlib_inflate = Lazy.force result_zlib_inflate
      ; object_parser
      }
  and result_zlib_inflate =
    lazy
      (Zlib.Inflate.create_uninitialised ~on_data_chunk:(fun buf ~pos ~len ->
         blit_into_result_buf (Lazy.force t) buf ~pos ~len))
  in
  Lazy.force t
;;

let reset (t : ('Sha1_validation, _, _, _) t) =
  (t :> ('Sha1_validation, [ `No_base ], [ `No_delta ], [ `No_result ]) t)
;;

let set_result_as_base
      (t :
         ( 'Sha1_validation
         , [< `No_base | `Have_base ]
         , 'delta
         , [ `Have_result of [ `Object ] ] )
           t)
  =
  let temp = t.base_buf in
  t.base_buf <- t.result_buf;
  t.result_buf <- temp;
  let temp = t.base_len in
  t.base_len <- t.result_len;
  t.result_len <- temp;
  (t :> ('Sha1_validation, [ `Have_base ], 'delta, [ `No_result ]) t)
;;

let set_result_as_delta
      (t :
         ( 'Sha1_validation
         , 'base
         , [< `No_delta | `Have_delta ]
         , [ `Have_result of [ `Delta ] ] )
           t)
  =
  let temp = t.delta_buf in
  t.delta_buf <- t.result_buf;
  t.result_buf <- temp;
  let temp = t.delta_len in
  t.delta_len <- t.result_len;
  t.result_len <- temp;
  (t :> ('Sha1_validation, 'base, [ `Have_delta ], [ `No_result ]) t)
;;

let begin_zlib_inflate_into_result
      (type kind)
      (t : ('Sha1_validation, 'base, 'delta, _) t)
      (object_type : kind Pack_object_type.t)
      ~expected_length
  =
  t.result_len <- 0;
  Zlib.Inflate.init_or_reset t.result_zlib_inflate;
  (match object_type with
   | (Commit | Tree | Blob | Tag) as object_type ->
     t.result_object_type <- Pack_object_type.to_object_type object_type
   | Ofs_delta | Ref_delta -> ());
  t.zlib_result_expected_length <- expected_length;
  (t :> ('Sha1_validation, 'base, 'delta, [ `Computing_result of kind ]) t)
;;

let feed_result_zlib_inflate (t : (_, _, _, [ `Computing_result of _ ]) t) buf ~pos ~len =
  Zlib.Inflate.process t.result_zlib_inflate buf ~pos ~len
;;

let finalise_result_zlib_inflate_exn
      (t : ('Sha1_validation, 'base, 'delta, [ `Computing_result of 'kind ]) t)
  =
  Zlib.Inflate.finalise t.result_zlib_inflate;
  if t.result_len <> t.zlib_result_expected_length
  then
    raise_s
      [%message
        "Unexpected decompressed object length"
          ~expected_length:(t.zlib_result_expected_length : int)
          ~actual_length:(t.result_len : int)];
  (t :> ('Sha1_validation, 'base, 'delta, [ `Have_result of 'kind ]) t)
;;

let[@inline] with_base_buffer_and_length
               (t : ('Sha1_validation, 'base, 'delta, _) t)
               object_type
               buf
               ~length
               f
  =
  let old_base_buf = t.base_buf in
  let old_base_len = t.base_len in
  t.base_buf <- buf;
  t.base_len <- length;
  t.result_object_type <- Pack_object_type.to_object_type object_type;
  (protect [@inlined hint])
    ~f:(fun [@inline] () ->
      let t =
        (t :> ('Sha1_validation, [ `Have_external_base ], 'delta, [ `No_result ]) t)
      in
      let t
        : ( 'Sha1_validation
          , [ `Have_external_base ]
          , 'delta
          , [ `Have_result of [ `Object ] ] )
            t
        =
        f t
      in
      (t :> ('Sha1_validation, 'base, 'delta, [ `Have_result of [ `Object ] ]) t))
    ~finally:(fun () ->
      t.base_buf <- old_base_buf;
      t.base_len <- old_base_len)
;;

let compute_result
      (t :
         ( 'Sha1_validation
         , ([< `Have_base | `Have_external_base ] as 'base)
         , [ `Have_delta ]
         , [ `No_result ] )
           t)
  =
  t.result_len <- 0;
  let pos = 0 in
  let expected_base_length =
    Low_level_reader.read_variable_length_integer t.delta_buf ~pos
  in
  if expected_base_length <> t.base_len
  then
    raise_s
      [%message
        "Delta object expected base object of different length"
          ~actual_base_length:(t.base_len : int)
          (expected_base_length : int)];
  let pos = Low_level_reader.skip_variable_length_integer t.delta_buf ~pos in
  let expected_result_length =
    Low_level_reader.read_variable_length_integer t.delta_buf ~pos
  in
  let pos = Low_level_reader.skip_variable_length_integer t.delta_buf ~pos in
  compute_result t ~delta_pos:pos;
  if expected_result_length <> t.result_len
  then
    raise_s
      [%message
        "Expected delta object result of different length"
          ~actual_result_length:(t.result_len : int)
          (expected_result_length : int)];
  (t :> ('Sha1_validation, 'base, [ `Have_delta ], [ `Have_result of [ `Object ] ]) t)
;;

let result_object_type (t : (_, _, _, [ `Have_result of [ `Object ] ]) t) =
  t.result_object_type
;;

let parse_result
      (t : (_, _, _, [ `Have_result of [ `Object ] ]) t)
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      sha1_validation
  =
  Object_parser.set_callback_and_reset_for_reading_object_type
    t.object_parser
    t.result_object_type
    ~payload_length:t.result_len
    ~on_blob_size
    ~on_blob_chunk
    ~on_commit
    ~on_tree_line
    ~on_tag;
  Object_parser.append_data t.object_parser t.result_buf ~pos:0 ~len:t.result_len;
  Object_parser.finalise t.object_parser sha1_validation
;;
