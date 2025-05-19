(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021-2025  Bogdan-Cristian Tataroiu

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

type t =
  { reverse_index_file : string
  ; fd : Fd.t
  ; file_size : int
  ; file_mmap : Bigstring.t
  ; object_count : int
  ; pack_count : int
  ; preferred_pack : int
  ; index : Multi_pack_index_reader.t
  }

let open_existing index =
  let pack_directory =
    Filename.dirname (Multi_pack_index_reader.multi_pack_index_file index)
  in
  let sha1 =
    Sha1.Raw.Volatile.to_hex (Multi_pack_index_reader.multi_pack_index_sha1 index)
  in
  let reverse_index_file =
    pack_directory ^/ [%string "multi-pack-index-%{sha1#Sha1.Hex}.rev"]
  in
  let object_count = Multi_pack_index_reader.object_count index in
  let pack_count = Array.length (Multi_pack_index_reader.pack_file_names index) in
  let expected_file_size = 12 + (4 * object_count) + (Sha1.Raw.length * 2) in
  Git_pack_files.Low_level.Util.with_file
    reverse_index_file
    ~f:(fun fd file_size file_mmap ->
      let result =
        let open Or_error.Let_syntax in
        let%bind () =
          if file_size <> expected_file_size
          then
            Or_error.error_s
              [%message
                "Unexpected reverse index file size"
                  (expected_file_size : int)
                  ~actual_file_size:(file_size : int)]
          else Ok ()
        in
        let%bind () =
          if Bigstring.get_uint32_be file_mmap ~pos:0 <> 0x52494458
          then Or_error.error_s [%sexp "Expected reverse index signature"]
          else Ok ()
        in
        let%bind () =
          if Bigstring.get_uint32_be file_mmap ~pos:4 <> 1
          then Or_error.error_s [%sexp "Expected reverse index version number 1"]
          else Ok ()
        in
        let%bind () =
          if Bigstring.get_uint32_be file_mmap ~pos:8 <> 1
          then Or_error.error_s [%sexp "Expected reverse index hash function SHA1"]
          else Ok ()
        in
        let%bind () =
          if
            not
              (Bytes.equal
                 (Sha1.Raw.Volatile.bytes
                    (Multi_pack_index_reader.multi_pack_index_sha1 index))
                 (Bigstring.To_bytes.sub
                    file_mmap
                    ~pos:(file_size - (Sha1.Raw.length * 2))
                    ~len:Sha1.Raw.length))
          then
            Or_error.error_s
              [%sexp "SHA1 checksums do not match between index and reverse index files"]
          else Ok ()
        in
        let preferred_pack =
          (* The preferred pack is the pack which the first object in the reverse object belongs
             to. *)
          Multi_pack_index_reader.pack_id
            index
            ~index:(Bigstring.get_uint32_be file_mmap ~pos:12)
        in
        return
          { reverse_index_file
          ; fd
          ; file_size
          ; file_mmap
          ; object_count
          ; pack_count
          ; preferred_pack
          ; index
          }
      in
      Deferred.return result)
;;

let[@cold] raise_invalid_pseudo_pack_order t ~pseudo_pack_order =
  raise_s
    [%message
      "Invalid value for pseudo_pack_order"
        (pseudo_pack_order : int)
        ~object_count:(t.object_count : int)]
;;

let[@inline] validate_pseudo_pack_order t ~pseudo_pack_order =
  if pseudo_pack_order < 0 || pseudo_pack_order >= t.object_count
  then raise_invalid_pseudo_pack_order t ~pseudo_pack_order
;;

let index_of_pseudo_pack_order t ~pseudo_pack_order =
  validate_pseudo_pack_order t ~pseudo_pack_order;
  Bigstring.get_uint32_be t.file_mmap ~pos:(12 + (pseudo_pack_order * 4))
;;

let pack_id_of_pseudo_pack_order t ~pseudo_pack_order =
  let index = index_of_pseudo_pack_order t ~pseudo_pack_order in
  Multi_pack_index_reader.pack_id t.index ~index
;;

let pack_offset_of_pseudo_pack_order t ~pseudo_pack_order =
  let index = index_of_pseudo_pack_order t ~pseudo_pack_order in
  Multi_pack_index_reader.pack_offset t.index ~index
;;

let compare t pack_id1 pack_offset1 pack_id2 pack_offset2 =
  match pack_id1 = t.preferred_pack, pack_id2 = t.preferred_pack with
  | true, false -> -1
  | false, true -> 1
  | true, true | false, false ->
    (match Int.compare pack_id1 pack_id2 with
     | 0 -> Int.compare pack_offset1 pack_offset2
     | result -> result)
;;

let pseudo_pack_order_of_pack_id_and_offset t ~pack_id ~pack_offset =
  let binary_search_start = 0 in
  let binary_search_end = t.object_count - 1 in
  let pos = ref binary_search_start in
  let step = ref 1 in
  while !step <= binary_search_end - binary_search_start do
    step := !step lsl 1
  done;
  step := !step lsr 1;
  while !step >= 1 do
    if
      let pseudo_pack_order = !pos + !step in
      pseudo_pack_order <= binary_search_end
      && compare
           t
           (pack_id_of_pseudo_pack_order t ~pseudo_pack_order)
           (pack_offset_of_pseudo_pack_order t ~pseudo_pack_order)
           pack_id
           pack_offset
         <= 0
    then pos := !pos + !step;
    step := !step lsr 1
  done;
  if
    pack_id_of_pseudo_pack_order t ~pseudo_pack_order:!pos = pack_id
    && pack_offset_of_pseudo_pack_order t ~pseudo_pack_order:!pos = pack_offset
  then !pos
  else
    raise_s
      [%message
        "Invalid pack id and pack offset values" (pack_id : int) (pack_offset : int)]
;;

let pseudo_pack_order_of_index t ~index =
  pseudo_pack_order_of_pack_id_and_offset
    t
    ~pack_id:(Multi_pack_index_reader.pack_id t.index ~index)
    ~pack_offset:(Multi_pack_index_reader.pack_offset t.index ~index)
;;

let index_of_pack_id_and_offset t ~pack_id ~pack_offset =
  index_of_pseudo_pack_order
    t
    ~pseudo_pack_order:(pseudo_pack_order_of_pack_id_and_offset t ~pack_id ~pack_offset)
;;
