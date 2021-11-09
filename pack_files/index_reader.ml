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

type t =
  { index_file : string
  ; fd : Fd.t
  ; file_size : int
  ; file_mmap : Bigstring.t
  ; items_in_pack : int
  ; offsets : Index_offsets.t
  }

let open_existing ~pack_file ~pack_file_mmap ~pack_file_size ~items_in_pack =
  let index_file = String.chop_suffix_exn pack_file ~suffix:".pack" ^ ".idx" in
  Util.with_file index_file ~f:(fun fd file_size file_mmap ->
    let result =
      let open Or_error.Let_syntax in
      let%bind () =
        (* At least 4 for signature, 4 for version, 256 * 4 for fan-out table,
           one raw SHA1, one CRC32 and one offset for each item in the pack and
           two raw SHA1s *)
        if file_size
           < 1032 + (items_in_pack * (Sha1.Raw.length + 8)) + (2 * Sha1.Raw.length)
        then Or_error.error_s [%sexp "Index file impossibly small"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint32_be file_mmap ~pos:0 <> 0xff744f63
        then Or_error.error_s [%sexp "Expected idx signature"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint32_be file_mmap ~pos:4 <> 2
        then Or_error.error_s [%sexp "Expected index version number 2"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.memcmp
             file_mmap
             ~pos1:(file_size - (Sha1.Raw.length * 2))
             pack_file_mmap
             ~pos2:(pack_file_size - Sha1.Raw.length)
             ~len:Sha1.Raw.length
           <> 0
        then
          Or_error.error_s
            [%sexp "SHA1 checksums do not match between index and pack files"]
        else Ok ()
      in
      return
        { index_file
        ; fd
        ; file_size
        ; file_mmap
        ; items_in_pack
        ; offsets = Index_offsets.create ~items_in_pack
        }
    in
    Deferred.return result)
;;

let index_file t = t.index_file
let items_in_pack t = t.items_in_pack

let pack_sha1 =
  let result = Sha1.Raw.Volatile.create () in
  fun t ->
    Bigstring.To_bytes.blit
      ~src:t.file_mmap
      ~src_pos:(t.file_size - (Sha1.Raw.length * 2))
      ~dst:(Sha1.Raw.Volatile.bytes result)
      ~dst_pos:0
      ~len:Sha1.Raw.length;
    result
;;

let[@cold] raise_invalid_index t ~index =
  raise_s
    [%message
      "Invalid value for index" (index : int) ~items_in_pack:(t.items_in_pack : int)]
;;

let[@inline] validate_index t ~index =
  if index < 0 || index >= t.items_in_pack then raise_invalid_index t ~index
;;

let sha1 =
  let result = Sha1.Raw.Volatile.create () in
  fun t ~index ->
    validate_index t ~index;
    Bigstring.To_bytes.blit
      ~src:t.file_mmap
      ~src_pos:(Index_offsets.sha1 t.offsets index)
      ~dst:(Sha1.Raw.Volatile.bytes result)
      ~dst_pos:0
      ~len:Sha1.Raw.length;
    result
;;

let pack_file_offset =
  let msb_mask = 1 lsl 31 in
  fun t ~index ->
    validate_index t ~index;
    let offset =
      Bigstring.get_uint32_be t.file_mmap ~pos:(Index_offsets.offset t.offsets index)
    in
    if offset land msb_mask = 0
    then offset
    else
      Bigstring.get_uint64_be_exn
        t.file_mmap
        ~pos:(Index_offsets.uint64_offset t.offsets (offset lxor msb_mask))
;;

let find_sha1_index_gen =
  let rec sha1_greater_than_or_equal t sha1_get_char sha1 ~index_pos ~pos =
    if pos = Sha1.Raw.length
    then true
    else (
      let from_sha1 = Char.to_int (sha1_get_char sha1 pos) in
      let from_index = Bigstring.get_uint8 t.file_mmap ~pos:(index_pos + pos) in
      if from_sha1 > from_index
      then true
      else if from_sha1 < from_index
      then false
      else sha1_greater_than_or_equal t sha1_get_char sha1 ~index_pos ~pos:(pos + 1))
  in
  let rec sha1_equal t sha1_get_char sha1 ~index_pos ~pos =
    if pos = Sha1.Raw.length
    then true
    else (
      let from_sha1 = Char.to_int (sha1_get_char sha1 pos) in
      let from_index = Bigstring.get_uint8 t.file_mmap ~pos:(index_pos + pos) in
      if from_sha1 <> from_index
      then false
      else sha1_equal t sha1_get_char sha1 ~index_pos ~pos:(pos + 1))
  in
  fun t sha1_get_char sha1 ->
    let first_byte = sha1_get_char sha1 0 in
    let binary_search_start =
      match first_byte with
      | '\000' -> Index_offsets.sha1 t.offsets 0
      | n ->
        Index_offsets.sha1
          t.offsets
          (Bigstring.get_uint32_be
             t.file_mmap
             ~pos:(Index_offsets.fanout t.offsets (Char.of_int_exn (Char.to_int n - 1))))
    in
    let binary_search_end =
      Index_offsets.sha1
        t.offsets
        (Bigstring.get_uint32_be
           t.file_mmap
           ~pos:(Index_offsets.fanout t.offsets first_byte)
         - 1)
    in
    let pos = ref binary_search_start in
    let step = ref Sha1.Raw.length in
    while !step <= binary_search_end - binary_search_start do
      step := !step lsl 1
    done;
    step := !step lsr 1;
    while !step >= Sha1.Raw.length do
      if !pos + !step <= binary_search_end
      && sha1_greater_than_or_equal
           t
           sha1_get_char
           sha1
           ~index_pos:(!pos + !step)
           ~pos:0
      then pos := !pos + !step;
      step := !step lsr 1
    done;
    if sha1_equal t sha1_get_char sha1 ~index_pos:!pos ~pos:0
    then
      Find_result.Volatile.some ((!pos - Index_offsets.sha1 t.offsets 0) / Sha1.Raw.length)
    else Find_result.Volatile.none
;;

let find_sha1_index t sha1 = find_sha1_index_gen t String.get (Sha1.Raw.to_string sha1)

let find_sha1_index' t sha1 =
  find_sha1_index_gen t Bytes.get (Sha1.Raw.Volatile.bytes sha1)
;;
