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
  { reverse_index_file : string
  ; fd : Fd.t
  ; file_size : int
  ; file_mmap : Bigstring.t
  ; items_in_pack : int
  ; index : Index_reader.t
  }

let open_existing index =
  let reverse_index_file =
    String.chop_suffix_exn (Index_reader.index_file index) ~suffix:".idx" ^ ".rev"
  in
  let items_in_pack = Index_reader.items_in_pack index in
  let expected_file_size = 12 + (4 * items_in_pack) + (Sha1.Raw.length * 2) in
  Util.with_file reverse_index_file ~f:(fun fd file_size file_mmap ->
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
        if not
             (Bytes.equal
                (Sha1.Raw.Volatile.bytes (Index_reader.pack_sha1 index))
                (Bigstring.To_bytes.sub
                   file_mmap
                   ~pos:(file_size - (Sha1.Raw.length * 2))
                   ~len:Sha1.Raw.length))
        then
          Or_error.error_s
            [%sexp "SHA1 checksums do not match between index and reverse index files"]
        else Ok ()
      in
      return { reverse_index_file; fd; file_size; file_mmap; items_in_pack; index }
    in
    Deferred.return result)
;;

let[@cold] raise_invalid_pack_order t ~pack_order =
  raise_s
    [%message
      "Invalid value for pack_order"
        (pack_order : int)
        ~items_in_pack:(t.items_in_pack : int)]
;;

let[@inline] validate_pack_order t ~pack_order =
  if pack_order < 0 || pack_order >= t.items_in_pack
  then raise_invalid_pack_order t ~pack_order
;;

let index_of_pack_order t ~pack_order =
  validate_pack_order t ~pack_order;
  Bigstring.get_uint32_be t.file_mmap ~pos:(12 + (pack_order * 4))
;;

let pack_file_offset_of_pack_order t ~pack_order =
  Index_reader.pack_file_offset t.index ~index:(index_of_pack_order t ~pack_order)
;;

let pack_order_of_pack_file_offset t ~pack_file_offset =
  let binary_search_start = 0 in
  let binary_search_end = t.items_in_pack - 1 in
  let pos = ref binary_search_start in
  let step = ref 1 in
  while !step <= binary_search_end - binary_search_start do
    step := !step lsl 1
  done;
  step := !step lsr 1;
  while !step >= 1 do
    if !pos + !step <= binary_search_end
    && pack_file_offset_of_pack_order t ~pack_order:(!pos + !step) <= pack_file_offset
    then pos := !pos + !step;
    step := !step lsr 1
  done;
  if pack_file_offset_of_pack_order t ~pack_order:!pos = pack_file_offset
  then !pos
  else raise_s [%message "Invalid pack file offset value" (pack_file_offset : int)]
;;

let pack_order_of_index t ~index =
  pack_order_of_pack_file_offset
    t
    ~pack_file_offset:(Index_reader.pack_file_offset t.index ~index)
;;

let index_of_pack_file_offset t ~pack_file_offset =
  index_of_pack_order t ~pack_order:(pack_order_of_pack_file_offset t ~pack_file_offset)
;;
