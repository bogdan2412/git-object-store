(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021-2024  Bogdan-Cristian Tataroiu

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

module Make_find_sha1_index (Sha1M : sig
    type t

    val get : t -> int -> char
  end) =
struct
  include Util.Make_sha1_binary_search (Sha1M)

  let find_sha1_index t sha1m =
    let first_byte = Sha1M.get sha1m 0 in
    let index_lower_bound =
      match first_byte with
      | '\000' -> 0
      | n ->
        Bigstring.get_uint32_be
          t.file_mmap
          ~pos:(Index_offsets.fanout t.offsets (Char.of_int_exn (Char.to_int n - 1)))
    in
    let index_search_candidates =
      Bigstring.get_uint32_be t.file_mmap ~pos:(Index_offsets.fanout t.offsets first_byte)
      - index_lower_bound
    in
    match
      sha1_binary_search
        ~index_buf:t.file_mmap
        ~index_pos:(Index_offsets.sha1 t.offsets index_lower_bound)
        ~index_element_count:index_search_candidates
        sha1m
    with
    | None -> Find_result.Volatile.none
    | Some { index } -> Find_result.Volatile.some (index_lower_bound + index)
  ;;
end

module Find_sha1_index = Make_find_sha1_index (String)
module Find_sha1_index' = Make_find_sha1_index (Bytes)

let find_sha1_index t sha1 = Find_sha1_index.find_sha1_index t (Sha1.Raw.to_string sha1)

let find_sha1_index' t sha1 =
  Find_sha1_index'.find_sha1_index t (Sha1.Raw.Volatile.bytes sha1)
;;
