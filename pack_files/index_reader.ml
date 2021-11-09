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
        if Bigstring.get_uint32_le file_mmap ~pos:0 <> 1666151679
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
        ; offsets = Index_offsets.create ~items_in_pack
        }
    in
    Deferred.return result)
;;
