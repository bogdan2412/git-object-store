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
  { fanout : int
  ; sha1s : int
  ; crc32s : int
  ; offsets : int
  ; uint64_offsets : int
  }

let create ~items_in_pack =
  let sha1s = 1032 in
  let crc32s = sha1s + (Sha1.Raw.length * items_in_pack) in
  let offsets = crc32s + (4 * items_in_pack) in
  let uint64_offsets = offsets + (4 * items_in_pack) in
  { fanout = 8; sha1s; crc32s; offsets; uint64_offsets }
;;

let fanout t byte = t.fanout + (4 * Char.to_int byte)
let sha1 t index = t.sha1s + (Sha1.Raw.length * index)
let crc32 t index = t.crc32s + (4 * index)
let offset t index = t.offsets + (4 * index)
let uint64_offset t index = t.uint64_offsets + (8 * index)
