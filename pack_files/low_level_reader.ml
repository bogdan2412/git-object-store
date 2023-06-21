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

let rec skip_variable_length_integer buf ~pos =
  if Bigstring.get_uint8 buf ~pos land 128 = 0
  then pos + 1
  else skip_variable_length_integer buf ~pos:(pos + 1)
;;

let read_variable_length_integer =
  let rec read_loop ~acc ~shift buf ~pos =
    let byte = Bigstring.get_uint8 buf ~pos in
    if byte land 128 = 0
    then acc lor (byte lsl shift)
    else
      read_loop
        ~acc:(acc lor ((byte land 127) lsl shift))
        ~shift:(shift + 7)
        buf
        ~pos:(pos + 1)
  in
  fun buf ~pos -> read_loop ~acc:0 ~shift:0 buf ~pos
;;

let read_variable_length_relative_offset =
  let rec read_loop ~acc buf ~pos =
    let byte = Bigstring.get_uint8 buf ~pos in
    let acc = ((acc + 1) lsl 7) lor (byte land 127) in
    if byte land 128 = 0 then acc else read_loop ~acc buf ~pos:(pos + 1)
  in
  fun buf ~pos ->
    let first_byte = Bigstring.get_uint8 buf ~pos in
    let acc = first_byte land 127 in
    if first_byte land 128 = 0 then acc else read_loop ~acc buf ~pos:(pos + 1)
;;

let object_type buf ~pos : Pack_object_type.packed =
  let type_int = (Bigstring.get_uint8 buf ~pos lsr 4) land 7 in
  match type_int with
  | 1 -> T Commit
  | 2 -> T Tree
  | 3 -> T Blob
  | 4 -> T Tag
  | 6 -> T Ofs_delta
  | 7 -> T Ref_delta
  | _ -> raise_s [%message "Invalid pack object type" (type_int : int)]
;;

let object_length buf ~pos =
  let first_byte = Bigstring.get_uint8 buf ~pos in
  if first_byte land 128 = 0
  then first_byte land 15
  else first_byte land 15 lor (read_variable_length_integer buf ~pos:(pos + 1) lsl 4)
;;
