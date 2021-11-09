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
