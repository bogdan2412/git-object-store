(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020-2022  Bogdan-Cristian Tataroiu

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
open! Import

let rec int_as_string_length ~acc n =
  if n <= 9 then acc + 1 else int_as_string_length ~acc:(acc + 1) (n / 10)
;;

let rec write_int_as_string buf ~pos ~value =
  if value <= 9
  then Bigstring.set buf pos (Char.unsafe_of_int (Char.to_int '0' + value))
  else (
    Bigstring.set buf pos (Char.unsafe_of_int (Char.to_int '0' + (value mod 10)));
    write_int_as_string buf ~pos:(pos - 1) ~value:(value / 10))
;;

let object_type_string object_type =
  match (object_type : Object_type.t) with
  | Commit -> "commit "
  | Tree -> "tree "
  | Blob -> "blob "
  | Tag -> "tag "
;;

let header_length object_type ~object_length =
  let object_type_string = object_type_string object_type in
  let object_type_length = String.length object_type_string in
  let object_length_length = int_as_string_length ~acc:0 object_length in
  object_type_length + object_length_length + 1
;;

let write_from_left buf ~pos object_type ~object_length =
  let object_type_string = object_type_string object_type in
  let object_type_length = String.length object_type_string in
  let object_length_length = int_as_string_length ~acc:0 object_length in
  let length = object_type_length + object_length_length + 1 in
  Bigstring.From_string.blit
    ~src:object_type_string
    ~src_pos:0
    ~dst:buf
    ~dst_pos:pos
    ~len:object_type_length;
  write_int_as_string buf ~pos:(pos + length - 2) ~value:object_length;
  Bigstring.set buf (pos + length - 1) '\000';
  length
;;

let write_from_right buf ~pos object_type ~object_length =
  let object_type_string = object_type_string object_type in
  let object_type_length = String.length object_type_string in
  let object_length_length = int_as_string_length ~acc:0 object_length in
  let length = object_type_length + object_length_length + 1 in
  Bigstring.set buf pos '\000';
  write_int_as_string buf ~pos:(pos - 1) ~value:object_length;
  Bigstring.From_string.blit
    ~src:object_type_string
    ~src_pos:0
    ~len:object_type_length
    ~dst:buf
    ~dst_pos:(pos - length + 1);
  length
;;
