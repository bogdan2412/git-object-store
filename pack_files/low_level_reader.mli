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

(** Skip over bytes that are used to represent a variable length integer and return the
    next position in the buffer from which other data can be read. *)
val skip_variable_length_integer : Bigstring.t -> pos:int -> int

(** Read a variable length integer from the given location in the provided buffer. *)
val read_variable_length_integer : Bigstring.t -> pos:int -> int

(** Read a variable length integer used to encode relative offsets to parent objects for
    [Ofs_delta] pack object types from the given location in the provided buffer. *)
val read_variable_length_relative_offset : Bigstring.t -> pos:int -> int

(** Read a given pack object's type from the given location in the provided buffer. *)
val object_type : Bigstring.t -> pos:int -> Pack_object_type.packed

(** Read a given pack object's length from the given location in the provided buffer. *)
val object_length : Bigstring.t -> pos:int -> int
