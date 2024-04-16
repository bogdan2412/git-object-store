(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020-2024  Bogdan-Cristian Tataroiu

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

(** [header_length] returns the length required to write down a git object header for an
    object of the given type and length. *)
val header_length : Object_type.t -> object_length:int -> int

(** [write_from_left] writes a git object header for the given object type and length to
    the specified [Bigstring.t] starting from position [pos].

    The method assumes that the given Bigstring has enough space available to be able to
    write the entire header.

    The return value corresponds to the length of the header data written. *)
val write_from_left : Bigstring.t -> pos:int -> Object_type.t -> object_length:int -> int

(** [write_from_right] writes a git object header for the given object type and length to
    the specified [Bigstring.t] ending on position [pos].

    The method assumes that the given Bigstring has enough space available to be able to
    write the entire header.

    The return value corresponds to the length of the header data written. *)
val write_from_right : Bigstring.t -> pos:int -> Object_type.t -> object_length:int -> int
