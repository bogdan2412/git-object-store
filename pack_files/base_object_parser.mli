(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021-2022  Bogdan-Cristian Tataroiu

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

type _ t

val create : 'Sha1_validation Sha1_validation.t -> 'Sha1_validation t

val reset_for_reading
  :  'Sha1_validation t
  -> Object_type.t
  -> payload_length:int
  -> on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> 'Sha1_validation
  -> unit

(** Returns number of bytes consumed from input. *)
val process : _ t -> Bigstring.t -> pos:int -> len:int -> int

val finalise : _ t -> unit
