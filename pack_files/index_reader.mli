(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021-2025  Bogdan-Cristian Tataroiu

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

type t

val open_existing
  :  pack_file:string
  -> pack_file_mmap:Bigstring.t
  -> pack_file_size:int
  -> items_in_pack:int
  -> t Or_error.t Deferred.t

(** Returns the number of items in the pack. *)
val items_in_pack : t -> int

(** Returns the sha1 corresponding to the object with the provided index position. *)
val sha1 : t -> index:int -> Sha1.Raw.Volatile.t

(** Returns the position in the pack file where the object with the provided index position
    can be found. *)
val pack_file_offset : t -> index:int -> int

(** Search for an object with the given SHA1 hash in the pack. *)
val find_sha1_index : t -> Sha1.Raw.t -> Find_result.Volatile.t

(** Search for an object with the given SHA1 hash in the pack. *)
val find_sha1_index' : t -> Sha1.Raw.Volatile.t -> Find_result.Volatile.t

(** Returns the path on disk to the index file. *)
val index_file : t -> string

(** Returns the corresponding pack's sha1. *)
val pack_sha1 : t -> Sha1.Raw.Volatile.t
