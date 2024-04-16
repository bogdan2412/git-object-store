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

type t

val open_existing : pack_directory:string -> t Or_error.t Deferred.t

(** Returns the list of pack file names indexed by the multi pack index. *)
val pack_file_names : t -> string array

(** Returns the number of objects indexed by the multi pack index. *)
val object_count : t -> int

(** Returns the sha1 corresponding to the object with the provided index position. *)
val sha1 : t -> index:int -> Sha1.Raw.Volatile.t

(** Returns the index of the pack in [pack_file_names] to which the object with the
    provided index position belongs. *)
val pack_id : t -> index:int -> int

(** Returns the offset within its corresponding pack at which the object with the
    provided index position can be found. *)
val pack_offset : t -> index:int -> int

(** Search for an object with the given SHA1 hash in the multi pack index. *)
val find_sha1_index : t -> Sha1.Raw.t -> Git_pack_files.Find_result.Volatile.t

(** Search for an object with the given SHA1 hash in the multi pack index. *)
val find_sha1_index' : t -> Sha1.Raw.Volatile.t -> Git_pack_files.Find_result.Volatile.t

(** Returns the index's SHA1. *)
val multi_pack_index_sha1 : t -> Sha1.Raw.Volatile.t

(** Returns the path to the underlying multi pack index file. *)
val multi_pack_index_file : t -> string

module For_testing : sig
  val print_out_multi_pack_index : pack_directory:string -> unit Deferred.t
end
