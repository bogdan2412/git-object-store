(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019  Bogdan-Cristian Tataroiu

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

open Core
open Async

type t

val create : pack_file:string -> t Or_error.t Deferred.t
val index_pack : pack_file:string -> unit Or_error.t Deferred.t
val items_in_pack : t -> int
val sha1 : t -> index:int -> Sha1.Raw.Volatile.t

val read_object
  :  t
  -> index:int
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> unit

val find_sha1_index : t -> Sha1.Raw.t -> int
val find_sha1_index' : t -> Sha1.Raw.Volatile.t -> int