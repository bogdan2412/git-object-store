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

(** Module that can transparently read git objects both from packs and regular unpacked
    files. *)

open Core
open Async

type t

(** Create a unified reader that reads from the given [object_directory].
    [max_concurrent_reads] throttles how many objects may be read from disk at the same time. *)
val create
  :  object_directory:string
  -> max_concurrent_reads:int
  -> t Or_error.t Deferred.t

val object_directory : t -> string

(** [read_object] reads an object with the given SHA1 hash and calls the relevant callbacks
    depending on the kind of object.

    Raises if no object with the given hash exists.

    The [push_back] callback is called every time we read a chunk of data and it gives the
    client an opportunity to push back on reading. *)
val read_object
  :  t
  -> Sha1.Hex.t
  -> on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> push_back:(unit -> [ `Ok | `Reader_closed ] Deferred.t)
  -> unit Deferred.t

(** [read_blob] is a convenience wrapper on top of [read_object] that raises if the type of
    object is not Blob. *)
val read_blob
  :  t
  -> Sha1.Hex.t
  -> on_size:(int -> unit)
  -> on_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> push_back:(unit -> [ `Ok | `Reader_closed ] Deferred.t)
  -> unit Deferred.t

(** [read_commit] is a convenience wrapper on top of [read_object] that raises if the type of
    object is not Commit. *)
val read_commit : t -> Sha1.Hex.t -> on_commit:(Commit.t -> unit) -> unit Deferred.t

(** [read_tree] is a convenience wrapper on top of [read_object] that raises if the type of
    object is not Tree. *)
val read_tree
  :  t
  -> Sha1.Hex.t
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> unit Deferred.t

(** [read_tag] is a convenience wrapper on top of [read_object] that raises if the type of
    object is not Tag. *)
val read_tag : t -> Sha1.Hex.t -> on_tag:(Tag.t -> unit) -> unit Deferred.t

(** Looks for an object with the given SHA1 hash and calls [f] with an on-disk file containing
    the object.

    Raises if the object does not exist.

    The file will not necessarily persist after [f] is called as packed objects are written
    to a temporary file that is deleted after [f] is called. *)
val with_on_disk_file : t -> Sha1.Hex.t -> f:(string -> 'a Deferred.t) -> 'a Deferred.t

module Object_location : sig
  type t =
    | Unpacked_file of string
    | In_pack_file of
        { pack_file : string
        ; index : int
        }
  [@@deriving sexp_of]
end

val all_objects_in_store : t -> Object_location.t list Sha1.Hex.Table.t Deferred.t
