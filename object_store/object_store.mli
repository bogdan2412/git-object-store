(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2021  Bogdan-Cristian Tataroiu

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

open! Core
open! Async
open! Import

type _ t

(** Create a unified reader that reads from the given [object_directory].
    [max_concurrent_reads] throttles how many objects may be read from disk at the same time.

    If [Validate_sha1] is passed, then the store will assert that the objects it reads
    match the expected checksum.  Otherwise, if [Do_not_validate_sha1] is passed, the
    store will not spend time maintaining the [Sha1] sum of the data. *)
val create
  :  object_directory:string
  -> max_concurrent_reads:int
  -> 'Sha1_validation Sha1_validation.t
  -> 'Sha1_validation t Or_error.t Deferred.t

val object_directory : _ t -> string

module Find_result : sig
  module Volatile : sig
    (** Do not keep references to instances of this type as they will be mutated
        on every call to [find_object]. *)
    type 'Sha1_validation t = private
      | In_pack_file of
          { mutable pack : 'Sha1_validation Pack_reader.t
          ; mutable index : int
          }
      | Unpacked_file_if_exists of { mutable path : string }
      (** [Unpacked_file_if_exists] does not verify that the path exists, it
          merely implies that the file is not present in any pack file. *)
  end
end

(** [find_object] returns whether the object with the given SHA1 exists in a pack
    file or should be read from an unpacked file (assuming it exists at all, the
    method does not check that this is the case). *)
val find_object
  :  'Sha1_validation t
  -> Sha1.Hex.t
  -> 'Sha1_validation Find_result.Volatile.t

(** [read_object] reads an object with the given SHA1 hash and calls the relevant callbacks
    depending on the kind of object.

    Raises if no object with the given hash exists.

    The [push_back] callback is called every time we read a chunk of data and it gives the
    client an opportunity to push back on reading. *)
val read_object
  :  _ t
  -> Sha1.Hex.t
  -> on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> push_back:(unit -> [ `Ok | `Reader_closed ] Deferred.t)
  -> unit Deferred.t

(** [read_raw_object] reads a raw object with the given SHA1 hash and calls the provided
    callbacks.

    Raises if no object with the given hash exists.

    The [push_back] callback is called every time we read a chunk of data and it gives the
    client an opportunity to push back on reading. *)
val read_raw_object
  :  _ t
  -> Sha1.Hex.t
  -> on_header:(Object_type.t -> size:int -> unit)
  -> on_payload:(Bigstring.t -> pos:int -> len:int -> unit)
  -> push_back:(unit -> [ `Ok | `Reader_closed ] Deferred.t)
  -> unit Deferred.t

(** [read_blob] is a convenience wrapper on top of [read_object] that raises if the type of
    object is not Blob. *)
val read_blob
  :  _ t
  -> Sha1.Hex.t
  -> on_size:(int -> unit)
  -> on_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> push_back:(unit -> [ `Ok | `Reader_closed ] Deferred.t)
  -> unit Deferred.t

(** [read_commit] is a convenience wrapper on top of [read_object] that raises if the type of
    object is not Commit. *)
val read_commit : _ t -> Sha1.Hex.t -> on_commit:(Commit.t -> unit) -> unit Deferred.t

(** [read_tree] is a convenience wrapper on top of [read_object] that raises if the type of
    object is not Tree. *)
val read_tree
  :  _ t
  -> Sha1.Hex.t
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> unit Deferred.t

(** [read_tag] is a convenience wrapper on top of [read_object] that raises if the type of
    object is not Tag. *)
val read_tag : _ t -> Sha1.Hex.t -> on_tag:(Tag.t -> unit) -> unit Deferred.t

(** [size] returns the size of the object identified by the provided SHA1. Raises if
    the object does not exist. *)
val size : _ t -> Sha1.Hex.t -> int Deferred.t

(** Looks for an object with the given SHA1 hash and calls [f] with an on-disk file containing
    the object.

    Raises if the object does not exist.

    The file will not necessarily persist after [f] is called as packed objects are written
    to a temporary file that is deleted after [f] is called. *)
val with_on_disk_file : _ t -> Sha1.Hex.t -> f:(string -> 'a Deferred.t) -> 'a Deferred.t

module Object_location : sig
  type t =
    | Unpacked_file of string
    | In_pack_file of
        { pack_file : string
        ; index : int
        }
  [@@deriving sexp_of]
end

val all_pack_files_in_store
  :  'Sha1_validation t
  -> (string * 'Sha1_validation Pack_reader.t) list

val all_unpacked_objects_in_store : _ t -> string Sha1.Hex.Table.t Deferred.t
val all_objects_in_store : _ t -> Object_location.t list Sha1.Hex.Table.t Deferred.t

module Packed : sig
  type _ non_packed
  type t = T : _ non_packed -> t

  (** Create a unified reader that reads from the given [object_directory].
      [max_concurrent_reads] throttles how many objects may be read from disk at the same time.

      If [Validate_sha1] is passed, then the store will assert that the objects it reads
      match the expected checksum.  Otherwise, if [Do_not_validate_sha1] is passed, the
      store will not spend time maintaining the [Sha1] sum of the data. *)
  val create
    :  object_directory:string
    -> max_concurrent_reads:int
    -> 'Sha1_validation Sha1_validation.t
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

  (** [read_raw_object] reads a raw object with the given SHA1 hash and calls the provided
      callbacks.

      Raises if no object with the given hash exists.

      The [push_back] callback is called every time we read a chunk of data and it gives the
      client an opportunity to push back on reading. *)
  val read_raw_object
    :  t
    -> Sha1.Hex.t
    -> on_header:(Object_type.t -> size:int -> unit)
    -> on_payload:(Bigstring.t -> pos:int -> len:int -> unit)
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

  (** [size] returns the size of the object identified by the provided SHA1. Raises if
      the object does not exist. *)
  val size : t -> Sha1.Hex.t -> int Deferred.t

  (** Looks for an object with the given SHA1 hash and calls [f] with an on-disk file containing
      the object.

      Raises if the object does not exist.

      The file will not necessarily persist after [f] is called as packed objects are written
      to a temporary file that is deleted after [f] is called. *)
  val with_on_disk_file : t -> Sha1.Hex.t -> f:(string -> 'a Deferred.t) -> 'a Deferred.t

  module Object_location = Object_location

  val all_unpacked_objects_in_store : t -> string Sha1.Hex.Table.t Deferred.t
  val all_objects_in_store : t -> Object_location.t list Sha1.Hex.Table.t Deferred.t
end
with type 'a non_packed := 'a t
