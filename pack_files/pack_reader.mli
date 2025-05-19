(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2025  Bogdan-Cristian Tataroiu

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

(** Creates a pack reader given a pack file. The reader expects a matching index file
    to exist.

    If [Validate_sha1] is passed, then the [read_object] methods will assert that the
    data it returns matches the checksum mentioned in the pack's index.  Otherwise, if
    [Do_not_validate_sha1] is passed, the [read_object] methods will not spend time
    maintaining the [Sha1] sum of the data. *)
val create
  :  pack_file:string
  -> 'Sha1_validation Sha1_validation.t
  -> 'Sha1_validation t Or_error.t Deferred.t

(** Read a pack file and generate a corresponding index file for it. *)
val write_pack_index : pack_file:string -> unit Or_error.t Deferred.t

(** Read a pack file's index and generate a corresponding reverse index file for it. *)
val write_pack_reverse_index : pack_file:string -> unit Or_error.t Deferred.t

(** Returns number of items in the pack. *)
val items_in_pack : _ t -> int

(** Returns the SHA1 hash on the [index]-th item in the pack.
    Raises if [index] is outside of the range [0 .. items_in_pack - 1]. *)
val sha1 : _ t -> index:int -> Sha1.Raw.Volatile.t

(** Search for an object with the given SHA1 hash in the pack. *)
val find_sha1_index : _ t -> Sha1.Raw.t -> Find_result.Volatile.t

(** Search for an object with the given SHA1 hash in the pack. *)
val find_sha1_index' : _ t -> Sha1.Raw.Volatile.t -> Find_result.Volatile.t

(** Read and parse the [index]-th object in the pack.
    Raises if [index] is outside of the range [0 .. items_in_pack - 1]. *)
val read_object
  :  _ t
  -> index:int
  -> on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> unit

(** Reads the [index]-th object in the pack without parsing the payload.
    Raises if [index] is outside of the range [0 .. items_in_pack - 1]. *)
val read_raw_object
  :  _ t
  -> index:int
  -> on_header:(Object_type.t -> size:int -> unit)
  -> on_payload:(Bigstring.t -> pos:int -> len:int -> unit)
  -> unit

module Size : sig
  module Volatile : sig
    (** Do not keep references to instances of this type as they will be mutated
        on every call to [size]. *)
    type t = private
      { mutable size : int (** [size] represents the size of the unpacked object. *)
      ; mutable delta_size : int
      (** For objects that are stored in deltified representation relative to a
          different object, [delta_size] represents the size of that representation.
          For objects that are stored in undeltified representation, [delta_size]
          will just be equal to [size]. *)
      ; mutable pack_size : int
      (** [pack_size] represents the size that the object occupies in the pack. *)
      }
  end
end

(** Returns the size of the object at the given [index].
    Raises if [index] is outside of the range [0 .. items_in_pack - 1]. *)
val size : _ t -> index:int -> Size.Volatile.t

module Packed : sig
  type _ non_packed
  type t = T : _ non_packed -> t
end
with type 'a non_packed := 'a t

module Low_level : sig
  val index : _ t -> Index_reader.t

  (** Read and parse the object found in the pack at the provided offset.
      Raises or produces garbage for an invalid value of [pack_offset]. *)
  val read_object
    :  'Sha1_validation t
    -> pack_offset:int
    -> on_blob_size:(int -> unit)
    -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
    -> on_commit:(Commit.t -> unit)
    -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
    -> on_tag:(Tag.t -> unit)
    -> 'Sha1_validation
    -> unit

  (** Read and parse the object found in the pack at the provided offset.
      Raises or produces garbage for an invalid value of [pack_offset]. *)
  val read_raw_object
    :  'Sha1_validation t
    -> pack_offset:int
    -> on_header:(Object_type.t -> size:int -> unit)
    -> on_payload:(Bigstring.t -> pos:int -> len:int -> unit)
    -> 'Sha1_validation
    -> unit

  (** Returns the size of the object found in the pack at the provided offset.
      Raises or produces garbage for an invalid value of [pack_offset]. *)
  val size : _ t -> pack_offset:int -> Size.Volatile.t
end

module For_testing : sig
  val print_out_pack_file : string -> unit Deferred.t
end
