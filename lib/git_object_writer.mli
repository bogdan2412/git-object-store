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

module Raw : sig
  (** The [Raw] module receives chunks of data, compresses it with zlib,
      computes the contents' SHA1 hash and saves the compressed output in a
      file under the [destination_directory] named after the contents [SHA1]
      hash as below:

      [destination_directory]/XX/XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  *)
  type t

  val create_uninitialised : destination_directory:string -> t
  val init_or_reset : t -> unit Deferred.t
  val append_data : t -> Bigstring.t -> pos:int -> len:int -> unit
  val finalise : t -> Sha1.Raw.t Deferred.t
  val abort : t -> unit Deferred.t
end

module For_unknown_contents_size : sig
  (** The [For_unknown_contents_size] module buffers chunks of data until it
      is fully read, prepends it with a header of the form
      "[prefix][data length as decimal integer]\000" and uses the [Raw] module
      to save it to disk.
  *)
  type t

  val create_uninitialised : destination_directory:string -> t
  val init_or_reset : t -> prefix:string -> unit Deferred.t
  val double_buffer_space : t -> unit
  val make_room : t -> for_bytes:int -> unit
  val buf : t -> Bigstring.t
  val pos : t -> int
  val len : t -> int
  val advance_pos : t -> by:int -> unit
  val written_so_far : t -> int
  val finalise : t -> Sha1.Raw.t Deferred.t
  val abort : t -> unit Deferred.t
end

module Commit : sig
  type t

  val create : destination_directory:string -> t

  (** Can be called multiple times for different commits using the same [t]. *)
  val write : t -> Commit.t -> Sha1.Raw.t Deferred.t

  (** Shortcut for [create ~destination_directory] followed by [write]. *)
  val write' : destination_directory:string -> Commit.t -> Sha1.Raw.t Deferred.t
end

module Tag : sig
  type t

  val create : destination_directory:string -> t

  (** Can be called multiple times for different tags using the same [t]. *)
  val write : t -> Tag.t -> Sha1.Raw.t Deferred.t

  (** Shortcut for [create ~destination_directory] followed by [write]. *)
  val write' : destination_directory:string -> Tag.t -> Sha1.Raw.t Deferred.t
end

module Tree : sig
  type t

  val create_uninitialised : destination_directory:string -> t
  val init_or_reset : t -> unit Deferred.t
  val write_tree_line : t -> File_mode.t -> Sha1.Raw.t -> name:string -> unit
  val write_tree_line' : t -> File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit
  val finalise : t -> Sha1.Raw.t Deferred.t
  val abort : t -> unit Deferred.t
end

module Blob : sig
  module Unknown_size : sig
    type t

    val create_uninitialised : destination_directory:string -> t
    val init_or_reset : t -> unit Deferred.t
    val append_data : t -> Bigstring.t -> pos:int -> len:int -> unit
    val written_so_far : t -> int
    val finalise : t -> Sha1.Raw.t Deferred.t
    val abort : t -> unit Deferred.t
  end

  module Known_size : sig
    type t

    val create_uninitialised : destination_directory:string -> t
    val init_or_reset : t -> length:int -> unit Deferred.t
    val append_data : t -> Bigstring.t -> pos:int -> len:int -> unit
    val finalise : t -> Sha1.Raw.t Deferred.t
    val abort : t -> unit Deferred.t
  end
end
