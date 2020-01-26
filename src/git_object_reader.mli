(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2020  Bogdan-Cristian Tataroiu

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

(** [create] returns a new reader that can be used for reading for parsing
    multiple git object files from disk.

    If [Validate_sha1] is passed, then the [read_file] methods will expect a [Sha1.Hex.t]
    to be passed in and the reader will assert that the passed in data matches the
    checksum provided.  Otherwise, if [Do_not_validate_sha1] is passed, [read_file] only
    expects a [unit] argument and does not spend time maintaining the [Sha1] sum of the
    data. *)
val create
  :  on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> on_error:(Error.t -> unit)
  -> 'Sha1_validation Sha1_validation.t
  -> 'Sha1_validation t

(** [read_file] reads the given git object file from disk and calls the
    relevant callbacks out of those specified in [create]. *)
val read_file : 'Sha1_validation t -> file:string -> 'Sha1_validation -> unit Deferred.t

(** [read_file] reads the given git object file from disk and calls the
    relevant callbacks out of those specified in [create]. The [push_back]
    callback is called every time we read a chunk of data from the file
    and it gives the client an opportunity to push back on reading. *)
val read_file'
  :  'Sha1_validation t
  -> file:string
  -> push_back:(unit -> [ `Ok | `Reader_closed ] Deferred.t)
  -> 'Sha1_validation
  -> unit Deferred.t

(** Change the callback that gets called while parsing [blob] objects. *)
val set_on_blob
  :  _ t
  -> on_size:(int -> unit)
  -> on_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> unit

(** Change the callback that gets called while parsing [commit] objects. *)
val set_on_commit : _ t -> (Commit.t -> unit) -> unit

(** Change the callback that gets called while parsing [tree] objects. *)
val set_on_tree_line
  :  _ t
  -> (File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> unit

(** Change the callback that gets called while parsing [tag] objects. *)
val set_on_tag : _ t -> (Tag.t -> unit) -> unit

module Expect_test_helpers : sig
  val blob_reader : 'Sha1_validation Sha1_validation.t -> 'Sha1_validation t
  val commit_reader : 'Sha1_validation Sha1_validation.t -> 'Sha1_validation t
  val tree_reader : 'Sha1_validation Sha1_validation.t -> 'Sha1_validation t
  val tag_reader : 'Sha1_validation Sha1_validation.t -> 'Sha1_validation t
end
