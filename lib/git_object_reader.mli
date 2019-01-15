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

(** [create] returns a new reader that can be used for reading for parsing
    multiple git object files from disk. *)
val create
  :  on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> on_error:(Error.t -> unit)
  -> t

(** [read_file] reads the given git object file from disk and calls the
    relevant callbacks out of those specified in [create]. *)
val read_file : t -> file:string -> unit Deferred.t

(** Change the callback that gets called while parsing [blob] objects. *)
val set_on_blob_chunk : t -> (Bigstring.t -> pos:int -> len:int -> unit) -> unit

(** Change the callback that gets called while parsing [commit] objects. *)
val set_on_commit : t -> (Commit.t -> unit) -> unit

(** Change the callback that gets called while parsing [tree] objects. *)
val set_on_tree_line
  :  t
  -> (File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> unit

(** Change the callback that gets called while parsing [tag] objects. *)
val set_on_tag : t -> (Tag.t -> unit) -> unit

module Expect_test_helpers : sig
  val blob_reader : unit -> t
  val commit_reader : unit -> t
  val tree_reader : unit -> t
  val tag_reader : unit -> t
end