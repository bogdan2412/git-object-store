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

type t [@@deriving sexp_of]

(** [create] returns a new parser that can be fed git object data incrementally and
    reused to parse multiple git objects. *)
val create
  :  on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> on_error:(Error.t -> unit)
  -> t

(** [append_data] should be called to feed the parser a chunk of the git object data
    for the currently parsed object. Some of the callbacks which were specified in
    [create] might be called. *)
val append_data : t -> Bigstring.t -> pos:int -> len:int -> unit

(** [finalise] should be called once the entire git object has been fed to the
    parser. Some of the callbacks which were specified in [create] might be called. *)
val finalise : t -> unit

(** [reset] should be called to start parsing a new object. *)
val reset : t -> unit

(** [set_state_reading_blob] can be called after [reset] to signal to the parser that
    what follows is a blob of the specified length. *)
val set_state_reading_blob : t -> payload_length:int -> unit

(** [set_state_reading_commit] can be called after [reset] to signal to the parser that
    what follows is a commit of the specified length. *)
val set_state_reading_commit : t -> payload_length:int -> unit

(** [set_state_reading_tree] can be called after [reset] to signal to the parser that
    what follows is a tree of the specified length. *)
val set_state_reading_tree : t -> payload_length:int -> unit

(** [set_state_reading_tag] can be called after [reset] to signal to the parser that
    what follows is a tag of the specified length. *)
val set_state_reading_tag : t -> payload_length:int -> unit

(** Change the callback that gets called while parsing [blob] objects. *)
val set_on_blob
  :  t
  -> on_size:(int -> unit)
  -> on_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> unit

(** Change the callback that gets called while parsing [commit] objects. *)
val set_on_commit : t -> (Commit.t -> unit) -> unit

(** Change the callback that gets called while parsing [tree] objects. *)
val set_on_tree_line
  :  t
  -> (File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> unit

(** Change the callback that gets called while parsing [tag] objects. *)
val set_on_tag : t -> (Tag.t -> unit) -> unit

(** Lower-level module that allows one to obtain raw payload data rather than have it be
    parsed. *)
module Raw : sig
  type t

  (** [create] returns a new raw parser that can be fed git object data incrementally and
      reused to parse multiple git objects.

      The return value of [on_payload_chunk] should be the number of bytes from the
      input that have been read.  Any unconsumed data will be fed to the method on
      the next call.

      Whenever more payload data becomes available, the [on_payload_chunk] method will
      be repeatedly called until either no data is left or until no data is consumed.

      Upon [finalise] being called, the [on_payload_chunk] will be called a final time
      with a value of [~final:true]. This final call may have a [len] of 0.
  *)
  val create
    :  on_header:(Object_type.t -> size:int -> unit)
    -> on_payload_chunk:(Bigstring.t -> pos:int -> len:int -> final:bool -> int)
    -> on_error:(Error.t -> unit)
    -> t

  val append_data : t -> Bigstring.t -> pos:int -> len:int -> unit
  val finalise : t -> unit
  val reset : t -> unit
end