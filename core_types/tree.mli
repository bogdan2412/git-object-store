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

open! Core
open! Import

module Git_object_payload_parser : sig
  module State : sig
    type t [@@deriving sexp_of]

    val create
      :  emit_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
      -> t

    val reset : t -> unit
  end

  (** Read tree lines from the bigstring range defined by [pos, pos + len - 1].

      Returns how many bytes were consumed from the input.

      The expectation is that any unconsumed adata that remains after one call
      will be passed in again on the next call.

      Raises on invalid input.
  *)
  val consume_payload_exn : State.t -> Bigstring.t -> pos:int -> len:int -> int
end

module Git_object_payload_formatter : sig
  module Write_result : sig
    (** Don't keep references to values of this type since we will reuse the same
        object across multiple calls to write. *)
    type t = private
      | Need_more_space
      | Wrote of { mutable bytes : int }
  end

  val write_tree_line
    :  File_mode.t
    -> Sha1.Raw.t
    -> name:string
    -> Bigstring.t
    -> pos:int
    -> len:int
    -> Write_result.t

  val write_tree_line'
    :  File_mode.t
    -> Sha1.Raw.Volatile.t
    -> name:string
    -> Bigstring.t
    -> pos:int
    -> len:int
    -> Write_result.t
end

module For_testing : sig
  val example_git_object_payload : string
end
