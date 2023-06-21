(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021-2023  Bogdan-Cristian Tataroiu

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

(** Feed the provided data to the parser in chunks of 16K until the parser consumes less
    data than provided to it. *)
val feed_parser_data
  :  'parser
  -> process:('parser -> Bigstring.t -> pos:int -> len:int -> int)
  -> Bigstring.t
  -> pos:int
  -> int

(** Convenience method that allows one to create a ['resource] that is then used to compute
    a ['result] while making sure to close the ['resource] if computing the ['result]
    returns an error. *)
val with_resource
  :  create:(unit -> 'resource Or_error.t Deferred.t)
  -> close_on_error:('resource -> unit Or_error.t Deferred.t)
  -> f:('resource -> 'result Or_error.t Deferred.t)
  -> 'result Or_error.t Deferred.t

(** Convenience method which opens the provided file and uses it to compute a ['result] while
    making sure to close the file descriptor if computing the ['result] returns an error. *)
val with_file
  :  string
  -> f:(Fd.t -> int -> Bigstring.t -> 'result Or_error.t Deferred.t)
  -> 'result Or_error.t Deferred.t

module Make_sha1_binary_search (Sha1M : sig
    type t

    val get : t -> int -> char
  end) : sig
  (** Search for the given [Sha1M.t] in an memory mapped index file.

      [index_buf] corresponds to the memory mapped index file buffer.
      [index_pos] corresponds to the position in [index_buf] where the first element in
      the searched range starts.
      [index_element_count] corresponds to the number of elements in the searched range.

      Element [0] occupies bytes [index_pos] to [index_pos + Sha1.Raw.length - 1] inclusive
      in [index_buf]. The last element in the range, [index_element_count - 1], occupies
      bytes [index_pos + (index_element_count - 1) * Sha1.Raw.length] to
      [index_pos + index_element_count * Sha1.Raw.length - 1] inclusive.

      The returned value will be in the range [0 .. index_element_count - 1] assuming that
      the [Sha1M.t] value is found in the range. *)
  val sha1_binary_search
    :  index_buf:Bigstring.t
    -> index_pos:int
    -> index_element_count:int
    -> Sha1M.t
    -> Find_result.Volatile.t
end
