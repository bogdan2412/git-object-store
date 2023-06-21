(** zlib bindings for OCaml.

    Copyright (C) 2019-2023  Bogdan-Cristian Tataroiu

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

exception
  Error of
    { fn_name : string
    ; msg : string
    ; ret_code : int
    }
[@@deriving sexp_of]

module Inflate : sig
  type t

  val create_uninitialised
    :  on_data_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
    -> t

  val init_or_reset : t -> unit

  (** [process] consumes input data from [src] up until the point at which
      the end of the zlib stream is reached or the input is exhausted.
      Returns the number of bytes consumed. If the returned value is not
      equal to [src_len], then the end of the zlib stream was reached
      early.
  *)
  val process : t -> Bigstring.t -> pos:int -> len:int -> int

  (** [finalise] should be called once no further data is going to be passed
      in. *)
  val finalise : t -> unit
end

module Deflate : sig
  type t

  val create_uninitialised
    :  on_data_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
    -> t

  val init_or_reset : t -> unit

  (** [process] consumes all of the input data from [src] between [pos] and
      [pos + len - 1]. *)
  val process : t -> Bigstring.t -> pos:int -> len:int -> unit

  (** [finalise] should be called once no further data is going to be passed
      in. *)
  val finalise : t -> unit
end
