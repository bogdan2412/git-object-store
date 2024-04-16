(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020-2024  Bogdan-Cristian Tataroiu

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

type 'a t

module Handle : sig
  type 'a t

  val to_int : 'a t -> int
  val unsafe_of_int : int -> 'a t
end

val create : initial_capacity:int -> empty_value:'a -> 'a t
val lease : 'a t -> 'a -> 'a Handle.t
val get : 'a t -> 'a Handle.t -> 'a
val set : 'a t -> 'a Handle.t -> 'a -> unit
val release : 'a t -> 'a Handle.t -> unit
