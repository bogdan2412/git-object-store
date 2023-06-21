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

module type Volatile = sig
  (** Do not keep references to instances of this type as they will be mutated
      on every call to [find_sha1_index]. *)
  type t = private
    | None
    | Some of { mutable index : int }
  [@@deriving sexp_of]

  val index_exn : t -> int
end

module type Public = sig
  module Volatile : Volatile
end

module type Find_result = sig
  module type Public = Public

  module Volatile : sig
    include Volatile

    val none : t
    val some : int -> t
  end
end
