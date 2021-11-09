(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021  Bogdan-Cristian Tataroiu

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

type _ t =
  | Commit : [ `Object ] t
  | Tree : [ `Object ] t
  | Blob : [ `Object ] t
  | Tag : [ `Object ] t
  | Ofs_delta : [ `Delta ] t
  | Ref_delta : [ `Delta ] t

type packed = T : _ t -> packed

val to_object_type : [ `Object ] t -> Object_type.t
val of_object_type : Object_type.t -> [ `Object ] t

module Flat : sig
  type t =
    | Commit
    | Tree
    | Blob
    | Tag
    | Ofs_delta
    | Ref_delta
  [@@deriving sexp]

  val to_object_type_exn : t -> Object_type.t
end

val to_flat : _ t -> Flat.t
val packed_of_flat : Flat.t -> packed
val packed_to_flat : packed -> Flat.t
