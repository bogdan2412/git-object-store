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

type _ t =
  | Commit : [ `Object ] t
  | Tree : [ `Object ] t
  | Blob : [ `Object ] t
  | Tag : [ `Object ] t
  | Ofs_delta : [ `Delta ] t
  | Ref_delta : [ `Delta ] t

type packed = T : _ t -> packed

let to_object_type : [ `Object ] t -> Object_type.t = function
  | Commit -> Commit
  | Tree -> Tree
  | Blob -> Blob
  | Tag -> Tag
;;

let of_object_type : Object_type.t -> [ `Object ] t = function
  | Commit -> Commit
  | Tree -> Tree
  | Blob -> Blob
  | Tag -> Tag
;;

module Flat = struct
  type t =
    | Commit
    | Tree
    | Blob
    | Tag
    | Ofs_delta
    | Ref_delta
  [@@deriving sexp]

  let to_object_type_exn : t -> Object_type.t = function
    | Commit -> Commit
    | Tree -> Tree
    | Blob -> Blob
    | Tag -> Tag
    | Ofs_delta | Ref_delta ->
      failwith "Cannot convert Delta object types to object types"
  ;;
end

let to_flat (type kind) : kind t -> Flat.t = function
  | Commit -> Commit
  | Tree -> Tree
  | Blob -> Blob
  | Tag -> Tag
  | Ofs_delta -> Ofs_delta
  | Ref_delta -> Ref_delta
;;

let packed_of_flat : Flat.t -> packed = function
  | Commit -> T Commit
  | Tree -> T Tree
  | Blob -> T Blob
  | Tag -> T Tag
  | Ofs_delta -> T Ofs_delta
  | Ref_delta -> T Ref_delta
;;

let packed_to_flat (T t) = to_flat t
