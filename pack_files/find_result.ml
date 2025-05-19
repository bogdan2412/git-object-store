(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021-2025  Bogdan-Cristian Tataroiu

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
include Find_result_intf

module Volatile = struct
  type t =
    | None
    | Some of { mutable index : int }

  let sexp_of_t = function
    | None -> Sexp.List []
    | Some { index } -> Sexp.List [ Sexp.Atom (Int.to_string index) ]
  ;;

  let none = None

  let some =
    let value = Some { index = -1 } in
    match value with
    | Some record ->
      fun index ->
        record.index <- index;
        value
    | None -> assert false
  ;;

  let index_exn = function
    | None -> failwith "SHA1 not present in pack file"
    | Some { index } -> index
  ;;
end
