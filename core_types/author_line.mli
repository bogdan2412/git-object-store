(** Library for manipulating a git object store via OCaml.

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

open! Core
open! Import

type t =
  { name : string
  ; email : string
  ; timestamp : Time_ns_unix.t
  ; zone : Time_ns_unix.Zone.t
  }
[@@deriving sexp]

(** Parses a line of the form

    Bogdan-Cristian Tataroiu <bogdan@example.com> 1547392527 +0000 *)
val parse_exn : string -> t

(** Formats a line of the form

    author Bogdan-Cristian Tataroiu <bogdan@example.com> 1547392527 +0000 *)
val format : t -> line_prefix:string -> string
