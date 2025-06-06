(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2025  Bogdan-Cristian Tataroiu

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
  { tree : Sha1.Hex.t
  ; parents : Sha1.Hex.t list
  ; author : Author_line.t
  ; committer : Author_line.t
  ; encoding : string option
  ; merge_tags : Tag.t list
  ; gpg_signature : string option
  ; description : string
  }
[@@deriving sexp]

val parse_git_object_payload_exn : string -> t
val format_as_git_object_payload : t -> string

module For_testing : sig
  val example_git_object_payload : string
  val example_commit : t
end
