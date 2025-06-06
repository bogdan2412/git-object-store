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

val write_multi_pack_index_file
  :  pack_directory:string
  -> preferred_pack:string option
  -> unit Deferred.t

module For_testing : sig
  val make_pack_files
    :  object_directory:string
    -> string list list
    -> string list Deferred.t

  val three_overlapping_packs_example : string list list
end
