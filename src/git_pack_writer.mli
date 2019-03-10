(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019  Bogdan-Cristian Tataroiu

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
open Async

type t

(** Begins writing a new pack file. *)
val create : pack_directory:string -> t Or_error.t Deferred.t

(** Aborts writing the pack file and cleans up temporary files. *)
val abort : t -> unit Or_error.t Deferred.t

(** Adds git object file from local disk to the new pack file.
    Exceptions will abort writing of the entire pack file. *)
val add_object_exn : t -> object_file:string -> unit Deferred.t

(** Finalises the pack file and returns the path to the newly created file.
    Exceptions will abort writing of the entire pack file. *)
val finalise_exn : t -> string Deferred.t
