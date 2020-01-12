(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2020  Bogdan-Cristian Tataroiu

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

type _ t

(** Begins writing a new pack file.

    If [Validate_sha1] is passed, then the [add_object_exn] method will expect a
    [Sha1.Hex.t] to be passed in and the writer will assert that the passed in object
    file matches the checksum provided.  Otherwise, if [Do_not_validate_sha1] is passed,
    the writer will not spend time validating the [Sha1] sum of the data. *)
val create
  :  pack_directory:string
  -> 'Sha1_validation Sha1_validation.t
  -> 'Sha1_validation t Or_error.t Deferred.t

(** Aborts writing the pack file and cleans up temporary files. *)
val abort : _ t -> unit Or_error.t Deferred.t

(** Adds git object file from local disk to the new pack file.
    Exceptions will abort writing of the entire pack file. *)
val add_object_exn
  :  'Sha1_validation t
  -> object_file:string
  -> 'Sha1_validation
  -> unit Deferred.t

(** Finalises the pack file and returns the path to the newly created file.
    Exceptions will abort writing of the entire pack file. *)
val finalise_exn : _ t -> string Deferred.t
