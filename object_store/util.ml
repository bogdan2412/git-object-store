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

let validate_preferred_pack ~pack_directory ~preferred_pack =
  Option.map preferred_pack ~f:(fun preferred_pack ->
    let preferred_pack =
      match Filename.is_posix_pathname_component preferred_pack with
      | true -> pack_directory ^/ preferred_pack
      | false -> preferred_pack
    in
    (match
       String.( = )
         (Filename_unix.realpath pack_directory)
         (Filename_unix.realpath (Filename.dirname preferred_pack))
     with
     | false ->
       raise_s
         [%message
           "Preferred pack file not within pack directory"
             (pack_directory : string)
             (preferred_pack : string)]
     | true -> ());
    match String.is_suffix ~suffix:".pack" preferred_pack with
    | false ->
      raise_s
        [%message
          "Expected preferred pack file to end with .pack extension"
            (preferred_pack : string)]
    | true -> preferred_pack)
;;
