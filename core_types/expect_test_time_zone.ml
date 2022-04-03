(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020-2022  Bogdan-Cristian Tataroiu

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

let with_fixed_time_zone f =
  let old_zone = Time_ns_unix.get_sexp_zone () in
  Time_ns_unix.set_sexp_zone (Time_ns_unix.Zone.of_string "America/New_York");
  let result = f () in
  Time_ns_unix.set_sexp_zone old_zone;
  result
;;

let with_fixed_time_zone_async f =
  let old_zone = Time_ns_unix.get_sexp_zone () in
  Time_ns_unix.set_sexp_zone (Time_ns_unix.Zone.of_string "America/New_York");
  let%map result = f () in
  Time_ns_unix.set_sexp_zone old_zone;
  result
;;
