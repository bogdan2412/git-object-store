(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020-2025  Bogdan-Cristian Tataroiu

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

open! Import
module Author_line = Author_line
module Commit = Commit
module Expect_test_time_zone = Expect_test_time_zone
module File_mode = File_mode
module Object_type = Object_type
module Sha1_validation = Sha1_validation
module Tag = Tag
module Tree = Tree

module Sha1 = struct
  module Hex = Sha1.Hex
  module Raw = Sha1.Raw
end
