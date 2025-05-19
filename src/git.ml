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

module Author_line = Git_core_types.Author_line
module Commit = Git_core_types.Commit
module File_mode = Git_core_types.File_mode
module Object_parser = Git_object_files.Parser
module Object_reader = Git_object_files.Reader
module Object_store = Git_object_store
module Object_type = Git_core_types.Object_type
module Object_writer = Git_object_files.Writer
module Pack_reader = Git_pack_files.Reader
module Pack_writer = Git_pack_files.Writer
module Sha1 = Git_core_types.Sha1
module Sha1_validation = Git_core_types.Sha1_validation
module Tag = Git_core_types.Tag
module Tree = Git_core_types.Tree
module Tree_cache = Git_tree_cache
module Tree_timestamps = Git_tree_timestamps
module Util = Util
