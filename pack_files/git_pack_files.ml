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

module Reader = Pack_reader
module Writer = Pack_writer

module Low_level = struct
  module Find_result = Find_result
  module Index_reader = Index_reader
  module Reverse_index_reader = Reverse_index_reader

  module Util = struct
    open Util

    let with_resource = with_resource
    let with_file = with_file

    module Make_sha1_binary_search = Make_sha1_binary_search
  end
end

module Find_result : Find_result.Public with type Volatile.t = Find_result.Volatile.t =
  Find_result
