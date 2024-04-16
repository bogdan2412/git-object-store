(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020-2024  Bogdan-Cristian Tataroiu

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

(** Adds an optional [-git-objects-directory] flag pointing to a repository's
    [.git/objects/] directory.  If the flag is not specified, then we try to infer the
    directory by walking up the file system starting with the current working directory
    looking for a [.git] directory or a [.git] file with a [gitdir: ...] redirection in
    it (used by e.g. git submodules). *)
val object_directory_param : string Command.Param.t
