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

open! Core
open! Async
open! Import

(** Modules which keeps track of [last_change_time] by path.

    Timestamps for new paths can be added and timestamps for existing
    paths can be updated by calling [update].

    We do not load timestamps for objects that already exist in the tree
    at startup and instead compute them lazily.  We do this to avoid long
    startup times and to avoid loading timestamps for paths that we will
    not access.

    We compute timestamps for on disk objects by binary searching through
    the commit history to find the first commit containing the file at the
    current SHA1 hash.  Note that this assumes only one such transition
    commit exists, if multiple such commits exists, the binary search will
    return one of them.

    This approach requires a linear history, so we approximate timestamps
    by only searching through the chain determined by the first parent of
    each commit. *)
type t

val create
  :  Object_store.Packed.t
  -> Tree_cache.t
  -> current_commit:Sha1.Hex.t option
  -> t Deferred.t

(** Raises on empty [path] or on a path pointing to a directory.

    Updating a file's timestamp will update the timestamp of all directories
    on the path to it. *)
val update_exn : t -> path:string list -> Time_ns.t -> unit

(** Raises if path does not exist or if we cannot find a commit where the
    path matches the SHA1 provided. *)
val last_change_time_exn
  :  t
  -> path:string list
  -> current_sha1:Sha1.Hex.t
  -> Time_ns.t Deferred.t
