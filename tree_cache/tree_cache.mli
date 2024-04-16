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
open! Async
open! Import

(** Provides an interface for lazily loading Tree objects from disk,
    mutating them in memory and persisting changes to disk on request. *)
type t

module File : sig
  module Kind : sig
    type t =
      | Regular_file
      | Executable_file
      | Link
    [@@deriving compare, sexp_of]
  end

  type t =
    { sha1 : Sha1.Hex.t
    ; kind : Kind.t
    }
  [@@deriving compare, sexp_of]
end

module Node : sig
  type tree_cache
  type _ state
  type t

  module Entry : sig
    type node := t

    type t =
      | File of File.t
      | Directory of node
      | Submodule of Sha1.Hex.t
  end

  (** Constructors *)

  val empty : unit -> t
  val of_disk_hash : Sha1.Hex.t -> t

  (** Accessors *)

  val ensure_loaded : tree_cache -> t -> [ `Loaded ] state Deferred.t
  val directories : [ `Loaded ] state -> t String.Map.t
  val files : [ `Loaded ] state -> File.t String.Map.t
  val submodules : [ `Loaded ] state -> Sha1.Hex.t String.Map.t

  (** Path accessors *)

  val get_entry : tree_cache -> t -> path:string list -> Entry.t option Deferred.t
  val get_file : tree_cache -> t -> path:string list -> File.t option Deferred.t
  val get_node : tree_cache -> t -> path:string list -> t option Deferred.t

  val get_submodule
    :  tree_cache
    -> t
    -> path:string list
    -> Sha1.Hex.t option Deferred.t

  (** Mutation methods *)

  val add_entry : tree_cache -> t -> path:string list -> Entry.t -> t Deferred.t

  val add_file
    :  tree_cache
    -> t
    -> path:string list
    -> Sha1.Hex.t
    -> File.Kind.t
    -> t Deferred.t

  val add_node : tree_cache -> t -> path:string list -> t -> t Deferred.t
  val add_submodule : tree_cache -> t -> path:string list -> Sha1.Hex.t -> t Deferred.t
  val remove_path : tree_cache -> t -> path:string list -> t option Deferred.t

  (** Persistent storage *)

  val is_persisted : t -> bool
  val persist : tree_cache -> t -> Sha1.Hex.t Deferred.t
end
with type tree_cache := t

(** Constructors *)

val create : Object_store.Packed.t -> root:Node.t -> t

(** Accessors *)

val root : t -> Node.t

(** Path accessors *)

val get_entry : t -> path:string list -> Node.Entry.t option Deferred.t
val get_file : t -> path:string list -> File.t option Deferred.t
val get_node : t -> path:string list -> Node.t option Deferred.t
val get_submodule : t -> path:string list -> Sha1.Hex.t option Deferred.t

(** Mutation methods *)

val add_entry : t -> path:string list -> Node.Entry.t -> unit Deferred.t
val add_file : t -> path:string list -> Sha1.Hex.t -> File.Kind.t -> unit Deferred.t
val add_node : t -> path:string list -> Node.t -> unit Deferred.t
val add_submodule : t -> path:string list -> Sha1.Hex.t -> unit Deferred.t
val remove_path : t -> path:string list -> unit Deferred.t

(** Persistent storage *)

val is_persisted : t -> bool
val persist : t -> Sha1.Hex.t Deferred.t
