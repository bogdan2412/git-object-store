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
