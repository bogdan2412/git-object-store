open! Core
open! Async
open! Import

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

(** Adds git object file from an existing pack to the new pack file.
    Exceptions will abort writing of the entire pack file. *)
val add_object_from_pack_exn
  :  'Sha1_validation t
  -> 'Sha1_validation Pack_reader.t
  -> index:int
  -> unit Deferred.t

module Low_level : sig
  (** Adds git object file from an existing pack to the new pack file.
      Exceptions will abort writing of the entire pack file. *)
  val add_object_from_pack_exn
    :  'Sha1_validation t
    -> 'Sha1_validation Pack_reader.t
    -> pack_offset:int
    -> 'Sha1_validation
    -> unit Deferred.t
end

(** Finalises the pack file and returns the path to the newly created file.
    Exceptions will abort writing of the entire pack file. *)
val finalise_exn : _ t -> string Deferred.t
