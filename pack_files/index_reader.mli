open! Core
open! Async
open! Import

type t

val open_existing
  :  pack_file:string
  -> pack_file_mmap:Bigstring.t
  -> pack_file_size:int
  -> items_in_pack:int
  -> t Or_error.t Deferred.t

(** Returns the sha1 corresponding to the object with the provided index position. *)
val sha1 : t -> index:int -> Sha1.Raw.Volatile.t

(** Returns the position in the pack file where the object with the provided index position
    can be found. *)
val pack_file_offset : t -> index:int -> int

(** Search for an object with the given SHA1 hash in the pack. *)
val find_sha1_index : t -> Sha1.Raw.t -> Find_result.Volatile.t

(** Search for an object with the given SHA1 hash in the pack. *)
val find_sha1_index' : t -> Sha1.Raw.Volatile.t -> Find_result.Volatile.t
