open! Core
open! Async
open! Import

type _ t

val create : 'Sha1_validation Sha1_validation.t -> 'Sha1_validation t

val reset_for_reading
  :  'Sha1_validation t
  -> Object_type.t
  -> payload_length:int
  -> on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> 'Sha1_validation
  -> unit

(** Returns number of bytes consumed from input. *)
val process : _ t -> Bigstring.t -> pos:int -> len:int -> int

val finalise : _ t -> unit
