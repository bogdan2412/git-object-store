open Core
open Async

type t

(** Creates a pack reader given a pack file. The reader expects a matching index file
    to exist. *)
val create : pack_file:string -> t Or_error.t Deferred.t

(** Read a pack file and generate a corresponding index file for it. *)
val index_pack : pack_file:string -> unit Or_error.t Deferred.t

(** Returns number of items in the pack. *)
val items_in_pack : t -> int

(** Returns the SHA1 hash on the [index]-th item in the pack.
    Raises if [index] is outside of the range [0 .. items_in_pack - 1]. *)
val sha1 : t -> index:int -> Sha1.Raw.Volatile.t

module Find_result : sig
  module Volatile : sig
    (** Do not keep references to instances of this type as they will be mutated
        on every call to [find_sha1_index]. *)
    type t = private
      | None
      | Some of { mutable index : int }
    [@@deriving sexp_of]

    val none : t
    val some : int -> t
    val index_exn : t -> int
  end
end

(** Search for an object with the given SHA1 hash in the pack. *)
val find_sha1_index : t -> Sha1.Raw.t -> Find_result.Volatile.t

(** Search for an object with the given SHA1 hash in the pack. *)
val find_sha1_index' : t -> Sha1.Raw.Volatile.t -> Find_result.Volatile.t

(** Read and parse the [index]-th index in the pack.
    Raises if [index] is outside of the range [0 .. items_in_pack - 1]. *)
val read_object
  :  t
  -> index:int
  -> on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> unit

(** Reads the [index]-th index in the pack without parsing the payload.
    Raises if [index] is outside of the range [0 .. items_in_pack - 1]. *)
val read_raw_object
  :  t
  -> index:int
  -> on_header:(Object_type.t -> size:int -> unit)
  -> on_payload:(Bigstring.t -> pos:int -> len:int -> unit)
  -> unit

module For_testing : sig
  val print_out_pack_file : string -> unit Deferred.t
end
