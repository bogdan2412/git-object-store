open Core
open Async

type t

val create : pack_file:string -> t Or_error.t Deferred.t
val index_pack : pack_file:string -> unit Or_error.t Deferred.t
val items_in_pack : t -> int

(** Raises if [index] is outside of the range [0 .. items_in_pack - 1]. *)
val sha1 : t -> index:int -> Sha1.Raw.Volatile.t

val read_object
  :  t
  -> index:int
  -> on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> unit

val read_raw_object
  :  t
  -> index:int
  -> on_header:(Object_type.t -> size:int -> unit)
  -> on_payload:(Bigstring.t -> pos:int -> len:int -> unit)
  -> unit

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

val find_sha1_index : t -> Sha1.Raw.t -> Find_result.Volatile.t
val find_sha1_index' : t -> Sha1.Raw.Volatile.t -> Find_result.Volatile.t

module For_testing : sig
  val print_out_pack_file : string -> unit Deferred.t
end
