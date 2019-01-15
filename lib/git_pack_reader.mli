open Core
open Async

type t

val create : pack_file:string -> t Or_error.t Deferred.t
val index_pack : pack_file:string -> unit Or_error.t Deferred.t
val items_in_pack : t -> int
val sha1 : t -> index:int -> Sha1.Raw.Volatile.t

val read_object
  :  t
  -> index:int
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> unit

val find_sha1_index : t -> Sha1.Raw.t -> int
val find_sha1_index' : t -> Sha1.Raw.Volatile.t -> int
