open Core
open Async

type t

(** [create] returns a new reader that can be used for reading for parsing
    multiple git object files from disk. *)
val create
  :  on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> on_error:(Error.t -> unit)
  -> t

(** [read_file] reads the given git object file from disk and calls the
    relevant callbacks out of those specified in [create]. *)
val read_file : t -> file:string -> unit Deferred.t

(** [read_file] reads the given git object file from disk and calls the
    relevant callbacks out of those specified in [create]. The [push_back]
    callback is called every time we read a chunk of data from the file
    and it gives the client an opportunity to push back on reading. *)
val read_file'
  :  t
  -> file:string
  -> push_back:(unit -> unit Deferred.t)
  -> unit Deferred.t

(** Change the callback that gets called while parsing [blob] objects. *)
val set_on_blob
  :  t
  -> on_size:(int -> unit)
  -> on_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> unit

(** Change the callback that gets called while parsing [commit] objects. *)
val set_on_commit : t -> (Commit.t -> unit) -> unit

(** Change the callback that gets called while parsing [tree] objects. *)
val set_on_tree_line
  :  t
  -> (File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> unit

(** Change the callback that gets called while parsing [tag] objects. *)
val set_on_tag : t -> (Tag.t -> unit) -> unit

module Expect_test_helpers : sig
  val blob_reader : unit -> t
  val commit_reader : unit -> t
  val tree_reader : unit -> t
  val tag_reader : unit -> t
end
