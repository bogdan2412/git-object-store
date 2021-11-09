open! Core
open! Async
open! Import

(** Feed the provided data to the parser in chunks of 16K until the parser consumes less
    data than provided to it. *)
val feed_parser_data
  :  'parser
  -> process:('parser -> Bigstring.t -> pos:int -> len:int -> int)
  -> Bigstring.t
  -> pos:int
  -> int

(** Convenience method that allows one to create a ['resource] that is then used to compute
    a ['result] while making sure to close the ['resource] if computing the ['result]
    returns an error. *)
val with_resource
  :  create:(unit -> 'resource Or_error.t Deferred.t)
  -> close_on_error:('resource -> unit Or_error.t Deferred.t)
  -> f:('resource -> 'result Or_error.t Deferred.t)
  -> 'result Or_error.t Deferred.t

(** Convenience method which opens the provided file and uses it to compute a ['result] while
    making sure to close the file descriptor if computing the ['result] returns an error. *)
val with_file
  :  string
  -> f:(Fd.t -> int -> Bigstring.t -> 'result Or_error.t Deferred.t)
  -> 'result Or_error.t Deferred.t
