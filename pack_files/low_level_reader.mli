open! Core
open! Async
open! Import

(** Skip over bytes that are used to represent a variable length integer and return the
    next position in the buffer from which other data can be read. *)
val skip_variable_length_integer : Bigstring.t -> pos:int -> int

(** Read a variable length integer from the given location in the provided buffer. *)
val read_variable_length_integer : Bigstring.t -> pos:int -> int
