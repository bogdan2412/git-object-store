open! Core
open! Async
open! Import

(** Skip over bytes that are used to represent a variable length integer and return the
    next position in the buffer from which other data can be read. *)
val skip_variable_length_integer : Bigstring.t -> pos:int -> int

(** Read a variable length integer from the given location in the provided buffer. *)
val read_variable_length_integer : Bigstring.t -> pos:int -> int

(** Read a variable length integer used to encode relative offsets to parent objects for
    [Ofs_delta] pack object types from the given location in the provided buffer. *)
val read_variable_length_relative_offset : Bigstring.t -> pos:int -> int

(** Read a given pack object's type from the given location in the provided buffer. *)
val object_type : Bigstring.t -> pos:int -> Pack_object_type.packed

(** Read a given pack object's length from the given location in the provided buffer. *)
val object_length : Bigstring.t -> pos:int -> int
