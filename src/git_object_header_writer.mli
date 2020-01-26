open! Core
open! Import

(** [header_length] returns the length required to write down a git object header for an
    object of the given type and length. *)
val header_length : Object_type.t -> object_length:int -> int

(** [write_from_left] writes a git object header for the given object type and length to
    the specified [Bigstring.t] starting from position [pos].

    The method assumes that the given Bigstring has enough space available to be able to
    write the entire header.

    The return value corresponds to the length of the header data written. *)
val write_from_left : Bigstring.t -> pos:int -> Object_type.t -> object_length:int -> int

(** [write_from_right] writes a git object header for the given object type and length to
    the specified [Bigstring.t] ending on position [pos].

    The method assumes that the given Bigstring has enough space available to be able to
    write the entire header.

    The return value corresponds to the length of the header data written. *)
val write_from_right : Bigstring.t -> pos:int -> Object_type.t -> object_length:int -> int
