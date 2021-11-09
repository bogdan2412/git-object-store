open! Core
open! Async
open! Import

type t

val open_existing : Index_reader.t -> t Or_error.t Deferred.t

(** Converts an object's pack position (i.e. its rank when ordering objects by offset in the pack
    file) to its index position (i.e. its rank when ordering objects by SHA1).

    Raises on values of [pack_order] which are out of bounds. *)
val index_of_pack_order : t -> pack_order:int -> int

(** Converts an object's index position to its pack position.

    Raises on values of [index] which are out of bounds. *)
val pack_order_of_index : t -> index:int -> int

(** Converts an object's pack position to its corresponding pack file offset.

    Raises on values of [pack_order] which are out of bounds. *)
val pack_file_offset_of_pack_order : t -> pack_order:int -> int

(** Converts a pack file offset to its object's pack position.

    Raises if [pack_file_offset] does not correspond to the offset at which a packed
    object's representation starts. *)
val pack_order_of_pack_file_offset : t -> pack_file_offset:int -> int

(** Converts a pack file offset to its object's index position.

    Raises if [pack_file_offset] does not correspond to the offset at which a packed
    object's representation starts. *)
val index_of_pack_file_offset : t -> pack_file_offset:int -> int
