open! Core
open! Async
open! Import

type t

val open_existing : Multi_pack_index_reader.t -> t Or_error.t Deferred.t

(** Converts an object's pseudo pack position (i.e. its rank when ordering pack files in
    (preferred-ness, lexicographic) order and objects in each pack file by offset) to its
    multi pack index position (i.e. its rank when ordering all objects by SHA1).

    Raises on values of [pseudo_pack_order] which are out of bounds. *)
val index_of_pseudo_pack_order : t -> pseudo_pack_order:int -> int

(** Converts an object's multi pack index position to its pack position.

    Raises on values of [index] which are out of bounds. *)
val pseudo_pack_order_of_index : t -> index:int -> int

(** Converts an object's pseudo pack position to its corresponding pack id.

    Raises on values of [pseudo_pack_order] which are out of bounds. *)
val pack_id_of_pseudo_pack_order : t -> pseudo_pack_order:int -> int

(** Converts an object's pseudo pack position to its corresponding pack file offset.

    Raises on values of [pseudo_pack_order] which are out of bounds. *)
val pack_offset_of_pseudo_pack_order : t -> pseudo_pack_order:int -> int

(** Converts a pack id and pack file offset to its object's pseudo pack position.

    Raises if the [pack_id, pack_offset] pair does not correspond to the offset at which a
    packed object's representation starts. *)
val pseudo_pack_order_of_pack_id_and_offset : t -> pack_id:int -> pack_offset:int -> int

(** Converts a pack id and pack file offset to its object's multi pack index position.

    Raises if the [pack_id, pack_offset] pair does not correspond to the offset at which a
    packed object's representation starts. *)
val index_of_pack_id_and_offset : t -> pack_id:int -> pack_offset:int -> int
