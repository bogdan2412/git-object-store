open! Core
open! Async
open! Import

type _ t =
  | Commit : [ `Object ] t
  | Tree : [ `Object ] t
  | Blob : [ `Object ] t
  | Tag : [ `Object ] t
  | Ofs_delta : [ `Delta ] t
  | Ref_delta : [ `Delta ] t

type packed = T : _ t -> packed

val to_object_type : [ `Object ] t -> Object_type.t
val of_object_type : Object_type.t -> [ `Object ] t

module Flat : sig
  type t =
    | Commit
    | Tree
    | Blob
    | Tag
    | Ofs_delta
    | Ref_delta
  [@@deriving sexp]

  val to_object_type_exn : t -> Object_type.t
end

val to_flat : _ t -> Flat.t
val packed_of_flat : Flat.t -> packed
val packed_to_flat : packed -> Flat.t
