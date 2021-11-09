open! Core
open! Async
open! Import

module type Volatile = sig
  (** Do not keep references to instances of this type as they will be mutated
      on every call to [find_sha1_index]. *)
  type t = private
    | None
    | Some of { mutable index : int }
  [@@deriving sexp_of]

  val index_exn : t -> int
end

module type Public = sig
  module Volatile : Volatile
end

module type Find_result = sig
  module type Public = Public

  module Volatile : sig
    include Volatile

    val none : t
    val some : int -> t
  end
end
