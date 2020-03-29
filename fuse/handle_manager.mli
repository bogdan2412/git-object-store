open! Core
open! Async
open! Import

type 'a t

module Handle : sig
  type 'a t

  val to_int : 'a t -> int
  val unsafe_of_int : int -> 'a t
end

val create : initial_capacity:int -> empty_value:'a -> 'a t
val lease : 'a t -> 'a -> 'a Handle.t
val get : 'a t -> 'a Handle.t -> 'a
val set : 'a t -> 'a Handle.t -> 'a -> unit
val release : 'a t -> 'a Handle.t -> unit
