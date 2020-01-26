open Core

type t [@@immediate]

val init : t
val process : t -> Bigstring.t -> pos:int -> len:int -> t
val finalise : t -> int
