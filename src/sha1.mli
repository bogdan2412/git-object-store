open Core

module Hex : sig
  type t

  val length : int

  include Stringable.S with type t := t
  include Sexpable.S with type t := t

  module Volatile :
  sig
    type non_volatile
    type t

    val create : unit -> t
    val non_volatile : t -> non_volatile
    val bytes : t -> Bytes.t
    val is_valid : t -> bool

    include Stringable.S with type t := t
    include Sexpable.S with type t := t
  end
  with type non_volatile := t
end

module Raw : sig
  type t

  val length : int

  include Comparable.S with type t := t
  include Stringable.S with type t := t
  include Sexpable.S with type t := t

  val of_hex : Hex.t -> t
  val of_hex_volatile : Hex.Volatile.t -> t
  val to_hex : t -> Hex.t
  val to_hex_volatile : t -> Hex.Volatile.t -> unit

  module Volatile :
  sig
    type non_volatile
    type t

    val create : unit -> t
    val non_volatile : t -> non_volatile
    val bytes : t -> Bytes.t

    include Stringable.S with type t := t
    include Sexpable.S with type t := t

    val of_hex : Hex.t -> t -> unit
    val of_hex_volatile : Hex.Volatile.t -> t -> unit
    val to_hex : t -> Hex.t
    val to_hex_volatile : t -> Hex.Volatile.t -> unit
  end
  with type non_volatile := t
end

module Compute : sig
  type _ t

  val create_uninitialised : unit -> [`Uninitialised] t
  val init_or_reset : _ t -> [`Initialised] t
  val process : [`Initialised] t -> Bigstring.t -> pos:int -> len:int -> unit
  val finalise : [`Initialised] t -> [`Finalised] t
  val get_hex : [`Finalised] t -> Hex.Volatile.t
  val get_raw : [`Finalised] t -> Raw.Volatile.t
end
