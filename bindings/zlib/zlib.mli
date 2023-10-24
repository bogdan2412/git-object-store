open Core

exception
  Error of
    { fn_name : string
    ; msg : string
    ; ret_code : int
    }
[@@deriving sexp_of]

module Inflate : sig
  type t

  val create_uninitialised
    :  on_data_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
    -> t

  val init_or_reset : t -> unit

  (** [process] consumes input data from [src] up until the point at which
      the end of the zlib stream is reached or the input is exhausted.
      Returns the number of bytes consumed. If the returned value is not
      equal to [src_len], then the end of the zlib stream was reached
      early. *)
  val process : t -> Bigstring.t -> pos:int -> len:int -> int

  (** [finalise] should be called once no further data is going to be passed
      in. *)
  val finalise : t -> unit
end

module Deflate : sig
  type t

  val create_uninitialised
    :  on_data_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
    -> t

  val init_or_reset : t -> unit

  (** [process] consumes all of the input data from [src] between [pos] and
      [pos + len - 1]. *)
  val process : t -> Bigstring.t -> pos:int -> len:int -> unit

  (** [finalise] should be called once no further data is going to be passed
      in. *)
  val finalise : t -> unit
end
