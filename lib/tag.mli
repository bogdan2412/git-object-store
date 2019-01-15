open! Core

type t =
  { object_sha1 : Sha1.Hex.t
  ; object_type : Object_type.t
  ; tag : string
  ; tagger : Author_line.t option
  ; description : string
  }
[@@deriving sexp_of]

val parse_git_object_payload_exn : string -> t
val format_as_git_object_payload : t -> string

module For_testing : sig
  val example_git_object_payload : string
  val example_tag : t
end
