open! Core
open! Import

type t =
  { tree : Sha1.Hex.t
  ; parents : Sha1.Hex.t list
  ; author : Author_line.t
  ; committer : Author_line.t
  ; encoding : string option
  ; merge_tags : Tag.t list
  ; gpg_signature : string option
  ; description : string
  }
[@@deriving sexp]

val parse_git_object_payload_exn : string -> t
val format_as_git_object_payload : t -> string

module For_testing : sig
  val example_git_object_payload : string
  val example_commit : t
end
