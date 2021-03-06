open! Core
open! Import

type t =
  | Commit
  | Tree
  | Blob
  | Tag
[@@deriving sexp]

include Stringable.S with type t := t
