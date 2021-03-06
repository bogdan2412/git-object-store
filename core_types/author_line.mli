open! Core
open! Import

type t =
  { name : string
  ; email : string
  ; timestamp : Time_ns_unix.t
  ; zone : Time_ns_unix.Zone.t
  }
[@@deriving sexp]

(** Parses a line of the form

    Bogdan-Cristian Tataroiu <bogdan@example.com> 1547392527 +0000 *)
val parse_exn : string -> t

(** Formats a line of the form

    author Bogdan-Cristian Tataroiu <bogdan@example.com> 1547392527 +0000 *)
val format : t -> line_prefix:string -> string
