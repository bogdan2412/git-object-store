open! Core

type _ t =
  | Do_not_validate_sha1 : unit t
  | Validate_sha1 : Sha1.Hex.t t
