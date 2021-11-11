open! Core
open! Async
open! Import

val validate_preferred_pack
  :  pack_directory:string
  -> preferred_pack:string option
  -> string option
