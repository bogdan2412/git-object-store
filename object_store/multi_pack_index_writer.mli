open! Core
open! Async
open! Import

val write_multi_pack_index_file
  :  pack_directory:string
  -> preferred_pack:string option
  -> unit Deferred.t
