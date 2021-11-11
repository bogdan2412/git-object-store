open! Core
open! Async
open! Import

val write_multi_pack_reverse_index
  :  Multi_pack_index_reader.t
  -> preferred_pack:string option
  -> unit Deferred.t
