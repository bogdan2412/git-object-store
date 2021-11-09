open! Core
open! Async
open! Import

val index_pack
  :  pack_file:string
  -> pack_file_mmap:Bigstring.t
  -> items_in_pack:int
  -> unit Deferred.t
