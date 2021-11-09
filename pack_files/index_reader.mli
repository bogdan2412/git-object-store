open! Core
open! Async
open! Import

type t =
  { index_file : string
  ; fd : Fd.t
  ; file_size : int
  ; file_mmap : Bigstring.t
  ; offsets : Index_offsets.t
  }

val open_existing
  :  pack_file:string
  -> pack_file_mmap:Bigstring.t
  -> pack_file_size:int
  -> items_in_pack:int
  -> t Or_error.t Deferred.t
