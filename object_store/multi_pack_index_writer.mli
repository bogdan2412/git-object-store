open! Core
open! Async
open! Import

val write_multi_pack_index_file
  :  pack_directory:string
  -> preferred_pack:string option
  -> unit Deferred.t

module For_testing : sig
  val make_pack_files
    :  object_directory:string
    -> string list list
    -> string list Deferred.t

  val three_overlapping_packs_example : string list list
end
