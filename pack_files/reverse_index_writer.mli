open! Core
open! Async
open! Import

val write_reverse_index : Index_reader.t -> unit Deferred.t
