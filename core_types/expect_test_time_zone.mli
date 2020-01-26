open! Core
open! Async
open! Import

val with_fixed_time_zone : (unit -> 'a) -> 'a
val with_fixed_time_zone_async : (unit -> 'a Deferred.t) -> 'a Deferred.t
