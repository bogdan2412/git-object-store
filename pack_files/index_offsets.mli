open! Core
open! Async
open! Import

type t

val create : items_in_pack:int -> t
val fanout : t -> char -> int
val sha1 : t -> int -> int
val crc32 : t -> int -> int
val offset : t -> int -> int
val uint64_offset : t -> int -> int
