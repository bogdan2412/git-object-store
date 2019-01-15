open Core

type t = int

let init = 0

external process : t -> Bigstring.t -> pos:int -> len:int -> t = "git_crc32_process"

let finalise t = t
