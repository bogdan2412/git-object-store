(** zlib bindings for OCaml.

    Copyright (C) 2019  Bogdan-Cristian Tataroiu

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>. *)

open Core

exception Error of { fn_name : string; msg : string; ret_code : int }
[@@deriving sexp_of]

let _ =
  Callback.register_exception
    "Zlib.Error"
    (Error { fn_name = ""; msg = ""; ret_code = 0 })
;;

module Zstream = struct
  type t

  external create_uninitialised : unit -> t = "git_zlib_create_uninitialised_zstream"
  external avail_in : t -> int = "git_zlib_avail_in"
  external avail_out : t -> int = "git_zlib_avail_out"
  external inflate_init : t -> unit = "git_zlib_inflate_init"
  external inflate_reset : t -> unit = "git_zlib_inflate_reset"

  external inflate_process
    :  t
      -> src:Bigstring.t
      -> src_pos:int
      -> src_len:int
      -> dst:Bigstring.t
      -> dst_pos:int
      -> dst_len:int
      -> finish:bool
      -> bool
    = "git_zlib_inflate_process_bytecode" "git_zlib_inflate_process"

  external deflate_init : t -> unit = "git_zlib_deflate_init"
  external deflate_reset : t -> unit = "git_zlib_deflate_reset"

  external deflate_process
    :  t
      -> src:Bigstring.t
      -> src_pos:int
      -> src_len:int
      -> dst:Bigstring.t
      -> dst_pos:int
      -> dst_len:int
      -> finish:bool
      -> bool
    = "git_zlib_deflate_process_bytecode" "git_zlib_deflate_process"
end

let empty_bigstring = Bigstring.create 0

module Make_flate (M : sig
    val zstream_flate_init : Zstream.t -> unit
    val zstream_flate_reset : Zstream.t -> unit

    val zstream_flate_process
      :  Zstream.t
      -> src:Bigstring.t
      -> src_pos:int
      -> src_len:int
      -> dst:Bigstring.t
      -> dst_pos:int
      -> dst_len:int
      -> finish:bool
      -> bool
  end) =
struct
  type t =
    { zstream : Zstream.t
    ; mutable initialised : bool
    ; mutable output : Bigstring.t
    ; mutable output_pos : int
    ; on_data_chunk : Bigstring.t -> pos:int -> len:int -> unit
    }

  let create_uninitialised ~on_data_chunk =
    { zstream = Zstream.create_uninitialised ()
    ; initialised = false
    ; output = Bigstring.create (if Core.am_running_inline_test then 1 else 1 lsl 16)
    ; output_pos = 0
    ; on_data_chunk
    }
  ;;

  let init_or_reset t =
    if t.initialised
    then M.zstream_flate_reset t.zstream
    else (
      t.initialised <- true;
      M.zstream_flate_init t.zstream)
  ;;

  let emit_data_chunk t ~len =
    if len <> 0
    then (
      t.on_data_chunk t.output ~pos:0 ~len;
      t.output_pos <- 0)
  ;;

  let rec process' t ~src ~src_pos ~src_len ~finish =
    let dst_len = Bigstring.length t.output - t.output_pos in
    let finished =
      M.zstream_flate_process
        t.zstream
        ~src
        ~src_pos
        ~src_len
        ~dst:t.output
        ~dst_pos:t.output_pos
        ~dst_len:(Bigstring.length t.output - t.output_pos)
        ~finish
    in
    let src_read = src_len - Zstream.avail_in t.zstream in
    let src_pos = src_pos + src_read in
    let src_len = src_len - src_read in
    let dst_written = dst_len - Zstream.avail_out t.zstream in
    if finished
    then (
      emit_data_chunk t ~len:(t.output_pos + dst_written);
      src_read)
    else if src_len <> 0 || finish
    then (
      let new_buf = Bigstring.create (Bigstring.length t.output * 2) in
      Bigstring.blit
        ~src:t.output
        ~src_pos:0
        ~len:(Bigstring.length t.output)
        ~dst:new_buf
        ~dst_pos:0;
      t.output <- new_buf;
      t.output_pos <- t.output_pos + dst_written;
      src_read + process' t ~src ~src_pos ~src_len ~finish)
    else (
      assert (src_len = 0);
      emit_data_chunk t ~len:(t.output_pos + dst_written);
      src_read)
  ;;

  let process t buf ~pos ~len =
    process' t ~src:buf ~src_pos:pos ~src_len:len ~finish:false
  ;;

  let finalise t =
    let consumed = process' t ~src:empty_bigstring ~src_pos:0 ~src_len:0 ~finish:true in
    assert (consumed = 0)
  ;;
end

module Inflate = Make_flate (struct
    let zstream_flate_init = Zstream.inflate_init
    let zstream_flate_reset = Zstream.inflate_reset
    let zstream_flate_process = Zstream.inflate_process
  end)

module Deflate = struct
  include Make_flate (struct
      let zstream_flate_init = Zstream.deflate_init
      let zstream_flate_reset = Zstream.deflate_reset
      let zstream_flate_process = Zstream.deflate_process
    end)

  let process t buf ~pos ~len =
    let consumed = process t buf ~pos ~len in
    assert (consumed = len)
  ;;
end
