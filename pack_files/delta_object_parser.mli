(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021  Bogdan-Cristian Tataroiu

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

open! Core
open! Async
open! Import

type ('Sha1_validation, _, _, _) t

type 'Sha1_validation packed_result =
  | T :
      ( 'Sha1_validation
      , [< `No_base | `Have_base ]
      , [< `No_delta | `Have_delta ]
      , [ `Have_result of [ `Object ] ] )
        t
      -> 'Sha1_validation packed_result

(** An instance of [t] contains three buffers: [base], [delta] and [result].

    [base] represents the base git object
    [delta] represents the set of delta instructions applied on top of [base].
    [result] will contain the result of applying [delta] to [base].
*)
val create
  :  'Sha1_validation Sha1_validation.t
  -> ('Sha1_validation, [ `No_base ], [ `No_delta ], [ `No_result ]) t

val reset
  :  ( 'Sha1_validation
     , [< `No_base | `Have_base ]
     , [< `No_delta | `Have_delta ]
     , [< `No_result | `Computing_result of _ | `Have_result of _ ] )
       t
  -> ('Sha1_validation, [ `No_base ], [ `No_delta ], [ `No_result ]) t

(** Returns the buffer containing a computed [result]. *)
val result_buf : (_, _, _, [ `Have_result of _ ]) t -> Bigstring.t

(** Returns length of computed [result]. *)
val result_len : (_, _, _, [ `Have_result of _ ]) t -> int

(** Reset [result] buffer and prepare to receive zlib compressed data which will be
    decompressed into it. *)
val begin_zlib_inflate_into_result
  :  ('Sha1_validation, 'base, 'delta, _) t
  -> 'kind Pack_object_type.t
  -> expected_length:int
  -> ('Sha1_validation, 'base, 'delta, [ `Computing_result of 'kind ]) t

(** Feed [result] buffer a chunk of zlib compressed data. *)
val feed_result_zlib_inflate
  :  (_, _, _, [ `Computing_result of _ ]) t
  -> Bigstring.t
  -> pos:int
  -> len:int
  -> int

(** Finish decompressing into [result] buffer.

    Raises if the resulting buffer's size does not match the [expected_length] value we
    initialized with. *)
val finalise_result_zlib_inflate_exn
  :  ('Sha1_validation, 'base, 'delta, [ `Computing_result of 'kind ]) t
  -> ('Sha1_validation, 'base, 'delta, [ `Have_result of 'kind ]) t

(** Replace the [base] buffer with the provided buffer and length and call the provided
    method. The original base buffer is placed back once the method returns.

    Note that the contents in the buffer is _not_ copied. *)
val with_base_buffer_and_length
  :  ('Sha1_validation, 'base, 'delta, _) t
  -> [ `Object ] Pack_object_type.t
  -> Bigstring.t
  -> length:int
  -> (('Sha1_validation, [ `Have_external_base ], 'delta, [ `No_result ]) t
      -> ( 'Sha1_validation
         , [ `Have_external_base ]
         , 'delta
         , [ `Have_result of [ `Object ] ] )
           t)
  -> ('Sha1_validation, 'base, 'delta, [ `Have_result of [ `Object ] ]) t

(** Swap [result] and [base] buffers assuming the [base] buffer was not externally
    provided. *)
val set_result_as_base
  :  ( 'Sha1_validation
     , [< `No_base | `Have_base ]
     , 'delta
     , [ `Have_result of [ `Object ] ] )
       t
  -> ('Sha1_validation, [ `Have_base ], 'delta, [ `No_result ]) t

(** Swap [result] and [delta] buffers. *)
val set_result_as_delta
  :  ( 'Sha1_validation
     , 'base
     , [< `No_delta | `Have_delta ]
     , [ `Have_result of [ `Delta ] ] )
       t
  -> ('Sha1_validation, 'base, [ `Have_delta ], [ `No_result ]) t

(** Compute [result] buffer from [base] and [delta] buffers. *)
val compute_result
  :  ( 'Sha1_validation
     , ([< `Have_base | `Have_external_base ] as 'base)
     , [ `Have_delta ]
     , [ `No_result ] )
       t
  -> ('Sha1_validation, 'base, [ `Have_delta ], [ `Have_result of [ `Object ] ]) t

(** The type of the git object present in the result buffer. *)
val result_object_type : (_, _, _, [ `Have_result of [ `Object ] ]) t -> Object_type.t

(** Parse output in [result] as a Git base object. *)
val parse_result
  :  ('Sha1_validation, _, _, [ `Have_result of [ `Object ] ]) t
  -> on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> 'Sha1_validation
  -> unit
