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

let feed_parser_data =
  let rec feed_data_loop ~acc parser_ ~process buf ~pos =
    let len = Int.min (Bigstring.length buf - pos) (1 lsl 16) in
    let consumed = process parser_ buf ~pos ~len in
    if consumed < len || len = 0
    then acc + consumed
    else feed_data_loop ~acc:(acc + len) parser_ ~process buf ~pos:(pos + len)
  in
  fun parser_ ~process buf ~pos -> feed_data_loop ~acc:0 parser_ ~process buf ~pos
;;

let with_resource ~create ~close_on_error ~f =
  let open Deferred.Or_error.Let_syntax in
  let%bind resource = create () in
  let open Deferred.Let_syntax in
  let%bind result = f resource in
  match result with
  | Ok _ -> return result
  | Error _ ->
    (match%map close_on_error resource with
     | Ok () -> result
     | Error error ->
       Log.Global.sexp ~level:`Error [%message "Error closing resource" (error : Error.t)];
       result)
;;

let with_file file_path ~f =
  let open Deferred.Or_error.Let_syntax in
  with_resource
    ~create:(fun () ->
      Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
        Unix.openfile ~mode:[ `Rdonly ] file_path))
    ~close_on_error:(fun fd ->
      Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () -> Unix.close fd))
    ~f:(fun fd ->
      let%bind file_size =
        Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
          let open Deferred.Let_syntax in
          let%map stat = Unix.fstat fd in
          Int64.to_int_exn stat.size)
      in
      let%bind file_mmap =
        Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
          Fd.syscall_in_thread_exn ~name:"git-pack-mmap-file" fd (fun file_descr ->
            Bigstring_unix.map_file ~shared:false file_descr file_size))
      in
      f fd file_size file_mmap)
;;

module Make_sha1_binary_search (Sha1M : sig
    type t

    val get : t -> int -> char
  end) =
struct
  let get_uint8 sha1m pos = Char.to_int (Sha1M.get sha1m pos)

  let sha1_greater_than_or_equal =
    let rec sha1_greater_than_or_equal sha1m ~index_buf ~index_pos ~pos =
      if pos = Sha1.Raw.length
      then true
      else (
        let from_sha1 = get_uint8 sha1m pos in
        let from_index = Bigstring.get_uint8 index_buf ~pos:(index_pos + pos) in
        if from_sha1 > from_index
        then true
        else if from_sha1 < from_index
        then false
        else sha1_greater_than_or_equal sha1m ~index_buf ~index_pos ~pos:(pos + 1))
    in
    fun sha1m ~index_buf ~index_pos ->
      sha1_greater_than_or_equal sha1m ~index_buf ~index_pos ~pos:0
  ;;

  let sha1_equal =
    let rec sha1_equal sha1m ~index_buf ~index_pos ~pos =
      if pos = Sha1.Raw.length
      then true
      else (
        let from_sha1 = get_uint8 sha1m pos in
        let from_index = Bigstring.get_uint8 index_buf ~pos:(index_pos + pos) in
        if from_sha1 <> from_index
        then false
        else sha1_equal sha1m ~index_buf ~index_pos ~pos:(pos + 1))
    in
    fun sha1m ~index_buf ~index_pos -> sha1_equal sha1m ~index_buf ~index_pos ~pos:0
  ;;

  let sha1_binary_search ~index_buf ~index_pos ~index_element_count sha1m =
    let index_len = index_element_count * Sha1.Raw.length in
    let stop = index_pos + index_len in
    let pos = ref index_pos in
    let step = ref Sha1.Raw.length in
    while !step <= index_len do
      step := !step lsl 1
    done;
    step := !step lsr 1;
    while !step >= Sha1.Raw.length do
      if !pos + !step < stop
      && sha1_greater_than_or_equal sha1m ~index_buf ~index_pos:(!pos + !step)
      then pos := !pos + !step;
      step := !step lsr 1
    done;
    if sha1_equal sha1m ~index_buf ~index_pos:!pos
    then Find_result.Volatile.some ((!pos - index_pos) / Sha1.Raw.length)
    else Find_result.Volatile.none
  ;;
end
