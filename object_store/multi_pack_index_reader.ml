(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021-2024  Bogdan-Cristian Tataroiu

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

module Chunks : sig
  type t

  val create : unit -> t
  val read_chunk : t -> Bigstring.t -> pos:int -> unit

  (** Accessors *)

  val pack_file_name_chunk_offset : t -> int
  val pack_file_name_chunk_size : t -> int
  val fanout_chunk_offset : t -> int
  val fanout_chunk_size : t -> int
  val object_id_chunk_offset : t -> int
  val object_id_chunk_size : t -> int
  val object_offset_chunk_offset : t -> int
  val object_offset_chunk_size : t -> int
  val large_object_offset_chunk_offset : t -> int
  val large_object_offset_chunk_size : t -> int
  val have_large_object_offset_chunk : t -> bool
end = struct
  type t =
    { mutable pack_file_name_chunk_offset : int
    ; mutable fanout_chunk_offset : int
    ; mutable object_id_chunk_offset : int
    ; mutable object_offset_chunk_offset : int
    ; mutable large_object_offset_chunk_offset : int
    ; mutable end_offset : int
    ; mutable unknown_chunk_offsets : int list
    }

  let create () =
    { pack_file_name_chunk_offset = 0
    ; fanout_chunk_offset = 0
    ; object_id_chunk_offset = 0
    ; object_offset_chunk_offset = 0
    ; large_object_offset_chunk_offset = 0
    ; end_offset = 0
    ; unknown_chunk_offsets = []
    }
  ;;

  let[@cold] raise_missing_chunk chunk =
    raise_s [%message "Missing chunk" (chunk : string)]
  ;;

  let[@inline] non_zero_exn chunk value =
    match value with
    | 0 -> raise_missing_chunk chunk
    | _ -> value
  ;;

  let[@inline] pack_file_name_chunk_offset t =
    non_zero_exn "pack_file_name" t.pack_file_name_chunk_offset
  ;;

  let[@inline] fanout_chunk_offset t = non_zero_exn "fanout" t.fanout_chunk_offset

  let[@inline] object_id_chunk_offset t =
    non_zero_exn "object_id" t.object_id_chunk_offset
  ;;

  let[@inline] object_offset_chunk_offset t =
    non_zero_exn "object_offset" t.object_offset_chunk_offset
  ;;

  let[@inline] large_object_offset_chunk_offset t =
    non_zero_exn "large_object_offset" t.large_object_offset_chunk_offset
  ;;

  let[@inline] have_large_object_offset_chunk t =
    t.large_object_offset_chunk_offset <> t.end_offset
  ;;

  let chunk_size t ~offset =
    let next_chunk_offset = non_zero_exn "terminating" t.end_offset in
    let[@inline] process next_chunk_offset field_getter =
      let field_value = field_getter t in
      if field_value > offset
      then Int.min field_value next_chunk_offset
      else next_chunk_offset
    in
    let next_chunk_offset = process next_chunk_offset pack_file_name_chunk_offset in
    let next_chunk_offset = process next_chunk_offset fanout_chunk_offset in
    let next_chunk_offset = process next_chunk_offset object_id_chunk_offset in
    let next_chunk_offset = process next_chunk_offset object_offset_chunk_offset in
    let next_chunk_offset = process next_chunk_offset large_object_offset_chunk_offset in
    let next_chunk_offset =
      List.fold t.unknown_chunk_offsets ~init:next_chunk_offset ~f:(fun acc value ->
        if value > offset then Int.min value acc else acc)
    in
    next_chunk_offset - offset
  ;;

  let pack_file_name_chunk_size t = chunk_size t ~offset:(pack_file_name_chunk_offset t)
  let fanout_chunk_size t = chunk_size t ~offset:(fanout_chunk_offset t)
  let object_id_chunk_size t = chunk_size t ~offset:(object_id_chunk_offset t)
  let object_offset_chunk_size t = chunk_size t ~offset:(object_offset_chunk_offset t)

  let large_object_offset_chunk_size t =
    chunk_size t ~offset:(large_object_offset_chunk_offset t)
  ;;

  let read_chunk t buf ~pos =
    let id = Bigstring.get_uint32_be buf ~pos in
    let offset = Bigstring.get_uint64_be_exn buf ~pos:(pos + 4) in
    match id with
    | 0x504e414d -> t.pack_file_name_chunk_offset <- offset
    | 0x4f494446 -> t.fanout_chunk_offset <- offset
    | 0x4f49444c -> t.object_id_chunk_offset <- offset
    | 0x4f4f4646 -> t.object_offset_chunk_offset <- offset
    | 0x4c4f4646 -> t.large_object_offset_chunk_offset <- offset
    | 0 ->
      if t.large_object_offset_chunk_offset = 0
      then t.large_object_offset_chunk_offset <- offset;
      t.end_offset <- offset
    | _ -> ()
  ;;
end

type t =
  { index_file : string
  ; fd : Fd.t
  ; file_size : int
  ; file_mmap : Bigstring.t
  ; pack_file_names : string array
  ; object_count : int
  ; chunks : Chunks.t
  }

let rec read_pack_file_name_chunk ~pack_file_count buf ~pos ~len =
  match pack_file_count, len with
  | 0, 0 -> Ok []
  | 0, (1 | 2 | 3) ->
    (match Bigstring.get_uint8 buf ~pos = 0 with
     | true -> read_pack_file_name_chunk ~pack_file_count buf ~pos:(pos + 1) ~len:(len - 1)
     | false -> Or_error.error_s [%sexp "Invalid pack file name chunk"])
  | 0, _ -> Or_error.error_s [%sexp "Invalid pack file name chunk"]
  | _ ->
    let next_null = Bigstring.unsafe_find buf ~pos ~len '\000' in
    (match next_null < 0 || next_null = pos with
     | true -> Or_error.error_s [%sexp "Invalid pack file name chunk"]
     | false ->
       let index_file_name = Bigstring.To_string.sub buf ~pos ~len:(next_null - pos) in
       (match String.chop_suffix ~suffix:".idx" index_file_name with
        | None ->
          Or_error.error_s [%sexp "Expected index file names to end in [.idx] extension"]
        | Some base ->
          let pack_file_name = base ^ ".pack" in
          let%map.Or_error pack_file_names =
            read_pack_file_name_chunk
              ~pack_file_count:(pack_file_count - 1)
              buf
              ~pos:(next_null + 1)
              ~len:(len - (next_null - pos + 1))
          in
          pack_file_name :: pack_file_names))
;;

let open_existing ~pack_directory =
  let index_file = pack_directory ^/ "multi-pack-index" in
  Git_pack_files.Low_level.Util.with_file index_file ~f:(fun fd file_size file_mmap ->
    let result =
      let open Or_error.Let_syntax in
      let%bind () =
        (* At least 12 for the header, (4 chunks + 1) * 12 for chunk descriptions, 256 * 4 for
           fan-out table and Sha1.Raw.length for the footer *)
        if file_size < 12 + (5 * 12) + (256 * 4) + Sha1.Raw.length
        then Or_error.error_s [%sexp "Index file impossibly small"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint32_be file_mmap ~pos:0 <> 0x4d494458
        then Or_error.error_s [%sexp "Expected multi pack index signature"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint8 file_mmap ~pos:4 <> 1
        then Or_error.error_s [%sexp "Expected multi pack index version number 1"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint8 file_mmap ~pos:5 <> 1
        then Or_error.error_s [%sexp "Expected multi pack index hash function SHA1"]
        else Ok ()
      in
      let chunk_count = Bigstring.get_uint8 file_mmap ~pos:6 in
      let%bind () =
        if chunk_count < 4
        then Or_error.error_s [%sexp "Expected at least 4 chunks in multi pack index"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint8 file_mmap ~pos:7 <> 0
        then Or_error.error_s [%sexp "Expected 0 base multi pack index files"]
        else Ok ()
      in
      let pack_file_count = Bigstring.get_uint32_be file_mmap ~pos:8 in
      let chunks = Chunks.create () in
      for chunk_index = 0 to chunk_count do
        Chunks.read_chunk chunks file_mmap ~pos:(12 + (chunk_index * 12))
      done;
      let%bind () =
        if Bigstring.get_uint32_be file_mmap ~pos:(12 + (chunk_count * 12)) <> 0
        then Or_error.error_s [%sexp "Expected last chunk id of 0"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint64_be_exn file_mmap ~pos:(12 + (chunk_count * 12) + 4)
           <> file_size - Sha1.Raw.length
        then
          Or_error.error_s
            [%sexp "Expected last chunk offset to point to the file's footer Sha1"]
        else Ok ()
      in
      let%bind pack_file_names =
        read_pack_file_name_chunk
          ~pack_file_count
          file_mmap
          ~pos:(Chunks.pack_file_name_chunk_offset chunks)
          ~len:(Chunks.pack_file_name_chunk_size chunks)
        >>| Array.of_list
      in
      let%bind () =
        if Chunks.fanout_chunk_size chunks <> 256 * 4
        then Or_error.error_s [%sexp "Unexpected fanout chunk size"]
        else Ok ()
      in
      let object_count =
        Bigstring.get_uint32_be
          file_mmap
          ~pos:(Chunks.fanout_chunk_offset chunks + (255 * 4))
      in
      let%bind () =
        if Chunks.object_id_chunk_size chunks <> Sha1.Raw.length * object_count
        then Or_error.error_s [%sexp "Unexpected object id chunk size"]
        else Ok ()
      in
      let%bind () =
        if Chunks.object_offset_chunk_size chunks <> 8 * object_count
        then Or_error.error_s [%sexp "Unexpected object offset chunk size"]
        else Ok ()
      in
      let%bind () =
        (* At least 12 for the header, (chunk_count + 1) * 12 for chunk descriptions,
           256 * 4 for fan-out table, (Sha1.Raw.length + 8) for each object and
           Sha1.Raw.length for the footer. *)
        if file_size
           < 12
             + ((chunk_count + 1) * 12)
             + Chunks.pack_file_name_chunk_size chunks
             + Chunks.fanout_chunk_size chunks
             + Chunks.object_id_chunk_size chunks
             + Chunks.object_offset_chunk_size chunks
             + Chunks.large_object_offset_chunk_size chunks
             + Sha1.Raw.length
        then Or_error.error_s [%sexp "Index file impossibly small"]
        else Ok ()
      in
      return
        { index_file; fd; file_size; file_mmap; pack_file_names; object_count; chunks }
    in
    Deferred.return result)
;;

let pack_file_names t = t.pack_file_names
let object_count t = t.object_count

let[@cold] raise_invalid_index t ~index =
  raise_s
    [%message
      "Invalid value for index" (index : int) ~object_count:(t.object_count : int)]
;;

let[@inline] validate_index t ~index =
  if index < 0 || index >= t.object_count then raise_invalid_index t ~index
;;

let sha1 =
  let result = Sha1.Raw.Volatile.create () in
  fun t ~index ->
    validate_index t ~index;
    Bigstring.To_bytes.blit
      ~src:t.file_mmap
      ~src_pos:(Chunks.object_id_chunk_offset t.chunks + (Sha1.Raw.length * index))
      ~dst:(Sha1.Raw.Volatile.bytes result)
      ~dst_pos:0
      ~len:Sha1.Raw.length;
    result
;;

let pack_id t ~index =
  validate_index t ~index;
  let object_offset_chunk_offset = Chunks.object_offset_chunk_offset t.chunks in
  Bigstring.get_uint32_be t.file_mmap ~pos:(object_offset_chunk_offset + (8 * index))
;;

let pack_offset =
  let msb_mask = 1 lsl 31 in
  fun t ~index ->
    validate_index t ~index;
    let object_offset_chunk_offset = Chunks.object_offset_chunk_offset t.chunks in
    let offset =
      Bigstring.get_uint32_be
        t.file_mmap
        ~pos:(object_offset_chunk_offset + ((8 * index) + 4))
    in
    match Chunks.have_large_object_offset_chunk t.chunks with
    | false -> offset
    | true ->
      (match offset land msb_mask = 0 with
       | true -> offset
       | false ->
         let large_object_offset_chunk_offset =
           Chunks.large_object_offset_chunk_offset t.chunks
         in
         Bigstring.get_uint64_be_exn
           t.file_mmap
           ~pos:(large_object_offset_chunk_offset + (8 * (offset lxor msb_mask))))
;;

module Make_find_sha1_index (Sha1M : sig
    type t

    val get : t -> int -> char
  end) =
struct
  include Git_pack_files.Low_level.Util.Make_sha1_binary_search (Sha1M)

  let fanout_chunk_value t uint8 =
    let fanout_chunk_offset = Chunks.fanout_chunk_offset t.chunks in
    fanout_chunk_offset + (4 * uint8)
  ;;

  let find_sha1_index t sha1m =
    let first_byte = Char.to_int (Sha1M.get sha1m 0) in
    let index_lower_bound =
      match first_byte with
      | 0 -> 0
      | _ ->
        Bigstring.get_uint32_be t.file_mmap ~pos:(fanout_chunk_value t (first_byte - 1))
    in
    let index_search_candidates =
      Bigstring.get_uint32_be t.file_mmap ~pos:(fanout_chunk_value t first_byte)
      - index_lower_bound
    in
    match
      sha1_binary_search
        ~index_buf:t.file_mmap
        ~index_pos:
          (Chunks.object_id_chunk_offset t.chunks + (index_lower_bound * Sha1.Raw.length))
        ~index_element_count:index_search_candidates
        sha1m
    with
    | None -> Git_pack_files.Low_level.Find_result.Volatile.none
    | Some { index } ->
      Git_pack_files.Low_level.Find_result.Volatile.some (index_lower_bound + index)
  ;;
end

module Find_sha1_index = Make_find_sha1_index (String)
module Find_sha1_index' = Make_find_sha1_index (Bytes)

let find_sha1_index t sha1 = Find_sha1_index.find_sha1_index t (Sha1.Raw.to_string sha1)

let find_sha1_index' t sha1 =
  Find_sha1_index'.find_sha1_index t (Sha1.Raw.Volatile.bytes sha1)
;;

let multi_pack_index_sha1 =
  let result = Sha1.Raw.Volatile.create () in
  fun t ->
    Bigstring.To_bytes.blit
      ~src:t.file_mmap
      ~src_pos:(t.file_size - Sha1.Raw.length)
      ~dst:(Sha1.Raw.Volatile.bytes result)
      ~dst_pos:0
      ~len:Sha1.Raw.length;
    result
;;

let multi_pack_index_file t = t.index_file

module For_testing = struct
  let print_out_multi_pack_index ~pack_directory =
    let%bind t = open_existing ~pack_directory >>| ok_exn in
    printf "items in index: %d\n" (object_count t);
    print_endline "packs in index:";
    Array.iter (pack_file_names t) ~f:print_endline;
    print_endline "";
    printf "idx | %40s | pack id | pack offset\n" "sha1";
    for index = 0 to object_count t - 1 do
      printf
        !"%3d | %{Sha1.Hex} | %7d | %11d\n"
        (Git_pack_files.Find_result.Volatile.index_exn
           (find_sha1_index' t (sha1 t ~index)))
        (Sha1.Raw.Volatile.to_hex (sha1 t ~index))
        (pack_id t ~index)
        (pack_offset t ~index)
    done;
    Deferred.unit
  ;;
end
