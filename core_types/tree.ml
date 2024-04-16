(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2024  Bogdan-Cristian Tataroiu

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
open! Import

let[@inline] mode_bytes_0_to_3 (file_mode : File_mode.t) =
  match file_mode with
  | Non_executable_file -> 909127729
  | Executable_file -> 925904945
  | Link -> 808464945
  | Directory -> 808464436
  | Git_submodule -> 808465969
;;

let[@inline] mode_bytes_4_to_5 (file_mode : File_mode.t) =
  match file_mode with
  | Non_executable_file -> 13364
  | Executable_file -> 13621
  | Link -> 12336
  | Directory -> 8240
  | Git_submodule -> 12336
;;

let[@inline] mode_byte_7_should_be_space (file_mode : File_mode.t) =
  match file_mode with
  | Non_executable_file -> true
  | Executable_file -> true
  | Link -> true
  | Directory -> false
  | Git_submodule -> true
;;

let[@inline] mode_bytes_match ~bytes_0_to_3 ~bytes_4_to_5 ~byte_7 file_mode =
  if bytes_0_to_3 = mode_bytes_0_to_3 file_mode
  && bytes_4_to_5 = mode_bytes_4_to_5 file_mode
  then (
    if mode_byte_7_should_be_space file_mode then assert (Char.( = ) byte_7 ' ');
    true)
  else false
;;

let[@inline] mode_length file_mode =
  6 + if mode_byte_7_should_be_space file_mode then 1 else 0
;;

(** The method is unsafe in the sense that it does not perform bound checks on the
    given Bigstring. *)
let[@inline] unsafe_parse_mode buf ~pos =
  let bytes_0_to_3 = Bigstring.get_uint32_le buf ~pos in
  let bytes_4_to_5 = Bigstring.get_uint16_le buf ~pos:(pos + 4) in
  let byte_7 = Bigstring.get buf (pos + 6) in
  let file_mode = File_mode.Non_executable_file in
  if mode_bytes_match ~bytes_0_to_3 ~bytes_4_to_5 ~byte_7 file_mode
  then file_mode
  else (
    let file_mode = File_mode.Executable_file in
    if mode_bytes_match ~bytes_0_to_3 ~bytes_4_to_5 ~byte_7 file_mode
    then file_mode
    else (
      let file_mode = File_mode.Link in
      if mode_bytes_match ~bytes_0_to_3 ~bytes_4_to_5 ~byte_7 file_mode
      then file_mode
      else (
        let file_mode = File_mode.Directory in
        if mode_bytes_match ~bytes_0_to_3 ~bytes_4_to_5 ~byte_7 file_mode
        then file_mode
        else (
          let file_mode = File_mode.Git_submodule in
          if mode_bytes_match ~bytes_0_to_3 ~bytes_4_to_5 ~byte_7 file_mode
          then file_mode
          else if (* Old repos sometimes use the now deprecated 100664 for Non_executable_file. *)
            bytes_0_to_3 = 909127729
            && bytes_4_to_5 = 13366
            && Char.( = ) byte_7 ' '
          then Non_executable_file
          else raise_s [%message "Invalid_tree_format"]))))
;;

module Git_object_payload_parser = struct
  module State = struct
    type t =
      { mutable walked_without_finding_null : int
      ; sha1 : Sha1.Raw.Volatile.t [@sexp_drop_if const true]
      ; emit_tree_line : File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit
                         [@sexp_drop_if const true]
      }
    [@@deriving fields, sexp_of]

    let create ~emit_tree_line =
      { walked_without_finding_null = 0
      ; sha1 = Sha1.Raw.Volatile.create ()
      ; emit_tree_line
      }
    ;;

    let reset t = Fields.Direct.set_all_mutable_fields t ~walked_without_finding_null:0
  end

  let consume_payload_exn (state : State.t) buf ~pos ~len =
    while
      state.walked_without_finding_null < len
      && Char.( <> ) (Bigstring.get buf (pos + state.walked_without_finding_null)) '\000'
    do
      state.walked_without_finding_null <- state.walked_without_finding_null + 1
    done;
    if state.walked_without_finding_null + 1 + Sha1.Raw.length <= len
    && Char.( = ) (Bigstring.get buf (pos + state.walked_without_finding_null)) '\000'
    then (
      let mode = unsafe_parse_mode buf ~pos in
      let name_offset = mode_length mode in
      assert (name_offset < state.walked_without_finding_null);
      let name =
        Bigstring.To_string.sub
          buf
          ~pos:(pos + name_offset)
          ~len:(state.walked_without_finding_null - name_offset)
      in
      let sha1_bytes = Sha1.Raw.Volatile.bytes state.sha1 in
      Bigstring.To_bytes.blit
        ~src:buf
        ~src_pos:(pos + state.walked_without_finding_null + 1)
        ~dst:sha1_bytes
        ~dst_pos:0
        ~len:Sha1.Raw.length;
      let consumed_bytes = state.walked_without_finding_null + 1 + Sha1.Raw.length in
      state.emit_tree_line mode state.sha1 ~name;
      state.walked_without_finding_null <- 0;
      consumed_bytes)
    else 0
  ;;
end

module Git_object_payload_formatter = struct
  module Write_result : sig
    type t = private
      | Need_more_space
      | Wrote of { mutable bytes : int }

    val need_more_space : t
    val wrote : int -> t
  end = struct
    type t =
      | Need_more_space
      | Wrote of { mutable bytes : int }

    let wrote_value = Wrote { bytes = 0 }
    let need_more_space = Need_more_space

    let wrote bytes =
      (match wrote_value with
       | Wrote inline_record -> inline_record.bytes <- bytes
       | Need_more_space -> assert false);
      wrote_value
    ;;
  end

  let write_tree_line_gen blit_sha mode sha1 ~name buf ~pos ~len =
    let mode_len = mode_length mode in
    let name_len = String.length name in
    let total_len = mode_len + name_len + 1 + Sha1.Raw.length in
    if total_len > len
    then Write_result.need_more_space
    else (
      Bigstring.unsafe_set_uint32_le buf ~pos (mode_bytes_0_to_3 mode);
      Bigstring.unsafe_set_uint16_le buf ~pos:(pos + 4) (mode_bytes_4_to_5 mode);
      if mode_byte_7_should_be_space mode then Bigstring.set buf (pos + 6) ' ';
      Bigstring.From_string.blit
        ~src:name
        ~src_pos:0
        ~len:name_len
        ~dst:buf
        ~dst_pos:(pos + mode_len);
      Bigstring.set buf (pos + mode_len + name_len) '\000';
      blit_sha
        ~src:sha1
        ~src_pos:0
        ~dst:buf
        ~dst_pos:(pos + mode_len + name_len + 1)
        ~len:Sha1.Raw.length;
      Write_result.wrote total_len)
  ;;

  let write_tree_line mode sha1 ~name buf ~pos ~len =
    write_tree_line_gen
      Bigstring.From_string.blit
      mode
      (Sha1.Raw.to_string sha1)
      ~name
      buf
      ~pos
      ~len
  ;;

  let write_tree_line' mode sha1 ~name buf ~pos ~len =
    write_tree_line_gen
      Bigstring.From_bytes.blit
      mode
      (Sha1.Raw.Volatile.bytes sha1)
      ~name
      buf
      ~pos
      ~len
  ;;
end

module For_testing = struct
  let example_git_object_payload =
    "40000 dir\x00\xbe\xc6>7\xd0\x8cEJ\xd3\xa6\x0c\xde\x90\xb7\x0f?}\x07xR100644 \
     file1\x00\xe2\x12\x97\x01\xf1\xa4\xd5M\xc4O\x03\xc9;\xca\n\
     *\xec|TI100644 \
     file2\x00lI?\xf7@\xf98\x03\x90\xd5\xc9\xdd\xefJ\xf1\x86\x97\xac\x93u120000 \
     link\x00\xde\xa9|5 \xa7U\xe4\xdbV\x94\xd7C\xaa\x85\x99Q\x1b\xbe\x9c"
  ;;

  let%expect_test "round-trip" =
    let lines = Queue.create () in
    let state =
      Git_object_payload_parser.State.create ~emit_tree_line:(fun mode sha1 ~name ->
        Queue.enqueue lines (mode, Sha1.Raw.Volatile.to_hex sha1, name))
    in
    let buf = Bigstring.of_string example_git_object_payload in
    let len = String.length example_git_object_payload in
    let rec consume ~pos =
      if pos = len
      then ()
      else
        consume
          ~pos:
            (pos
             + Git_object_payload_parser.consume_payload_exn
                 state
                 buf
                 ~pos
                 ~len:(len - pos))
    in
    consume ~pos:0;
    printf !"%{sexp: (File_mode.t * Sha1.Hex.t * string) Queue.t}\n" lines;
    [%expect
      {|
          ((Directory bec63e37d08c454ad3a60cde90b70f3f7d077852 dir)
           (Non_executable_file e2129701f1a4d54dc44f03c93bca0a2aec7c5449 file1)
           (Non_executable_file 6c493ff740f9380390d5c9ddef4af18697ac9375 file2)
           (Link dea97c3520a755e4db5694d743aa8599511bbe9c link)) |}];
    let rec write_out buf ~pos ~len =
      match Queue.peek lines with
      | None -> buf, 0, pos
      | Some (mode, sha1, name) ->
        (match
           Git_object_payload_formatter.write_tree_line
             mode
             (Sha1.Raw.of_hex sha1)
             ~name
             buf
             ~pos
             ~len
         with
         | Wrote { bytes } ->
           ignore (Queue.dequeue_exn lines : File_mode.t * Sha1.Hex.t * string);
           write_out buf ~pos:(pos + bytes) ~len:(len - bytes)
         | Need_more_space ->
           let new_buf = Bigstring.create (Bigstring.length buf * 2) in
           Bigstring.blit ~src:buf ~src_pos:0 ~len:pos ~dst:new_buf ~dst_pos:0;
           write_out new_buf ~pos ~len:(Bigstring.length new_buf - pos))
    in
    let buf, pos, len = write_out (Bigstring.create 1) ~pos:0 ~len:1 in
    let as_payload = Bigstring.To_string.sub buf ~pos ~len in
    [%test_eq: string] example_git_object_payload as_payload
  ;;
end
