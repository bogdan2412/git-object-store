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
open! Async
open! Import

module Mode = struct
  type t =
    | Write of { should_discard : Sha1.Raw.Volatile.t -> bool }
    | Dry_run

  let write_all = Write { should_discard = Fn.const false }
end

module Raw : sig
  type t

  val create_uninitialised : object_directory:string -> t
  val init_or_reset : t -> Mode.t -> unit Deferred.t
  val append_data : t -> Bigstring.t -> pos:int -> len:int -> unit
  val finalise : t -> Sha1.Raw.t Deferred.t
  val abort : t -> unit Deferred.t
end = struct
  module State : sig
    module View : sig
      type t = private
        | Not_initialised
        | Dry_run of { mutable sha1_compute : [ `Initialised ] Sha1.Compute.t }
        | Initialised of
            { mutable writer : Writer.t
            ; mutable temp_file_name : string
            ; mutable sha1_compute : [ `Initialised ] Sha1.Compute.t
            ; zlib_deflate : Zlib.Deflate.t
            ; mutable should_discard : Sha1.Raw.Volatile.t -> bool
            }
        | Finalizing
    end

    type t

    val create : unit -> t
    val set_not_initialised : t -> unit
    val set_dry_run : t -> unit

    val set_initialised
      :  t
      -> writer:Writer.t
      -> temp_file_name:string
      -> should_discard:(Sha1.Raw.Volatile.t -> bool)
      -> unit

    val set_finalizing : t -> unit
    val view : t -> View.t
  end = struct
    module View = struct
      type t =
        | Not_initialised
        | Dry_run of { mutable sha1_compute : [ `Initialised ] Sha1.Compute.t }
        | Initialised of
            { mutable writer : Writer.t
            ; mutable temp_file_name : string
            ; mutable sha1_compute : [ `Initialised ] Sha1.Compute.t
            ; zlib_deflate : Zlib.Deflate.t
            ; mutable should_discard : Sha1.Raw.Volatile.t -> bool
            }
        | Finalizing
    end

    type t =
      { mutable current : View.t
      ; dry_run : View.t
      ; initialised : View.t
      }

    let create () =
      let sha1_compute =
        Sha1.Compute.create_uninitialised () |> Sha1.Compute.init_or_reset
      in
      let rec initialised =
        lazy
          (View.Initialised
             { writer = Lazy.force Writer.stdout
             ; temp_file_name = "/dev/stdout"
             ; sha1_compute
             ; zlib_deflate = force zlib_deflate
             ; should_discard = Fn.const false
             })
      and zlib_deflate =
        lazy
          (Zlib.Deflate.create_uninitialised ~on_data_chunk:(fun buf ~pos ~len ->
             match force initialised with
             | Initialised record -> Writer.write_bigstring record.writer buf ~pos ~len
             | Dry_run _ | Not_initialised | Finalizing -> assert false))
      in
      { current = Not_initialised
      ; dry_run = Dry_run { sha1_compute }
      ; initialised = force initialised
      }
    ;;

    let set_not_initialised t = t.current <- Not_initialised

    let set_dry_run t =
      t.current <- t.dry_run;
      match t.dry_run with
      | Dry_run record ->
        record.sha1_compute <- Sha1.Compute.init_or_reset record.sha1_compute
      | Initialised _ | Not_initialised | Finalizing -> assert false
    ;;

    let set_initialised t ~writer ~temp_file_name ~should_discard =
      t.current <- t.initialised;
      match t.initialised with
      | Initialised record ->
        record.writer <- writer;
        record.temp_file_name <- temp_file_name;
        record.sha1_compute <- Sha1.Compute.init_or_reset record.sha1_compute;
        Zlib.Deflate.init_or_reset record.zlib_deflate;
        if not (phys_equal should_discard record.should_discard)
        then record.should_discard <- should_discard
      | Dry_run _ | Not_initialised | Finalizing -> assert false
    ;;

    let set_finalizing t = t.current <- Finalizing
    let view t = t.current
  end

  type t =
    { object_directory : string
    ; state : State.t
    }

  let create_uninitialised ~object_directory =
    { object_directory; state = State.create () }
  ;;

  let[@cold] raise_finalizing () = failwith "Git_object_writer used while finalizing"

  let init_or_reset t mode =
    match State.view t.state with
    | Finalizing -> raise_finalizing ()
    | Not_initialised | Initialised _ | Dry_run _ ->
      (match (mode : Mode.t) with
       | Dry_run ->
         State.set_dry_run t.state;
         Deferred.unit
       | Write { should_discard } ->
         let temp_file_name =
           Filename_unix.temp_file ~in_dir:t.object_directory "write-in-progress" ""
         in
         let%map writer = Writer.open_file temp_file_name in
         State.set_initialised t.state ~writer ~temp_file_name ~should_discard)
  ;;

  let[@cold] raise_not_initialised () =
    failwith "Git_object_writer used while not initialised"
  ;;

  let append_data t buf ~pos ~len =
    match State.view t.state with
    | Not_initialised -> raise_not_initialised ()
    | Finalizing -> raise_finalizing ()
    | Dry_run { sha1_compute } -> Sha1.Compute.process sha1_compute buf ~pos ~len
    | Initialised
        { writer = _; temp_file_name = _; sha1_compute; zlib_deflate; should_discard = _ }
      ->
      Sha1.Compute.process sha1_compute buf ~pos ~len;
      Zlib.Deflate.process zlib_deflate buf ~pos ~len
  ;;

  let finalise t =
    match State.view t.state with
    | Not_initialised -> raise_not_initialised ()
    | Finalizing -> raise_finalizing ()
    | Dry_run { sha1_compute } ->
      let sha1_compute = Sha1.Compute.finalise sha1_compute in
      let sha1_raw = Sha1.Compute.get_raw sha1_compute in
      State.set_not_initialised t.state;
      return (Sha1.Raw.Volatile.non_volatile sha1_raw)
    | Initialised { writer; temp_file_name; sha1_compute; zlib_deflate; should_discard }
      ->
      Zlib.Deflate.finalise zlib_deflate;
      State.set_finalizing t.state;
      let%bind () = Writer.flushed writer in
      let%bind () = Writer.close writer in
      let sha1_compute = Sha1.Compute.finalise sha1_compute in
      let sha1_raw = Sha1.Compute.get_raw sha1_compute in
      let%bind () =
        match should_discard sha1_raw with
        | true -> Unix.unlink temp_file_name
        | false ->
          let sha1_hex = Sha1.Compute.get_hex sha1_compute in
          let dir, file =
            let str = Sha1.Hex.Volatile.to_string sha1_hex in
            ( Filename.concat t.object_directory (String.sub str ~pos:0 ~len:2)
            , String.sub str ~pos:2 ~len:(Sha1.Hex.length - 2) )
          in
          let new_path = Filename.concat dir file in
          (match%bind Sys.is_file_exn new_path with
           | true -> Unix.unlink temp_file_name
           | false ->
             let%bind () = Unix.mkdir ~p:() dir in
             let%bind () = Sys.rename temp_file_name new_path in
             Deferred.unit)
      in
      State.set_not_initialised t.state;
      return (Sha1.Raw.Volatile.non_volatile sha1_raw)
  ;;

  let abort t =
    match State.view t.state with
    | Not_initialised -> Deferred.unit
    | Finalizing -> raise_finalizing ()
    | Dry_run { sha1_compute = _ } ->
      State.set_not_initialised t.state;
      Deferred.unit
    | Initialised
        { writer; temp_file_name; sha1_compute = _; zlib_deflate = _; should_discard = _ }
      ->
      State.set_not_initialised t.state;
      let%bind () = Writer.flushed writer in
      let%bind () = Writer.close writer in
      Unix.unlink temp_file_name
  ;;
end

module With_header = struct
  module Unknown_size : sig
    type t

    val create_uninitialised : object_directory:string -> t
    val init_or_reset : t -> Object_type.t -> Mode.t -> unit Deferred.t
    val double_buffer_space : t -> unit
    val make_room : t -> for_bytes:int -> unit
    val buf : t -> Bigstring.t
    val pos : t -> int
    val len : t -> int
    val advance_pos : t -> by:int -> unit
    val written_so_far : t -> int
    val finalise : t -> Sha1.Raw.t Deferred.t
    val abort : t -> unit Deferred.t
  end = struct
    type t =
      { raw : Raw.t
      ; mutable object_type : Object_type.t
      ; mutable buf : Bigstring.t
      ; mutable start : int
      ; mutable pos : int
      }

    let create_uninitialised ~object_directory =
      { raw = Raw.create_uninitialised ~object_directory
      ; object_type = Blob
      ; buf = Bigstring.create (if Core.am_running_test then 1 else 1 lsl 15)
      ; start = 0
      ; pos = 0
      }
    ;;

    let double_buffer_space t =
      let new_buf = Bigstring.create (Bigstring.length t.buf * 2) in
      Bigstring.blit ~src:t.buf ~src_pos:0 ~dst:new_buf ~dst_pos:0 ~len:t.pos;
      t.buf <- new_buf
    ;;

    let make_room_for_pos t ~pos =
      while pos >= Bigstring.length t.buf do
        double_buffer_space t
      done
    ;;

    let make_room t ~for_bytes = make_room_for_pos t ~pos:(t.pos + for_bytes - 1)
    let buf t = t.buf
    let pos t = t.pos
    let len t = Bigstring.length t.buf - t.pos
    let advance_pos t ~by = t.pos <- t.pos + by

    let init_or_reset t object_type mode =
      let%map () = Raw.init_or_reset t.raw mode in
      make_room_for_pos t ~pos:32;
      t.object_type <- object_type;
      t.start <- 32;
      t.pos <- 32
    ;;

    let written_so_far t = t.pos - t.start

    let finalise t =
      let data_len = t.pos - t.start in
      assert (data_len >= 0);
      let header_len =
        Header_writer.write_from_right
          t.buf
          ~pos:(t.start - 1)
          t.object_type
          ~object_length:data_len
      in
      Raw.append_data t.raw t.buf ~pos:(t.start - header_len) ~len:(header_len + data_len);
      Raw.finalise t.raw
    ;;

    let abort t = Raw.abort t.raw
  end

  module Known_size : sig
    type t

    val create_uninitialised : object_directory:string -> t
    val init_or_reset : t -> Object_type.t -> length:int -> Mode.t -> unit Deferred.t
    val append_data : t -> Bigstring.t -> pos:int -> len:int -> unit
    val finalise_exn : t -> Sha1.Raw.t Deferred.t
    val abort : t -> unit Deferred.t
  end = struct
    type t =
      { raw : Raw.t
      ; buf : Bigstring.t
      ; mutable expected : int
      ; mutable written : int
      }

    let create_uninitialised ~object_directory =
      { raw = Raw.create_uninitialised ~object_directory
      ; buf = Bigstring.create 32
      ; expected = 0
      ; written = 0
      }
    ;;

    let init_or_reset t object_type ~length mode =
      let header_len =
        Header_writer.write_from_left t.buf ~pos:0 object_type ~object_length:length
      in
      t.expected <- length;
      t.written <- 0;
      let%map () = Raw.init_or_reset t.raw mode in
      Raw.append_data t.raw t.buf ~pos:0 ~len:header_len
    ;;

    let append_data t buf ~pos ~len =
      Raw.append_data t.raw buf ~pos ~len;
      t.written <- t.written + len
    ;;

    let finalise_exn t =
      if t.expected <> t.written
      then
        raise_s
          [%message
            "With_header.Known_size was not fed the expected amount of data"
              ~expected:(t.expected : int)
              ~written:(t.written : int)];
      Raw.finalise t.raw
    ;;

    let abort t = Raw.abort t.raw
  end
end

module Commit_ = Commit

module Commit = struct
  type t = With_header.Unknown_size.t

  let create ~object_directory =
    With_header.Unknown_size.create_uninitialised ~object_directory
  ;;

  let write t commit mode =
    let%bind () = With_header.Unknown_size.init_or_reset t Commit mode in
    let commit_str = Commit.format_as_git_object_payload commit in
    let commit_len = String.length commit_str in
    With_header.Unknown_size.make_room t ~for_bytes:commit_len;
    Bigstring.From_string.blit
      ~src:commit_str
      ~src_pos:0
      ~dst:(With_header.Unknown_size.buf t)
      ~dst_pos:(With_header.Unknown_size.pos t)
      ~len:commit_len;
    With_header.Unknown_size.advance_pos t ~by:commit_len;
    With_header.Unknown_size.finalise t
  ;;

  let write' ~object_directory commit mode =
    let t = create ~object_directory in
    write t commit mode
  ;;
end

module Tag_ = Tag

module Tag = struct
  type t = With_header.Unknown_size.t

  let create ~object_directory =
    With_header.Unknown_size.create_uninitialised ~object_directory
  ;;

  let write t tag mode =
    let%bind () = With_header.Unknown_size.init_or_reset t Tag mode in
    let tag_str = Tag.format_as_git_object_payload tag in
    let tag_len = String.length tag_str in
    With_header.Unknown_size.make_room t ~for_bytes:tag_len;
    Bigstring.From_string.blit
      ~src:tag_str
      ~src_pos:0
      ~dst:(With_header.Unknown_size.buf t)
      ~dst_pos:(With_header.Unknown_size.pos t)
      ~len:tag_len;
    With_header.Unknown_size.advance_pos t ~by:tag_len;
    With_header.Unknown_size.finalise t
  ;;

  let write' ~object_directory tag mode =
    let t = create ~object_directory in
    write t tag mode
  ;;
end

module Tree = struct
  type t = With_header.Unknown_size.t

  let create_uninitialised ~object_directory =
    With_header.Unknown_size.create_uninitialised ~object_directory
  ;;

  let init_or_reset t = With_header.Unknown_size.init_or_reset t Tree

  let rec write_tree_line_gen write_tree_line t mode sha1 ~name =
    match
      (write_tree_line
         mode
         sha1
         ~name
         (With_header.Unknown_size.buf t)
         ~pos:(With_header.Unknown_size.pos t)
         ~len:(With_header.Unknown_size.len t)
       : Tree.Git_object_payload_formatter.Write_result.t)
    with
    | Wrote { bytes } -> With_header.Unknown_size.advance_pos t ~by:bytes
    | Need_more_space ->
      With_header.Unknown_size.double_buffer_space t;
      write_tree_line_gen write_tree_line t mode sha1 ~name
  ;;

  let write_tree_line =
    write_tree_line_gen Tree.Git_object_payload_formatter.write_tree_line
  ;;

  let write_tree_line' =
    write_tree_line_gen Tree.Git_object_payload_formatter.write_tree_line'
  ;;

  let finalise t = With_header.Unknown_size.finalise t
  let abort t = With_header.Unknown_size.abort t
end

module Blob = struct
  module Unknown_size = struct
    type t = With_header.Unknown_size.t

    let create_uninitialised ~object_directory =
      With_header.Unknown_size.create_uninitialised ~object_directory
    ;;

    let init_or_reset t = With_header.Unknown_size.init_or_reset t Blob

    let append_data t buf ~pos ~len =
      With_header.Unknown_size.make_room t ~for_bytes:len;
      Bigstring.blit
        ~src:buf
        ~src_pos:pos
        ~dst:(With_header.Unknown_size.buf t)
        ~dst_pos:(With_header.Unknown_size.pos t)
        ~len;
      With_header.Unknown_size.advance_pos t ~by:len
    ;;

    let written_so_far t = With_header.Unknown_size.written_so_far t
    let finalise t = With_header.Unknown_size.finalise t
    let abort t = With_header.Unknown_size.abort t
  end

  module Known_size = struct
    type t = With_header.Known_size.t

    let create_uninitialised ~object_directory =
      With_header.Known_size.create_uninitialised ~object_directory
    ;;

    let init_or_reset t ~length = With_header.Known_size.init_or_reset t Blob ~length
    let append_data t buf ~pos ~len = With_header.Known_size.append_data t buf ~pos ~len
    let finalise_exn t = With_header.Known_size.finalise_exn t
    let abort t = With_header.Known_size.abort t
  end
end

let%expect_test "write known_size blob" =
  Expect_test_helpers_async.with_temp_dir (fun object_directory ->
    let t = Blob.Known_size.create_uninitialised ~object_directory in
    let reader = Object_reader.Expect_test_helpers.blob_reader Do_not_validate_sha1 in
    let write_blob blob =
      let%bind () =
        Blob.Known_size.init_or_reset t ~length:(String.length blob) Mode.write_all
      in
      Blob.Known_size.append_data
        t
        (Bigstring.of_string blob)
        ~pos:0
        ~len:(String.length blob);
      let%map sha1 = Blob.Known_size.finalise_exn t in
      printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1)
    in
    let%bind () = write_blob "first file\n" in
    [%expect {| 303ff981c488b812b6215f7db7920dedb3b59d9a |}];
    let expected_file_path =
      (object_directory ^/ "30") ^/ "3ff981c488b812b6215f7db7920dedb3b59d9a"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    [%expect {|
      "x\156K\202\201OR04dH\203,*.QH\203\204I\229\002\000=7\006\020" |}];
    let%bind () = Object_reader.read_file reader ~file:expected_file_path () in
    [%expect {|
      Blob size: 11
      Blob chunk: first file |}];
    let%bind () = write_blob "second file\n" in
    [%expect {| 1c59427adc4b205a270d8f810310394962e79a8b |}];
    let expected_file_path =
      (object_directory ^/ "1c") ^/ "59427adc4b205a270d8f810310394962e79a8b"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    [%expect
      {|
      "x\156K\202\201OR04b(NM\206\207KQH\203\204I\229\002\000C\209\006i" |}];
    let%bind () = Object_reader.read_file reader ~file:expected_file_path () in
    [%expect {|
      Blob size: 12
      Blob chunk: second file |}];
    Deferred.unit)
;;

let%expect_test "write unknown_size blob" =
  Expect_test_helpers_async.with_temp_dir (fun object_directory ->
    let t = Blob.Unknown_size.create_uninitialised ~object_directory in
    let reader = Object_reader.Expect_test_helpers.blob_reader Do_not_validate_sha1 in
    let write_blob blob =
      let%bind () = Blob.Unknown_size.init_or_reset t Mode.write_all in
      Blob.Unknown_size.append_data
        t
        (Bigstring.of_string blob)
        ~pos:0
        ~len:(String.length blob);
      let%map sha1 = Blob.Unknown_size.finalise t in
      printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1)
    in
    let%bind () = write_blob "first file\n" in
    [%expect {| 303ff981c488b812b6215f7db7920dedb3b59d9a |}];
    let expected_file_path =
      (object_directory ^/ "30") ^/ "3ff981c488b812b6215f7db7920dedb3b59d9a"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    [%expect {| "x\156K\202\201OR04dH\203,*.QH\203\204I\229\002\000=7\006\020" |}];
    let%bind () = Object_reader.read_file reader ~file:expected_file_path () in
    [%expect {|
      Blob size: 11
      Blob chunk: first file |}];
    let%bind () = write_blob "second file\n" in
    [%expect {| 1c59427adc4b205a270d8f810310394962e79a8b |}];
    let expected_file_path =
      (object_directory ^/ "1c") ^/ "59427adc4b205a270d8f810310394962e79a8b"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    [%expect {| "x\156K\202\201OR04b(NM\206\207KQH\203\204I\229\002\000C\209\006i" |}];
    let%bind () = Object_reader.read_file reader ~file:expected_file_path () in
    [%expect {|
      Blob size: 12
      Blob chunk: second file |}];
    Deferred.unit)
;;

let compare_hex_dump here ~expected_contents contents =
  [%test_result: string]
    ~here:[ here ]
    ~expect:
      (String.strip expected_contents
       |> String.split_lines
       |> List.map ~f:String.strip
       |> String.concat ~sep:"\n")
    (String.Hexdump.to_string_hum contents)
;;

let%expect_test "write commit" =
  Expect_test_helpers_async.with_temp_dir (fun object_directory ->
    Expect_test_time_zone.with_fixed_time_zone_async (fun () ->
      let t = Commit.create ~object_directory in
      let reader = Object_reader.Expect_test_helpers.commit_reader Do_not_validate_sha1 in
      let%bind sha1 = Commit.write t Commit_.For_testing.example_commit Mode.write_all in
      printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
      [%expect {| 3678df8b9ac798bf8a19b6477254b0ca24a20954 |}];
      let expected_file_path =
        (object_directory ^/ "36") ^/ "78df8b9ac798bf8a19b6477254b0ca24a20954"
      in
      let%bind contents = Reader.file_contents expected_file_path in
      let expected_contents =
        match Zlib.For_testing.using_zlib_ng with
        | true ->
          {|
            00000000  78 9c d5 91 c1 6e d4 40  0c 86 39 cf 53 f8 d6 03  |x....n.@..9.S...|
            00000010  4a e5 99 78 66 32 08 21  28 ac a2 1e 58 ad 4a a9  |J..xf2.!(...X.J.|
            00000020  7a f5 cc 38 69 10 49 56  c9 6c 05 6f 8f b2 2b c4  |z..8i.IV.l.o..+.|
            00000030  85 13 3d e1 8b 2d fb d7  a7 df 76 9a c7 71 28 d0  |..=..-....v..q(.|
            00000040  d4 f8 aa 2c 22 60 44 d0  11 99 ba 8e ce 77 31 48  |...,"`D......w1H|
            00000050  53 67 a6 4c ba a9 53 76  56 d0 3b d6 5a 5b 75 e4  |Sg.L..SvV.;.Z[u.|
            00000060  45 a6 02 e4 3a ed b9 f3  1e d1 25 d2 09 c5 a0 b5  |E...:.....%.....|
            00000070  8e 03 05 66 df 25 62 4d  51 32 fe d6 73 88 da 84  |...f.%bMQ2..s...|
            00000080  4c 9a ba 14 29 db e0 24  b2 6f 62 e3 51 74 e7 1b  |L...)..$.ob.Qt..|
            00000090  8c 0e 23 06 ad f8 54 9e  e6 05 6e e6 3e f3 54 7d  |..#...T...n.>.T}|
            000000a0  5c 86 b5 0c 3c c1 3d 17  5e e6 e1 04 6f e3 79 f2  |\...<.=.^...o.y.|
            000000b0  5e 7e f0 78 fc 2e d7 69  1e df 81 b6 54 7b 1b a8  |^~.x...i....T{..|
            000000c0  26 a8 d0 22 aa 74 5e af  c8 8b 41 32 a5 39 0f 53  |&..".t^...A2.9.S|
            000000d0  0f c3 3a 57 4d 63 43 d5  a8 51 96 5e 0a f7 30 c7  |..:WMcC..Q.^..0.|
            000000e0  6f 92 0a 74 19 a3 c1 a0  b7 a5 1c 85 ce 25 26 6b  |o..t.........%&k|
            000000f0  b4 71 26 d5 8c 89 73 c4  8c 64 44 41 f9 79 14 b8  |.q&...s..dDA.y..|
            00000100  78 53 b0 11 9e 8b ac 97  b2 ff 17 af be 0e c6 1a  |xS..............|
            00000110  0f af 11 11 15 28 d8 70  1b 4d f5 c7 7e 1d 7a a8  |.....(.p.M..~.z.|
            00000120  b6 b8 d9 b5 b7 7b 38 b4  07 f8 72 db ee 3f dc 7f  |.....{8...r..?..|
            00000130  bd db 9d fb 0a 1e 64 59  87 79 7a 03 ed 74 3a b4  |......dY.yz..t:.|
            00000140  f0 ac af 69 a3 3c be 30  fe 2b c2 1f a1 ba 9c 6b  |...i.<.0.+.....k|
            00000150  b7 ff f4 b7 63 a9 cf db  db 21 2e 3c a5 27 b8 ba  |....c....!.<.'..|
            00000160  e4 2b f5 0b 19 bd 04 62                           |.+.....b|
          |}
        | false ->
          {x|
            00000000  78 9c d5 91 4f 6f d4 30  10 c5 39 fb 53 cc ad 07  |x...Oo.0..9.S...|
            00000010  94 6a ec f8 2f 42 08 0a  ab a8 07 56 ab d2 56 5c  |.j../B.....V..V\|
            00000020  c7 f6 24 0d 22 c9 2a f1  56 f0 ed 49 76 85 b8 70  |..$.".*.V..Iv..p|
            00000030  a2 27 de c5 9e f1 e8 a7  37 cf 69 1a 86 be 80 af  |.'......7.i.....|
            00000040  f1 55 99 99 41 31 a3 d5  5a d5 75 b4 ae 8d 81 7d  |.U..A1..Z.u....}|
            00000050  9d 49 67 2d 7d 9d b2 35  8c ce 92 94 d2 88 23 cd  |.Ig-}..5......#.|
            00000060  3c 16 d0 b6 95 8e 5a e7  10 6d d2 32 21 2b 34 c6  |<.....Z..m.2!+4.|
            00000070  52 d0 81 c8 b5 49 93 d4  91 33 fe 9e a7 10 a5 0a  |R....I...3......|
            00000080  2b 4f b7 29 ea 6c 82 e5  48 ce 47 ef 90 65 eb 3c  |+O.).l..H.G..e.<|
            00000090  46 8b 11 83 14 74 2a 4f  d3 0c 37 53 97 69 ac 3e  |F....t*O..7S.i.>|
            000000a0  ce fd 52 7a 1a e1 9e 0a  cd 53 7f 82 b7 f1 fc f2  |..Rz.....S......|
            000000b0  9e 7f d0 70 fc ce d7 69  1a de 81 34 ba 76 26 e8  |...p...i...4.v&.|
            000000c0  5a 43 85 06 51 a4 f3 7a  85 5f 0c e2 31 4d b9 1f  |ZC..Q..z._..1M..|
            000000d0  3b e8 97 a9 f2 de 84 ca  8b 81 e7 8e 0b 75 30 c5  |;............u0.|
            000000e0  6f 9c 0a b4 19 a3 5a bd  6f 4b 59 1d 5a 9b 48 1b  |o.....Z.oKY.Z.H.|
            000000f0  25 95 55 a9 26 4c 94 23  66 d4 8a 05 94 9f 47 86  |%.U.&L.#f.....G.|
            00000100  8b b7 b5 58 09 cf 85 97  cb b5 fb 17 af ae 0e ca  |...X............|
            00000110  28 07 af 71 95 80 15 b4  e2 36 9a e8 8e dd d2 77  |(..q.....6.....w|
            00000120  50 6d ba d9 35 b7 7b 38  34 07 f8 72 db ec 3f dc  |Pm..5.{84..r..?.|
            00000130  3f dc ed ce 7d 01 8f 3c  2f fd 34 be 81 66 3c 1d  |?...}..</.4..f<.|
            00000140  1a 78 96 d7 7a a3 7c 7d  a1 fe 2b c2 9f 41 71 89  |.x..z.|}..+..Aq.|
            00000150  6b b7 ff f4 b7 b0 c4 e7  ed db 21 ce 34 a6 27 b8  |k.........!.4.'.|
            00000160  ba 9c 57 e2 17 19 bd 04  62                       |..W.....b|
          |x}
      in
      compare_hex_dump [%here] ~expected_contents contents;
      let%bind () = Object_reader.read_file reader ~file:expected_file_path () in
      [%expect
        {|
        ((tree 2ee0644233b67fb9e83da4d4183cd65e076a1115)
         (parents
          (46f17af77006c41c0e20556a949aa7fc4a14bed0
           a9b129d414fcb4d596eba78b870e1f780b60b091))
         (author
          ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
           (timestamp (2018-12-02 09:03:54.000000000-05:00)) (zone UTC-5)))
         (committer
          ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
           (timestamp (2018-12-02 09:03:54.000000000-05:00)) (zone UTC-5)))
         (encoding (iso-8859-8))
         (merge_tags
          (((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e)
            (object_type Commit) (tag vtest)
            (tagger
             (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
               (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC+0))))
            (description "test tag"))))
         (gpg_signature
          ( "-----BEGIN PGP SIGNATURE-----\
           \nVersion: GnuPG v1.4\
           \n\
           \nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
           \nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
           \nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
           \nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
           \nXXXXXXXX\
           \n-----END PGP SIGNATURE-----"))
         (description "Merge branch 'branch'\n")) |}];
      let%bind sha1 =
        Commit.write
          t
          { Commit_.tree = Sha1.Hex.of_string "b39daecaf9bc405deea72ff4dcbd5bb16613eb1f"
          ; parents = []
          ; author =
              { name = "Bogdan-Cristian Tataroiu"
              ; email = "bogdan@example.com"
              ; timestamp = Time_ns_unix.of_string "2019-01-05 12:26:44.000000000Z"
              ; zone = Time_ns_unix.Zone.utc
              }
          ; committer =
              { name = "Bogdan-Cristian Tataroiu"
              ; email = "bogdan@example.com"
              ; timestamp = Time_ns_unix.of_string "2019-01-05 12:26:44.000000000Z"
              ; zone = Time_ns_unix.Zone.utc
              }
          ; encoding = None
          ; merge_tags = []
          ; gpg_signature = None
          ; description = "test commit\n"
          }
          Mode.write_all
      in
      printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
      [%expect {| d2ef8c710416f38bdf6e8487630486830edc6c7f |}];
      let expected_file_path =
        (object_directory ^/ "d2") ^/ "ef8c710416f38bdf6e8487630486830edc6c7f"
      in
      let%bind contents = Reader.file_contents expected_file_path in
      let expected_contents =
        match Zlib.For_testing.using_zlib_ng with
        | true ->
          {|
            00000000  78 9c a5 cd 41 0a 02 31  0c 40 51 d7 3d 45 f6 a2  |x...A..1.@Q.=E..|
            00000010  a4 9d 4e 65 40 44 f4 0a  5e 20 69 53 2d d8 a9 74  |..Ne@D..^ iS-..t|
            00000020  32 e0 f1 05 3d 82 eb 0f  ef c7 56 6b 51 70 e8 36  |2...=.....VkQp.6|
            00000030  da 45 80 87 29 91 44 ca  13 47 8f 63 12 a1 83 cb  |.E..).D..G.c....|
            00000040  d9 a7 c8 69 64 b6 21 d8  41 d8 66 43 ab 3e 5a 87  |...id.!.A.fC.>Z.|
            00000050  4b bb 27 9a 77 d7 5e 16  2d 34 c3 8d 94 7a 2b 2b  |K.'.w.^.-4...z++|
            00000060  1c f9 5b ce f2 a6 fa 7a  ca 3e b6 7a 02 3b fa 10  |..[....z.>.z.;..|
            00000070  26 eb d0 c3 16 11 d1 c4  ef 5e e5 6f c8 a8 2c 0a  |&........^.o..,.|
            00000080  3f ce 7c 00 2d 69 44 c0                           |?.|.-iD.|
          |}
        | false ->
          {|
            00000000  78 9c a5 8d 51 0a 02 21  10 40 fb f6 14 f3 1f 85  |x...Q..!.@......|
            00000010  ba 6a 2c 44 44 5d a1 0b  cc e8 58 42 ae e1 ce 42  |.j,DD]....XB...B|
            00000020  c7 2f b6 23 f4 7e 1f bc  17 5b ad 45 c0 6a bb 91  |./.#.~...[.E.j..|
            00000030  ce 0c 34 8c 09 39 62 1e  29 3a ed 13 33 1e 6c ce  |..4..9b.):..3.l.|
            00000040  2e 45 4a 9e c8 84 60 06  26 93 15 2e f2 68 1d 2e  |.EJ...`.&....h..|
            00000050  ed 9e 70 da 5d 7b 99 a5  e0 04 37 14 ec ad 2c 70  |..p.]{....7...,p|
            00000060  a4 d5 9c f9 8d f5 f5 e4  7d 6c f5 04 c6 bb 10 46  |........}l.....F|
            00000070  63 b5 83 ad fe a2 e2 ba  17 fe 3b a4 84 67 81 5f  |c.........;..g._|
            00000080  4e 7d 00 2d 69 44 c0                              |N}.-iD.|
          |}
      in
      compare_hex_dump [%here] ~expected_contents contents;
      let%bind () = Object_reader.read_file reader ~file:expected_file_path () in
      [%expect
        {|
        ((tree b39daecaf9bc405deea72ff4dcbd5bb16613eb1f) (parents ())
         (author
          ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
           (timestamp (2019-01-05 07:26:44.000000000-05:00)) (zone UTC+0)))
         (committer
          ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
           (timestamp (2019-01-05 07:26:44.000000000-05:00)) (zone UTC+0)))
         (encoding ()) (merge_tags ()) (gpg_signature ())
         (description "test commit\n")) |}];
      Deferred.unit))
;;

let%expect_test "write tree" =
  Expect_test_helpers_async.with_temp_dir (fun object_directory ->
    let t = Tree.create_uninitialised ~object_directory in
    let reader = Object_reader.Expect_test_helpers.tree_reader Do_not_validate_sha1 in
    let%bind () = Tree.init_or_reset t Mode.write_all in
    Tree.write_tree_line
      t
      Non_executable_file
      (Sha1.Raw.of_hex (Sha1.Hex.of_string "303ff981c488b812b6215f7db7920dedb3b59d9a"))
      ~name:"a";
    Tree.write_tree_line
      t
      Non_executable_file
      (Sha1.Raw.of_hex (Sha1.Hex.of_string "1c59427adc4b205a270d8f810310394962e79a8b"))
      ~name:"b";
    Tree.write_tree_line
      t
      Directory
      (Sha1.Raw.of_hex (Sha1.Hex.of_string "d0b2476d5a6fc7bced4f6ef841b7e7022fad0493"))
      ~name:"c";
    Tree.write_tree_line
      t
      Link
      (Sha1.Raw.of_hex (Sha1.Hex.of_string "68bdebaba7f41affa1aabce553c79818984181a9"))
      ~name:"d";
    let%bind sha1 = Tree.finalise t in
    printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
    [%expect {| b39daecaf9bc405deea72ff4dcbd5bb16613eb1f |}];
    let expected_file_path =
      (object_directory ^/ "b3") ^/ "9daecaf9bc405deea72ff4dcbd5bb16613eb1f"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    let expected_contents =
      match Zlib.For_testing.using_zlib_ng with
      | true ->
        {|
          00000000  78 9c 2b 29 4a 4d 55 30  34 34 65 30 34 30 30 33  |x.+)JMU044e04003|
          00000010  31 51 48 64 30 b0 ff d9  78 a4 63 87 d0 36 c5 f8  |1QHd0...x.c..6..|
          00000020  da ed 93 78 df 6e de 3a  77 16 54 32 89 41 26 d2  |...x.n.:w.T2.A&.|
          00000030  a9 ea 8e b7 42 94 3a 6f  7f 23 b3 80 a5 67 d2 f3  |....B.:o.#...g..|
          00000040  59 dd 26 06 06 06 06 0a  c9 0c 17 36 b9 e7 46 e5  |Y.&........6..F.|
          00000050  1f df f3 d6 3f ef 87 e3  f6 e7 4c fa 6b 59 26 1b  |....?.....L.kY&.|
          00000060  1a 81 25 53 18 32 f6 be  5e bd fc 8b d4 ff 85 ab  |..%S.2..^.......|
          00000070  f6 3c 0d 3e 3e 43 62 86  63 e3 4a 00 e0 1b 31 34  |.<.>>Cb.c.J...14|
        |}
      | false ->
        {|
          00000000  78 9c 2b 29 4a 4d 55 30  34 34 65 30 34 30 30 33  |x.+)JMU044e04003|
          00000010  31 51 48 64 30 b0 ff d9  78 a4 63 87 d0 36 c5 f8  |1QHd0...x.c..6..|
          00000020  da ed 93 78 df 6e de 3a  77 16 54 32 89 41 26 d2  |...x.n.:w.T2.A&.|
          00000030  a9 ea 8e b7 42 94 3a 6f  7f 23 b3 80 a5 67 d2 f3  |....B.:o.#...g..|
          00000040  59 dd 26 06 40 a0 90 cc  70 61 93 7b 6e 54 fe f1  |Y.&.@...pa.{nT..|
          00000050  3d 6f fd f3 7e 38 6e 7f  ce a4 bf 96 65 b2 a1 11  |=o..~8n.....e...|
          00000060  58 32 85 21 63 ef eb d5  cb bf 48 fd 5f b8 6a cf  |X2.!c.....H._.j.|
          00000070  d3 e0 e3 33 24 66 38 36  ae 04 00 e0 1b 31 34     |...3$f86.....14|
        |}
    in
    compare_hex_dump [%here] ~expected_contents contents;
    let%bind () = Object_reader.read_file reader ~file:expected_file_path () in
    [%expect
      {|
      Received tree line: Non_executable_file 303ff981c488b812b6215f7db7920dedb3b59d9a a
      Received tree line: Non_executable_file 1c59427adc4b205a270d8f810310394962e79a8b b
      Received tree line: Directory d0b2476d5a6fc7bced4f6ef841b7e7022fad0493 c
      Received tree line: Link 68bdebaba7f41affa1aabce553c79818984181a9 d |}];
    let%bind () = Tree.init_or_reset t Mode.write_all in
    Tree.write_tree_line
      t
      Non_executable_file
      (Sha1.Raw.of_hex (Sha1.Hex.of_string "667bb3858a056cc96e79c0c3b1edfb60135c2359"))
      ~name:"d";
    let%bind sha1 = Tree.finalise t in
    printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
    [%expect {| d0b2476d5a6fc7bced4f6ef841b7e7022fad0493 |}];
    let expected_file_path =
      (object_directory ^/ "d0") ^/ "b2476d5a6fc7bced4f6ef841b7e7022fad0493"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    [%expect
      {|
      "x\156+)JMU0\178d040031QHaH\171\222\220\218\197\154s2\175\242\192\225\141o\127'\b\199(G\002\000\230<\014\026" |}];
    let%bind () = Object_reader.read_file reader ~file:expected_file_path () in
    [%expect
      {|
      Received tree line: Non_executable_file 667bb3858a056cc96e79c0c3b1edfb60135c2359 d |}];
    Deferred.unit)
;;

let%expect_test "write tag" =
  Expect_test_helpers_async.with_temp_dir (fun object_directory ->
    Expect_test_time_zone.with_fixed_time_zone_async (fun () ->
      let t = Tag.create ~object_directory in
      let reader = Object_reader.Expect_test_helpers.tag_reader Do_not_validate_sha1 in
      let%bind sha1 = Tag.write t Tag_.For_testing.example_tag Mode.write_all in
      printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
      [%expect {| ac5f368017e73cac599c7dfd77bd36da2b816eaf |}];
      let expected_file_path =
        (object_directory ^/ "ac") ^/ "5f368017e73cac599c7dfd77bd36da2b816eaf"
      in
      let%bind contents = Reader.file_contents expected_file_path in
      let expected_contents =
        match Zlib.For_testing.using_zlib_ng with
        | true ->
          {|
            00000000  78 9c 1d cc 41 0a c2 30  10 40 51 d7 39 c5 ec 45  |x...A..0.@Q.9..E|
            00000010  99 a4 49 4a 41 44 f4 0a  5e 60 92 4c 43 c4 34 a5  |..IJAD..^`.LC.4.|
            00000020  1d 45 6f 2f 76 f7 e1 c1  17 ca a0 1d ee 5a 78 70  |.Eo/v........Zxp|
            00000030  14 18 13 06 83 83 76 83  67 6f 87 d1 47 b2 ce 68  |......v.go..G..h|
            00000040  e3 4d ec 08 23 a5 80 09  ad 61 25 df 99 21 b6 5a  |.M..#....a%..!.Z|
            00000050  8b 28 a1 0c 6f e1 75 ab  cc 0b 5c 5b 4e 34 1d 6e  |.(..o.u...\[N4.n|
            00000060  4b 59 a5 d0 04 77 12 5a  5a 79 c1 29 6c 72 e1 0f  |KY...w.ZZy.)lr..|
            00000070  d5 f9 c9 c7 d8 ea 19 b4  b3 7d 37 18 67 7a d8 23  |.........}7.gz.#|
            00000080  22 2a f5 7f 81 50 56 3f  40 8e 31 47              |"*...PV?@.1G|
          |}
        | false ->
          {|
            00000000  78 9c 1d cd 51 0a c2 30  10 04 50 bf 73 8a fd 17  |x...Q..0..P.s...|
            00000010  65 9b 26 29 01 11 d1 2b  78 81 4d b2 2d 11 db 94  |e.&)...+x.M.-...|
            00000020  76 15 bd bd b1 f3 35 30  f0 46 68 80 c6 e2 ae 84  |v.....50.Fh.....|
            00000030  07 47 81 3e 61 d0 e8 1b  eb 1d 3b e3 7b 17 c9 58  |.G.>a.....;.{..X|
            00000040  dd 68 a7 63 4b 18 29 05  4c 68 34 2b f9 ce 0c b1  |.h.cK.).Lh4+....|
            00000050  8c 63 16 25 d5 78 0b af  5b 1b 78 81 6b 19 12 4d  |.c.%.x..[.x.k..M|
            00000060  87 db 92 57 c9 34 c1 9d  84 96 92 5f 70 0a db 72  |...W.4....._p..r|
            00000070  e1 0f 8d f3 93 8f 15 38  d7 7b d3 b5 5e 5b dd c1  |.......8.{..^[..|
            00000080  1e 6b 94 fa 5b 50 2d f5  03 40 8e 31 47           |.k..[P-..@.1G|
          |}
      in
      compare_hex_dump [%here] ~expected_contents contents;
      let%bind () = Object_reader.read_file reader ~file:expected_file_path () in
      [%expect
        {|
        ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
         (tag vtest)
         (tagger
          (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
            (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC+0))))
         (description "test tag\n")) |}];
      let%bind sha1 =
        Tag.write
          t
          { Tag_.object_sha1 =
              Sha1.Hex.of_string "c2985c3a03cdf4db34571e2719f43b9651a586c0"
          ; object_type = Tag
          ; tag = "tag-test"
          ; tagger =
              Some
                { name = "Bogdan-Cristian Tataroiu"
                ; email = "bogdan@example.com"
                ; timestamp = Time_ns_unix.of_string "2019-01-13 12:26:44.000000000Z"
                ; zone = Time_ns_unix.Zone.utc
                }
          ; description = "test tag of a tag\n"
          }
          Mode.write_all
      in
      printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
      [%expect {| 5cddf48d3977bd7688e0fdaf8a307cc9e99ba238 |}];
      let expected_file_path =
        (object_directory ^/ "5c") ^/ "ddf48d3977bd7688e0fdaf8a307cc9e99ba238"
      in
      let%bind contents = Reader.file_contents expected_file_path in
      let expected_contents =
        match Zlib.For_testing.using_zlib_ng with
        | true ->
          {|
            00000000  78 9c 1d 8d 41 0a 83 30  10 45 bb ce 29 66 5f 2c  |x...A..0.E..)f_,|
            00000010  13 93 a8 81 52 4a 7b 85  5e 60 12 c7 60 51 23 3a  |....RJ{.^`..`Q#:|
            00000020  85 f6 f6 25 ae 3e 8f 07  ef 0b 25 d0 ce 9f 72 78  |...%.>....%...rx|
            00000030  73 14 88 b5 ef 5c 34 84  26 f6 83 ed 83 b1 ae d5  |s....\4.&.......|
            00000040  5c b7 da 0f d6 04 df 38  4d ae 6b 22 2a f9 ad 0c  |\......8M.k"*...|
            00000050  42 49 09 a5 b2 95 f0 2e  05 12 6f f0 c8 a9 a7 a5  |BI........o.....|
            00000060  7a 6e e3 2e 23 2d f0 22  a1 2d 8f 1f b8 86 c3 dc  |zn..#-.".-......|
            00000070  f9 4b f3 3a f1 25 e6 f9  06 da d9 d6 74 b5 45 0b  |.K.:.%......t.E.|
            00000080  67 44 44 a5 4a ab 44 21  0f 40 c7 cb 1f d1 7b 33  |gDD.J.D!.@....{3|
            00000090  7a                                                |z|
          |}
        | false ->
          {|
            00000000  78 9c 1d 8d 51 0a 83 30  10 44 fb 9d 53 ec 7f b1  |x...Q..0.D..S...|
            00000010  24 26 51 03 a5 94 f6 0a  bd c0 26 ae c1 a2 46 e2  |$&Q.......&...F.|
            00000020  16 da db d7 38 30 0c c3  c0 1b c6 08 ca ba 53 f2  |....80........S.|
            00000030  6f 0a 0c a1 76 9d 0d 1a  a5 0e fd 60 7a af 8d 6d  |o...v......`z..m|
            00000040  15 d5 ad 72 83 d1 de 35  56 a1 ed 9a 20 05 ff 56  |...r...5V... ..V|
            00000050  02 c6 28 76 97 ac 98 36  2e 25 52 86 47 8a 3d 2e  |..(v...6.%R.G.=.|
            00000060  d5 33 8f 1b 8f b8 c0 0b  19 73 1a 3f 70 f5 c7 72  |.3.......s.?p..r|
            00000070  a7 2f ce eb 44 97 90 e6  db 7e 6f 5a dd d5 46 1a  |./..D....~oZ..F.|
            00000080  38 cb 5d 42 14 56 81 42  1a 00 8f 97 3f d1 7b 33  |8.]B.V.B....?.{3|
            00000090  7a                                                |z|
          |}
      in
      compare_hex_dump [%here] ~expected_contents contents;
      let%bind () = Object_reader.read_file reader ~file:expected_file_path () in
      [%expect
        {|
        ((object_sha1 c2985c3a03cdf4db34571e2719f43b9651a586c0) (object_type Tag)
         (tag tag-test)
         (tagger
          (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
            (timestamp (2019-01-13 07:26:44.000000000-05:00)) (zone UTC+0))))
         (description "test tag of a tag\n")) |}];
      Deferred.unit))
;;
