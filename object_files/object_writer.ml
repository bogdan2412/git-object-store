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
      printf "%S" contents;
      [%expect
        {x|
        "x\156\213\145Oo\2120\016\1979\251S\204\173\007\148j\236\248/B\b\n\171\168\007V\171\210V\\\199\246$\r\"\201*\241V\240\237Iv\133\184p\162'\222\197\158\241\232\1677\207i\026\134\190\128\175\241U\153\153A1\163\213Z\213u\180\174\141\129}\157Ig-}\157\1785\140\206\146\148\210\136#\205<\022\208\182\149\142Z\231\016m\2102!+4\198R\208\129\200\181I\147\212\1453\254\158\167\016\165\n+O\183)\234l\130\229H\206G\239\144e\235<F\139\017\131\020t*O\211\0127S\151i\172>\206\253Rz\026\225\158\n\205S\127\130\183\241\252\242\158\127\208p\252\206\215i\026\222\1294\186v&\232ZC\133\006Q\164\243z\133_\012\2261M\185\031;\232\151\169\242\222\132\202\139\129\231\142\011u0\197o\156\n\180\025\163Z\189oKY\029Z\155H\027%\149U\169&L\148#f\212\138\005\148\159G\134\139\183\181X\t\207\133\151\203\181\251\023\175\174\014\202(\007\175q\149\128\021\180\2266\154\232\142\221\210wPm\186\2175\183{84\007\248r\219\236?\220?\220\237\206}\001\143</\2534\190\129f<\029\026x\150\215z\163|}\161\254+\194\159Aq\137k\183\255\244\183\176\196\231\237\219!\2064\166'\184\186\156W\226\023\025\189\004b" |x}];
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
      printf "%S" contents;
      [%expect
        {|
        "x\156\165\141Q\n\002!\016@\251\246\020\243\031\133\186j,DD]\161\011\204\232XB\174\225\206B\199/\182#\244~\031\188\023[\173E\192j\187\145\206\0124\140\t9b\030):\237\0193\030l\206.EJ\158\200\132`\006&\147\021.\242h\029.\237\158p\218]{\153\165\224\0047\020\236\173,p\164\213\156\249\141\245\245\228}l\245\004\198\187\016Fc\181\131\173\254\162\226\186\023\254;\164\132g\129_N}\000-iD\192" |}];
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
    printf "%S" contents;
    [%expect
      {|
      "x\156+)JMU044e040031QHd0\176\255\217x\164c\135\2086\197\248\218\237\147x\223n\222:w\022T2\137A&\210\169\234\142\183B\148:o\127#\179\128\165g\210\243Y\221&\006@\160\144\204pa\147{nT\254\241=o\253\243~8n\127\206\164\191\150e\178\161\017X2\133!c\239\235\213\203\191H\253_\184j\207\211\224\2273$f86\174\004\000\224\02714" |}];
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
      printf "%S" contents;
      [%expect
        {|
        "x\156\029\205Q\n\1940\016\004P\191s\138\253\023e\155&)\001\017\209+x\129M\178-\017\219\148v\021\189\189\177\24350\240Fh\128\198\226\174\132\007G\129>a\208\232\027\235\029;\227{\023\201X\221h\167cK\024)\005Lh4+\249\206\012\177\140c\022%\213x\011\175[\027x\129k\025\018M\135\219\146W\2014\193\157\132\150\146_p\n\219r\225\015\141\243\147\143\0218\215{\211\181^[\221\193\030k\148\250[P-\245\003@\1421G" |}];
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
      printf "%S" contents;
      [%expect
        {|
        "x\156\029\141Q\n\1310\016D\251\157S\236\127\177$&Q\003\165\148\246\n\189\192&\174\193\162F\226\022\218\219\21580\012\195\192\027\198\b\202\186S\242o\n\012\161v\157\r\026\165\014\253`z\175\141m\021\213\173r\131\209\2225V\161\237\154 \005\255V\002\198(v\151\172\1526.%R\134G\138=.\2133\143\027\143\184\192\011\025s\026?p\245\199r\167/\206\235D\151\144\230\219~oZ\221\213F\0268\203]B\020V\129B\026\000\143\151?\209{3z" |}];
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
