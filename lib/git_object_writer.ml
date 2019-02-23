(** Library for manipulating a git object store via OCaml.

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
open Async

module Raw : sig
  type t

  val create_uninitialised : destination_directory:string -> t
  val init_or_reset : t -> unit Deferred.t
  val append_data : t -> Bigstring.t -> pos:int -> len:int -> unit
  val finalise : t -> Sha1.Raw.t Deferred.t
  val abort : t -> unit Deferred.t
end = struct
  type t =
    { destination_directory : string
    ; mutable temp_file_name : string
    ; mutable writer : Writer.t
    ; mutable sha1_compute : [`Initialised] Sha1.Compute.t
    ; zlib_deflate : Zlib.Deflate.t
    ; mutable initialised : bool
    }

  let create_uninitialised ~destination_directory =
    let temp_file_name = "/dev/stdout" in
    let writer = Lazy.force Writer.stdout in
    let sha1_compute =
      Sha1.Compute.create_uninitialised () |> Sha1.Compute.init_or_reset
    in
    let rec zlib_deflate =
      lazy
        (Zlib.Deflate.create_uninitialised ~on_data_chunk:(fun buf ~pos ~len ->
           Writer.write_bigstring (Lazy.force t).writer buf ~pos ~len))
    and t =
      lazy
        { zlib_deflate = Lazy.force zlib_deflate
        ; sha1_compute
        ; destination_directory
        ; temp_file_name
        ; writer
        ; initialised = false
        }
    in
    Lazy.force t
  ;;

  let init_or_reset t =
    let temp_file_name =
      Filename.temp_file ~in_dir:t.destination_directory "write-in-progress" ""
    in
    let%map writer = Writer.open_file temp_file_name in
    t.temp_file_name <- temp_file_name;
    t.writer <- writer;
    Zlib.Deflate.init_or_reset t.zlib_deflate;
    t.sha1_compute <- Sha1.Compute.init_or_reset t.sha1_compute;
    t.initialised <- true
  ;;

  let assert_initialised t =
    if not t.initialised then failwith "Git_object_writer used while not initialised"
  ;;

  let append_data t buf ~pos ~len =
    assert_initialised t;
    Zlib.Deflate.process t.zlib_deflate buf ~pos ~len;
    Sha1.Compute.process t.sha1_compute buf ~pos ~len
  ;;

  let finalise t =
    assert_initialised t;
    Zlib.Deflate.finalise t.zlib_deflate;
    let%bind () = Writer.flushed t.writer in
    let%bind () = Writer.close t.writer in
    let sha1_compute = Sha1.Compute.finalise t.sha1_compute in
    let sha1_hex = Sha1.Compute.get_hex sha1_compute in
    let sha1_raw = Sha1.Raw.Volatile.non_volatile (Sha1.Compute.get_raw sha1_compute) in
    let dir, file =
      let str = Sha1.Hex.Volatile.to_string sha1_hex in
      ( Filename.concat t.destination_directory (String.sub str ~pos:0 ~len:2)
      , String.sub str ~pos:2 ~len:(Sha1.Hex.length - 2) )
    in
    let%bind () = Unix.mkdir ~p:() dir in
    let new_path = Filename.concat dir file in
    let%map () = Sys.rename t.temp_file_name new_path in
    t.initialised <- false;
    sha1_raw
  ;;

  let abort t =
    if t.initialised
    then (
      let%bind () = Writer.flushed t.writer in
      let%bind () = Writer.close t.writer in
      let%map () = Unix.unlink t.temp_file_name in
      t.initialised <- false)
    else Deferred.unit
  ;;
end

let rec int_as_string_length ~acc n =
  if n <= 9 then acc + 1 else int_as_string_length ~acc:(acc + 1) (n / 10)
;;

let rec write_int_as_string buf ~pos ~value =
  if value <= 9
  then Bigstring.set buf pos (Char.unsafe_of_int (Char.to_int '0' + value))
  else (
    Bigstring.set buf pos (Char.unsafe_of_int (Char.to_int '0' + (value mod 10)));
    write_int_as_string buf ~pos:(pos - 1) ~value:(value / 10))
;;

module For_unknown_contents_size : sig
  type t

  val create_uninitialised : destination_directory:string -> t
  val init_or_reset : t -> prefix:string -> unit Deferred.t
  val double_buffer_space : t -> unit
  val make_room : t -> for_bytes:int -> unit
  val buf : t -> Bigstring.t
  val pos : t -> int
  val len : t -> int
  val advance_pos : t -> by:int -> unit
  val finalise : t -> Sha1.Raw.t Deferred.t
  val abort : t -> unit Deferred.t
end = struct
  type t =
    { raw : Raw.t
    ; mutable prefix : string
    ; mutable buf : Bigstring.t
    ; mutable start : int
    ; mutable pos : int
    }

  let create_uninitialised ~destination_directory =
    { raw = Raw.create_uninitialised ~destination_directory
    ; prefix = ""
    ; buf = Bigstring.create (if Core.am_running_inline_test then 1 else 1 lsl 15)
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

  let init_or_reset t ~prefix =
    let%map () = Raw.init_or_reset t.raw in
    let new_pos = String.length prefix + 32 in
    make_room_for_pos t ~pos:new_pos;
    t.prefix <- prefix;
    t.start <- new_pos;
    t.pos <- new_pos
  ;;

  let finalise t =
    let data_len = t.pos - t.start in
    assert (data_len >= 0);
    let len_str_len = int_as_string_length ~acc:0 data_len in
    let prefix_len = String.length t.prefix in
    Bigstring.set t.buf (t.start - 1) '\000';
    write_int_as_string t.buf ~pos:(t.start - 2) ~value:data_len;
    let len = prefix_len + len_str_len + 1 + data_len in
    let pos = t.pos - len in
    Bigstring.From_string.blit
      ~src:t.prefix
      ~src_pos:0
      ~len:prefix_len
      ~dst:t.buf
      ~dst_pos:pos;
    Raw.append_data t.raw t.buf ~pos ~len;
    Raw.finalise t.raw
  ;;

  let abort t = Raw.abort t.raw
end

module Commit_ = Commit

module Commit = struct
  type t = For_unknown_contents_size.t

  let create ~destination_directory =
    For_unknown_contents_size.create_uninitialised ~destination_directory
  ;;

  let write t commit =
    let%bind () = For_unknown_contents_size.init_or_reset t ~prefix:"commit " in
    let commit_str = Commit.format_as_git_object_payload commit in
    let commit_len = String.length commit_str in
    For_unknown_contents_size.make_room t ~for_bytes:commit_len;
    Bigstring.From_string.blit
      ~src:commit_str
      ~src_pos:0
      ~dst:(For_unknown_contents_size.buf t)
      ~dst_pos:(For_unknown_contents_size.pos t)
      ~len:commit_len;
    For_unknown_contents_size.advance_pos t ~by:commit_len;
    For_unknown_contents_size.finalise t
  ;;

  let write' ~destination_directory commit =
    let t = create ~destination_directory in
    write t commit
  ;;
end

module Tag_ = Tag

module Tag = struct
  type t = For_unknown_contents_size.t

  let create ~destination_directory =
    For_unknown_contents_size.create_uninitialised ~destination_directory
  ;;

  let write t tag =
    let%bind () = For_unknown_contents_size.init_or_reset t ~prefix:"tag " in
    let tag_str = Tag.format_as_git_object_payload tag in
    let tag_len = String.length tag_str in
    For_unknown_contents_size.make_room t ~for_bytes:tag_len;
    Bigstring.From_string.blit
      ~src:tag_str
      ~src_pos:0
      ~dst:(For_unknown_contents_size.buf t)
      ~dst_pos:(For_unknown_contents_size.pos t)
      ~len:tag_len;
    For_unknown_contents_size.advance_pos t ~by:tag_len;
    For_unknown_contents_size.finalise t
  ;;

  let write' ~destination_directory tag =
    let t = create ~destination_directory in
    write t tag
  ;;
end

module Tree = struct
  type t = For_unknown_contents_size.t

  let create_uninitialised ~destination_directory =
    For_unknown_contents_size.create_uninitialised ~destination_directory
  ;;

  let init_or_reset t = For_unknown_contents_size.init_or_reset t ~prefix:"tree "

  let write_tree_line_gen write_tree_line t mode sha1 ~name =
    match
      ( write_tree_line
          mode
          sha1
          ~name
          (For_unknown_contents_size.buf t)
          ~pos:(For_unknown_contents_size.pos t)
          ~len:(For_unknown_contents_size.len t)
        : Tree.Git_object_payload_formatter.Write_result.t )
    with
    | Wrote { bytes } -> For_unknown_contents_size.advance_pos t ~by:bytes
    | Need_more_space -> For_unknown_contents_size.double_buffer_space t
  ;;

  let write_tree_line =
    write_tree_line_gen Tree.Git_object_payload_formatter.write_tree_line
  ;;

  let write_tree_line' =
    write_tree_line_gen Tree.Git_object_payload_formatter.write_tree_line'
  ;;

  let finalise t = For_unknown_contents_size.finalise t
  let abort t = For_unknown_contents_size.abort t
end

module Blob = struct
  module Unknown_size = struct
    type t = For_unknown_contents_size.t

    let create_uninitialised ~destination_directory =
      For_unknown_contents_size.create_uninitialised ~destination_directory
    ;;

    let init_or_reset t = For_unknown_contents_size.init_or_reset t ~prefix:"blob "

    let append_data t buf ~pos ~len =
      For_unknown_contents_size.make_room t ~for_bytes:len;
      Bigstring.blit
        ~src:buf
        ~src_pos:pos
        ~dst:(For_unknown_contents_size.buf t)
        ~dst_pos:(For_unknown_contents_size.pos t)
        ~len;
      For_unknown_contents_size.advance_pos t ~by:len
    ;;

    let finalise t = For_unknown_contents_size.finalise t
    let abort t = For_unknown_contents_size.abort t
  end

  module Known_size = struct
    type t =
      { raw : Raw.t
      ; buf : Bigstring.t
      ; mutable expected : int
      ; mutable written : int
      }

    let create_uninitialised ~destination_directory =
      let t =
        { raw = Raw.create_uninitialised ~destination_directory
        ; buf = Bigstring.create 32
        ; expected = 0
        ; written = 0
        }
      in
      Bigstring.From_string.blit ~src:"blob " ~src_pos:0 ~dst:t.buf ~dst_pos:0 ~len:5;
      t
    ;;

    let init_or_reset t ~length =
      let len_str_len = int_as_string_length ~acc:0 length in
      write_int_as_string t.buf ~pos:(5 + len_str_len - 1) ~value:length;
      Bigstring.set t.buf (5 + len_str_len) '\000';
      t.expected <- length;
      t.written <- 0;
      let%map () = Raw.init_or_reset t.raw in
      Raw.append_data t.raw t.buf ~pos:0 ~len:(5 + len_str_len + 1)
    ;;

    let append_data t buf ~pos ~len =
      Raw.append_data t.raw buf ~pos ~len;
      t.written <- t.written + len
    ;;

    let finalise t =
      if t.expected <> t.written
      then
        raise_s
          [%message
            "Blob.Known_size was not fed the expected amount of data"
              ~expected:(t.expected : int)
              ~written:(t.written : int)];
      Raw.finalise t.raw
    ;;

    let abort t = Raw.abort t.raw
  end
end

let%expect_test "write known_size blob" =
  Expect_test_helpers.with_temp_dir (fun destination_directory ->
    let t = Blob.Known_size.create_uninitialised ~destination_directory in
    let reader = Git_object_reader.Expect_test_helpers.blob_reader () in
    let write_blob blob =
      let%bind () = Blob.Known_size.init_or_reset t ~length:(String.length blob) in
      Blob.Known_size.append_data
        t
        (Bigstring.of_string blob)
        ~pos:0
        ~len:(String.length blob);
      let%map sha1 = Blob.Known_size.finalise t in
      printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1)
    in
    let%bind () = write_blob "first file\n" in
    let%bind () = [%expect {| 303ff981c488b812b6215f7db7920dedb3b59d9a |}] in
    let expected_file_path =
      (destination_directory ^/ "30") ^/ "3ff981c488b812b6215f7db7920dedb3b59d9a"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    let%bind () =
      [%expect {| "x\156K\202\201OR04dH\203,*.QH\203\204I\229\002\000=7\006\020" |}]
    in
    let%bind () = Git_object_reader.read_file reader ~file:expected_file_path in
    let%bind () = [%expect {| Blob chunk: first file |}] in
    let%bind () = write_blob "second file\n" in
    let%bind () = [%expect {| 1c59427adc4b205a270d8f810310394962e79a8b |}] in
    let expected_file_path =
      (destination_directory ^/ "1c") ^/ "59427adc4b205a270d8f810310394962e79a8b"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    let%bind () =
      [%expect
        {| "x\156K\202\201OR04b(NM\206\207KQH\203\204I\229\002\000C\209\006i" |}]
    in
    let%bind () = Git_object_reader.read_file reader ~file:expected_file_path in
    [%expect {| Blob chunk: second file |}])
;;

let%expect_test "write unknown_size blob" =
  Expect_test_helpers.with_temp_dir (fun destination_directory ->
    let t = Blob.Unknown_size.create_uninitialised ~destination_directory in
    let reader = Git_object_reader.Expect_test_helpers.blob_reader () in
    let write_blob blob =
      let%bind () = Blob.Unknown_size.init_or_reset t in
      Blob.Unknown_size.append_data
        t
        (Bigstring.of_string blob)
        ~pos:0
        ~len:(String.length blob);
      let%map sha1 = Blob.Unknown_size.finalise t in
      printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1)
    in
    let%bind () = write_blob "first file\n" in
    let%bind () = [%expect {| 303ff981c488b812b6215f7db7920dedb3b59d9a |}] in
    let expected_file_path =
      (destination_directory ^/ "30") ^/ "3ff981c488b812b6215f7db7920dedb3b59d9a"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    let%bind () =
      [%expect {| "x\156K\202\201OR04dH\203,*.QH\203\204I\229\002\000=7\006\020" |}]
    in
    let%bind () = Git_object_reader.read_file reader ~file:expected_file_path in
    let%bind () = [%expect {| Blob chunk: first file |}] in
    let%bind () = write_blob "second file\n" in
    let%bind () = [%expect {| 1c59427adc4b205a270d8f810310394962e79a8b |}] in
    let expected_file_path =
      (destination_directory ^/ "1c") ^/ "59427adc4b205a270d8f810310394962e79a8b"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    let%bind () =
      [%expect
        {| "x\156K\202\201OR04b(NM\206\207KQH\203\204I\229\002\000C\209\006i" |}]
    in
    let%bind () = Git_object_reader.read_file reader ~file:expected_file_path in
    [%expect {| Blob chunk: second file |}])
;;

let%expect_test "write commit" =
  Expect_test_helpers.with_temp_dir (fun destination_directory ->
    let t = Commit.create ~destination_directory in
    let reader = Git_object_reader.Expect_test_helpers.commit_reader () in
    let%bind sha1 = Commit.write t Commit_.For_testing.example_commit in
    printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
    let%bind () = [%expect {| 3678df8b9ac798bf8a19b6477254b0ca24a20954 |}] in
    let expected_file_path =
      (destination_directory ^/ "36") ^/ "78df8b9ac798bf8a19b6477254b0ca24a20954"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    let%bind () =
      [%expect
        {x| "x\156\213\145Oo\2120\016\1979\251S\204\173\007\148j\236\248/B\b\n\171\168\007V\171\210V\\\199\246$\r\"\201*\241V\240\237Iv\133\184p\162'\222\197\158\241\232\1677\207i\026\134\190\128\175\241U\153\153A1\163\213Z\213u\180\174\141\129}\157Ig-}\157\1785\140\206\146\148\210\136#\205<\022\208\182\149\142Z\231\016m\2102!+4\198R\208\129\200\181I\147\212\1453\254\158\167\016\165\n+O\183)\234l\130\229H\206G\239\144e\235<F\139\017\131\020t*O\211\0127S\151i\172>\206\253Rz\026\225\158\n\205S\127\130\183\241\252\242\158\127\208p\252\206\215i\026\222\1294\186v&\232ZC\133\006Q\164\243z\133_\012\2261M\185\031;\232\151\169\242\222\132\202\139\129\231\142\011u0\197o\156\n\180\025\163Z\189oKY\029Z\155H\027%\149U\169&L\148#f\212\138\005\148\159G\134\139\183\181X\t\207\133\151\203\181\251\023\175\174\014\202(\007\175q\149\128\021\180\2266\154\232\142\221\210wPm\186\2175\183{84\007\248r\219\236?\220?\220\237\206}\001\143</\2534\190\129f<\029\026x\150\215z\163|}\161\254+\194\159Aq\137k\183\255\244\183\176\196\231\237\219!\2064\166'\184\186\156W\226\023\025\189\004b" |x}]
    in
    let%bind () = Git_object_reader.read_file reader ~file:expected_file_path in
    let%bind () =
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
                  (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC))))
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
            (description "Merge branch 'branch'\n")) |}]
    in
    let%bind sha1 =
      Commit.write
        t
        { Commit_.tree = Sha1.Hex.of_string "b39daecaf9bc405deea72ff4dcbd5bb16613eb1f"
        ; parents = []
        ; author =
            { name = "Bogdan-Cristian Tataroiu"
            ; email = "bogdan@example.com"
            ; timestamp = Time_ns.of_string "2019-01-05 12:26:44.000000000Z"
            ; zone = Time_ns.Zone.utc
            }
        ; committer =
            { name = "Bogdan-Cristian Tataroiu"
            ; email = "bogdan@example.com"
            ; timestamp = Time_ns.of_string "2019-01-05 12:26:44.000000000Z"
            ; zone = Time_ns.Zone.utc
            }
        ; encoding = None
        ; merge_tags = []
        ; gpg_signature = None
        ; description = "test commit\n"
        }
    in
    printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
    let%bind () = [%expect {| d2ef8c710416f38bdf6e8487630486830edc6c7f |}] in
    let expected_file_path =
      (destination_directory ^/ "d2") ^/ "ef8c710416f38bdf6e8487630486830edc6c7f"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    let%bind () =
      [%expect
        {| "x\156\165\141Q\n\002!\016@\251\246\020\243\031\133\186j,DD]\161\011\204\232XB\174\225\206B\199/\182#\244~\031\188\023[\173E\192j\187\145\206\0124\140\t9b\030):\237\0193\030l\206.EJ\158\200\132`\006&\147\021.\242h\029.\237\158p\218]{\153\165\224\0047\020\236\173,p\164\213\156\249\141\245\245\228}l\245\004\198\187\016Fc\181\131\173\254\162\226\186\023\254;\164\132g\129_N}\000-iD\192" |}]
    in
    let%bind () = Git_object_reader.read_file reader ~file:expected_file_path in
    [%expect
      {|
      ((tree b39daecaf9bc405deea72ff4dcbd5bb16613eb1f) (parents ())
       (author
        ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
         (timestamp (2019-01-05 07:26:44.000000000-05:00)) (zone UTC)))
       (committer
        ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
         (timestamp (2019-01-05 07:26:44.000000000-05:00)) (zone UTC)))
       (encoding ()) (merge_tags ()) (gpg_signature ())
       (description "test commit\n")) |}])
;;

let%expect_test "write tree" =
  Expect_test_helpers.with_temp_dir (fun destination_directory ->
    let t = Tree.create_uninitialised ~destination_directory in
    let reader = Git_object_reader.Expect_test_helpers.tree_reader () in
    let%bind () = Tree.init_or_reset t in
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
    let%bind () = [%expect {| b39daecaf9bc405deea72ff4dcbd5bb16613eb1f |}] in
    let expected_file_path =
      (destination_directory ^/ "b3") ^/ "9daecaf9bc405deea72ff4dcbd5bb16613eb1f"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    let%bind () =
      [%expect
        {| "x\156+)JMU044e040031QHd0\176\255\217x\164c\135\2086\197\248\218\237\147x\223n\222:w\022T2\137A&\210\169\234\142\183B\148:o\127#\179\128\165g\210\243Y\221&\006@\160\144\204pa\147{nT\254\241=o\253\243~8n\127\206\164\191\150e\178\161\017X2\133!c\239\235\213\203\191H\253_\184j\207\211\224\2273$f86\174\004\000\224\02714" |}]
    in
    let%bind () = Git_object_reader.read_file reader ~file:expected_file_path in
    let%bind () =
      [%expect
        {|
            Received tree line: Non_executable_file 303ff981c488b812b6215f7db7920dedb3b59d9a a
            Received tree line: Non_executable_file 1c59427adc4b205a270d8f810310394962e79a8b b
            Received tree line: Directory d0b2476d5a6fc7bced4f6ef841b7e7022fad0493 c
            Received tree line: Link 68bdebaba7f41affa1aabce553c79818984181a9 d |}]
    in
    let%bind () = Tree.init_or_reset t in
    Tree.write_tree_line
      t
      Non_executable_file
      (Sha1.Raw.of_hex (Sha1.Hex.of_string "667bb3858a056cc96e79c0c3b1edfb60135c2359"))
      ~name:"d";
    let%bind sha1 = Tree.finalise t in
    printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
    let%bind () = [%expect {| d0b2476d5a6fc7bced4f6ef841b7e7022fad0493 |}] in
    let expected_file_path =
      (destination_directory ^/ "d0") ^/ "b2476d5a6fc7bced4f6ef841b7e7022fad0493"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    let%bind () =
      [%expect
        {| "x\156+)JMU0\178d040031QHaH\171\222\220\218\197\154s2\175\242\192\225\141o\127'\b\199(G\002\000\230<\014\026" |}]
    in
    let%bind () = Git_object_reader.read_file reader ~file:expected_file_path in
    [%expect
      {| Received tree line: Non_executable_file 667bb3858a056cc96e79c0c3b1edfb60135c2359 d |}])
;;

let%expect_test "write tag" =
  Expect_test_helpers.with_temp_dir (fun destination_directory ->
    let t = Tag.create ~destination_directory in
    let reader = Git_object_reader.Expect_test_helpers.tag_reader () in
    let%bind sha1 = Tag.write t Tag_.For_testing.example_tag in
    printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
    let%bind () = [%expect {| ac5f368017e73cac599c7dfd77bd36da2b816eaf |}] in
    let expected_file_path =
      (destination_directory ^/ "ac") ^/ "5f368017e73cac599c7dfd77bd36da2b816eaf"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    let%bind () =
      [%expect
        {| "x\156\029\205Q\n\1940\016\004P\191s\138\253\023e\155&)\001\017\209+x\129M\178-\017\219\148v\021\189\189\177\24350\240Fh\128\198\226\174\132\007G\129>a\208\232\027\235\029;\227{\023\201X\221h\167cK\024)\005Lh4+\249\206\012\177\140c\022%\213x\011\175[\027x\129k\025\018M\135\219\146W\2014\193\157\132\150\146_p\n\219r\225\015\141\243\147\143\0218\215{\211\181^[\221\193\030k\148\250[P-\245\003@\1421G" |}]
    in
    let%bind () = Git_object_reader.read_file reader ~file:expected_file_path in
    let%bind () =
      [%expect
        {|
           ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
            (tag vtest)
            (tagger
             (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
               (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC))))
            (description "test tag\n")) |}]
    in
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
              ; timestamp = Time_ns.of_string "2019-01-13 12:26:44.000000000Z"
              ; zone = Time_ns.Zone.utc
              }
        ; description = "test tag of a tag\n"
        }
    in
    printf !"%{Sha1.Hex}" (Sha1.Raw.to_hex sha1);
    let%bind () = [%expect {| 5cddf48d3977bd7688e0fdaf8a307cc9e99ba238 |}] in
    let expected_file_path =
      (destination_directory ^/ "5c") ^/ "ddf48d3977bd7688e0fdaf8a307cc9e99ba238"
    in
    let%bind contents = Reader.file_contents expected_file_path in
    printf "%S" contents;
    let%bind () =
      [%expect
        {| "x\156\029\141Q\n\1310\016D\251\157S\236\127\177$&Q\003\165\148\246\n\189\192&\174\193\162F\226\022\218\219\21580\012\195\192\027\198\b\202\186S\242o\n\012\161v\157\r\026\165\014\253`z\175\141m\021\213\173r\131\209\2225V\161\237\154 \005\255V\002\198(v\151\172\1526.%R\134G\138=.\2133\143\027\143\184\192\011\025s\026?p\245\199r\167/\206\235D\151\144\230\219~oZ\221\213F\0268\203]B\020V\129B\026\000\143\151?\209{3z" |}]
    in
    let%bind () = Git_object_reader.read_file reader ~file:expected_file_path in
    [%expect
      {|
      ((object_sha1 c2985c3a03cdf4db34571e2719f43b9651a586c0) (object_type Tag)
       (tag tag-test)
       (tagger
        (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
          (timestamp (2019-01-13 07:26:44.000000000-05:00)) (zone UTC))))
       (description "test tag of a tag\n")) |}])
;;
