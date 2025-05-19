(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2025  Bogdan-Cristian Tataroiu

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

module type Parser = sig
  type _ t

  val append_data : _ t -> Bigstring.t -> pos:int -> len:int -> unit
  val finalise : 'Sha1_validation t -> 'Sha1_validation -> unit
  val reset : _ t -> unit
end

module Make (Parser : Parser) = struct
  type 'Sha1_validation t =
    { parser : 'Sha1_validation Parser.t
    ; zlib_inflate : Zlib.Inflate.t
    ; buf : Bigstring.t
    ; on_error : Error.t -> unit
    }

  let initial_read_chunk = 256

  let create parser ~on_error =
    let zlib_inflate =
      Zlib.Inflate.create_uninitialised ~on_data_chunk:(Parser.append_data parser)
    in
    let buf = Bigstring.create initial_read_chunk in
    { parser; zlib_inflate; buf; on_error }
  ;;

  let zlib_finalize t expected_sha1 =
    Or_error.try_with (fun () ->
      Zlib.Inflate.finalise t.zlib_inflate;
      Parser.finalise t.parser expected_sha1)
    |> Or_error.iter_error ~f:t.on_error
  ;;

  let read_file' t ~file ~push_back expected_sha1 =
    Parser.reset t.parser;
    Zlib.Inflate.init_or_reset t.zlib_inflate;
    let handle_chunk buf ~pos ~len =
      let consumed = Zlib.Inflate.process t.zlib_inflate buf ~pos ~len in
      if consumed <> len
      then
        return
          (`Stop_consumed
              ( Or_error.error_string "Unexpected extra data at the end of git object file"
              , consumed ))
      else (
        match%map push_back () with
        | `Ok -> `Continue
        | `Reader_closed -> `Stop (Ok ()))
    in
    let%bind fd = Unix.openfile file ~mode:[ `Rdonly ] ~perm:0o000 in
    Monitor.protect
      ~rest:`Raise
      ~run:`Now
      ~finally:(fun () -> Fd.close fd)
      (fun () ->
         let%bind len =
           Fd.syscall_in_thread_exn fd ~name:"read" (fun file_descr ->
             Bigstring_unix.read file_descr t.buf ~pos:0 ~len:initial_read_chunk)
         in
         match%bind handle_chunk t.buf ~pos:0 ~len with
         | `Stop or_error | `Stop_consumed (or_error, _) ->
           Or_error.iter_error or_error ~f:t.on_error;
           Deferred.unit
         | `Continue when Int.( <> ) len initial_read_chunk ->
           zlib_finalize t expected_sha1;
           Deferred.unit
         | `Continue ->
           let reader = Reader.create fd in
           (match%map
              Reader.with_close reader ~f:(fun () ->
                Reader.read_one_chunk_at_a_time reader ~handle_chunk)
            with
            | `Eof -> zlib_finalize t expected_sha1
            | `Eof_with_unconsumed_data _ ->
              failwith "Reader left unconsumed input, should be impossible"
            | `Stopped or_error -> Or_error.iter_error or_error ~f:t.on_error))
  ;;

  let read_file t ~file expected_sha1 =
    read_file' t ~file ~push_back:(Fn.const (return `Ok)) expected_sha1
  ;;
end

include Make (Parser)

let create
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      ~on_error
      sha1_validation
  =
  let parser =
    Parser.create
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      ~on_error
      sha1_validation
  in
  create parser ~on_error
;;

let set_on_blob t ~on_size ~on_chunk = Parser.set_on_blob t.parser ~on_size ~on_chunk
let set_on_commit t on_commit = Parser.set_on_commit t.parser on_commit
let set_on_tree_line t on_tree_line = Parser.set_on_tree_line t.parser on_tree_line
let set_on_tag t on_tag = Parser.set_on_tag t.parser on_tag

module Raw = struct
  module Base = Make (Parser.Raw)

  type 'Sha1_validation t =
    { base : 'Sha1_validation Base.t
    ; mutable on_payload : Bigstring.t -> pos:int -> len:int -> unit
    }

  let create ~on_header ~on_payload ~on_error sha1_validation =
    let rec t = lazy { base = force base; on_payload }
    and base =
      lazy
        (let parser =
           Parser.Raw.create
             ~on_header
             ~on_payload_chunk:(fun buf ~pos ~len ~final:_ ->
               (force t).on_payload buf ~pos ~len;
               len)
             ~on_error
             sha1_validation
         in
         Base.create parser ~on_error)
    in
    force t
  ;;

  let read_file t ~file sha1_validation = Base.read_file t.base ~file sha1_validation

  let read_file' t ~file ~push_back sha1_validation =
    Base.read_file' t.base ~file ~push_back sha1_validation
  ;;

  let set_on_header t on_header = Parser.Raw.set_on_header t.base.parser on_header
  let set_on_payload t on_payload = t.on_payload <- on_payload
end

module Expect_test_helpers = struct
  let blob_reader sha1_validation =
    create
      ~on_blob_size:(fun size -> printf "Blob size: %d\n" size)
      ~on_blob_chunk:(fun buf ~pos ~len ->
        printf "Blob chunk: %s\n" (Bigstring.To_string.sub buf ~pos ~len))
      ~on_commit:(fun _ -> failwith "Expected blob")
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected blob")
      ~on_tag:(fun _ -> failwith "Expected blob")
      ~on_error:Error.raise
      sha1_validation
  ;;

  let commit_reader sha1_validation =
    create
      ~on_blob_size:(fun _ -> failwith "Expected commit")
      ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> failwith "Expected commit")
      ~on_commit:(fun commit -> printf !"%{sexp: Commit.t}\n" commit)
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected commit")
      ~on_tag:(fun _ -> failwith "Expected commit")
      ~on_error:Error.raise
      sha1_validation
  ;;

  let tree_reader sha1_validation =
    create
      ~on_blob_size:(fun _ -> failwith "Expected tree")
      ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> failwith "Expected tree")
      ~on_commit:(fun _ -> failwith "Expected tree")
      ~on_tree_line:(fun mode sha1 ~name ->
        printf
          !"Received tree line: %{sexp: File_mode.t} %{Sha1.Hex} %s\n"
          mode
          (Sha1.Raw.Volatile.to_hex sha1)
          name)
      ~on_tag:(fun _ -> failwith "Expected tree")
      ~on_error:Error.raise
      sha1_validation
  ;;

  let tag_reader sha1_validation =
    create
      ~on_blob_size:(fun _ -> failwith "Expected tag")
      ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> failwith "Expected tag")
      ~on_commit:(fun _ -> failwith "Expected tag")
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected tag")
      ~on_tag:(fun tag -> printf !"%{sexp: Tag.t}\n" tag)
      ~on_error:Error.raise
      sha1_validation
  ;;
end

let%expect_test "file blob object" =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    let t = Expect_test_helpers.blob_reader Do_not_validate_sha1 in
    let file = dir ^/ "git-object" in
    let%bind () =
      Writer.save
        file
        ~contents:"x\001K\202\201OR04dH\203,*.QH\203\204I\229\002\000=7\006\020"
    in
    let%bind () = read_file t ~file () in
    [%expect
      {|
      Blob size: 11
      Blob chunk: first file |}];
    let t = Expect_test_helpers.blob_reader Validate_sha1 in
    let file = dir ^/ "git-object" in
    let%bind () =
      Writer.save
        file
        ~contents:"x\001K\202\201OR04dH\203,*.QH\203\204I\229\002\000=7\006\020"
    in
    let%bind () =
      Expect_test_helpers_async.show_raise_async (fun () ->
        read_file t ~file (Sha1.Hex.of_string "0000000000000000000000000000000000000000"))
    in
    [%expect
      {|
      Blob size: 11
      Blob chunk: first file

      (raised (
        Unexpected_sha1
        (actual_sha1 303ff981c488b812b6215f7db7920dedb3b59d9a)
        (expected_sha1 0000000000000000000000000000000000000000))) |}];
    Deferred.unit)
;;

let%expect_test "link blob object" =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    let t = Expect_test_helpers.blob_reader Do_not_validate_sha1 in
    let file = dir ^/ "git-object" in
    let%bind () =
      Writer.save file ~contents:"x\001K\202\201OR0fH\214O\001\000\017z\002\233"
    in
    let%bind () = read_file t ~file () in
    [%expect
      {|
      Blob size: 3
      Blob chunk: c/d |}];
    Deferred.unit)
;;

let%expect_test "commit object" =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    Expect_test_time_zone.with_fixed_time_zone_async (fun () ->
      let t = Expect_test_helpers.commit_reader Do_not_validate_sha1 in
      let file = dir ^/ "git-object" in
      let%bind () =
        Writer.save
          file
          ~contents:
            "x\001\165\141A\n\
             \1940\016E]\231\020\179\023%I\211HAD\244\n\
             ^`&\153h\1924\146N\193\227\027\244\b\254\229{\240~\168\165d\001\171\237F\0263\2080E\228\128i\162\224\244\024\153\241`Sr1P\028\137\140\247f`2I\225*\143\218\224R\239\017\231\221\181\229E2\206pC\193V\243\n\
             G\250\1543\191\177\188\158\188\015\181\156\192\140\206\251\201X\237`\171\251T\167\253^\248\239\144\018^\004~9\245\001-iD\192"
      in
      let%bind () = read_file t ~file () in
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

let%expect_test "tree object" =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    let t = Expect_test_helpers.tree_reader Do_not_validate_sha1 in
    let file = dir ^/ "git-object" in
    let%bind () =
      Writer.save
        file
        ~contents:
          "x\001+)JMU044e040031QHd0\176\255\217x\164c\135\2086\197\248\218\237\147x\223n\222:w\022T2\137A&\210\169\234\142\183B\148:o\127#\179\128\165g\210\243Y\221&\006@\160\144\204pa\147{nT\254\241=o\253\243~8n\127\206\164\191\150e\178\161\017X2\133!c\239\235\213\203\191H\253_\184j\207\211\224\2273$f86\174\004\000\224\02714"
    in
    let%bind () = read_file t ~file () in
    [%expect
      {|
      Received tree line: Non_executable_file 303ff981c488b812b6215f7db7920dedb3b59d9a a
      Received tree line: Non_executable_file 1c59427adc4b205a270d8f810310394962e79a8b b
      Received tree line: Directory d0b2476d5a6fc7bced4f6ef841b7e7022fad0493 c
      Received tree line: Link 68bdebaba7f41affa1aabce553c79818984181a9 d |}];
    Deferred.unit)
;;

let%expect_test "tag object" =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    Expect_test_time_zone.with_fixed_time_zone_async (fun () ->
      let t = Expect_test_helpers.tag_reader Do_not_validate_sha1 in
      let file = dir ^/ "git-object" in
      let%bind () =
        Writer.save
          file
          ~contents:
            "x\001\029\205Q\n\
             \1940\016\004P\191s\138\253\023e\155&)\001\017\209+x\129M\178-\017\219\148v\021\189\189\177\24350\240Fh\128\198\226\174\132\007G\129>a\208\232\027\235\029;\227{\023\201X\221h\167cK\024)\005Lh4+\249\206\012\177\140c\022%\213x\011\175[\027x\129k\025\018M\135\219\146W\2014\193\157\132\150\146_p\n\
             \219r\225\015\141\243\147\143\0218\215{\211\181^[\221\193\030k\148\250[PU\245\003@\1421G"
      in
      let%bind () = read_file t ~file () in
      [%expect
        {|
           ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
            (tag vtest)
            (tagger
             (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
               (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC+0))))
            (description "test tag\n")) |}];
      Deferred.unit))
;;
