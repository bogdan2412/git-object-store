(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2020  Bogdan-Cristian Tataroiu

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

type t =
  { git_object_parser : Git_object_parser.t
  ; zlib_inflate : Zlib.Inflate.t
  ; on_error : Error.t -> unit
  }

let create ~on_blob_size ~on_blob_chunk ~on_commit ~on_tree_line ~on_tag ~on_error =
  let git_object_parser =
    Git_object_parser.create
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      ~on_error
  in
  let zlib_inflate =
    Zlib.Inflate.create_uninitialised
      ~on_data_chunk:(Git_object_parser.append_data git_object_parser)
  in
  { git_object_parser; zlib_inflate; on_error }
;;

let read_file' t ~file ~push_back =
  Git_object_parser.reset t.git_object_parser;
  Zlib.Inflate.init_or_reset t.zlib_inflate;
  match%map
    Reader.with_file file ~f:(fun reader ->
      Reader.read_one_chunk_at_a_time reader ~handle_chunk:(fun buf ~pos ~len ->
        let consumed = Zlib.Inflate.process t.zlib_inflate buf ~pos ~len in
        if consumed <> len
        then
          return
            (`Stop_consumed
               ( Or_error.error_string
                   "Unexpected extra data at the end of git object file"
               , consumed ))
        else (
          match%map push_back () with
          | `Ok -> `Continue
          | `Reader_closed -> `Stop (Ok ()))))
  with
  | `Eof ->
    Or_error.try_with (fun () ->
      Zlib.Inflate.finalise t.zlib_inflate;
      Git_object_parser.finalise t.git_object_parser)
    |> Or_error.iter_error ~f:t.on_error
  | `Eof_with_unconsumed_data _ ->
    failwith "Reader left unconsumed input, should be impossible"
  | `Stopped or_error -> Or_error.iter_error or_error ~f:t.on_error
;;

let read_file t ~file = read_file' t ~file ~push_back:(Fn.const (return `Ok))

let set_on_blob t ~on_size ~on_chunk =
  Git_object_parser.set_on_blob t.git_object_parser ~on_size ~on_chunk
;;

let set_on_commit t on_commit =
  Git_object_parser.set_on_commit t.git_object_parser on_commit
;;

let set_on_tree_line t on_tree_line =
  Git_object_parser.set_on_tree_line t.git_object_parser on_tree_line
;;

let set_on_tag t on_tag = Git_object_parser.set_on_tag t.git_object_parser on_tag

module Expect_test_helpers = struct
  let with_temp_dir = Expect_test_helpers.with_temp_dir

  let blob_reader () =
    create
      ~on_blob_size:(fun size -> printf "Blob size: %d\n" size)
      ~on_blob_chunk:(fun buf ~pos ~len ->
        printf "Blob chunk: %s\n" (Bigstring.To_string.sub buf ~pos ~len))
      ~on_commit:(fun _ -> failwith "Expected blob")
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected blob")
      ~on_tag:(fun _ -> failwith "Expected blob")
      ~on_error:Error.raise
  ;;

  let commit_reader () =
    create
      ~on_blob_size:(fun _ -> failwith "Expected commit")
      ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> failwith "Expected commit")
      ~on_commit:(fun commit -> printf !"%{sexp: Commit.t}\n" commit)
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected commit")
      ~on_tag:(fun _ -> failwith "Expected commit")
      ~on_error:Error.raise
  ;;

  let tree_reader () =
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
  ;;

  let tag_reader () =
    create
      ~on_blob_size:(fun _ -> failwith "Expected tag")
      ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> failwith "Expected tag")
      ~on_commit:(fun _ -> failwith "Expected tag")
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected tag")
      ~on_tag:(fun tag -> printf !"%{sexp: Tag.t}\n" tag)
      ~on_error:Error.raise
  ;;
end

let%expect_test "file blob object" =
  Expect_test_helpers.with_temp_dir (fun dir ->
    let t = Expect_test_helpers.blob_reader () in
    let file = dir ^/ "git-object" in
    let%bind () =
      Writer.save
        file
        ~contents:"x\001K\202\201OR04dH\203,*.QH\203\204I\229\002\000=7\006\020"
    in
    let%bind () = read_file t ~file in
    [%expect {|
      Blob size: 11
      Blob chunk: first file |}])
;;

let%expect_test "link blob object" =
  Expect_test_helpers.with_temp_dir (fun dir ->
    let t = Expect_test_helpers.blob_reader () in
    let file = dir ^/ "git-object" in
    let%bind () =
      Writer.save file ~contents:"x\001K\202\201OR0fH\214O\001\000\017z\002\233"
    in
    let%bind () = read_file t ~file in
    [%expect {|
      Blob size: 3
      Blob chunk: c/d |}])
;;

let%expect_test "commit object" =
  Expect_test_helpers.with_temp_dir (fun dir ->
    let t = Expect_test_helpers.commit_reader () in
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
    let%bind () = read_file t ~file in
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

let%expect_test "tree object" =
  Expect_test_helpers.with_temp_dir (fun dir ->
    let t = Expect_test_helpers.tree_reader () in
    let file = dir ^/ "git-object" in
    let%bind () =
      Writer.save
        file
        ~contents:
          "x\001+)JMU044e040031QHd0\176\255\217x\164c\135\2086\197\248\218\237\147x\223n\222:w\022T2\137A&\210\169\234\142\183B\148:o\127#\179\128\165g\210\243Y\221&\006@\160\144\204pa\147{nT\254\241=o\253\243~8n\127\206\164\191\150e\178\161\017X2\133!c\239\235\213\203\191H\253_\184j\207\211\224\2273$f86\174\004\000\224\02714"
    in
    let%bind () = read_file t ~file in
    [%expect
      {|
         Received tree line: Non_executable_file 303ff981c488b812b6215f7db7920dedb3b59d9a a
         Received tree line: Non_executable_file 1c59427adc4b205a270d8f810310394962e79a8b b
         Received tree line: Directory d0b2476d5a6fc7bced4f6ef841b7e7022fad0493 c
         Received tree line: Link 68bdebaba7f41affa1aabce553c79818984181a9 d |}])
;;

let%expect_test "tag object" =
  Expect_test_helpers.with_temp_dir (fun dir ->
    let t = Expect_test_helpers.tag_reader () in
    let file = dir ^/ "git-object" in
    let%bind () =
      Writer.save
        file
        ~contents:
          "x\001\029\205Q\n\
           \1940\016\004P\191s\138\253\023e\155&)\001\017\209+x\129M\178-\017\219\148v\021\189\189\177\24350\240Fh\128\198\226\174\132\007G\129>a\208\232\027\235\029;\227{\023\201X\221h\167cK\024)\005Lh4+\249\206\012\177\140c\022%\213x\011\175[\027x\129k\025\018M\135\219\146W\2014\193\157\132\150\146_p\n\
           \219r\225\015\141\243\147\143\0218\215{\211\181^[\221\193\030k\148\250[PU\245\003@\1421G"
    in
    let%bind () = read_file t ~file in
    [%expect
      {|
         ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
          (tag vtest)
          (tagger
           (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
             (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC))))
          (description "test tag\n")) |}])
;;
