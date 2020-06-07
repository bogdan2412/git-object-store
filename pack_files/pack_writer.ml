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

open! Core
open! Async
open! Import

type 'Sha1_validation t =
  { pack_directory : string
  ; temp_file_name : string
  ; writer : Writer.t
  ; mutable items_in_pack : int
  ; pack_object_zlib_deflate : Zlib.Deflate.t
  ; add_object_on_header : Object_type.t -> size:int -> unit
  ; add_object_on_payload : Bigstring.t -> pos:int -> len:int -> unit
  ; raw_object_reader : 'Sha1_validation Object_reader.Raw.t
  }

let write_object_type_and_length writer (object_type : Object_type.t) ~size =
  let type_int =
    match object_type with
    | Commit -> 1
    | Tree -> 2
    | Blob -> 3
    | Tag -> 4
  in
  let first_byte =
    if size <= 15
    then (type_int lsl 4) lor size
    else 128 lor (type_int lsl 4) lor (size land 15)
  in
  Writer.write_char writer (Char.of_int_exn first_byte);
  if size > 15
  then (
    let rec loop writer size =
      if size <= 127
      then Writer.write_char writer (Char.of_int_exn size)
      else (
        Writer.write_char writer (Char.of_int_exn (128 lor (size land 127)));
        loop writer (size lsr 7))
    in
    loop writer (size lsr 4))
;;

let create ~pack_directory sha1_validation =
  let open Deferred.Or_error.Let_syntax in
  let%bind temp_file_name =
    Or_error.try_with (fun () ->
      Filename.temp_file ~in_dir:pack_directory "pack-in-progress" "")
    |> Deferred.return
  in
  let%map writer =
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      Writer.open_file temp_file_name)
  in
  Writer.write writer "PACK\000\000\000\002\000\000\000\000";
  let pack_object_zlib_deflate =
    Zlib.Deflate.create_uninitialised ~on_data_chunk:(fun buf ~pos ~len ->
      Writer.write_bigstring writer buf ~pos ~len)
  in
  let on_header object_type ~size =
    write_object_type_and_length writer object_type ~size
  in
  let on_payload buf ~pos ~len =
    Zlib.Deflate.process pack_object_zlib_deflate buf ~pos ~len
  in
  let raw_object_reader =
    Object_reader.Raw.create ~on_header ~on_payload ~on_error:Error.raise sha1_validation
  in
  { pack_directory
  ; temp_file_name
  ; writer
  ; items_in_pack = 0
  ; pack_object_zlib_deflate
  ; add_object_on_header = on_header
  ; add_object_on_payload = on_payload
  ; raw_object_reader
  }
;;

let abort t =
  Monitor.try_with_or_error ~extract_exn:true (fun () ->
    let%bind () = Writer.close t.writer in
    Unix.unlink t.temp_file_name)
;;

let add_object_exn t ~object_file expected_sha1 =
  t.items_in_pack <- t.items_in_pack + 1;
  Zlib.Deflate.init_or_reset t.pack_object_zlib_deflate;
  let%map () =
    Object_reader.Raw.read_file t.raw_object_reader ~file:object_file expected_sha1
  in
  Zlib.Deflate.finalise t.pack_object_zlib_deflate
;;

let add_object_from_pack_exn t pack_reader ~index =
  t.items_in_pack <- t.items_in_pack + 1;
  Zlib.Deflate.init_or_reset t.pack_object_zlib_deflate;
  Pack_reader.read_raw_object
    pack_reader
    ~index
    ~on_header:t.add_object_on_header
    ~on_payload:t.add_object_on_payload;
  Zlib.Deflate.finalise t.pack_object_zlib_deflate
;;

let add_object_exn t ~object_file expected_sha1 =
  if Writer.is_closed t.writer
  then failwith "[add_object_exn] called on aborted pack writer";
  match%bind
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      add_object_exn t ~object_file expected_sha1)
  with
  | Ok () -> Deferred.unit
  | Error _ as error ->
    let%map abort_result = abort t in
    ok_exn (Or_error.combine_errors_unit [ error; abort_result ])
;;

let add_object_from_pack_exn t pack_reader ~index =
  if Writer.is_closed t.writer
  then failwith "[add_object_from_pack_exn] called on aborted pack writer";
  match Or_error.try_with (fun () -> add_object_from_pack_exn t pack_reader ~index) with
  | Ok () -> Deferred.unit
  | Error _ as error ->
    let%map abort_result = abort t in
    ok_exn (Or_error.combine_errors_unit [ error; abort_result ])
;;

let finalise_exn t =
  let%bind () = Writer.close t.writer in
  let%bind () =
    let%bind fd = Unix.openfile ~mode:[ `Wronly ] t.temp_file_name in
    Fd.with_close fd ~f:(fun fd ->
      let%map result = Unix.lseek fd (Int64.of_int_exn 8) ~mode:`Set in
      assert (Int64.(result = of_int_exn 8));
      let buf = Bigstring.create 4 in
      Bigstring.set_uint32_be_exn buf ~pos:0 t.items_in_pack;
      Fd.syscall_exn fd (fun desc -> Bigstring_unix.really_write desc buf))
  in
  let sha1 = Sha1.Compute.create_uninitialised () in
  let sha1 = Sha1.Compute.init_or_reset sha1 in
  let%bind () =
    Reader.with_file t.temp_file_name ~f:(fun reader ->
      Reader.read_one_chunk_at_a_time reader ~handle_chunk:(fun buf ~pos ~len ->
        Sha1.Compute.process sha1 buf ~pos ~len;
        return `Continue))
    >>| function
    | `Eof -> ()
    | `Eof_with_unconsumed_data _ | `Stopped _ ->
      failwith "Reader.read_one_chunk_at_a_time bug"
  in
  let sha1 = Sha1.Compute.finalise sha1 in
  let%bind () =
    let%bind fd = Unix.openfile ~mode:[ `Wronly ] t.temp_file_name in
    Fd.with_close fd ~f:(fun fd ->
      let%map _ = Unix.lseek fd Int64.zero ~mode:`End in
      let buf = Bigstring.create 20 in
      Bigstring.From_bytes.blit
        ~src:(Sha1.Raw.Volatile.bytes (Sha1.Compute.get_raw sha1))
        ~src_pos:0
        ~dst:buf
        ~dst_pos:0
        ~len:Sha1.Raw.length;
      Fd.syscall_exn fd (fun desc -> Bigstring_unix.really_write desc buf))
  in
  let file_name =
    Filename.concat
      t.pack_directory
      (sprintf !"pack-%{Sha1.Hex.Volatile}.pack" (Sha1.Compute.get_hex sha1))
  in
  let%bind () = Sys.rename t.temp_file_name file_name in
  let%bind () = Pack_reader.index_pack ~pack_file:file_name >>| ok_exn in
  return file_name
;;

let finalise_exn t =
  match%bind Monitor.try_with_or_error ~extract_exn:true (fun () -> finalise_exn t) with
  | Ok result -> return result
  | Error error ->
    let%map abort_result = abort t in
    (match abort_result with
     | Ok () -> Error.raise error
     | Error abort_error -> Error.raise (Error.of_list [ error; abort_error ]))
;;

let%expect_test "write simple pack" =
  Expect_test_helpers.with_temp_dir (fun pack_directory ->
    Expect_test_time_zone.with_fixed_time_zone_async (fun () ->
      let%bind t = create ~pack_directory Validate_sha1 >>| ok_exn in
      let temp_file_name = pack_directory ^/ "file" in
      let add_object_exn t contents expected_sha1 =
        let%bind () = Writer.save temp_file_name ~contents in
        add_object_exn
          t
          ~object_file:temp_file_name
          (Sha1.Hex.of_string expected_sha1)
      in
      let%bind () =
        Expect_test_helpers.show_raise_async (fun () ->
          add_object_exn
            t
            "x\156\029\205Q\n\
             \1940\016\004P\191s\138\253\023e\155&)\001\017\209+x\129M\178-\017\219\148v\021\189\189\177\24350\240Fh\128\198\226\174\132\007G\129>a\208\232\027\235\029;\227{\023\201X\221h\167cK\024)\005Lh4+\249\206\012\177\140c\022%\213x\011\175[\027x\129k\025\018M\135\219\146W\2014\193\157\132\150\146_p\n\
             \219r\225\015\141\243\147\143\0218\215{\211\181^[\221\193\030k\148\250[P-\245\003@\1421G"
            "0000000000000000000000000000000000000000")
      in
      let%bind () =
        [%expect
          {|
        (raised (
          Unexpected_sha1
          (actual_sha1 ac5f368017e73cac599c7dfd77bd36da2b816eaf)
          (expected_sha1 0000000000000000000000000000000000000000))) |}]
      in
      let%bind t = create ~pack_directory Validate_sha1 >>| ok_exn in
      let%bind () =
        add_object_exn
          t
          "x\156\029\205Q\n\
           \1940\016\004P\191s\138\253\023e\155&)\001\017\209+x\129M\178-\017\219\148v\021\189\189\177\24350\240Fh\128\198\226\174\132\007G\129>a\208\232\027\235\029;\227{\023\201X\221h\167cK\024)\005Lh4+\249\206\012\177\140c\022%\213x\011\175[\027x\129k\025\018M\135\219\146W\2014\193\157\132\150\146_p\n\
           \219r\225\015\141\243\147\143\0218\215{\211\181^[\221\193\030k\148\250[P-\245\003@\1421G"
          "ac5f368017e73cac599c7dfd77bd36da2b816eaf"
      in
      let%bind () =
        add_object_exn
          t
          "x\156\165\141Q\n\
           \002!\016@\251\246\020\243\031\133\186j,DD]\161\011\204\232XB\174\225\206B\199/\182#\244~\031\188\023[\173E\192j\187\145\206\0124\140\t9b\030):\237\0193\030l\206.EJ\158\200\132`\006&\147\021.\242h\029.\237\158p\218]{\153\165\224\0047\020\236\173,p\164\213\156\249\141\245\245\228}l\245\004\198\187\016Fc\181\131\173\254\162\226\186\023\254;\164\132g\129_N}\000-iD\192"
          "d2ef8c710416f38bdf6e8487630486830edc6c7f"
      in
      let%bind () =
        add_object_exn
          t
          "x\156K\202\201OR04dH\203,*.QH\203\204I\229\002\000=7\006\020"
          "303ff981c488b812b6215f7db7920dedb3b59d9a"
      in
      let%bind () =
        add_object_exn
          t
          "x\156K\202\201OR04b(NM\206\207KQH\203\204I\229\002\000C\209\006i"
          "1c59427adc4b205a270d8f810310394962e79a8b"
      in
      let%bind () =
        add_object_exn
          t
          "x\156+)JMU044e040031QHd0\176\255\217x\164c\135\2086\197\248\218\237\147x\223n\222:w\022T2\137A&\210\169\234\142\183B\148:o\127#\179\128\165g\210\243Y\221&\006@\160\144\204pa\147{nT\254\241=o\253\243~8n\127\206\164\191\150e\178\161\017X2\133!c\239\235\213\203\191H\253_\184j\207\211\224\2273$f86\174\004\000\224\02714"
          "b39daecaf9bc405deea72ff4dcbd5bb16613eb1f"
      in
      let%bind pack_file = finalise_exn t in
      printf "%s\n" (Filename.basename pack_file);
      let%bind () =
        [%expect {| pack-b883b23ef64c73bd5909caf3596129cb0816f07d.pack |}]
      in
      let%bind () = Pack_reader.For_testing.print_out_pack_file pack_file in
      [%expect
        {|
        items in pack: 5
        idx |                                     sha1 | pack file offset | object length | object type
          0 | 1c59427adc4b205a270d8f810310394962e79a8b |              300 |            12 | Blob
          1 | 303ff981c488b812b6215f7db7920dedb3b59d9a |              280 |            11 | Blob
          2 | ac5f368017e73cac599c7dfd77bd36da2b816eaf |               12 |           150 | Tag
          3 | b39daecaf9bc405deea72ff4dcbd5bb16613eb1f |              321 |           115 | Tree
          4 | d2ef8c710416f38bdf6e8487630486830edc6c7f |              150 |           202 | Commit

        1c59427adc4b205a270d8f810310394962e79a8b
        Header data: Blob size 12
        Blob size: 12
        Blob chunk: "second file\n"

        303ff981c488b812b6215f7db7920dedb3b59d9a
        Header data: Blob size 11
        Blob size: 11
        Blob chunk: "first file\n"

        ac5f368017e73cac599c7dfd77bd36da2b816eaf
        Header data: Tag size 150
        ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
         (tag vtest)
         (tagger
          (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
            (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC))))
         (description "test tag\n"))

        b39daecaf9bc405deea72ff4dcbd5bb16613eb1f
        Header data: Tree size 115
        Tree line: Non_executable_file 303ff981c488b812b6215f7db7920dedb3b59d9a a
        Tree line: Non_executable_file 1c59427adc4b205a270d8f810310394962e79a8b b
        Tree line: Directory d0b2476d5a6fc7bced4f6ef841b7e7022fad0493 c
        Tree line: Link 68bdebaba7f41affa1aabce553c79818984181a9 d

        d2ef8c710416f38bdf6e8487630486830edc6c7f
        Header data: Commit size 202
        ((tree b39daecaf9bc405deea72ff4dcbd5bb16613eb1f) (parents ())
         (author
          ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
           (timestamp (2019-01-05 07:26:44.000000000-05:00)) (zone UTC)))
         (committer
          ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
           (timestamp (2019-01-05 07:26:44.000000000-05:00)) (zone UTC)))
         (encoding ()) (merge_tags ()) (gpg_signature ())
         (description "test commit\n")) |}]))
;;
