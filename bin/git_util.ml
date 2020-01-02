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
open Git_object_store

let read_git_object_file file =
  Monitor.try_with_or_error ~extract_exn:true (fun () ->
    let git_object_reader =
      Git_object_reader.create
        ~on_blob_size:(fun (_ : int) -> ())
        ~on_blob_chunk:(fun buf ~pos ~len ->
          Writer.write_bigstring (force Writer.stdout) ~pos ~len buf)
        ~on_commit:(fun commit -> printf !"%{sexp: Commit.t}\n" commit)
        ~on_tree_line:(fun mode sha1 ~name ->
          printf
            !"%19s    %{Sha1.Hex}    %s\n"
            (Sexp.to_string_hum ([%sexp_of: File_mode.t] mode))
            (Sha1.Raw.Volatile.to_hex sha1)
            name)
        ~on_tag:(fun tag -> printf !"%{sexp: Tag.t}\n" tag)
        ~on_error:(fun error -> Error.raise error)
    in
    Git_object_reader.read_file git_object_reader ~file)
;;

let read_git_pack_file pack_file =
  let open Deferred.Or_error.Let_syntax in
  let%map t = Git_pack_reader.create ~pack_file in
  let items_in_pack = Git_pack_reader.items_in_pack t in
  Core.printf "items in pack: %d\n" items_in_pack;
  Core.printf "    idx | %40s\n" "sha1";
  for index = 0 to items_in_pack - 1 do
    Core.printf
      !"%7d | %{Sha1.Hex}\n"
      index
      (Sha1.Raw.Volatile.to_hex (Git_pack_reader.sha1 t ~index))
  done;
  for index = 0 to items_in_pack - 1 do
    Core.printf
      !"\n%{Sha1.Hex}\n"
      (Sha1.Raw.Volatile.to_hex (Git_pack_reader.sha1 t ~index));
    try
      Git_pack_reader.read_object
        t
        ~index
        ~on_blob_size:(fun (_ : int) -> ())
        ~on_blob_chunk:(fun buf ~pos ~len ->
          Core.printf "Blob chunk:\n%s\n" (Bigstring.To_string.sub buf ~pos ~len))
        ~on_commit:(Core.printf !"%{sexp: Commit.t}\n")
        ~on_tree_line:(fun file_mode sha1 ~name ->
          Core.printf
            !"Tree line: %{sexp: File_mode.t} %{Sha1.Hex} %s\n"
            file_mode
            (Sha1.Raw.Volatile.to_hex sha1)
            name)
        ~on_tag:(Core.printf !"%{sexp: Tag.t}\n")
    with
    | exn ->
      Core.eprintf
        !"Error parsing %{Sha1.Hex}:\n%{Exn}\n"
        (Sha1.Raw.Volatile.to_hex (Git_pack_reader.sha1 t ~index))
        exn
  done
;;

let index_git_pack_file pack_file = Git_pack_reader.index_pack ~pack_file

let read_sha1_from_store ~object_directory sha1 =
  let open Deferred.Or_error.Let_syntax in
  let%bind git_unified_reader =
    Git_unified_reader.create ~object_directory ~max_concurrent_reads:1
  in
  Monitor.try_with_or_error ~extract_exn:true (fun () ->
    let open Deferred.Let_syntax in
    Git_unified_reader.read_object
      git_unified_reader
      sha1
      ~on_blob_size:(fun (_ : int) -> ())
      ~on_blob_chunk:(fun buf ~pos ~len ->
        Writer.write_bigstring (force Writer.stdout) ~pos ~len buf)
      ~on_commit:(fun commit -> printf !"%{sexp: Commit.t}\n" commit)
      ~on_tree_line:(fun mode sha1 ~name ->
        printf
          !"%19s    %s    %s\n"
          (Sexp.to_string_hum ([%sexp_of: File_mode.t] mode))
          (Sexp.to_string_hum ([%sexp_of: Sha1.Hex.t] (Sha1.Raw.Volatile.to_hex sha1)))
          (Sexp.to_string_hum ([%sexp_of: string] name)))
      ~on_tag:(fun tag -> printf !"%{sexp: Tag.t}\n" tag)
      ~push_back:(fun () ->
        let%bind () = Writer.flushed (force Writer.stdout) in
        return `Ok))
;;

let write_commit_from_file ~object_directory file =
  Monitor.try_with_or_error ~extract_exn:true (fun () ->
    let%bind commit = Reader.load_sexp_exn file [%of_sexp: Commit.t] in
    let%map sha1 = Git_object_writer.Commit.write' ~object_directory commit in
    printf !"%{Sha1.Hex}\n" (Sha1.Raw.to_hex sha1))
;;

let write_tree_from_file ~object_directory file =
  Monitor.try_with_or_error ~extract_exn:true (fun () ->
    let tree_writer = Git_object_writer.Tree.create_uninitialised ~object_directory in
    let%bind () = Git_object_writer.Tree.init_or_reset tree_writer in
    let%bind raw_tree_lines = Reader.file_lines file in
    List.iter raw_tree_lines ~f:(fun raw_line ->
      let line = Sexp.scan_sexps (Lexing.from_string raw_line) in
      let fail ~exn =
        raise_s
          [%message
            "Cannot parse line - expected [mode], [sha1] and [name] separated by \
             spaces"
              (raw_line : string)
              (exn : Exn.t option)]
      in
      match line with
      | [ mode; sha1; name ] ->
        let mode =
          match [%of_sexp: File_mode.t] mode with
          | exception exn -> fail ~exn:(Some exn)
          | mode -> mode
        in
        let sha1 =
          match [%of_sexp: Sha1.Hex.t] sha1 with
          | exception exn -> fail ~exn:(Some exn)
          | sha1 -> sha1
        in
        let name = [%of_sexp: string] name in
        Git_object_writer.Tree.write_tree_line
          tree_writer
          mode
          (Sha1.Raw.of_hex sha1)
          ~name
      | _ -> fail ~exn:None);
    let%map sha1 = Git_object_writer.Tree.finalise tree_writer in
    printf !"%{Sha1.Hex}\n" (Sha1.Raw.to_hex sha1))
;;

let write_blob_from_file ~object_directory file =
  Monitor.try_with_or_error ~extract_exn:true (fun () ->
    let%bind stat = Unix.stat file in
    let length = Int64.to_int_exn stat.size in
    let blob_writer =
      Git_object_writer.Blob.Known_size.create_uninitialised ~object_directory
    in
    let%bind () =
      Reader.with_file file ~f:(fun reader ->
        let%bind () =
          Git_object_writer.Blob.Known_size.init_or_reset blob_writer ~length
        in
        match%bind
          Reader.read_one_chunk_at_a_time reader ~handle_chunk:(fun buf ~pos ~len ->
            Git_object_writer.Blob.Known_size.append_data blob_writer buf ~pos ~len;
            return `Continue)
        with
        | `Eof -> Deferred.unit
        | (`Eof_with_unconsumed_data _ | `Stopped _) as result ->
          raise_s
            [%message
              "bug - unexpected read_one_chunk_at_a_time result"
                (result : _ Reader.read_one_chunk_at_a_time_result)])
    in
    let%map sha1 = Git_object_writer.Blob.Known_size.finalise_exn blob_writer in
    printf !"%{Sha1.Hex}\n" (Sha1.Raw.to_hex sha1))
;;

let read_git_object_file_command =
  Command.async_or_error
    ~summary:"print git object file"
    [%map_open.Command
      let file = anon ("FILE" %: Filename.arg_type) in
      fun () -> read_git_object_file file]
;;

let read_git_pack_file_command =
  Command.async_or_error
    ~summary:"print git pack file"
    [%map_open.Command
      let file = anon ("FILE" %: Filename.arg_type) in
      fun () -> read_git_pack_file file]
;;

let index_git_pack_file_command =
  Command.async_or_error
    ~summary:"generate index for git pack file"
    [%map_open.Command
      let file = anon ("FILE" %: Filename.arg_type) in
      fun () -> index_git_pack_file file]
;;

let read_sha1_from_store_command =
  Command.async_or_error
    ~summary:
      "print git object identified by sha1 from either a raw object file or a pack file"
    [%map_open.Command
      let sha1 = anon ("SHA1" %: sexp_conv [%of_sexp: Sha1.Hex.t])
      and object_directory = anon ("OBJECT-DIRECTORY" %: Filename.arg_type) in
      fun () -> read_sha1_from_store ~object_directory sha1]
;;

let write_commit_from_file_command =
  Command.async_or_error
    ~summary:
      "create a commit given a sexp representation in the same format as output by the \
       read commands"
    [%map_open.Command
      let file = anon ("SEXP-FILE" %: Filename.arg_type)
      and object_directory = anon ("OBJECT-DIRECTORY" %: Filename.arg_type) in
      fun () -> write_commit_from_file ~object_directory file]
;;

let write_tree_from_file_command =
  Command.async_or_error
    ~summary:
      "create a tree given a line-based representation in the same format as output by \
       the read commands"
    [%map_open.Command
      let file = anon ("FILE" %: Filename.arg_type)
      and object_directory = anon ("OBJECT-DIRECTORY" %: Filename.arg_type) in
      fun () -> write_tree_from_file ~object_directory file]
;;

let write_blob_from_file_command =
  Command.async_or_error
    ~summary:"create a blob given a file"
    [%map_open.Command
      let file = anon ("FILE" %: Filename.arg_type)
      and object_directory = anon ("OBJECT-DIRECTORY" %: Filename.arg_type) in
      fun () -> write_blob_from_file ~object_directory file]
;;

let command =
  Command.group
    ~summary:"git object store"
    [ "read-object-file", read_git_object_file_command
    ; "read-pack-file", read_git_pack_file_command
    ; "index-pack-file", index_git_pack_file_command
    ; "read-sha1-from-store", read_sha1_from_store_command
    ; "write-commit-from-file", write_commit_from_file_command
    ; "write-tree-from-file", write_tree_from_file_command
    ; "write-blob-from-file", write_blob_from_file_command
    ]
;;

let () = Command.run command
