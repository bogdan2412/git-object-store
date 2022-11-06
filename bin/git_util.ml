(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2022  Bogdan-Cristian Tataroiu

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
module Sha1 = Git.Sha1

let read_object_file file =
  let sha1 =
    let part2, part3 = Filename.split (Filename_unix.realpath file) in
    let _part1, part2 = Filename.split part2 in
    Option.try_with (fun () -> Sha1.Hex.of_string (part2 ^ part3))
  in
  let create_git_object_reader sha1_validation =
    Git.Object_reader.create
      ~on_blob_size:(fun (_ : int) -> ())
      ~on_blob_chunk:(fun buf ~pos ~len ->
        Writer.write_bigstring (force Writer.stdout) ~pos ~len buf)
      ~on_commit:(fun commit -> printf !"%{sexp: Git.Commit.t}\n" commit)
      ~on_tree_line:(fun mode sha1 ~name ->
        printf
          !"%19s    %{Sha1.Hex}    %s\n"
          (Sexp.to_string_hum ([%sexp_of: Git.File_mode.t] mode))
          (Sha1.Raw.Volatile.to_hex sha1)
          name)
      ~on_tag:(fun tag -> printf !"%{sexp: Git.Tag.t}\n" tag)
      ~on_error:(fun error -> Error.raise error)
      sha1_validation
  in
  Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
    match sha1 with
    | None ->
      let git_object_reader = create_git_object_reader Do_not_validate_sha1 in
      Git.Object_reader.read_file git_object_reader ~file ()
    | Some sha1 ->
      let git_object_reader = create_git_object_reader Validate_sha1 in
      Git.Object_reader.read_file git_object_reader ~file sha1)
;;

let read_multi_pack_index ~object_directory =
  let open Deferred.Or_error.Let_syntax in
  let module Reader = Git.Object_store.Low_level.Multi_pack_index_reader in
  let pack_directory = object_directory ^/ "pack" in
  let%map t = Reader.open_existing ~pack_directory in
  let object_count = Reader.object_count t in
  let pack_file_names = Reader.pack_file_names t in
  printf "items in index: %d\n" object_count;
  print_endline "packs in index:";
  Array.iter pack_file_names ~f:print_endline;
  print_endline "";
  printf "    idx | %40s | %50s | pack offset\n" "sha1" "pack file";
  for index = 0 to object_count - 1 do
    let sha1 = Reader.sha1 t ~index in
    printf
      !"%7d | %{Sha1.Hex} | %49s | %11d\n"
      (Git_pack_files.Find_result.Volatile.index_exn (Reader.find_sha1_index' t sha1))
      (Sha1.Raw.Volatile.to_hex sha1)
      pack_file_names.(Reader.pack_id t ~index)
      (Reader.pack_offset t ~index)
  done
;;

let read_pack_file ~print_contents pack_file =
  let open Deferred.Or_error.Let_syntax in
  let%map t = Git.Pack_reader.create ~pack_file Validate_sha1 in
  let items_in_pack = Git.Pack_reader.items_in_pack t in
  Core.printf "items in pack: %d\n" items_in_pack;
  Core.printf
    "    idx | %40s | %10s | %10s | %10s\n"
    "sha1"
    "pack_size"
    "delta_size"
    "size";
  for index = 0 to items_in_pack - 1 do
    let { Git.Pack_reader.Size.Volatile.size; delta_size; pack_size } =
      Git.Pack_reader.size t ~index
    in
    Core.printf
      !"%7d | %{Sha1.Hex} | %10d | %10d | %10d\n"
      index
      (Sha1.Raw.Volatile.to_hex (Git.Pack_reader.sha1 t ~index))
      pack_size
      delta_size
      size
  done;
  if print_contents
  then
    for index = 0 to items_in_pack - 1 do
      Core.printf
        !"\n%{Sha1.Hex}\n"
        (Sha1.Raw.Volatile.to_hex (Git.Pack_reader.sha1 t ~index));
      try
        Git.Pack_reader.read_object
          t
          ~index
          ~on_blob_size:(fun (_ : int) -> ())
          ~on_blob_chunk:(fun buf ~pos ~len ->
            Core.printf "Blob chunk:\n%s\n" (Bigstring.To_string.sub buf ~pos ~len))
          ~on_commit:(Core.printf !"%{sexp: Git.Commit.t}\n")
          ~on_tree_line:(fun file_mode sha1 ~name ->
            Core.printf
              !"Tree line: %{sexp: Git.File_mode.t} %{Sha1.Hex} %s\n"
              file_mode
              (Sha1.Raw.Volatile.to_hex sha1)
              name)
          ~on_tag:(Core.printf !"%{sexp: Git.Tag.t}\n")
      with
      | exn ->
        Core.eprintf
          !"Error parsing %{Sha1.Hex}:\n%{Exn}\n"
          (Sha1.Raw.Volatile.to_hex (Git.Pack_reader.sha1 t ~index))
          exn
    done
;;

let write_pack_index pack_file = Git.Pack_reader.write_pack_index ~pack_file

let write_pack_reverse_index pack_file =
  Git.Pack_reader.write_pack_reverse_index ~pack_file
;;

let write_multi_pack_index ~object_directory ~preferred_pack =
  Git.Object_store.write_multi_pack_index ~object_directory ~preferred_pack
;;

let write_multi_pack_reverse_index ~object_directory ~preferred_pack =
  Git.Object_store.write_multi_pack_reverse_index ~object_directory ~preferred_pack
;;

let read_sha1_from_store ~object_directory sha1 =
  let open Deferred.Or_error.Let_syntax in
  let%bind git_object_store =
    Git.Object_store.create ~object_directory ~max_concurrent_reads:1 Validate_sha1
  in
  Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
    let open Deferred.Let_syntax in
    Git.Object_store.read_object
      git_object_store
      sha1
      ~on_blob_size:(fun (_ : int) -> ())
      ~on_blob_chunk:(fun buf ~pos ~len ->
        Writer.write_bigstring (force Writer.stdout) ~pos ~len buf)
      ~on_commit:(fun commit -> printf !"%{sexp: Git.Commit.t}\n" commit)
      ~on_tree_line:(fun mode sha1 ~name ->
        printf
          !"%19s    %s    %s\n"
          (Sexp.to_string_hum ([%sexp_of: Git.File_mode.t] mode))
          (Sexp.to_string_hum ([%sexp_of: Sha1.Hex.t] (Sha1.Raw.Volatile.to_hex sha1)))
          (Sexp.to_string_hum ([%sexp_of: string] name)))
      ~on_tag:(fun tag -> printf !"%{sexp: Git.Tag.t}\n" tag)
      ~push_back:(fun () ->
        let%bind () = Writer.flushed (force Writer.stdout) in
        return `Ok))
;;

let write_commit_from_file ~object_directory file ~dry_run =
  Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
    let%bind commit = Reader.load_sexp_exn file [%of_sexp: Git.Commit.t] in
    let%map sha1 = Git.Object_writer.Commit.write' ~object_directory commit ~dry_run in
    printf !"%{Sha1.Hex}\n" (Sha1.Raw.to_hex sha1))
;;

let write_tree_from_file ~object_directory file ~dry_run =
  Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
    let tree_writer = Git.Object_writer.Tree.create_uninitialised ~object_directory in
    let%bind () = Git.Object_writer.Tree.init_or_reset tree_writer ~dry_run in
    let%bind raw_tree_lines = Reader.file_lines file in
    List.iter raw_tree_lines ~f:(fun raw_line ->
      let line = Sexp.scan_sexps (Lexing.from_string raw_line) in
      let fail ~exn =
        raise_s
          [%message
            "Cannot parse line - expected [mode], [sha1] and [name] separated by spaces"
              (raw_line : string)
              (exn : Exn.t option)]
      in
      match line with
      | [ mode; sha1; name ] ->
        let mode =
          match [%of_sexp: Git.File_mode.t] mode with
          | exception exn -> fail ~exn:(Some exn)
          | mode -> mode
        in
        let sha1 =
          match [%of_sexp: Sha1.Hex.t] sha1 with
          | exception exn -> fail ~exn:(Some exn)
          | sha1 -> sha1
        in
        let name = [%of_sexp: string] name in
        Git.Object_writer.Tree.write_tree_line
          tree_writer
          mode
          (Sha1.Raw.of_hex sha1)
          ~name
      | _ -> fail ~exn:None);
    let%map sha1 = Git.Object_writer.Tree.finalise tree_writer in
    printf !"%{Sha1.Hex}\n" (Sha1.Raw.to_hex sha1))
;;

let write_blob_from_file' ~object_directory file ~dry_run =
  Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
    let%bind stat = Unix.stat file in
    let length = Int64.to_int_exn stat.size in
    let blob_writer =
      Git.Object_writer.Blob.Known_size.create_uninitialised ~object_directory
    in
    let%bind () =
      Reader.with_file file ~f:(fun reader ->
        let%bind () =
          Git.Object_writer.Blob.Known_size.init_or_reset blob_writer ~length ~dry_run
        in
        match%bind
          Reader.read_one_chunk_at_a_time reader ~handle_chunk:(fun buf ~pos ~len ->
            Git.Object_writer.Blob.Known_size.append_data blob_writer buf ~pos ~len;
            return `Continue)
        with
        | `Eof -> Deferred.unit
        | (`Eof_with_unconsumed_data _ | `Stopped _) as result ->
          raise_s
            [%message
              "bug - unexpected read_one_chunk_at_a_time result"
                (result : _ Reader.read_one_chunk_at_a_time_result)])
    in
    Git.Object_writer.Blob.Known_size.finalise_exn blob_writer)
;;

let write_blob_from_file ~object_directory file ~dry_run =
  let%map.Deferred.Or_error sha1 =
    write_blob_from_file' ~object_directory file ~dry_run
  in
  printf !"%{Sha1.Hex}\n" (Sha1.Raw.to_hex sha1)
;;

let rec write_tree_from_directory' ~object_directory ~source_directory ~dry_run =
  Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
    let tree_writer = Git.Object_writer.Tree.create_uninitialised ~object_directory in
    let%bind () = Git.Object_writer.Tree.init_or_reset tree_writer ~dry_run in
    let%bind entries = Sys.readdir source_directory in
    Array.sort entries ~compare:[%compare: string];
    let%bind () =
      Deferred.Array.iter ~how:`Sequential entries ~f:(fun entry ->
        let path = source_directory ^/ entry in
        let%bind stat = Unix.lstat path in
        match stat.kind with
        | `Socket | `Block | `Fifo | `Char ->
          raise_s
            [%message
              "Unable to handle path kind"
                (path : string)
                ~kind:(stat.kind : Unix.File_kind.t)]
        | `File ->
          let%bind sha1 =
            write_blob_from_file' ~object_directory path ~dry_run >>| ok_exn
          in
          Git.Object_writer.Tree.write_tree_line
            tree_writer
            (if stat.perm land 0o111 = 0 then Non_executable_file else Executable_file)
            sha1
            ~name:entry;
          Deferred.unit
        | `Link ->
          let%bind link_path = Unix.readlink path in
          let blob_writer =
            Git.Object_writer.Blob.Known_size.create_uninitialised ~object_directory
          in
          let%bind () =
            Git.Object_writer.Blob.Known_size.init_or_reset
              blob_writer
              ~length:(String.length link_path)
              ~dry_run
          in
          Git.Object_writer.Blob.Known_size.append_data
            blob_writer
            (Bigstring.of_string link_path)
            ~pos:0
            ~len:(String.length link_path);
          let%bind sha1 = Git.Object_writer.Blob.Known_size.finalise_exn blob_writer in
          Git.Object_writer.Tree.write_tree_line tree_writer Link sha1 ~name:entry;
          Deferred.unit
        | `Directory ->
          let%bind sha1 =
            write_tree_from_directory' ~object_directory ~source_directory:path ~dry_run
            >>| ok_exn
          in
          Git.Object_writer.Tree.write_tree_line tree_writer Directory sha1 ~name:entry;
          Deferred.unit)
    in
    Git.Object_writer.Tree.finalise tree_writer)
;;

let write_tree_from_directory ~object_directory ~source_directory ~dry_run =
  let%map.Deferred.Or_error sha1 =
    write_tree_from_directory' ~object_directory ~source_directory ~dry_run
  in
  printf !"%{Sha1.Hex}\n" (Sha1.Raw.to_hex sha1)
;;

let read_object_file_command =
  Command.async_or_error
    ~summary:"print git object file"
    [%map_open.Command
      let file = anon ("FILE" %: Filename_unix.arg_type) in
      fun () -> read_object_file file]
;;

let read_pack_file_command =
  Command.async_or_error
    ~summary:"print git pack file"
    [%map_open.Command
      let file = anon ("FILE" %: Filename_unix.arg_type)
      and print_contents =
        flag
          "-print-contents"
          no_arg
          ~doc:" print pack object contents in addition to the index"
      in
      fun () -> read_pack_file ~print_contents file]
;;

let read_multi_pack_index_command =
  Command.async_or_error
    ~summary:"print git multi pack index file"
    [%map_open.Command
      let object_directory = Git.Util.object_directory_param in
      fun () -> read_multi_pack_index ~object_directory]
;;

let write_pack_index_command =
  Command.async_or_error
    ~summary:"generate index for git pack file"
    [%map_open.Command
      let file = anon ("FILE" %: Filename_unix.arg_type) in
      fun () -> write_pack_index file]
;;

let write_pack_reverse_index_command =
  Command.async_or_error
    ~summary:"generate reverse index for git pack file"
    [%map_open.Command
      let file = anon ("FILE" %: Filename_unix.arg_type) in
      fun () -> write_pack_reverse_index file]
;;

let write_multi_pack_index_command =
  Command.async_or_error
    ~summary:"generate multi-pack-index file for multiple git pack files"
    [%map_open.Command
      let object_directory = Git.Util.object_directory_param
      and preferred_pack =
        flag
          "-preferred-pack"
          (optional Filename_unix.arg_type)
          ~doc:
            "FILE specify the tie-breaking pack used when multiple packs contain the \
             same object."
      in
      fun () -> write_multi_pack_index ~object_directory ~preferred_pack]
;;

let write_multi_pack_reverse_index_command =
  Command.async_or_error
    ~summary:"generate reverse index for an existing multi-pack-index file pseudo-pack"
    [%map_open.Command
      let object_directory = Git.Util.object_directory_param
      and preferred_pack =
        flag
          "-preferred-pack"
          (optional Filename_unix.arg_type)
          ~doc:
            "FILE specify the tie-breaking pack used when multiple packs contain the \
             same object."
      in
      fun () -> write_multi_pack_reverse_index ~object_directory ~preferred_pack]
;;

let read_sha1_from_store_command =
  Command.async_or_error
    ~summary:
      "print git object identified by sha1 from either a raw object file or a pack file"
    [%map_open.Command
      let object_directory = Git.Util.object_directory_param
      and sha1 = anon ("SHA1" %: sexp_conv [%of_sexp: Sha1.Hex.t]) in
      fun () -> read_sha1_from_store ~object_directory sha1]
;;

let dry_run_flag =
  Command.Param.(
    flag "-dry-run" no_arg ~doc:" Do not persist any changes to the object directory")
;;

let write_commit_from_file_command =
  Command.async_or_error
    ~summary:
      "create a commit given a sexp representation in the same format as output by the \
       read commands"
    [%map_open.Command
      let object_directory = Git.Util.object_directory_param
      and file = anon ("SEXP-FILE" %: Filename_unix.arg_type)
      and dry_run = dry_run_flag in
      fun () -> write_commit_from_file ~object_directory file ~dry_run]
;;

let write_tree_from_file_command =
  Command.async_or_error
    ~summary:
      "create a tree given a line-based representation in the same format as output by \
       the read commands"
    [%map_open.Command
      let object_directory = Git.Util.object_directory_param
      and file = anon ("FILE" %: Filename_unix.arg_type)
      and dry_run = dry_run_flag in
      fun () -> write_tree_from_file ~object_directory file ~dry_run]
;;

let write_blob_from_file_command =
  Command.async_or_error
    ~summary:"create a blob given a file"
    [%map_open.Command
      let object_directory = Git.Util.object_directory_param
      and file = anon ("FILE" %: Filename_unix.arg_type)
      and dry_run = dry_run_flag in
      fun () -> write_blob_from_file ~object_directory file ~dry_run]
;;

let write_tree_from_directory_command =
  Command.async_or_error
    ~summary:"create a tree from a directory on disk and its contents"
    [%map_open.Command
      let object_directory = Git.Util.object_directory_param
      and source_directory = anon ("SOURCE-DIRECTORY" %: Filename_unix.arg_type)
      and dry_run = dry_run_flag in
      fun () -> write_tree_from_directory ~object_directory ~source_directory ~dry_run]
;;

let command =
  Command.group
    ~summary:"git object store"
    [ "read-object-file", read_object_file_command
    ; "read-pack-file", read_pack_file_command
    ; "read-multi-pack-index", read_multi_pack_index_command
    ; "write-pack-index", write_pack_index_command
    ; "write-pack-reverse-index", write_pack_reverse_index_command
    ; "write-multi-pack-index", write_multi_pack_index_command
    ; "write-multi-pack-reverse-index", write_multi_pack_reverse_index_command
    ; "read-sha1-from-store", read_sha1_from_store_command
    ; "write-commit-from-file", write_commit_from_file_command
    ; "write-tree-from-file", write_tree_from_file_command
    ; "write-blob-from-file", write_blob_from_file_command
    ; "write-tree-from-directory", write_tree_from_directory_command
    ]
;;

let () = Command_unix.run command
