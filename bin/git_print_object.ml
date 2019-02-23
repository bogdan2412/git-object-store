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
open Git_object_store_lib

let read_git_object file =
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

let read_git_object_command =
  Command.async_or_error
    ~summary:"print git object"
    [%map_open.Command.Let_syntax
      let file = anon ("FILE" %: Filename.arg_type) in
      fun () -> read_git_object file]
;;

let read_git_pack_file_command =
  Command.async_or_error
    ~summary:"print git pack file"
    [%map_open.Command.Let_syntax
      let file = anon ("FILE" %: Filename.arg_type) in
      fun () -> read_git_pack_file file]
;;

let index_git_pack_file_command =
  Command.async_or_error
    ~summary:"generate index for git pack file"
    [%map_open.Command.Let_syntax
      let file = anon ("FILE" %: Filename.arg_type) in
      fun () -> index_git_pack_file file]
;;

let command =
  Command.group
    ~summary:"git object store"
    [ "read-object", read_git_object_command
    ; "read-pack-file", read_git_pack_file_command
    ; "index-pack-file", index_git_pack_file_command
    ]
;;

let () = Command.run command
