(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020  Bogdan-Cristian Tataroiu

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

let object_directory_param =
  let fail =
    Deferred.Or_error.error_s [%message "Unable to determine git repository root"]
  in
  [%map_open.Command
    let directory =
      flag
        "-git-objects-directory"
        (optional Filename.arg_type)
        ~doc:
          " Directory containig all git objects (defaults to <repository \
           root>/.git/objects/)"
    in
    match directory with
    | Some directory -> directory
    | None ->
      let result =
        Thread_safe.block_on_async_exn (fun () ->
          let open Deferred.Or_error.Let_syntax in
          let rec loop path =
            let git_dir = path ^/ ".git" in
            match%bind
              Monitor.try_with_or_error ~extract_exn:true (fun () ->
                Sys.is_directory_exn git_dir)
            with
            | true -> return (git_dir ^/ "objects")
            | false ->
              (match%bind
                 Monitor.try_with_or_error ~extract_exn:true (fun () ->
                   Sys.is_file_exn git_dir)
               with
               | true ->
                 let%bind file_contents =
                   Monitor.try_with_or_error ~extract_exn:true (fun () ->
                     Reader.file_contents git_dir)
                 in
                 (match String.chop_prefix ~prefix:"gitdir: " file_contents with
                  | None -> fail
                  | Some git_dir ->
                    let git_dir = String.rstrip ~drop:(Char.( = ) '\n') git_dir in
                    let git_dir =
                      match String.is_prefix ~prefix:"/" git_dir with
                      | true -> git_dir
                      | false -> Filename.concat path git_dir
                    in
                    if String.exists git_dir ~f:(Char.( = ) '\n')
                    then fail
                    else return (git_dir ^/ "objects"))
               | false ->
                 if String.( = ) path Filename.root
                 then fail
                 else loop (Filename.dirname path))
          in
          let%bind.Deferred cwd = Sys.getcwd () in
          loop cwd)
      in
      (match result with
       | Ok directory -> directory
       | Error error -> Error.raise error)]
;;
