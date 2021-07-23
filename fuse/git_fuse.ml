(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020-2021  Bogdan-Cristian Tataroiu

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

type t =
  { commit_timestamp : Time_ns.t
  ; uid : int
  ; gid : int
  ; object_store : unit Object_store.t
  ; tree_cache : Tree_cache.t
  ; tree_timestamps : Tree_timestamps.t option
  ; tree_handles : Tree_cache.Node.t Handle_manager.t
  ; blob_handles : Reader.t Sequencer.t option Handle_manager.t
  }

let global_t = Set_once.create ()
let object_directory ~git_directory = git_directory ^/ "objects"
let branch_ref ~git_directory ~branch = git_directory ^/ "refs/heads" ^/ branch
let log_file_basename ~git_directory = git_directory ^/ "git-fuse"

let init ~git_directory ~branch ~stat_accurate_timestamps ~max_concurrent_reads =
  let t =
    Thread_safe.block_on_async (fun () ->
      Log.Global.set_output
        [ Log.Output.rotating_file
            `Sexp
            ~basename:(log_file_basename ~git_directory)
            (Log.Rotation.create ~keep:`All ~naming_scheme:`Timestamped ())
        ];
      let%bind object_store =
        Object_store.create
          ~object_directory:(object_directory ~git_directory)
          ~max_concurrent_reads
          Do_not_validate_sha1
      in
      let object_store = ok_exn object_store in
      let%bind commit_sha1 =
        Reader.file_contents (branch_ref ~git_directory ~branch)
        >>| String.strip
        >>| Sha1.Hex.of_string
      in
      let commit = Set_once.create () in
      let%bind () =
        Object_store.read_commit
          object_store
          commit_sha1
          ~on_commit:(Set_once.set_exn commit [%here])
      in
      let commit = Set_once.get_exn commit [%here] in
      let commit_timestamp = commit.author.timestamp in
      let uid = Unix.getuid () in
      let gid = Unix.getgid () in
      let tree_cache =
        Tree_cache.create
          (T object_store)
          ~root:(Tree_cache.Node.of_disk_hash commit.tree)
      in
      let%bind tree_timestamps =
        match stat_accurate_timestamps with
        | true ->
          Tree_timestamps.create
            (T object_store)
            tree_cache
            ~current_commit:(Some commit_sha1)
          |> Deferred.map ~f:Option.some
        | false -> return None
      in
      let tree_handles =
        Handle_manager.create
          ~initial_capacity:64
          ~empty_value:(Tree_cache.Node.empty ())
      in
      let blob_handles =
        Handle_manager.create ~initial_capacity:128 ~empty_value:None
      in
      return
        { commit_timestamp
        ; uid
        ; gid
        ; object_store
        ; tree_cache
        ; tree_timestamps
        ; tree_handles
        ; blob_handles
        })
    |> Or_error.of_exn_result
  in
  match t with
  | Ok t -> Set_once.set_exn global_t [%here] t
  | Error error ->
    Log.Global.error_s [%message "Error initializing object store" (error : Error.t)];
    Shutdown.shutdown 1
;;

let split_path path =
  match String.is_prefix ~prefix:"/" path with
  | true -> if String.( = ) path "/" then [] else List.tl_exn (String.split ~on:'/' path)
  | false -> raise_s [%message "Expected absolute path starting with /" (path : string)]
;;

let opendir path _flags =
  let t = Set_once.get_exn global_t [%here] in
  let split_path = split_path path in
  let node =
    Thread_safe.block_on_async_exn (fun () ->
      Tree_cache.get_node t.tree_cache ~path:split_path)
  in
  match node with
  | None -> raise (Unix.Unix_error (ENOENT, "opendir", path))
  | Some node ->
    Some (Handle_manager.lease t.tree_handles node |> Handle_manager.Handle.to_int)
;;

let readdir _path handle =
  let t = Set_once.get_exn global_t [%here] in
  let node =
    Handle_manager.get t.tree_handles (Handle_manager.Handle.unsafe_of_int handle)
  in
  let loaded =
    Thread_safe.block_on_async_exn (fun () ->
      Tree_cache.Node.ensure_loaded t.tree_cache node)
  in
  let files = Tree_cache.Node.files loaded |> Map.keys in
  let directories = Tree_cache.Node.directories loaded |> Map.keys in
  ("." :: ".." :: files) @ directories
;;

let releasedir _path _flags handle =
  let t = Set_once.get_exn global_t [%here] in
  Handle_manager.release t.tree_handles (Handle_manager.Handle.unsafe_of_int handle)
;;

let stats t ~path sha1 kind perm =
  let%bind size = Object_store.size t.object_store sha1 >>| Int64.of_int in
  let%bind time =
    match t.tree_timestamps with
    | Some tree_timestamps ->
      Tree_timestamps.last_change_time_exn tree_timestamps ~path ~current_sha1:sha1
    | None -> return t.commit_timestamp
  in
  let time =
    Time_ns.to_time_float_round_nearest time
    |> Time.to_span_since_epoch
    |> Time.Span.to_sec
  in
  return
    { Core_unix.st_dev = 0
    ; st_ino = 0
    ; st_kind = kind
    ; st_perm = perm
    ; st_nlink = 1
    ; st_uid = t.uid
    ; st_gid = t.gid
    ; st_rdev = 0
    ; st_size = size
    ; st_atime = time
    ; st_mtime = time
    ; st_ctime = time
    }
;;

let getattr path =
  let t = Set_once.get_exn global_t [%here] in
  let split_path = split_path path in
  Thread_safe.block_on_async_exn (fun () ->
    let%bind node = Tree_cache.get_node t.tree_cache ~path:split_path in
    match node with
    | None ->
      let%bind file = Tree_cache.get_file t.tree_cache ~path:split_path in
      (match file with
       | None -> raise (Unix.Unix_error (ENOENT, "stat", path))
       | Some { sha1; kind } ->
         (match kind with
          | Regular_file -> stats t ~path:split_path sha1 S_REG 0o644
          | Executable_file -> stats t ~path:split_path sha1 S_REG 0o755
          | Link -> stats t ~path:split_path sha1 S_LNK 0o755))
    | Some node ->
      let%bind sha1 = Tree_cache.Node.persist t.tree_cache node in
      stats t ~path:split_path sha1 S_DIR 0o755)
;;

let fopen path _flags =
  let t = Set_once.get_exn global_t [%here] in
  let split_path = split_path path in
  Thread_safe.block_on_async_exn (fun () ->
    let%bind file = Tree_cache.get_file t.tree_cache ~path:split_path in
    match file with
    | None -> raise (Unix.Unix_error (ENOENT, "fopen", path))
    | Some { sha1; kind = _ } ->
      let filename =
        Filename_unix.temp_file
          ~in_dir:(Object_store.object_directory t.object_store)
          "reading"
          "in_progress"
      in
      let%bind reader =
        Monitor.protect
          ~rest:`Raise
          ~run:`Now
          (fun () ->
             let%bind () =
               Writer.with_file filename ~f:(fun writer ->
                 Object_store.read_blob
                   t.object_store
                   sha1
                   ~on_size:(fun (_ : int) -> ())
                   ~on_chunk:(fun buf ~pos ~len ->
                     Writer.write_bigstring writer buf ~pos ~len)
                   ~push_back:(fun () -> return `Ok))
             in
             Reader.open_file filename)
          ~finally:(fun () -> Unix.unlink filename)
      in
      return
        (Some
           (Handle_manager.lease t.blob_handles (Some (Sequencer.create reader))
            |> Handle_manager.Handle.to_int)))
;;

let read _path buf file_offset handle =
  let t = Set_once.get_exn global_t [%here] in
  let handle = Handle_manager.Handle.unsafe_of_int handle in
  let reader = Handle_manager.get t.blob_handles handle |> Option.value_exn in
  let buf_length = Bigstring.length buf in
  Thread_safe.block_on_async_exn (fun () ->
    Throttle.enqueue reader (fun reader ->
      let%bind (_ : Int64.t) = Reader.lseek reader file_offset ~mode:`Set in
      match%map Reader.really_read_bigsubstring reader (Bigsubstring.create buf) with
      | `Ok -> buf_length
      | `Eof read -> read))
;;

let release _path _flags handle =
  let t = Set_once.get_exn global_t [%here] in
  let handle = Handle_manager.Handle.unsafe_of_int handle in
  let reader = Handle_manager.get t.blob_handles handle |> Option.value_exn in
  Thread_safe.block_on_async_exn (fun () ->
    Throttle.enqueue reader (fun reader -> Reader.close reader));
  Handle_manager.set t.blob_handles handle None;
  Handle_manager.release t.blob_handles handle
;;

let wrap1 f name path =
  match f path with
  | exception (Unix.Unix_error _ as exn) -> raise exn
  | exception exn ->
    (match Monitor.extract_exn exn with
     | Unix.Unix_error _ as exn -> raise exn
     | _ ->
       Log.Global.error_s
         [%message
           "Exception encountered while running method"
             (name : string)
             (path : string)
             (exn : Exn.t)];
       raise exn)
  | result -> result
;;

let wrap2 f name path arg2 =
  match f path arg2 with
  | exception (Unix.Unix_error _ as exn) -> raise exn
  | exception exn ->
    (match Monitor.extract_exn exn with
     | Unix.Unix_error _ as exn -> raise exn
     | _ ->
       Log.Global.error_s
         [%message
           "Exception encountered while running method"
             (name : string)
             (path : string)
             (exn : Exn.t)];
       raise exn)
  | result -> result
;;

let wrap3 f name path arg2 arg3 =
  match f path arg2 arg3 with
  | exception (Unix.Unix_error _ as exn) -> raise exn
  | exception exn ->
    (match Monitor.extract_exn exn with
     | Unix.Unix_error _ as exn -> raise exn
     | _ ->
       Log.Global.error_s
         [%message
           "Exception encountered while running method"
             (name : string)
             (path : string)
             (exn : Exn.t)];
       raise exn)
  | result -> result
;;

let wrap4 f name path arg2 arg3 arg4 =
  match f path arg2 arg3 arg4 with
  | exception (Unix.Unix_error _ as exn) -> raise exn
  | exception exn ->
    (match Monitor.extract_exn exn with
     | Unix.Unix_error _ as exn -> raise exn
     | _ ->
       Log.Global.error_s
         [%message
           "Exception encountered while running method"
             (name : string)
             (path : string)
             (exn : Exn.t)];
       raise exn)
  | result -> result
;;

let () =
  Command.basic
    ~summary:""
    [%map_open.Command
      let git_directory = anon ("GIT-DIRECTORY" %: Filename_unix.arg_type)
      and branch =
        flag
          "-branch-name"
          (optional_with_default "master" string)
          ~doc:" Look at specified git branch (default master)"
      and stat_accurate_timestamps =
        flag
          "-disable-stat-accurate-timestamps"
          no_arg
          ~doc:
            " Return the current commit timestamp for every object instead of \
             determining more accurate ones per path in [stat]. Can improve performance."
        |> map ~f:(fun bool -> not bool)
      and max_concurrent_reads =
        flag
          "-max-concurrent-reads"
          (optional_with_default 4 int)
          ~doc:" max concurrent reads from git object store"
      and mountpoint = anon ("MOUNTPOINT" %: Filename_unix.arg_type) in
      fun () ->
        let argv = Sys.get_argv () in
        let git_directory = Filename_unix.realpath git_directory in
        (* Do some sanity checks before mounting. *)
        if not (Sys_unix.is_directory_exn git_directory)
        then
          raise_s [%message "GIT-DIRECTORY is not a directory" (git_directory : string)];
        let object_directory = object_directory ~git_directory in
        if not (Sys_unix.is_directory_exn object_directory)
        then
          raise_s
            [%message
              "GIT-DIRECTORY is missing an objects directory" (object_directory : string)];
        let branch_ref = branch_ref ~git_directory ~branch in
        if not (Sys_unix.is_file_exn branch_ref)
        then
          raise_s
            [%message
              "GIT-DIRECTORY is missing a ref file for the branch" (branch_ref : string)];
        (* Go ahead and mount. *)
        Fuse.main
          [| argv.(0); mountpoint |]
          { Fuse.default_operations with
            init =
              (fun () ->
                 init
                   ~git_directory
                   ~branch
                   ~stat_accurate_timestamps
                   ~max_concurrent_reads)
          ; fopen = wrap2 fopen "fopen"
          ; read = wrap4 read "read"
          ; release = wrap3 release "release"
          ; getattr = wrap1 getattr "getattr"
          ; opendir = wrap2 opendir "opendir"
          ; readdir = wrap2 readdir "readdir"
          ; releasedir = wrap3 releasedir "releasedir"
          }]
  |> Command_unix.run
;;
