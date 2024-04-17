(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021-2024  Bogdan-Cristian Tataroiu

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

let open_all_packs ~pack_directory =
  let open Deferred.Or_error.Let_syntax in
  let%bind packs =
    Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
      let open Deferred.Let_syntax in
      let%bind () = Unix.mkdir ~p:() pack_directory in
      Sys.readdir pack_directory)
  in
  Array.to_list packs
  |> List.filter ~f:(fun pack_file -> String.is_suffix ~suffix:".pack" pack_file)
  |> Deferred.Or_error.List.map ~how:`Sequential ~f:(fun pack_file ->
    let pack_file = pack_directory ^/ pack_file in
    let%bind stat =
      Monitor.try_with_or_error ~rest:`Raise ~extract_exn:true (fun () ->
        Unix.lstat pack_file)
    in
    let%map pack_reader = Pack_reader.create ~pack_file Do_not_validate_sha1 in
    ( Pack_reader.Low_level.index pack_reader
    , Time_ns.of_time_float_round_nearest stat.mtime ))
  >>| Array.of_list
;;

module Cursor : sig
  type t

  val create : pack_id:int -> pack_priority:int -> Pack_index_reader.t -> t
  val pack_id : t -> int
  val object_offset : t -> int
  val sha1_raw : t -> Sha1.Raw.t
  val advance : t -> unit
  val is_done : t -> bool
  val compare : ignore_priority:bool -> t -> t -> int
end = struct
  type t =
    { pack_id : int
    ; pack_priority : int
    ; pack_index : Pack_index_reader.t
    ; items_in_pack : int
    ; mutable object_index : int
    ; sha1_raw : Sha1.Raw.Volatile.t
    }

  let pack_id t = t.pack_id

  let object_offset t =
    Pack_index_reader.pack_file_offset t.pack_index ~index:t.object_index
  ;;

  let sha1_raw t = Sha1.Raw.Volatile.non_volatile t.sha1_raw
  let is_done t = t.object_index >= t.items_in_pack

  let advance t =
    t.object_index <- t.object_index + 1;
    match is_done t with
    | false ->
      let new_sha1_raw = Pack_index_reader.sha1 t.pack_index ~index:t.object_index in
      Bytes.blit
        ~src:(Sha1.Raw.Volatile.bytes new_sha1_raw)
        ~src_pos:0
        ~dst:(Sha1.Raw.Volatile.bytes t.sha1_raw)
        ~dst_pos:0
        ~len:Sha1.Raw.length
    | true -> ()
  ;;

  let create ~pack_id ~pack_priority pack_index =
    let t =
      { pack_id
      ; pack_priority
      ; pack_index
      ; items_in_pack = Pack_index_reader.items_in_pack pack_index
      ; object_index = -1
      ; sha1_raw = Sha1.Raw.Volatile.create ()
      }
    in
    advance t;
    t
  ;;

  let compare ~ignore_priority t1 t2 =
    match is_done t1, is_done t2 with
    | true, true -> 0
    | true, false -> 1
    | false, true -> -1
    | false, false ->
      let result =
        Bytes.compare
          (Sha1.Raw.Volatile.bytes t1.sha1_raw)
          (Sha1.Raw.Volatile.bytes t2.sha1_raw)
      in
      (match result with
       | 0 ->
         (match ignore_priority with
          | true -> 0
          | false -> Int.compare t1.pack_priority t2.pack_priority)
       | _ -> result)
  ;;
end

module Object : sig
  type t

  val of_cursor : Cursor.t -> t
  val sha1_raw : t -> Sha1.Raw.t
  val pack_id : t -> int
  val object_offset : t -> int
end = struct
  type t =
    { sha1_raw : Sha1.Raw.t
    ; pack_id : int
    ; object_offset : int
    }
  [@@deriving fields]

  let of_cursor cursor =
    { sha1_raw = Cursor.sha1_raw cursor
    ; pack_id = Cursor.pack_id cursor
    ; object_offset = Cursor.object_offset cursor
    }
  ;;
end

let minimum_cursor cursors =
  let result = ref cursors.(0) in
  for index = 1 to Array.length cursors - 1 do
    match Cursor.compare ~ignore_priority:false cursors.(index) !result < 0 with
    | true -> result := cursors.(index)
    | false -> ()
  done;
  !result
;;

let merge_pack_cursors cursors =
  let objects = Queue.create () in
  while
    let minimum_cursor = minimum_cursor cursors in
    match Cursor.is_done minimum_cursor with
    | true -> false
    | false ->
      Queue.enqueue objects (Object.of_cursor minimum_cursor);
      for index = 0 to Array.length cursors - 1 do
        match phys_equal minimum_cursor cursors.(index) with
        | true -> ()
        | false ->
          (match Cursor.compare ~ignore_priority:true cursors.(index) minimum_cursor with
           | 0 -> Cursor.advance cursors.(index)
           | _ -> ())
      done;
      Cursor.advance minimum_cursor;
      true
  do
    ()
  done;
  objects
;;

let write_header file_mmap ~chunk_count ~pack_file_count =
  Bigstring.unsafe_set_uint32_be file_mmap ~pos:0 0x4d494458;
  Bigstring.unsafe_set_uint8 file_mmap ~pos:4 1 (* version 1 *);
  Bigstring.unsafe_set_uint8 file_mmap ~pos:5 1 (* SHA-1 hash function *);
  Bigstring.unsafe_set_uint8 file_mmap ~pos:6 chunk_count;
  Bigstring.unsafe_set_uint8 file_mmap ~pos:7 0
  (* number of base multi-pack-index files *);
  Bigstring.unsafe_set_uint32_be file_mmap ~pos:8 pack_file_count;
  12
;;

let add_chunk file_mmap ~pos ~id ~offset =
  Bigstring.unsafe_set_uint32_be file_mmap ~pos id;
  Bigstring.unsafe_set_uint64_be file_mmap ~pos:(pos + 4) offset;
  pos + 12
;;

let write_pack_file_names_chunk file_mmap ~pos ~pack_file_names =
  let pos =
    Array.fold pack_file_names ~init:pos ~f:(fun pos pack_file_name ->
      let len = String.length pack_file_name in
      Bigstring.From_string.blit
        ~src:pack_file_name
        ~src_pos:0
        ~dst:file_mmap
        ~dst_pos:pos
        ~len;
      let pos = pos + len in
      Bigstring.unsafe_set_uint8 file_mmap ~pos 0;
      pos + 1)
  in
  let pos = ref pos in
  while !pos mod 4 <> 0 do
    Bigstring.unsafe_set_uint8 file_mmap ~pos:!pos 0;
    incr pos
  done;
  !pos
;;

let write_fanout_chunk file_mmap ~pos objects =
  let count = Array.init 256 ~f:(fun (_ : int) -> 0) in
  Queue.iter objects ~f:(fun object_ ->
    let bucket =
      Char.to_int (String.get (Sha1.Raw.to_string (Object.sha1_raw object_)) 0)
    in
    count.(bucket) <- count.(bucket) + 1);
  for bucket = 1 to 255 do
    count.(bucket) <- count.(bucket) + count.(bucket - 1)
  done;
  for bucket = 0 to 255 do
    Bigstring.set_uint32_be_exn file_mmap ~pos:(pos + (bucket * 4)) count.(bucket)
  done;
  pos + (256 * 4)
;;

let write_object_id_chunk file_mmap ~pos objects =
  Queue.iteri objects ~f:(fun idx object_ ->
    Bigstring.From_string.blit
      ~src:(Sha1.Raw.to_string (Object.sha1_raw object_))
      ~src_pos:0
      ~dst:file_mmap
      ~dst_pos:(pos + (Sha1.Raw.length * idx))
      ~len:Sha1.Raw.length);
  pos + (Queue.length objects * Sha1.Raw.length)
;;

let max_uint32_offset = (1 lsl 31) - 1

let write_object_offset_chunks file_mmap ~pos objects large_offsets =
  Queue.iteri objects ~f:(fun idx object_ ->
    Bigstring.set_uint32_be_exn file_mmap ~pos:(pos + (8 * idx)) (Object.pack_id object_);
    Bigstring.set_uint32_be_exn
      file_mmap
      ~pos:(pos + (8 * idx) + 4)
      (match large_offsets with
       | `No_large_offsets -> Object.object_offset object_
       | `Large_offsets uint64_offsets ->
         (match Object.object_offset object_ > max_uint32_offset with
          | false -> Object.object_offset object_
          | true -> Hashtbl.find_exn uint64_offsets idx lor (1 lsl 31))));
  let pos = pos + (Queue.length objects * 8) in
  match large_offsets with
  | `No_large_offsets -> pos
  | `Large_offsets uint64_offsets ->
    let uint64_offsets = Hashtbl.to_alist uint64_offsets |> Array.of_list in
    Array.sort uint64_offsets ~compare:(Comparable.lift Int.compare ~f:snd);
    Array.iter uint64_offsets ~f:(fun (object_index, position) ->
      let object_ = Queue.get objects object_index in
      let offset = Object.object_offset object_ in
      Bigstring.set_uint64_be_exn file_mmap ~pos:(pos + (8 * position)) offset);
    pos + (8 * Array.length uint64_offsets)
;;

let write_index_file ~pack_directory ~pack_file_names ~objects large_offsets =
  let index_file = pack_directory ^/ "multi-pack-index" in
  let%bind fd = Unix.openfile ~mode:[ `Rdwr; `Creat; `Trunc ] index_file in
  let chunk_count =
    4
    +
    match large_offsets with
    | `No_large_offsets -> 0
    | `Large_offsets _ -> 1
  in
  let header_size =
    12
    (* header *)
    + ((chunk_count + 1) * 12)
    (* chunk descriptions *)
  in
  let pack_file_name_chunk_offset = header_size in
  let pack_file_name_chunk_size =
    Array.map pack_file_names ~f:(fun file -> String.length file + 1)
    |> Array.fold ~init:0 ~f:( + )
  in
  let pack_file_name_chunk_size =
    match pack_file_name_chunk_size mod 4 with
    | 0 -> pack_file_name_chunk_size
    | padding -> pack_file_name_chunk_size + (4 - padding)
  in
  let fanout_chunk_offset = pack_file_name_chunk_offset + pack_file_name_chunk_size in
  let fanout_chunk_size = 256 * 4 in
  let object_id_chunk_offset = fanout_chunk_offset + fanout_chunk_size in
  let object_id_chunk_size = Queue.length objects * Sha1.Raw.length in
  let object_offset_chunk_offset = object_id_chunk_offset + object_id_chunk_size in
  let object_offset_chunk_size = Queue.length objects * 8 in
  let large_object_offset_chunk_offset =
    object_offset_chunk_offset + object_offset_chunk_size
  in
  let large_object_offset_chunk_size =
    match large_offsets with
    | `No_large_offsets -> 0
    | `Large_offsets large_offsets -> 8 * Hashtbl.length large_offsets
  in
  let index_file_size =
    large_object_offset_chunk_offset + large_object_offset_chunk_size + Sha1.Raw.length
  in
  let%map file_mmap =
    Fd.syscall_in_thread_exn ~name:"git-pack-mmap-file" fd (fun file_descr ->
      Bigstring_unix.map_file ~shared:true file_descr index_file_size)
  in
  let pos =
    write_header file_mmap ~chunk_count ~pack_file_count:(Array.length pack_file_names)
  in
  let pos = add_chunk file_mmap ~pos ~id:0x504e414d ~offset:pack_file_name_chunk_offset in
  let pos = add_chunk file_mmap ~pos ~id:0x4f494446 ~offset:fanout_chunk_offset in
  let pos = add_chunk file_mmap ~pos ~id:0x4f49444c ~offset:object_id_chunk_offset in
  let pos = add_chunk file_mmap ~pos ~id:0x4f4f4646 ~offset:object_offset_chunk_offset in
  let pos =
    match large_offsets with
    | `No_large_offsets -> pos
    | `Large_offsets _ ->
      add_chunk file_mmap ~pos ~id:0x4c4f4646 ~offset:large_object_offset_chunk_offset
  in
  let pos = add_chunk file_mmap ~pos ~id:0 ~offset:(index_file_size - Sha1.Raw.length) in
  assert (pos = pack_file_name_chunk_offset);
  let pos = write_pack_file_names_chunk file_mmap ~pos ~pack_file_names in
  assert (pos = fanout_chunk_offset);
  let pos = write_fanout_chunk file_mmap ~pos objects in
  assert (pos = object_id_chunk_offset);
  let pos = write_object_id_chunk file_mmap ~pos objects in
  assert (pos = object_offset_chunk_offset);
  let pos = write_object_offset_chunks file_mmap ~pos objects large_offsets in
  assert (pos = index_file_size - Sha1.Raw.length);
  let sha1_compute = Sha1.Compute.create_uninitialised () |> Sha1.Compute.init_or_reset in
  Sha1.Compute.process
    sha1_compute
    file_mmap
    ~pos:0
    ~len:(index_file_size - Sha1.Raw.length);
  let sha1_compute = Sha1.Compute.finalise sha1_compute in
  Bigstring.From_bytes.blit
    ~src:(Sha1.Raw.Volatile.bytes (Sha1.Compute.get_raw sha1_compute))
    ~src_pos:0
    ~dst:file_mmap
    ~dst_pos:(index_file_size - Sha1.Raw.length)
    ~len:Sha1.Raw.length
;;

let write_multi_pack_index_file ~pack_directory ~preferred_pack =
  let%bind packs_with_mtimes = open_all_packs ~pack_directory >>| ok_exn in
  let preferred_pack = Util.validate_preferred_pack ~pack_directory ~preferred_pack in
  let preferred_index =
    Option.map preferred_pack ~f:(fun preferred_pack ->
      Filename.basename (String.chop_suffix_exn ~suffix:".pack" preferred_pack ^ ".idx"))
  in
  let packs_in_lexicographic_order = Array.map ~f:fst packs_with_mtimes in
  Array.sort
    packs_in_lexicographic_order
    ~compare:(Comparable.lift String.compare ~f:Pack_index_reader.index_file);
  let pack_priorities =
    let packs_in_priority_order = Array.copy packs_with_mtimes in
    Array.sort packs_in_priority_order ~compare:(Comparable.lift Time_ns.compare ~f:snd);
    Array.mapi packs_in_priority_order ~f:(fun order (pack_index, _) ->
      let index_file = Pack_index_reader.index_file pack_index in
      let order =
        match preferred_index with
        | Some preferred_index
          when String.( = ) (Filename.basename index_file) preferred_index -> 0
        | _ -> Array.length packs_in_priority_order - order
      in
      index_file, order)
    |> Array.to_list
    |> String.Table.of_alist_exn
  in
  (match preferred_index with
   | None -> ()
   | Some preferred_index ->
     (match List.mem ~equal:Int.equal (Hashtbl.data pack_priorities) 0 with
      | true -> ()
      | false ->
        raise_s [%message "Preferred index file not found" (preferred_index : string)]));
  let objects =
    merge_pack_cursors
      (Array.mapi packs_in_lexicographic_order ~f:(fun pack_id pack_index ->
         let pack_priority =
           Hashtbl.find_exn pack_priorities (Pack_index_reader.index_file pack_index)
         in
         Cursor.create ~pack_id ~pack_priority pack_index))
  in
  let uint64_offset_needed = (1 lsl 32) - 1 in
  let uint64_offsets = Int.Table.create () in
  let need_large_offsets = ref false in
  Queue.iteri objects ~f:(fun idx object_ ->
    let offset = Object.object_offset object_ in
    match offset > max_uint32_offset with
    | true ->
      Hashtbl.add_exn uint64_offsets ~key:idx ~data:(Hashtbl.length uint64_offsets);
      (match offset > uint64_offset_needed with
       | true -> need_large_offsets := true
       | false -> ())
    | false -> ());
  let pack_file_names =
    Array.map packs_in_lexicographic_order ~f:(fun pack_index_reader ->
      Pack_index_reader.index_file pack_index_reader |> Filename.basename)
  in
  write_index_file
    ~pack_directory
    ~pack_file_names
    ~objects
    (match !need_large_offsets with
     | false -> `No_large_offsets
     | true -> `Large_offsets uint64_offsets)
;;

module For_testing = struct
  module Pack_writer = Git_pack_files.Writer

  let make_pack_files ~object_directory packs =
    let blob_writer =
      Object_writer.Blob.Known_size.create_uninitialised ~object_directory
    in
    let pack_directory = object_directory ^/ "pack" in
    let%bind () = Unix.mkdir pack_directory in
    Deferred.List.mapi packs ~how:`Sequential ~f:(fun idx pack ->
      let%bind pack_writer =
        Pack_writer.create ~pack_directory Validate_sha1 >>| ok_exn
      in
      let%bind () =
        Deferred.List.iter pack ~how:`Sequential ~f:(fun blob ->
          let%bind () =
            Object_writer.Blob.Known_size.init_or_reset
              blob_writer
              ~length:(String.length blob)
              Object_writer.Mode.write_all
          in
          Object_writer.Blob.Known_size.append_data
            blob_writer
            (Bigstring.of_string blob)
            ~pos:0
            ~len:(String.length blob);
          let%bind sha1 = Object_writer.Blob.Known_size.finalise_exn blob_writer in
          let sha1 = Sha1.Raw.to_hex sha1 in
          let object_file =
            let sha1_string = Sha1.Hex.to_string sha1 in
            object_directory
            ^/ String.sub sha1_string ~pos:0 ~len:2
            ^/ String.sub sha1_string ~pos:2 ~len:(Sha1.Hex.length - 2)
          in
          Pack_writer.add_object_exn pack_writer ~object_file sha1)
      in
      let%bind pack_file = Pack_writer.finalise_exn pack_writer in
      let%bind () =
        let time =
          Time_ns.of_date_ofday
            ~zone:Time_ns_unix.Zone.utc
            (Date.create_exn ~y:1990 ~m:Dec ~d:24)
            (Time_ns.Ofday.create ~min:idx ())
          |> Time_ns.to_span_since_epoch
          |> Time_ns.Span.to_sec
        in
        Unix.utimes pack_file ~access:time ~modif:time
      in
      let%map () = Pack_reader.write_pack_index ~pack_file >>| ok_exn in
      Filename.basename pack_file)
  ;;

  let three_overlapping_packs_example =
    [ [ "just 0"; "both 0 and 1"; "both 0 and 2"; "all three" ]
    ; [ "just 1"; "both 0 and 1"; "all three"; "another just 1"; "both 1 and 2" ]
    ; [ "just 2"
      ; "both 1 and 2"
      ; "all three"
      ; "another just 2"
      ; "both 0 and 2"
      ; "yet another just 2"
      ]
    ]
  ;;
end

let%test_module "Multi_pack_index_writer_tests" =
  (module struct
    module Expect_test_time_zone = Git_core_types.Expect_test_time_zone

    let write_multi_pack_index_file ~pack_directory ~preferred_pack print =
      let index_file = pack_directory ^/ "multi-pack-index" in
      let%bind () =
        match%bind Sys.file_exists_exn index_file with
        | true -> Unix.unlink index_file
        | false -> Deferred.unit
      in
      let%bind () = write_multi_pack_index_file ~pack_directory ~preferred_pack in
      match print with
      | `Parsed ->
        Multi_pack_index_reader.For_testing.print_out_multi_pack_index ~pack_directory
      | `Raw_contents ->
        let%bind index_contents = Reader.file_contents index_file in
        printf "%S" index_contents;
        Deferred.unit
    ;;

    let%expect_test "" =
      Expect_test_helpers_async.with_temp_dir (fun object_directory ->
        Expect_test_time_zone.with_fixed_time_zone_async (fun () ->
          let pack_directory = object_directory ^/ "pack" in
          let%bind packs =
            For_testing.make_pack_files
              ~object_directory
              For_testing.three_overlapping_packs_example
          in
          let packs = Array.of_list packs in
          print_endline "packs by mtime order:";
          Array.iter packs ~f:print_endline;
          print_endline "";
          let%bind () =
            write_multi_pack_index_file ~pack_directory ~preferred_pack:None `Parsed
          in
          let%bind () =
            Deferred.Array.iter packs ~how:`Sequential ~f:(fun preferred_pack ->
              print_endline "";
              write_multi_pack_index_file
                ~pack_directory
                ~preferred_pack:(Some preferred_pack)
                `Parsed)
          in
          [%expect
            {|
              packs by mtime order:
              pack-68d020a4f35b82da5646c7cee22bcf25add57c2d.pack
              pack-dfa729d1b4f4f638dcf554f804d034fe3a60b495.pack
              pack-bb8f3b6cbb5b4bd45f33bded1ffd1c249f573c3a.pack

              items in index: 10
              packs in index:
              pack-68d020a4f35b82da5646c7cee22bcf25add57c2d.pack
              pack-bb8f3b6cbb5b4bd45f33bded1ffd1c249f573c3a.pack
              pack-dfa729d1b4f4f638dcf554f804d034fe3a60b495.pack

              idx |                                     sha1 | pack id | pack offset
                0 | 0c9b49b9d4cfbb050b0afd14ea7db256ef9944e0 |       1 |          27
                1 | 16b0c1c19ea9637257e5d995d1c813e5f0853907 |       2 |          27
                2 | 20e835236611481ce4ccafdac16508891cf61e63 |       1 |          89
                3 | 2b3c9ff3f41ac696913677376057935e3fef1961 |       1 |         110
                4 | 6fe15ebc2c6930ef98e9d38646916ac560452bd7 |       2 |          66
                5 | a303af3d4213373e953487b23c5b6f0043961dd9 |       2 |          12
                6 | be4925d5c96283b239a4491ce3dbb93a0f3ac6d9 |       1 |          12
                7 | c3a1154f81f4f14631ceeb1b1939431c82a68f99 |       1 |          48
                8 | fddc9657fc8d959135ec88f11d4df9cc1aba515f |       1 |          66
                9 | ffaa09f42972cc377033b5ab8dbd055620ec2b6c |       0 |          12

              items in index: 10
              packs in index:
              pack-68d020a4f35b82da5646c7cee22bcf25add57c2d.pack
              pack-bb8f3b6cbb5b4bd45f33bded1ffd1c249f573c3a.pack
              pack-dfa729d1b4f4f638dcf554f804d034fe3a60b495.pack

              idx |                                     sha1 | pack id | pack offset
                0 | 0c9b49b9d4cfbb050b0afd14ea7db256ef9944e0 |       1 |          27
                1 | 16b0c1c19ea9637257e5d995d1c813e5f0853907 |       0 |          27
                2 | 20e835236611481ce4ccafdac16508891cf61e63 |       0 |          48
                3 | 2b3c9ff3f41ac696913677376057935e3fef1961 |       1 |         110
                4 | 6fe15ebc2c6930ef98e9d38646916ac560452bd7 |       2 |          66
                5 | a303af3d4213373e953487b23c5b6f0043961dd9 |       2 |          12
                6 | be4925d5c96283b239a4491ce3dbb93a0f3ac6d9 |       1 |          12
                7 | c3a1154f81f4f14631ceeb1b1939431c82a68f99 |       0 |          69
                8 | fddc9657fc8d959135ec88f11d4df9cc1aba515f |       1 |          66
                9 | ffaa09f42972cc377033b5ab8dbd055620ec2b6c |       0 |          12

              items in index: 10
              packs in index:
              pack-68d020a4f35b82da5646c7cee22bcf25add57c2d.pack
              pack-bb8f3b6cbb5b4bd45f33bded1ffd1c249f573c3a.pack
              pack-dfa729d1b4f4f638dcf554f804d034fe3a60b495.pack

              idx |                                     sha1 | pack id | pack offset
                0 | 0c9b49b9d4cfbb050b0afd14ea7db256ef9944e0 |       2 |          89
                1 | 16b0c1c19ea9637257e5d995d1c813e5f0853907 |       2 |          27
                2 | 20e835236611481ce4ccafdac16508891cf61e63 |       1 |          89
                3 | 2b3c9ff3f41ac696913677376057935e3fef1961 |       1 |         110
                4 | 6fe15ebc2c6930ef98e9d38646916ac560452bd7 |       2 |          66
                5 | a303af3d4213373e953487b23c5b6f0043961dd9 |       2 |          12
                6 | be4925d5c96283b239a4491ce3dbb93a0f3ac6d9 |       1 |          12
                7 | c3a1154f81f4f14631ceeb1b1939431c82a68f99 |       2 |          48
                8 | fddc9657fc8d959135ec88f11d4df9cc1aba515f |       1 |          66
                9 | ffaa09f42972cc377033b5ab8dbd055620ec2b6c |       0 |          12

              items in index: 10
              packs in index:
              pack-68d020a4f35b82da5646c7cee22bcf25add57c2d.pack
              pack-bb8f3b6cbb5b4bd45f33bded1ffd1c249f573c3a.pack
              pack-dfa729d1b4f4f638dcf554f804d034fe3a60b495.pack

              idx |                                     sha1 | pack id | pack offset
                0 | 0c9b49b9d4cfbb050b0afd14ea7db256ef9944e0 |       1 |          27
                1 | 16b0c1c19ea9637257e5d995d1c813e5f0853907 |       2 |          27
                2 | 20e835236611481ce4ccafdac16508891cf61e63 |       1 |          89
                3 | 2b3c9ff3f41ac696913677376057935e3fef1961 |       1 |         110
                4 | 6fe15ebc2c6930ef98e9d38646916ac560452bd7 |       2 |          66
                5 | a303af3d4213373e953487b23c5b6f0043961dd9 |       2 |          12
                6 | be4925d5c96283b239a4491ce3dbb93a0f3ac6d9 |       1 |          12
                7 | c3a1154f81f4f14631ceeb1b1939431c82a68f99 |       1 |          48
                8 | fddc9657fc8d959135ec88f11d4df9cc1aba515f |       1 |          66
                9 | ffaa09f42972cc377033b5ab8dbd055620ec2b6c |       0 |          12 |}];
          let%bind () =
            write_multi_pack_index_file ~pack_directory ~preferred_pack:None `Raw_contents
          in
          [%expect
            {| "MIDX\001\001\004\000\000\000\000\003PNAM\000\000\000\000\000\000\000HOIDF\000\000\000\000\000\000\000\224OIDL\000\000\000\000\000\000\004\224OOFF\000\000\000\000\000\000\005\168\000\000\000\000\000\000\000\000\000\000\005\248pack-68d020a4f35b82da5646c7cee22bcf25add57c2d.idx\000pack-bb8f3b6cbb5b4bd45f33bded1ffd1c249f573c3a.idx\000pack-dfa729d1b4f4f638dcf554f804d034fe3a60b495.idx\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\t\000\000\000\t\000\000\000\n\012\155I\185\212\207\187\005\011\n\253\020\234}\178V\239\153D\224\022\176\193\193\158\169crW\229\217\149\209\200\019\229\240\1339\007 \2325#f\017H\028\228\204\175\218\193e\b\137\028\246\030c+<\159\243\244\026\198\150\1456w7`W\147^?\239\025ao\225^\188,i0\239\152\233\211\134F\145j\197`E+\215\163\003\175=B\0197>\1494\135\178<[o\000C\150\029\217\190I%\213\201b\131\1789\164I\028\227\219\185:\015:\198\217\195\161\021O\129\244\241F1\206\235\027\0259C\028\130\166\143\153\253\220\150W\252\141\149\1455\236\136\241\029M\249\204\026\186Q_\255\170\t\244)r\2047p3\181\171\141\189\005V \236+l\000\000\000\001\000\000\000\027\000\000\000\002\000\000\000\027\000\000\000\001\000\000\000Y\000\000\000\001\000\000\000n\000\000\000\002\000\000\000B\000\000\000\002\000\000\000\012\000\000\000\001\000\000\000\012\000\000\000\001\000\000\0000\000\000\000\001\000\000\000B\000\000\000\000\000\000\000\012\215\209\210\255\017\174\223<\192\017bIin\166\000.,\194x" |}];
          let%bind () =
            write_multi_pack_index_file
              ~pack_directory
              ~preferred_pack:(Some packs.(0))
              `Raw_contents
          in
          [%expect
            {| "MIDX\001\001\004\000\000\000\000\003PNAM\000\000\000\000\000\000\000HOIDF\000\000\000\000\000\000\000\224OIDL\000\000\000\000\000\000\004\224OOFF\000\000\000\000\000\000\005\168\000\000\000\000\000\000\000\000\000\000\005\248pack-68d020a4f35b82da5646c7cee22bcf25add57c2d.idx\000pack-bb8f3b6cbb5b4bd45f33bded1ffd1c249f573c3a.idx\000pack-dfa729d1b4f4f638dcf554f804d034fe3a60b495.idx\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\t\000\000\000\t\000\000\000\n\012\155I\185\212\207\187\005\011\n\253\020\234}\178V\239\153D\224\022\176\193\193\158\169crW\229\217\149\209\200\019\229\240\1339\007 \2325#f\017H\028\228\204\175\218\193e\b\137\028\246\030c+<\159\243\244\026\198\150\1456w7`W\147^?\239\025ao\225^\188,i0\239\152\233\211\134F\145j\197`E+\215\163\003\175=B\0197>\1494\135\178<[o\000C\150\029\217\190I%\213\201b\131\1789\164I\028\227\219\185:\015:\198\217\195\161\021O\129\244\241F1\206\235\027\0259C\028\130\166\143\153\253\220\150W\252\141\149\1455\236\136\241\029M\249\204\026\186Q_\255\170\t\244)r\2047p3\181\171\141\189\005V \236+l\000\000\000\001\000\000\000\027\000\000\000\000\000\000\000\027\000\000\000\000\000\000\0000\000\000\000\001\000\000\000n\000\000\000\002\000\000\000B\000\000\000\002\000\000\000\012\000\000\000\001\000\000\000\012\000\000\000\000\000\000\000E\000\000\000\001\000\000\000B\000\000\000\000\000\000\000\012\148\133\188\020\222\156\130\005_\241\191u0i\215$|\1671<" |}];
          let%bind () =
            write_multi_pack_index_file
              ~pack_directory
              ~preferred_pack:(Some packs.(1))
              `Raw_contents
          in
          [%expect
            {| "MIDX\001\001\004\000\000\000\000\003PNAM\000\000\000\000\000\000\000HOIDF\000\000\000\000\000\000\000\224OIDL\000\000\000\000\000\000\004\224OOFF\000\000\000\000\000\000\005\168\000\000\000\000\000\000\000\000\000\000\005\248pack-68d020a4f35b82da5646c7cee22bcf25add57c2d.idx\000pack-bb8f3b6cbb5b4bd45f33bded1ffd1c249f573c3a.idx\000pack-dfa729d1b4f4f638dcf554f804d034fe3a60b495.idx\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\t\000\000\000\t\000\000\000\n\012\155I\185\212\207\187\005\011\n\253\020\234}\178V\239\153D\224\022\176\193\193\158\169crW\229\217\149\209\200\019\229\240\1339\007 \2325#f\017H\028\228\204\175\218\193e\b\137\028\246\030c+<\159\243\244\026\198\150\1456w7`W\147^?\239\025ao\225^\188,i0\239\152\233\211\134F\145j\197`E+\215\163\003\175=B\0197>\1494\135\178<[o\000C\150\029\217\190I%\213\201b\131\1789\164I\028\227\219\185:\015:\198\217\195\161\021O\129\244\241F1\206\235\027\0259C\028\130\166\143\153\253\220\150W\252\141\149\1455\236\136\241\029M\249\204\026\186Q_\255\170\t\244)r\2047p3\181\171\141\189\005V \236+l\000\000\000\002\000\000\000Y\000\000\000\002\000\000\000\027\000\000\000\001\000\000\000Y\000\000\000\001\000\000\000n\000\000\000\002\000\000\000B\000\000\000\002\000\000\000\012\000\000\000\001\000\000\000\012\000\000\000\002\000\000\0000\000\000\000\001\000\000\000B\000\000\000\000\000\000\000\012\182g\022A\241\167\020d\193\254@\178\222]CK&bk\216" |}];
          let%bind () =
            write_multi_pack_index_file
              ~pack_directory
              ~preferred_pack:(Some packs.(2))
              `Raw_contents
          in
          [%expect
            {| "MIDX\001\001\004\000\000\000\000\003PNAM\000\000\000\000\000\000\000HOIDF\000\000\000\000\000\000\000\224OIDL\000\000\000\000\000\000\004\224OOFF\000\000\000\000\000\000\005\168\000\000\000\000\000\000\000\000\000\000\005\248pack-68d020a4f35b82da5646c7cee22bcf25add57c2d.idx\000pack-bb8f3b6cbb5b4bd45f33bded1ffd1c249f573c3a.idx\000pack-dfa729d1b4f4f638dcf554f804d034fe3a60b495.idx\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\b\000\000\000\t\000\000\000\t\000\000\000\n\012\155I\185\212\207\187\005\011\n\253\020\234}\178V\239\153D\224\022\176\193\193\158\169crW\229\217\149\209\200\019\229\240\1339\007 \2325#f\017H\028\228\204\175\218\193e\b\137\028\246\030c+<\159\243\244\026\198\150\1456w7`W\147^?\239\025ao\225^\188,i0\239\152\233\211\134F\145j\197`E+\215\163\003\175=B\0197>\1494\135\178<[o\000C\150\029\217\190I%\213\201b\131\1789\164I\028\227\219\185:\015:\198\217\195\161\021O\129\244\241F1\206\235\027\0259C\028\130\166\143\153\253\220\150W\252\141\149\1455\236\136\241\029M\249\204\026\186Q_\255\170\t\244)r\2047p3\181\171\141\189\005V \236+l\000\000\000\001\000\000\000\027\000\000\000\002\000\000\000\027\000\000\000\001\000\000\000Y\000\000\000\001\000\000\000n\000\000\000\002\000\000\000B\000\000\000\002\000\000\000\012\000\000\000\001\000\000\000\012\000\000\000\001\000\000\0000\000\000\000\001\000\000\000B\000\000\000\000\000\000\000\012\215\209\210\255\017\174\223<\192\017bIin\166\000.,\194x" |}];
          Deferred.unit))
    ;;
  end)
;;
