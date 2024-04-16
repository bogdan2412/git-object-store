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

let write_multi_pack_reverse_index index ~preferred_pack =
  let pack_directory =
    Filename.dirname (Multi_pack_index_reader.multi_pack_index_file index)
  in
  let pack_file_names = Multi_pack_index_reader.pack_file_names index in
  let preferred_pack = Util.validate_preferred_pack ~pack_directory ~preferred_pack in
  let%bind preferred_pack =
    match preferred_pack with
    | Some preferred_pack ->
      let preferred_pack = Filename.basename preferred_pack in
      let preferred_pack_index =
        Array.find_mapi pack_file_names ~f:(fun index pack_file_name ->
          match String.( = ) pack_file_name preferred_pack with
          | true -> Some index
          | false -> None)
      in
      (match preferred_pack_index with
       | None ->
         raise_s
           [%message
             "Preferred pack is not part of the multi-pack-index"
               (preferred_pack : string)
               ~index_pack_files:(pack_file_names : string array)]
       | Some index -> return index)
    | None ->
      (* If the user doesn't specify a preferred pack, upstream git chooses the pack with
         the oldest mtime as the preferred one (despite choosing packs with more *recent*
         mtimes for objects that are not present in the preferred pack when breaking ties
         among multiple non-preferred packs) *)
      let%map packs_with_mtimes =
        Deferred.Array.mapi
          pack_file_names
          ~how:`Sequential
          ~f:(fun index pack_file_name ->
            let pack_file_path = pack_directory ^/ pack_file_name in
            let%map stat = Unix.lstat pack_file_path in
            stat.mtime, index)
      in
      Array.sort packs_with_mtimes ~compare:(Comparable.lift ~f:fst Time_float.compare);
      snd packs_with_mtimes.(0)
  in
  let sha1 =
    Sha1.Raw.Volatile.to_hex (Multi_pack_index_reader.multi_pack_index_sha1 index)
  in
  let reverse_index_file =
    pack_directory ^/ [%string "multi-pack-index-%{sha1#Sha1.Hex}.rev"]
  in
  let object_count = Multi_pack_index_reader.object_count index in
  let reverse_index_file_size = 12 + (4 * object_count) + (Sha1.Raw.length * 2) in
  let%bind fd = Unix.openfile ~mode:[ `Rdwr; `Creat; `Trunc ] reverse_index_file in
  let%map file_mmap =
    Fd.syscall_in_thread_exn ~name:"git-pack-mmap-file" fd (fun file_descr ->
      Bigstring_unix.map_file ~shared:true file_descr reverse_index_file_size)
  in
  Bigstring.unsafe_set_uint32_be file_mmap ~pos:0 0x52494458;
  Bigstring.unsafe_set_uint32_be file_mmap ~pos:4 1;
  Bigstring.unsafe_set_uint32_be file_mmap ~pos:8 1;
  let pack_offsets =
    Array.init object_count ~f:(fun pos ->
      let pack_id = Multi_pack_index_reader.pack_id index ~index:pos in
      let pack_order =
        match pack_id = preferred_pack with
        | false -> pack_id
        | true -> -1
      in
      pack_order, Multi_pack_index_reader.pack_offset index ~index:pos)
  in
  let ordering = Array.init object_count ~f:Fn.id in
  Array.sort ordering ~compare:(fun a b ->
    [%compare: int * int] pack_offsets.(a) pack_offsets.(b));
  Array.iteri ordering ~f:(fun pack_order index ->
    Bigstring.set_uint32_be_exn file_mmap ~pos:(12 + (pack_order * 4)) index);
  Bigstring.From_bytes.blit
    ~src:(Sha1.Raw.Volatile.bytes (Multi_pack_index_reader.multi_pack_index_sha1 index))
    ~src_pos:0
    ~dst:file_mmap
    ~dst_pos:(reverse_index_file_size - (Sha1.Raw.length * 2))
    ~len:Sha1.Raw.length;
  let sha1_compute = Sha1.Compute.create_uninitialised () |> Sha1.Compute.init_or_reset in
  Sha1.Compute.process
    sha1_compute
    file_mmap
    ~pos:0
    ~len:(reverse_index_file_size - Sha1.Raw.length);
  let sha1_compute = Sha1.Compute.finalise sha1_compute in
  Bigstring.From_bytes.blit
    ~src:(Sha1.Raw.Volatile.bytes (Sha1.Compute.get_raw sha1_compute))
    ~src_pos:0
    ~dst:file_mmap
    ~dst_pos:(reverse_index_file_size - Sha1.Raw.length)
    ~len:Sha1.Raw.length
;;

let%test_module "Multi_pack_reverse_index_writer" =
  (module struct
    module Expect_test_time_zone = Git_core_types.Expect_test_time_zone

    let write_multi_pack_reverse_index ~pack_directory ~preferred_pack print =
      let%bind () =
        let%bind files = Sys.readdir pack_directory in
        let old_rev_indexes =
          Array.filter files ~f:(String.is_suffix ~suffix:".rev")
          |> Array.filter ~f:(String.is_prefix ~prefix:"multi-pack-index-")
          |> Array.map ~f:(fun file_name -> pack_directory ^/ file_name)
        in
        Deferred.Array.iter old_rev_indexes ~how:`Sequential ~f:Unix.unlink
      in
      let%bind multi_pack_index =
        Multi_pack_index_reader.open_existing ~pack_directory >>| ok_exn
      in
      let%bind () = write_multi_pack_reverse_index multi_pack_index ~preferred_pack in
      match print with
      | `Raw_contents ->
        let sha1 =
          Sha1.Raw.Volatile.to_hex
            (Multi_pack_index_reader.multi_pack_index_sha1 multi_pack_index)
        in
        let reverse_index_file = [%string "multi-pack-index-%{sha1#Sha1.Hex}.rev"] in
        let%bind reverse_index_contents =
          Reader.file_contents (pack_directory ^/ reverse_index_file)
        in
        printf "%S" reverse_index_contents;
        Deferred.unit
      | `Parsed ->
        let%map reverse_index =
          Multi_pack_reverse_index_reader.open_existing multi_pack_index >>| ok_exn
        in
        let object_count = Multi_pack_index_reader.object_count multi_pack_index in
        printf "pseudo pack order | index | pack id | pack offset\n";
        for pseudo_pack_order = 0 to object_count - 1 do
          let index =
            Multi_pack_reverse_index_reader.index_of_pseudo_pack_order
              reverse_index
              ~pseudo_pack_order
          in
          let pack_id =
            Multi_pack_reverse_index_reader.pack_id_of_pseudo_pack_order
              reverse_index
              ~pseudo_pack_order
          in
          let pack_offset =
            Multi_pack_reverse_index_reader.pack_offset_of_pseudo_pack_order
              reverse_index
              ~pseudo_pack_order
          in
          let pseudo_pack_order_index_round_trip =
            Multi_pack_reverse_index_reader.pseudo_pack_order_of_index
              reverse_index
              ~index
          in
          let pseudo_pack_order_offset_round_trip =
            Multi_pack_reverse_index_reader.pseudo_pack_order_of_pack_id_and_offset
              reverse_index
              ~pack_id
              ~pack_offset
          in
          let index_offset_round_trip =
            Multi_pack_reverse_index_reader.index_of_pack_id_and_offset
              reverse_index
              ~pack_id
              ~pack_offset
          in
          printf
            "%11d %2d %2d | %2d %2d | %7d | %11d \n"
            pseudo_pack_order
            pseudo_pack_order_index_round_trip
            pseudo_pack_order_offset_round_trip
            index
            index_offset_round_trip
            pack_id
            pack_offset
        done
    ;;

    let%expect_test "" =
      Expect_test_helpers_async.with_temp_dir (fun object_directory ->
        Expect_test_time_zone.with_fixed_time_zone_async (fun () ->
          let pack_directory = object_directory ^/ "pack" in
          let%bind packs =
            Multi_pack_index_writer.For_testing.make_pack_files
              ~object_directory
              (let packs =
                 Multi_pack_index_writer.For_testing.three_overlapping_packs_example
               in
               (* Make it such that the first pack lexicographically isn't also the oldest
                  pack. *)
               List.tl_exn packs @ [ List.hd_exn packs ])
          in
          let%bind () =
            Multi_pack_index_writer.write_multi_pack_index_file
              ~pack_directory
              ~preferred_pack:None
          in
          let packs = Array.of_list packs in
          print_endline "packs by mtime order:";
          Array.iter packs ~f:print_endline;
          print_endline "";
          let%bind () =
            write_multi_pack_reverse_index ~pack_directory ~preferred_pack:None `Parsed
          in
          let%bind () =
            Deferred.Array.iter packs ~how:`Sequential ~f:(fun preferred_pack ->
              print_endline "";
              write_multi_pack_reverse_index
                ~pack_directory
                ~preferred_pack:(Some preferred_pack)
                `Parsed)
          in
          [%expect
            {|
            packs by mtime order:
            pack-dfa729d1b4f4f638dcf554f804d034fe3a60b495.pack
            pack-bb8f3b6cbb5b4bd45f33bded1ffd1c249f573c3a.pack
            pack-68d020a4f35b82da5646c7cee22bcf25add57c2d.pack

            pseudo pack order | index | pack id | pack offset
                      0  0  0 |  5  5 |       2 |          12
                      1  1  1 |  4  4 |       2 |          66
                      2  2  2 |  9  9 |       0 |          12
                      3  3  3 |  1  1 |       0 |          27
                      4  4  4 |  2  2 |       0 |          48
                      5  5  5 |  7  7 |       0 |          69
                      6  6  6 |  6  6 |       1 |          12
                      7  7  7 |  0  0 |       1 |          27
                      8  8  8 |  8  8 |       1 |          66
                      9  9  9 |  3  3 |       1 |         110

            pseudo pack order | index | pack id | pack offset
                      0  0  0 |  5  5 |       2 |          12
                      1  1  1 |  4  4 |       2 |          66
                      2  2  2 |  9  9 |       0 |          12
                      3  3  3 |  1  1 |       0 |          27
                      4  4  4 |  2  2 |       0 |          48
                      5  5  5 |  7  7 |       0 |          69
                      6  6  6 |  6  6 |       1 |          12
                      7  7  7 |  0  0 |       1 |          27
                      8  8  8 |  8  8 |       1 |          66
                      9  9  9 |  3  3 |       1 |         110

            pseudo pack order | index | pack id | pack offset
                      0  0  0 |  6  6 |       1 |          12
                      1  1  1 |  0  0 |       1 |          27
                      2  2  2 |  8  8 |       1 |          66
                      3  3  3 |  3  3 |       1 |         110
                      4  4  4 |  9  9 |       0 |          12
                      5  5  5 |  1  1 |       0 |          27
                      6  6  6 |  2  2 |       0 |          48
                      7  7  7 |  7  7 |       0 |          69
                      8  8  8 |  5  5 |       2 |          12
                      9  9  9 |  4  4 |       2 |          66

            pseudo pack order | index | pack id | pack offset
                      0  0  0 |  9  9 |       0 |          12
                      1  1  1 |  1  1 |       0 |          27
                      2  2  2 |  2  2 |       0 |          48
                      3  3  3 |  7  7 |       0 |          69
                      4  4  4 |  6  6 |       1 |          12
                      5  5  5 |  0  0 |       1 |          27
                      6  6  6 |  8  8 |       1 |          66
                      7  7  7 |  3  3 |       1 |         110
                      8  8  8 |  5  5 |       2 |          12
                      9  9  9 |  4  4 |       2 |          66 |}];
          let%bind () =
            write_multi_pack_reverse_index
              ~pack_directory
              ~preferred_pack:None
              `Raw_contents
          in
          [%expect
            {| "RIDX\000\000\000\001\000\000\000\001\000\000\000\005\000\000\000\004\000\000\000\t\000\000\000\001\000\000\000\002\000\000\000\007\000\000\000\006\000\000\000\000\000\000\000\b\000\000\000\003\148\133\188\020\222\156\130\005_\241\191u0i\215$|\1671<5\180\254\236\228\255_\189x\177\015\149u\142F\238t4DQ" |}];
          let%bind () =
            write_multi_pack_reverse_index
              ~pack_directory
              ~preferred_pack:(Some packs.(0))
              `Raw_contents
          in
          [%expect
            {| "RIDX\000\000\000\001\000\000\000\001\000\000\000\005\000\000\000\004\000\000\000\t\000\000\000\001\000\000\000\002\000\000\000\007\000\000\000\006\000\000\000\000\000\000\000\b\000\000\000\003\148\133\188\020\222\156\130\005_\241\191u0i\215$|\1671<5\180\254\236\228\255_\189x\177\015\149u\142F\238t4DQ" |}];
          let%bind () =
            write_multi_pack_reverse_index
              ~pack_directory
              ~preferred_pack:(Some packs.(1))
              `Raw_contents
          in
          [%expect
            {| "RIDX\000\000\000\001\000\000\000\001\000\000\000\006\000\000\000\000\000\000\000\b\000\000\000\003\000\000\000\t\000\000\000\001\000\000\000\002\000\000\000\007\000\000\000\005\000\000\000\004\148\133\188\020\222\156\130\005_\241\191u0i\215$|\1671<\155N\t\214\148\021\231\222\241`F\175\200\191\206\130\217h\133o" |}];
          let%bind () =
            write_multi_pack_reverse_index
              ~pack_directory
              ~preferred_pack:(Some packs.(2))
              `Raw_contents
          in
          [%expect
            {| "RIDX\000\000\000\001\000\000\000\001\000\000\000\t\000\000\000\001\000\000\000\002\000\000\000\007\000\000\000\006\000\000\000\000\000\000\000\b\000\000\000\003\000\000\000\005\000\000\000\004\148\133\188\020\222\156\130\005_\241\191u0i\215$|\1671<\237D\212fQ\015(\163\206\134;b\226\007\003MV\019w\231" |}];
          Deferred.unit))
    ;;
  end)
;;
