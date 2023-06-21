(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2021-2023  Bogdan-Cristian Tataroiu

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

let write_reverse_index index =
  let reverse_index_file =
    String.chop_suffix_exn ~suffix:".idx" (Index_reader.index_file index) ^ ".rev"
  in
  let items_in_pack = Index_reader.items_in_pack index in
  let reverse_index_file_size = 12 + (4 * items_in_pack) + (Sha1.Raw.length * 2) in
  let%bind fd = Unix.openfile ~mode:[ `Rdwr; `Creat; `Trunc ] reverse_index_file in
  let%map file_mmap =
    Fd.syscall_in_thread_exn ~name:"git-pack-mmap-file" fd (fun file_descr ->
      Bigstring_unix.map_file ~shared:true file_descr reverse_index_file_size)
  in
  Bigstring.unsafe_set_uint32_be file_mmap ~pos:0 0x52494458;
  Bigstring.unsafe_set_uint32_be file_mmap ~pos:4 1;
  Bigstring.unsafe_set_uint32_be file_mmap ~pos:8 1;
  let pack_offsets =
    Array.init items_in_pack ~f:(fun pos ->
      Index_reader.pack_file_offset index ~index:pos)
  in
  let ordering = Array.init items_in_pack ~f:Fn.id in
  Array.sort ordering ~compare:(fun a b -> Int.compare pack_offsets.(a) pack_offsets.(b));
  Array.iteri ordering ~f:(fun pack_order index ->
    Bigstring.set_uint32_be_exn file_mmap ~pos:(12 + (pack_order * 4)) index);
  Bigstring.From_bytes.blit
    ~src:(Sha1.Raw.Volatile.bytes (Index_reader.pack_sha1 index))
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
