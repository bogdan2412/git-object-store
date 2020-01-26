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

(** Skip over bytes that are used to represent a variable length integer and return the
    next position in the buffer from which other data can be read. *)
let rec skip_variable_length_integer buf ~pos =
  if Bigstring.get_uint8 buf ~pos land 128 = 0
  then pos + 1
  else skip_variable_length_integer buf ~pos:(pos + 1)
;;

let read_variable_length_integer =
  let rec read_loop ~acc ~shift buf ~pos =
    let byte = Bigstring.get_uint8 buf ~pos in
    if byte land 128 = 0
    then acc lor (byte lsl shift)
    else
      read_loop
        ~acc:(acc lor ((byte land 127) lsl shift))
        ~shift:(shift + 7)
        buf
        ~pos:(pos + 1)
  in
  fun buf ~pos -> read_loop ~acc:0 ~shift:0 buf ~pos
;;

let read_variable_length_relative_offset =
  let rec read_loop ~acc buf ~pos =
    let byte = Bigstring.get_uint8 buf ~pos in
    let acc = ((acc + 1) lsl 7) lor (byte land 127) in
    if byte land 128 = 0 then acc else read_loop ~acc buf ~pos:(pos + 1)
  in
  fun buf ~pos ->
    let first_byte = Bigstring.get_uint8 buf ~pos in
    let acc = first_byte land 127 in
    if first_byte land 128 = 0 then acc else read_loop ~acc buf ~pos:(pos + 1)
;;

let feed_parser_data =
  let rec feed_data_loop ~acc parser_ ~process ~finalise buf ~pos =
    let len = Int.min (Bigstring.length buf - pos) (1 lsl 16) in
    let consumed = process parser_ buf ~pos ~len in
    if consumed < len || len = 0
    then (
      finalise parser_;
      acc + consumed)
    else feed_data_loop ~acc:(acc + len) parser_ ~process ~finalise buf ~pos:(pos + len)
  in
  fun parser_ ~process ~finalise buf ~pos ->
    feed_data_loop ~acc:0 parser_ ~process ~finalise buf ~pos
;;

module Base_object_parser : sig
  type _ t

  val create : 'Sha1_validation Sha1_validation.t -> 'Sha1_validation t

  val reset_for_reading_blob
    :  'Sha1_validation t
    -> payload_length:int
    -> on_size:(int -> unit)
    -> on_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
    -> 'Sha1_validation
    -> unit

  val reset_for_reading_commit
    :  'Sha1_validation t
    -> payload_length:int
    -> on_commit:(Commit.t -> unit)
    -> 'Sha1_validation
    -> unit

  val reset_for_reading_tree
    :  'Sha1_validation t
    -> payload_length:int
    -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
    -> 'Sha1_validation
    -> unit

  val reset_for_reading_tag
    :  'Sha1_validation t
    -> payload_length:int
    -> on_tag:(Tag.t -> unit)
    -> 'Sha1_validation
    -> unit

  (** Returns number of bytes consumed from input. *)
  val process : _ t -> Bigstring.t -> pos:int -> len:int -> int

  val finalise : _ t -> unit
end = struct
  type 'Sha1_validation t =
    { git_object_parser : 'Sha1_validation Git_object_parser.t
    ; zlib_inflate : Zlib.Inflate.t
    ; mutable expected_sha1 : 'Sha1_validation
    }

  let create (type a) (sha1_validation : a Sha1_validation.t) : a t =
    let git_object_parser =
      Git_object_parser.create
        ~on_blob_size:(fun (_ : int) -> ())
        ~on_blob_chunk:(fun (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) -> ())
        ~on_commit:(fun (_ : Commit.t) -> ())
        ~on_tree_line:
          (fun (_ : File_mode.t) (_ : Sha1.Raw.Volatile.t) ~name:(_ : string) -> ())
        ~on_tag:(fun (_ : Tag.t) -> ())
        ~on_error:Error.raise
        sha1_validation
    in
    let zlib_inflate =
      Zlib.Inflate.create_uninitialised ~on_data_chunk:(fun buf ~pos ~len ->
        Git_object_parser.append_data git_object_parser buf ~pos ~len)
    in
    match sha1_validation with
    | Do_not_validate_sha1 -> { git_object_parser; zlib_inflate; expected_sha1 = () }
    | Validate_sha1 ->
      { git_object_parser
      ; zlib_inflate
      ; expected_sha1 = Sha1.Hex.of_string "0000000000000000000000000000000000000000"
      }
  ;;

  let reset_for_reading_blob t ~payload_length ~on_size ~on_chunk expected_sha1 =
    Zlib.Inflate.init_or_reset t.zlib_inflate;
    Git_object_parser.set_on_blob t.git_object_parser ~on_size ~on_chunk;
    Git_object_parser.reset_for_reading_blob t.git_object_parser ~payload_length;
    t.expected_sha1 <- expected_sha1
  ;;

  let reset_for_reading_commit t ~payload_length ~on_commit expected_sha1 =
    Zlib.Inflate.init_or_reset t.zlib_inflate;
    Git_object_parser.set_on_commit t.git_object_parser on_commit;
    Git_object_parser.reset_for_reading_commit t.git_object_parser ~payload_length;
    t.expected_sha1 <- expected_sha1
  ;;

  let reset_for_reading_tree t ~payload_length ~on_tree_line expected_sha1 =
    Zlib.Inflate.init_or_reset t.zlib_inflate;
    Git_object_parser.set_on_tree_line t.git_object_parser on_tree_line;
    Git_object_parser.reset_for_reading_tree t.git_object_parser ~payload_length;
    t.expected_sha1 <- expected_sha1
  ;;

  let reset_for_reading_tag t ~payload_length ~on_tag expected_sha1 =
    Zlib.Inflate.init_or_reset t.zlib_inflate;
    Git_object_parser.set_on_tag t.git_object_parser on_tag;
    Git_object_parser.reset_for_reading_tag t.git_object_parser ~payload_length;
    t.expected_sha1 <- expected_sha1
  ;;

  let process t buf ~pos ~len = Zlib.Inflate.process t.zlib_inflate buf ~pos ~len

  let finalise t =
    Zlib.Inflate.finalise t.zlib_inflate;
    Git_object_parser.finalise t.git_object_parser t.expected_sha1
  ;;
end

module Delta_object_parser : sig
  type 'Sha1_validation t

  (** An instance of [t] contains three buffers: [base], [delta] and [result].

      [base] represents the base git object
      [delta] represents the set of delta instructions applied on top of [base].
      [result] will contain the result of applying [delta] to [base].
  *)
  val create : 'Sha1_validation Sha1_validation.t -> 'Sha1_validation t

  (** Returns the buffer containing a computed [result]. *)
  val result_buf : _ t -> Bigstring.t

  (** Returns length of computed [result]. *)
  val result_len : _ t -> int

  (** Reset [result] buffer and prepare to receive zlib compressed data which will be
      decompressed into it. *)
  val begin_zlib_inflate_into_result : _ t -> unit

  (** Feed [result] buffer a chunk of zlib compressed data. *)
  val feed_result_zlib_inflate : _ t -> Bigstring.t -> pos:int -> len:int -> int

  (** Finish decompressing into [result] buffer. *)
  val finalise_result_zlib_inflate : _ t -> unit

  (** Swap [result] and [delta] buffers. *)
  val set_result_as_delta : _ t -> unit

  (** Swap [result] and [base] buffers. *)
  val set_result_as_base : _ t -> unit

  (** Replace the [base] buffer with the provided buffer and length.
      Note that the contents in the buffer is _not_ copied. *)
  val set_base_buffer_and_length : _ t -> Bigstring.t -> length:int -> unit

  (** Compute [result] buffer from [base] and [delta] buffers. *)
  val compute_result : _ t -> unit

  (** Parse output in [result] as a Git base object. *)
  val parse_result
    :  'Sha1_validation t
    -> Object_type.t
    -> on_blob_size:(int -> unit)
    -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
    -> on_commit:(Commit.t -> unit)
    -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
    -> on_tag:(Tag.t -> unit)
    -> 'Sha1_validation
    -> unit
end = struct
  type 'Sha1_validation t =
    { mutable base_buf : Bigstring.t
    ; mutable base_len : int
    ; mutable delta_buf : Bigstring.t
    ; mutable delta_len : int
    ; mutable result_buf : Bigstring.t
    ; mutable result_len : int
    ; result_zlib_inflate : Zlib.Inflate.t
    ; git_object_parser : 'Sha1_validation Git_object_parser.t
    }
  [@@deriving fields]

  let blit_into_result_buf t buf ~pos ~len =
    while t.result_len + len > Bigstring.length t.result_buf do
      let new_buf = Bigstring.create (2 * Bigstring.length t.result_buf) in
      Bigstring.blit
        ~src:t.result_buf
        ~src_pos:0
        ~dst:new_buf
        ~dst_pos:0
        ~len:t.result_len;
      t.result_buf <- new_buf
    done;
    Bigstring.blit ~src:buf ~src_pos:pos ~dst:t.result_buf ~dst_pos:t.result_len ~len;
    t.result_len <- t.result_len + len
  ;;

  let rec compute_result t ~delta_pos =
    if delta_pos >= t.delta_len
    then ()
    else (
      let first_byte = Bigstring.get_uint8 t.delta_buf ~pos:delta_pos in
      if first_byte land 128 = 0
      then (
        (* Instruction to add new data. *)
        let length = first_byte in
        if length = 0 then raise_s [%message "Unsupported reserved delta instruction"];
        if delta_pos + 1 + length > t.delta_len
        then raise_s [%message "Delta instruction to add new data out of bounds"];
        blit_into_result_buf t t.delta_buf ~pos:(delta_pos + 1) ~len:length;
        compute_result t ~delta_pos:(delta_pos + 1 + length))
      else (
        (* Instruction to copy from base object. *)
        let offset, delta_pos = 0, delta_pos + 1 in
        let offset, delta_pos =
          if first_byte land 1 = 0
          then offset, delta_pos
          else offset lor Bigstring.get_uint8 t.delta_buf ~pos:delta_pos, delta_pos + 1
        in
        let offset, delta_pos =
          if first_byte land 2 = 0
          then offset, delta_pos
          else
            ( offset lor (Bigstring.get_uint8 t.delta_buf ~pos:delta_pos lsl 8)
            , delta_pos + 1 )
        in
        let offset, delta_pos =
          if first_byte land 4 = 0
          then offset, delta_pos
          else
            ( offset lor (Bigstring.get_uint8 t.delta_buf ~pos:delta_pos lsl 16)
            , delta_pos + 1 )
        in
        let offset, delta_pos =
          if first_byte land 8 = 0
          then offset, delta_pos
          else
            ( offset lor (Bigstring.get_uint8 t.delta_buf ~pos:delta_pos lsl 24)
            , delta_pos + 1 )
        in
        let size = 0 in
        let size, delta_pos =
          if first_byte land 16 = 0
          then size, delta_pos
          else size lor Bigstring.get_uint8 t.delta_buf ~pos:delta_pos, delta_pos + 1
        in
        let size, delta_pos =
          if first_byte land 32 = 0
          then size, delta_pos
          else
            size lor (Bigstring.get_uint8 t.delta_buf ~pos:delta_pos lsl 8), delta_pos + 1
        in
        assert (first_byte land 64 = 0);
        let size = if size = 0 then 0x10000 else size in
        blit_into_result_buf t t.base_buf ~pos:offset ~len:size;
        compute_result t ~delta_pos))
  ;;

  let create sha1_validation =
    let git_object_parser =
      Git_object_parser.create
        ~on_blob_size:(fun (_ : int) -> ())
        ~on_blob_chunk:(fun (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) -> ())
        ~on_commit:(fun (_ : Commit.t) -> ())
        ~on_tree_line:
          (fun (_ : File_mode.t) (_ : Sha1.Raw.Volatile.t) ~name:(_ : string) -> ())
        ~on_tag:(fun (_ : Tag.t) -> ())
        ~on_error:Error.raise
        sha1_validation
    in
    let rec t =
      lazy
        { base_buf =
            Bigstring.create (if Core.am_running_inline_test then 1 else 1 lsl 15)
        ; base_len = 0
        ; delta_buf =
            Bigstring.create (if Core.am_running_inline_test then 1 else 1 lsl 15)
        ; delta_len = 0
        ; result_buf =
            Bigstring.create (if Core.am_running_inline_test then 1 else 1 lsl 15)
        ; result_len = 0
        ; result_zlib_inflate = Lazy.force result_zlib_inflate
        ; git_object_parser
        }
    and result_zlib_inflate =
      lazy
        (Zlib.Inflate.create_uninitialised ~on_data_chunk:(fun buf ~pos ~len ->
           blit_into_result_buf (Lazy.force t) buf ~pos ~len))
    in
    Lazy.force t
  ;;

  let set_result_as_base t =
    let temp = t.base_buf in
    t.base_buf <- t.result_buf;
    t.result_buf <- temp;
    let temp = t.base_len in
    t.base_len <- t.result_len;
    t.result_len <- temp
  ;;

  let set_result_as_delta t =
    let temp = t.delta_buf in
    t.delta_buf <- t.result_buf;
    t.result_buf <- temp;
    let temp = t.delta_len in
    t.delta_len <- t.result_len;
    t.result_len <- temp
  ;;

  let begin_zlib_inflate_into_result t =
    t.result_len <- 0;
    Zlib.Inflate.init_or_reset t.result_zlib_inflate
  ;;

  let feed_result_zlib_inflate t buf ~pos ~len =
    Zlib.Inflate.process t.result_zlib_inflate buf ~pos ~len
  ;;

  let finalise_result_zlib_inflate t = Zlib.Inflate.finalise t.result_zlib_inflate

  let set_base_buffer_and_length t buf ~length =
    t.base_buf <- buf;
    t.base_len <- length
  ;;

  let compute_result t =
    t.result_len <- 0;
    let pos = 0 in
    let expected_base_length = read_variable_length_integer t.delta_buf ~pos in
    if expected_base_length <> t.base_len
    then
      raise_s
        [%message
          "Delta object expected base object of different length"
            ~actual_base_length:(t.base_len : int)
            (expected_base_length : int)];
    let pos = skip_variable_length_integer t.delta_buf ~pos in
    let expected_result_length = read_variable_length_integer t.delta_buf ~pos in
    let pos = skip_variable_length_integer t.delta_buf ~pos in
    compute_result t ~delta_pos:pos;
    if expected_result_length <> t.result_len
    then
      raise_s
        [%message
          "Expected delta object result of different length"
            ~actual_result_length:(t.result_len : int)
            (expected_result_length : int)]
  ;;

  let parse_result
        t
        object_type
        ~on_blob_size
        ~on_blob_chunk
        ~on_commit
        ~on_tree_line
        ~on_tag
        sha1_validation
    =
    (match (object_type : Object_type.t) with
     | Commit ->
       Git_object_parser.set_on_commit t.git_object_parser on_commit;
       Git_object_parser.reset_for_reading_commit
         t.git_object_parser
         ~payload_length:t.result_len
     | Tree ->
       Git_object_parser.set_on_tree_line t.git_object_parser on_tree_line;
       Git_object_parser.reset_for_reading_tree
         t.git_object_parser
         ~payload_length:t.result_len
     | Blob ->
       Git_object_parser.set_on_blob
         t.git_object_parser
         ~on_size:on_blob_size
         ~on_chunk:on_blob_chunk;
       Git_object_parser.reset_for_reading_blob
         t.git_object_parser
         ~payload_length:t.result_len
     | Tag ->
       Git_object_parser.set_on_tag t.git_object_parser on_tag;
       Git_object_parser.reset_for_reading_tag
         t.git_object_parser
         ~payload_length:t.result_len);
    Git_object_parser.append_data
      t.git_object_parser
      t.result_buf
      ~pos:0
      ~len:t.result_len;
    Git_object_parser.finalise t.git_object_parser sha1_validation
  ;;
end

module Pack_object_type = struct
  type t =
    | Commit
    | Tree
    | Blob
    | Tag
    | Ofs_delta
    | Ref_delta
  [@@deriving sexp]
end

let object_type' buf ~pos : Pack_object_type.t =
  let type_int = (Bigstring.get_uint8 buf ~pos lsr 4) land 7 in
  match type_int with
  | 1 -> Commit
  | 2 -> Tree
  | 3 -> Blob
  | 4 -> Tag
  | 6 -> Ofs_delta
  | 7 -> Ref_delta
  | _ -> raise_s [%message "Invalid pack object type" (type_int : int)]
;;

let object_length' buf ~pos =
  let first_byte = Bigstring.get_uint8 buf ~pos in
  if first_byte land 128 = 0
  then first_byte land 15
  else first_byte land 15 lor (read_variable_length_integer buf ~pos:(pos + 1) lsl 4)
;;

let with_resource ~create ~close_on_error ~f =
  let open Deferred.Or_error.Let_syntax in
  let%bind resource = create () in
  let open Deferred.Let_syntax in
  let%bind result = f resource in
  match result with
  | Ok _ -> return result
  | Error _ ->
    (match%map close_on_error resource with
     | Ok () -> result
     | Error error ->
       Log.Global.sexp ~level:`Error [%message "Error closing resource" (error : Error.t)];
       result)
;;

let with_file file_path ~f =
  let open Deferred.Or_error.Let_syntax in
  with_resource
    ~create:(fun () ->
      Monitor.try_with_or_error ~extract_exn:true (fun () ->
        Unix.openfile ~mode:[ `Rdonly ] file_path))
    ~close_on_error:(fun fd ->
      Monitor.try_with_or_error ~extract_exn:true (fun () -> Unix.close fd))
    ~f:(fun fd ->
      let%bind file_size =
        Monitor.try_with_or_error ~extract_exn:true (fun () ->
          let open Deferred.Let_syntax in
          let%map stat = Unix.fstat fd in
          Int64.to_int_exn stat.size)
      in
      let%bind file_mmap =
        Monitor.try_with_or_error ~extract_exn:true (fun () ->
          Fd.syscall_in_thread_exn ~name:"git-pack-mmap-file" fd (fun file_descr ->
            Bigstring.map_file ~shared:false file_descr file_size))
      in
      f fd file_size file_mmap)
;;

module Index = struct
  module Section_pos : sig
    type t

    val create : items_in_pack:int -> t
    val fanout : t -> char -> int
    val sha1 : t -> int -> int
    val crc32 : t -> int -> int
    val offset : t -> int -> int
    val uint64_offset : t -> int -> int
  end = struct
    type t =
      { fanout : int
      ; sha1s : int
      ; crc32s : int
      ; offsets : int
      ; uint64_offsets : int
      }

    let create ~items_in_pack =
      let sha1s = 1032 in
      let crc32s = sha1s + (Sha1.Raw.length * items_in_pack) in
      let offsets = crc32s + (4 * items_in_pack) in
      let uint64_offsets = offsets + (4 * items_in_pack) in
      { fanout = 8; sha1s; crc32s; offsets; uint64_offsets }
    ;;

    let fanout t byte = t.fanout + (4 * Char.to_int byte)
    let sha1 t index = t.sha1s + (Sha1.Raw.length * index)
    let crc32 t index = t.crc32s + (4 * index)
    let offset t index = t.offsets + (4 * index)
    let uint64_offset t index = t.uint64_offsets + (8 * index)
  end

  type t =
    { index_file : string
    ; fd : Fd.t
    ; file_size : int
    ; file_mmap : Bigstring.t
    ; section_pos : Section_pos.t
    }

  let open_existing ~pack_file ~pack_file_mmap ~pack_file_size ~items_in_pack =
    let index_file = String.chop_suffix_exn pack_file ~suffix:".pack" ^ ".idx" in
    with_file index_file ~f:(fun fd file_size file_mmap ->
      let result =
        let open Or_error.Let_syntax in
        let%bind () =
          (* At least 4 for signature, 4 for version, 256 * 4 for fan-out table,
             one raw SHA1, one CRC32 and one offset for each item in the pack and
             two raw SHA1s *)
          if file_size
             < 1032 + (items_in_pack * (Sha1.Raw.length + 8)) + (2 * Sha1.Raw.length)
          then Or_error.error_s [%sexp "Index file impossibly small"]
          else Ok ()
        in
        let%bind () =
          if Bigstring.get_uint32_le file_mmap ~pos:0 <> 1666151679
          then Or_error.error_s [%sexp "Expected idx signature"]
          else Ok ()
        in
        let%bind () =
          if Bigstring.get_uint32_be file_mmap ~pos:4 <> 2
          then Or_error.error_s [%sexp "Expected index version number 2"]
          else Ok ()
        in
        let%bind () =
          if Bigstring.memcmp
               file_mmap
               ~pos1:(file_size - (Sha1.Raw.length * 2))
               pack_file_mmap
               ~pos2:(pack_file_size - Sha1.Raw.length)
               ~len:Sha1.Raw.length
             <> 0
          then
            Or_error.error_s
              [%sexp "SHA1 checksums do not match between index and pack files"]
          else Ok ()
        in
        return
          { index_file
          ; fd
          ; file_size
          ; file_mmap
          ; section_pos = Section_pos.create ~items_in_pack
          }
      in
      Deferred.return result)
  ;;

  module Builder = struct
    module Object = struct
      type t =
        { pack_pos : int
        ; pack_data_start_pos : int
        ; pack_data_length : int
        ; object_type : Object_type.t
        ; object_length : int
        ; crc32 : int
        ; sha1 : Sha1.Raw.t Set_once.t
        ; delta_parent : t option
        ; mutable delta_children : t list
        ; mutable contents : Bigstring.t option
        }

      [@@@ocaml.warning "-32"]

      let sexp_of_t
            { pack_pos
            ; pack_data_start_pos
            ; pack_data_length
            ; object_type
            ; object_length
            ; crc32
            ; sha1
            ; delta_parent
            ; delta_children
            ; contents
            }
        =
        let sha1 =
          match Set_once.get sha1 with
          | None -> Sexp.Atom "unset"
          | Some sha1 -> [%sexp_of: Sha1.Hex.t] (Sha1.Raw.to_hex sha1)
        in
        [%sexp
          { pack_pos : int
          ; pack_data_start_pos : int
          ; pack_data_length : int
          ; object_type : Object_type.t
          ; object_length : int
          ; crc32 : int
          ; sha1 : Sexp.t
          ; delta_parent =
              (Option.map delta_parent ~f:(fun delta_parent -> delta_parent.pack_pos) : int
                                                                                          option)
          ; delta_children =
              (List.map delta_children ~f:(fun delta_child -> delta_child.pack_pos) : int
                                                                                        list)
          ; contents : Bigstring.t sexp_option
          }]
      ;;

      [@@@ocaml.warning "+32"]
    end

    let index_objects =
      let rec index_objects_loop ~objects ~zlib_inflate ~items_left buf ~pos =
        if items_left = 0
        then pos
        else (
          let pack_object_type = object_type' buf ~pos in
          let object_length = object_length' buf ~pos in
          let data_start_pos = skip_variable_length_integer buf ~pos in
          let data_start_pos, (object_type : Object_type.t), delta_parent =
            match pack_object_type with
            | Commit -> data_start_pos, Commit, None
            | Tree -> data_start_pos, Tree, None
            | Blob -> data_start_pos, Blob, None
            | Tag -> data_start_pos, Tag, None
            | Ofs_delta ->
              let rel_offset =
                read_variable_length_relative_offset buf ~pos:data_start_pos
              in
              let delta_parent_pos = pos - rel_offset in
              let (delta_parent_object : Object.t) =
                Hashtbl.find_exn objects delta_parent_pos
              in
              let object_type = delta_parent_object.object_type in
              ( skip_variable_length_integer buf ~pos:data_start_pos
              , object_type
              , Some delta_parent_object )
            | Ref_delta -> failwith "Cannot re-index packs with Ref_delta objects"
          in
          Zlib.Inflate.init_or_reset zlib_inflate;
          let data_length =
            feed_parser_data
              zlib_inflate
              ~process:Zlib.Inflate.process
              ~finalise:Zlib.Inflate.finalise
              buf
              ~pos:data_start_pos
          in
          let object_ =
            { Object.pack_pos = pos
            ; pack_data_start_pos = data_start_pos
            ; pack_data_length = data_length
            ; object_type
            ; object_length
            ; crc32 =
                Crc32.finalise
                  (Crc32.process
                     Crc32.init
                     buf
                     ~pos
                     ~len:(data_start_pos - pos + data_length))
            ; sha1 = Set_once.create ()
            ; delta_parent
            ; delta_children = []
            ; contents = None
            }
          in
          Hashtbl.add_exn objects ~key:pos ~data:object_;
          Option.iter delta_parent ~f:(fun delta_parent ->
            delta_parent.delta_children <- object_ :: delta_parent.delta_children);
          index_objects_loop
            ~objects
            ~zlib_inflate
            ~items_left:(items_left - 1)
            buf
            ~pos:(data_start_pos + data_length))
      in
      fun ~pack_file_mmap ~items_in_pack ->
        let objects = Int.Table.create () in
        let zlib_inflate =
          Zlib.Inflate.create_uninitialised
            ~on_data_chunk:(fun (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) -> ())
        in
        let pos =
          index_objects_loop
            ~objects
            ~zlib_inflate
            ~items_left:items_in_pack
            pack_file_mmap
            ~pos:12
        in
        assert (pos = Bigstring.length pack_file_mmap - Sha1.Raw.length);
        objects
    ;;

    let rec dfs =
      let object_type_buf = Bigstring.create 32 in
      fun delta_object_parser sha1_compute_uninit (object_ : Object.t) buf ->
        let pos = object_.pack_data_start_pos in
        Delta_object_parser.begin_zlib_inflate_into_result delta_object_parser;
        let (_ : int) =
          feed_parser_data
            delta_object_parser
            ~process:Delta_object_parser.feed_result_zlib_inflate
            ~finalise:Delta_object_parser.finalise_result_zlib_inflate
            buf
            ~pos
        in
        if Delta_object_parser.result_len delta_object_parser <> object_.object_length
        then
          raise_s
            [%message
              "Unexpected decompressed object length"
                ~expected_length:(object_.object_length : int)
                ~actual_length:(Delta_object_parser.result_len delta_object_parser : int)];
        (match object_.delta_parent with
         | None -> ()
         | Some delta_parent ->
           let parent_contents = Option.value_exn delta_parent.contents in
           Delta_object_parser.set_base_buffer_and_length
             delta_object_parser
             parent_contents
             ~length:(Bigstring.length parent_contents);
           Delta_object_parser.set_result_as_delta delta_object_parser;
           (* The method below validates that the length of the result is correct. *)
           Delta_object_parser.compute_result delta_object_parser);
        let sha1_compute = Sha1.Compute.init_or_reset sha1_compute_uninit in
        let header_len =
          Git_object_header_writer.write_from_left
            object_type_buf
            ~pos:0
            object_.object_type
            ~object_length:(Delta_object_parser.result_len delta_object_parser)
        in
        Sha1.Compute.process sha1_compute object_type_buf ~pos:0 ~len:header_len;
        Sha1.Compute.process
          sha1_compute
          (Delta_object_parser.result_buf delta_object_parser)
          ~pos:0
          ~len:(Delta_object_parser.result_len delta_object_parser);
        let sha1_compute = Sha1.Compute.finalise sha1_compute in
        let sha1_raw =
          Sha1.Raw.Volatile.non_volatile (Sha1.Compute.get_raw sha1_compute)
        in
        Set_once.set_exn object_.sha1 [%here] sha1_raw;
        match object_.delta_children with
        | [] -> ()
        | children ->
          object_.contents
          <- Some
               (Bigstring.sub
                  (Delta_object_parser.result_buf delta_object_parser)
                  ~pos:0
                  ~len:(Delta_object_parser.result_len delta_object_parser));
          List.iter children ~f:(fun child ->
            dfs delta_object_parser sha1_compute_uninit child buf);
          object_.contents <- None
    ;;

    let write_index_file ~index_file ~(objects_in_sha1_order : Object.t array) ~pack_sha1 =
      let items_in_pack = Array.length objects_in_sha1_order in
      let section_pos = Section_pos.create ~items_in_pack in
      let max_uint32_offset = (1 lsl 31) - 1 in
      let uint64_offsets, _ =
        Array.foldi
          objects_in_sha1_order
          ~init:(Int.Map.empty, 0)
          ~f:(fun key acc object_ ->
            if object_.pack_pos <= max_uint32_offset
            then acc
            else (
              let map, cnt = acc in
              Map.add_exn map ~key ~data:cnt, cnt + 1))
      in
      let index_file_size =
        Section_pos.uint64_offset section_pos (Map.length uint64_offsets)
        + (Sha1.Raw.length * 2)
      in
      let%bind fd = Unix.openfile ~mode:[ `Rdwr; `Creat; `Trunc ] index_file in
      let%map file_mmap =
        Fd.syscall_in_thread_exn ~name:"git-pack-mmap-file" fd (fun file_descr ->
          Bigstring.map_file ~shared:true file_descr index_file_size)
      in
      Bigstring.unsafe_set_uint32_le file_mmap ~pos:0 1666151679;
      Bigstring.unsafe_set_uint32_be file_mmap ~pos:4 2;
      Array.iter objects_in_sha1_order ~f:(fun object_ ->
        let first_byte =
          (Sha1.Raw.to_string (Set_once.get_exn object_.sha1 [%here])).[0]
        in
        let pos = Section_pos.fanout section_pos first_byte in
        Bigstring.set_uint32_be_exn
          file_mmap
          ~pos
          (Bigstring.get_uint32_be file_mmap ~pos + 1));
      for idx = 1 to 255 do
        let pos = Section_pos.fanout section_pos (Char.of_int_exn idx) in
        Bigstring.set_uint32_be_exn
          file_mmap
          ~pos
          (Bigstring.get_uint32_be file_mmap ~pos:(pos - 4)
           + Bigstring.get_uint32_be file_mmap ~pos)
      done;
      Array.iteri objects_in_sha1_order ~f:(fun idx object_ ->
        let pos = Section_pos.sha1 section_pos idx in
        Bigstring.From_string.blit
          ~src:(Sha1.Raw.to_string (Set_once.get_exn object_.sha1 [%here]))
          ~src_pos:0
          ~dst:file_mmap
          ~dst_pos:pos
          ~len:Sha1.Raw.length);
      Array.iteri objects_in_sha1_order ~f:(fun idx object_ ->
        let pos = Section_pos.crc32 section_pos idx in
        Bigstring.set_uint32_be_exn file_mmap ~pos object_.crc32);
      Array.iteri objects_in_sha1_order ~f:(fun idx object_ ->
        let pos = Section_pos.offset section_pos idx in
        let offset_value =
          if object_.pack_pos <= max_uint32_offset
          then object_.pack_pos
          else (
            let big_offset = Map.find_exn uint64_offsets idx in
            Bigstring.set_uint64_be_exn
              file_mmap
              ~pos:(Section_pos.uint64_offset section_pos big_offset)
              object_.pack_pos;
            big_offset lor (1 lsl 31))
        in
        Bigstring.unsafe_set_uint32_be file_mmap ~pos offset_value);
      Bigstring.From_string.blit
        ~src:(Sha1.Raw.to_string pack_sha1)
        ~src_pos:0
        ~dst:file_mmap
        ~dst_pos:(index_file_size - (Sha1.Raw.length * 2))
        ~len:Sha1.Raw.length;
      let sha1_compute =
        Sha1.Compute.create_uninitialised () |> Sha1.Compute.init_or_reset
      in
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

    let index_pack ~pack_file ~pack_file_mmap ~items_in_pack =
      let index_file = String.chop_suffix_exn ~suffix:".pack" pack_file ^ ".idx" in
      let delta_object_parser = Delta_object_parser.create Do_not_validate_sha1 in
      let sha1_compute = Sha1.Compute.create_uninitialised () in
      let objects = index_objects ~pack_file_mmap ~items_in_pack in
      let objects_in_pack_order = Array.of_list (Hashtbl.data objects) in
      Array.sort
        objects_in_pack_order
        ~compare:(Comparable.lift Int.compare ~f:(fun object_ -> object_.Object.pack_pos));
      Array.iter objects_in_pack_order ~f:(fun object_ ->
        match object_.delta_parent with
        | Some _ -> ()
        | None -> dfs delta_object_parser sha1_compute object_ pack_file_mmap);
      let objects_in_sha1_order = Array.of_list (Hashtbl.data objects) in
      Array.sort
        objects_in_sha1_order
        ~compare:
          (Comparable.lift Sha1.Raw.compare ~f:(fun object_ ->
             Set_once.get_exn object_.Object.sha1 [%here]));
      let pack_sha1 =
        Sha1.Raw.of_string
          (Bigstring.To_string.sub
             pack_file_mmap
             ~pos:(Bigstring.length pack_file_mmap - Sha1.Raw.length)
             ~len:Sha1.Raw.length)
      in
      write_index_file ~index_file ~objects_in_sha1_order ~pack_sha1
    ;;
  end
end

type 'Sha1_validation t =
  { pack_file : string
  ; pack_fd : Fd.t
  ; pack_file_size : int
  ; pack_file_mmap : Bigstring.t
  ; items_in_pack : int
  ; base_object_parser : 'Sha1_validation Base_object_parser.t
  ; delta_object_parser : 'Sha1_validation Delta_object_parser.t
  ; sha1_validation : 'Sha1_validation Sha1_validation.t
  ; index : Index.t
  }

let create ~pack_file sha1_validation =
  let pack_file =
    if String.is_suffix ~suffix:".pack" pack_file then pack_file else pack_file ^ ".pack"
  in
  with_file pack_file ~f:(fun pack_fd pack_file_size pack_file_mmap ->
    let open Deferred.Or_error.Let_syntax in
    let items_in_pack = Bigstring.get_uint32_be pack_file_mmap ~pos:8 in
    let%bind index =
      Index.open_existing ~pack_file ~pack_file_mmap ~pack_file_size ~items_in_pack
    in
    let result =
      let open Or_error.Let_syntax in
      let%bind () =
        (* At least 4 for signature, 4 for size, 4 for number of items and one raw SHA1 *)
        if pack_file_size < 12 + Sha1.Raw.length
        then Or_error.error_s [%sexp "Pack file impossibly small"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint32_le pack_file_mmap ~pos:0 <> 1262698832
        then Or_error.error_s [%sexp "Expected pack signature"]
        else Ok ()
      in
      let%bind () =
        if Bigstring.get_uint32_be pack_file_mmap ~pos:4 <> 2
        then Or_error.error_s [%sexp "Expected pack version number 2"]
        else Ok ()
      in
      return
        { pack_file
        ; pack_fd
        ; pack_file_size
        ; pack_file_mmap
        ; items_in_pack
        ; base_object_parser = Base_object_parser.create sha1_validation
        ; delta_object_parser = Delta_object_parser.create sha1_validation
        ; sha1_validation
        ; index
        }
    in
    Deferred.return result)
;;

let index_pack ~pack_file =
  let pack_file =
    if String.is_suffix ~suffix:".pack" pack_file then pack_file else pack_file ^ ".pack"
  in
  with_file pack_file ~f:(fun (_ : Fd.t) (_pack_file_size : int) pack_file_mmap ->
    let items_in_pack = Bigstring.get_uint32_be pack_file_mmap ~pos:8 in
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      Index.Builder.index_pack ~pack_file ~pack_file_mmap ~items_in_pack))
;;

let validate_index t ~index =
  if index < 0 || index >= t.items_in_pack
  then
    raise_s
      [%message
        "Invalid value for index" (index : int) ~items_in_pack:(t.items_in_pack : int)]
;;

module Sha1_function = struct
  let result = Sha1.Raw.Volatile.create ()

  let impl t ~index =
    validate_index t ~index;
    Bigstring.To_bytes.blit
      ~src:t.index.file_mmap
      ~src_pos:(Index.Section_pos.sha1 t.index.section_pos index)
      ~dst:(Sha1.Raw.Volatile.bytes result)
      ~dst_pos:0
      ~len:Sha1.Raw.length;
    result
  ;;
end

let sha1 = Sha1_function.impl
let items_in_pack t = t.items_in_pack

module Pack_file_object_offset_function = struct
  let msb_mask = 1 lsl 31

  let impl t ~index =
    validate_index t ~index;
    let offset =
      Bigstring.get_uint32_be
        t.index.file_mmap
        ~pos:(Index.Section_pos.offset t.index.section_pos index)
    in
    if offset land msb_mask = 0
    then offset
    else
      Bigstring.get_uint64_be_exn
        t.index.file_mmap
        ~pos:(Index.Section_pos.uint64_offset t.index.section_pos (offset lxor msb_mask))
  ;;
end

let pack_file_object_offset = Pack_file_object_offset_function.impl

let object_type t ~index =
  let pos = pack_file_object_offset t ~index in
  object_type' t.pack_file_mmap ~pos
;;

let object_length t ~index =
  let pos = pack_file_object_offset t ~index in
  object_length' t.pack_file_mmap ~pos
;;

module Find_result : sig
  module Volatile : sig
    type t = private
      | None
      | Some of { mutable index : int }
    [@@deriving sexp_of]

    val none : t
    val some : int -> t
    val index_exn : t -> int
  end
end = struct
  module Volatile = struct
    type t =
      | None
      | Some of { mutable index : int }

    let sexp_of_t = function
      | None -> Sexp.List []
      | Some { index } -> Sexp.List [ Sexp.Atom (Int.to_string index) ]
    ;;

    let none = None

    let some =
      let value = Some { index = -1 } in
      fun index ->
        (match value with
         | Some record -> record.index <- index
         | None -> assert false);
        value
    ;;

    let index_exn = function
      | None -> failwith "SHA1 not present in pack file"
      | Some { index } -> index
    ;;
  end
end

let find_sha1_index_gen =
  let rec sha1_greater_than_or_equal t sha1_get_char sha1 ~index_pos ~pos =
    if pos = Sha1.Raw.length
    then true
    else (
      let from_sha1 = Char.to_int (sha1_get_char sha1 pos) in
      let from_index = Bigstring.get_uint8 t.index.file_mmap ~pos:(index_pos + pos) in
      if from_sha1 > from_index
      then true
      else if from_sha1 < from_index
      then false
      else sha1_greater_than_or_equal t sha1_get_char sha1 ~index_pos ~pos:(pos + 1))
  in
  let rec sha1_equal t sha1_get_char sha1 ~index_pos ~pos =
    if pos = Sha1.Raw.length
    then true
    else (
      let from_sha1 = Char.to_int (sha1_get_char sha1 pos) in
      let from_index = Bigstring.get_uint8 t.index.file_mmap ~pos:(index_pos + pos) in
      if from_sha1 <> from_index
      then false
      else sha1_equal t sha1_get_char sha1 ~index_pos ~pos:(pos + 1))
  in
  fun t sha1_get_char sha1 ->
    let first_byte = sha1_get_char sha1 0 in
    let binary_search_start =
      match first_byte with
      | '\000' -> Index.Section_pos.sha1 t.index.section_pos 0
      | n ->
        Index.Section_pos.sha1
          t.index.section_pos
          (Bigstring.get_uint32_be
             t.index.file_mmap
             ~pos:
               (Index.Section_pos.fanout
                  t.index.section_pos
                  (Char.of_int_exn (Char.to_int n - 1))))
    in
    let binary_search_end =
      Index.Section_pos.sha1
        t.index.section_pos
        (Bigstring.get_uint32_be
           t.index.file_mmap
           ~pos:(Index.Section_pos.fanout t.index.section_pos first_byte)
         - 1)
    in
    let pos = ref binary_search_start in
    let step = ref Sha1.Raw.length in
    while !step <= binary_search_end - binary_search_start do
      step := !step lsl 1
    done;
    step := !step lsr 1;
    while !step >= Sha1.Raw.length do
      if !pos + !step <= binary_search_end
      && sha1_greater_than_or_equal
           t
           sha1_get_char
           sha1
           ~index_pos:(!pos + !step)
           ~pos:0
      then pos := !pos + !step;
      step := !step lsr 1
    done;
    if sha1_equal t sha1_get_char sha1 ~index_pos:!pos ~pos:0
    then
      Find_result.Volatile.some
        ((!pos - Index.Section_pos.sha1 t.index.section_pos 0) / Sha1.Raw.length)
    else Find_result.Volatile.none
;;

let find_sha1_index t sha1 = find_sha1_index_gen t String.get (Sha1.Raw.to_string sha1)

let find_sha1_index' t sha1 =
  find_sha1_index_gen t Bytes.get (Sha1.Raw.Volatile.bytes sha1)
;;

module Read_raw_delta_object_function = struct
  let feed_parser_data delta_object_parser ~expected_length buf pos =
    Delta_object_parser.begin_zlib_inflate_into_result delta_object_parser;
    let (_ : int) =
      feed_parser_data
        delta_object_parser
        ~process:Delta_object_parser.feed_result_zlib_inflate
        ~finalise:Delta_object_parser.finalise_result_zlib_inflate
        buf
        ~pos
    in
    let actual_length = Delta_object_parser.result_len delta_object_parser in
    if expected_length <> actual_length
    then
      raise_s
        [%message
          "Unexpected object length" (actual_length : int) (expected_length : int)]
  ;;

  let sha1 = Sha1.Raw.Volatile.create ()

  let rec impl t ~pos =
    let data_start_pos = skip_variable_length_integer t.pack_file_mmap ~pos in
    match object_type' t.pack_file_mmap ~pos with
    | (Commit | Tree | Blob | Tag) as object_type ->
      feed_parser_data
        t.delta_object_parser
        ~expected_length:(object_length' t.pack_file_mmap ~pos)
        t.pack_file_mmap
        data_start_pos;
      (match object_type with
       | Commit -> Object_type.Commit
       | Tree -> Tree
       | Blob -> Blob
       | Tag -> Tag
       | Ofs_delta | Ref_delta -> assert false)
    | (Ofs_delta | Ref_delta) as object_type ->
      let base_pos, data_start_pos =
        match object_type with
        | Commit | Tree | Blob | Tag -> assert false
        | Ofs_delta ->
          let rel_offset =
            read_variable_length_relative_offset t.pack_file_mmap ~pos:data_start_pos
          in
          ( pos - rel_offset
          , skip_variable_length_integer t.pack_file_mmap ~pos:data_start_pos )
        | Ref_delta ->
          Bigstring.To_bytes.blit
            ~src:t.pack_file_mmap
            ~src_pos:data_start_pos
            ~dst:(Sha1.Raw.Volatile.bytes sha1)
            ~dst_pos:0
            ~len:Sha1.Raw.length;
          let index = Find_result.Volatile.index_exn (find_sha1_index' t sha1) in
          pack_file_object_offset t ~index, data_start_pos + Sha1.Raw.length
      in
      let object_type = impl t ~pos:base_pos in
      Delta_object_parser.set_result_as_base t.delta_object_parser;
      feed_parser_data
        t.delta_object_parser
        ~expected_length:(object_length' t.pack_file_mmap ~pos)
        t.pack_file_mmap
        data_start_pos;
      Delta_object_parser.set_result_as_delta t.delta_object_parser;
      Delta_object_parser.compute_result t.delta_object_parser;
      object_type
  ;;
end

let read_raw_delta_object = Read_raw_delta_object_function.impl

module Read_raw_object_function = struct
  let sha1_context = Sha1.Compute.create_uninitialised ()
  let buf = Bigstring.create 32

  let impl (type a) (t : a t) ~index ~on_header ~on_payload =
    let pos = pack_file_object_offset t ~index in
    let object_type = read_raw_delta_object t ~pos in
    let len = Delta_object_parser.result_len t.delta_object_parser in
    on_header object_type ~size:len;
    on_payload (Delta_object_parser.result_buf t.delta_object_parser) ~pos:0 ~len;
    match t.sha1_validation with
    | Do_not_validate_sha1 -> ()
    | Validate_sha1 ->
      let sha1_context = Sha1.Compute.init_or_reset sha1_context in
      let header_len =
        Git_object_header_writer.write_from_left buf ~pos:0 object_type ~object_length:len
      in
      Sha1.Compute.process sha1_context buf ~pos:0 ~len:header_len;
      Sha1.Compute.process
        sha1_context
        (Delta_object_parser.result_buf t.delta_object_parser)
        ~pos:0
        ~len;
      let sha1_context = Sha1.Compute.finalise sha1_context in
      let actual_sha1 =
        Sha1.Hex.Volatile.non_volatile (Sha1.Compute.get_hex sha1_context)
      in
      let expected_sha1 = Sha1.Raw.Volatile.to_hex (sha1 t ~index) in
      if [%compare.equal: Sha1.Hex.t] actual_sha1 expected_sha1
      then ()
      else
        raise_s
          [%message
            "Unexpected_sha1" (actual_sha1 : Sha1.Hex.t) (expected_sha1 : Sha1.Hex.t)]
  ;;
end

let read_raw_object = Read_raw_object_function.impl

module Read_object_function = struct
  let feed_parser_data t pos =
    let (_ : int) =
      feed_parser_data
        t.base_object_parser
        ~process:Base_object_parser.process
        ~finalise:Base_object_parser.finalise
        t.pack_file_mmap
        ~pos
    in
    ()
  ;;

  let impl
        (type a)
        (t : a t)
        ~index
        ~on_blob_size
        ~on_blob_chunk
        ~on_commit
        ~on_tree_line
        ~on_tag
    =
    let pos = pack_file_object_offset t ~index in
    let payload_length = object_length' t.pack_file_mmap ~pos in
    let data_start_pos = skip_variable_length_integer t.pack_file_mmap ~pos in
    match object_type' t.pack_file_mmap ~pos with
    | Commit ->
      Base_object_parser.reset_for_reading_commit
        t.base_object_parser
        ~payload_length
        ~on_commit
        (match t.sha1_validation with
         | Do_not_validate_sha1 -> ()
         | Validate_sha1 -> Sha1.Raw.Volatile.to_hex (sha1 t ~index));
      feed_parser_data t data_start_pos
    | Tree ->
      Base_object_parser.reset_for_reading_tree
        t.base_object_parser
        ~payload_length
        ~on_tree_line
        (match t.sha1_validation with
         | Do_not_validate_sha1 -> ()
         | Validate_sha1 -> Sha1.Raw.Volatile.to_hex (sha1 t ~index));
      feed_parser_data t data_start_pos
    | Blob ->
      Base_object_parser.reset_for_reading_blob
        t.base_object_parser
        ~payload_length
        ~on_size:on_blob_size
        ~on_chunk:on_blob_chunk
        (match t.sha1_validation with
         | Do_not_validate_sha1 -> ()
         | Validate_sha1 -> Sha1.Raw.Volatile.to_hex (sha1 t ~index));
      feed_parser_data t data_start_pos
    | Tag ->
      Base_object_parser.reset_for_reading_tag
        t.base_object_parser
        ~payload_length
        ~on_tag
        (match t.sha1_validation with
         | Do_not_validate_sha1 -> ()
         | Validate_sha1 -> Sha1.Raw.Volatile.to_hex (sha1 t ~index));
      feed_parser_data t data_start_pos
    | Ofs_delta | Ref_delta ->
      let object_type = read_raw_delta_object t ~pos in
      Delta_object_parser.parse_result
        t.delta_object_parser
        object_type
        ~on_blob_size
        ~on_blob_chunk
        ~on_commit
        ~on_tree_line
        ~on_tag
        (match t.sha1_validation with
         | Do_not_validate_sha1 -> ()
         | Validate_sha1 -> Sha1.Raw.Volatile.to_hex (sha1 t ~index))
  ;;
end

let read_object = Read_object_function.impl

module For_testing = struct
  let print_out_pack_file pack_file =
    let%map t = create ~pack_file Validate_sha1 >>| ok_exn in
    printf "items in pack: %d\n" t.items_in_pack;
    printf "idx | %40s | pack file offset | object length | object type\n" "sha1";
    for index = 0 to t.items_in_pack - 1 do
      printf
        !"%3d | %{Sha1.Hex} | %16d | %13d | %{sexp: Pack_object_type.t}\n"
        (Find_result.Volatile.index_exn (find_sha1_index' t (sha1 t ~index)))
        (Sha1.Raw.Volatile.to_hex (sha1 t ~index))
        (pack_file_object_offset t ~index)
        (object_length t ~index)
        (object_type t ~index)
    done;
    for index = 0 to t.items_in_pack - 1 do
      printf !"\n%{Sha1.Hex}\n" (Sha1.Raw.Volatile.to_hex (sha1 t ~index));
      read_raw_object
        t
        ~index
        ~on_header:(fun object_type ~size ->
          printf !"Header data: %{Object_type} size %d\n" object_type size)
        ~on_payload:(fun (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) -> ());
      read_object
        t
        ~index
        ~on_blob_size:(fun size -> printf "Blob size: %d\n" size)
        ~on_blob_chunk:(fun buf ~pos ~len ->
          printf "Blob chunk: %S\n" (Bigstring.To_string.sub buf ~pos ~len))
        ~on_commit:(printf !"%{sexp: Commit.t}\n")
        ~on_tree_line:(fun file_mode sha1 ~name ->
          printf
            !"Tree line: %{sexp: File_mode.t} %{Sha1.Hex} %s\n"
            file_mode
            (Sha1.Raw.Volatile.to_hex sha1)
            name)
        ~on_tag:(printf !"%{sexp: Tag.t}\n")
    done
  ;;
end

let%expect_test "read pack" =
  let pack_contents =
    "PACK\000\000\000\002\000\000\000\005\198\tx\156\029\204A\n\
     \1940\016\133\225}N1{Q\210i\146\018\144\"z\005/0I\166%b\155\146\142\162\183\183\246\173~x\240\149\240\224(0$\029P\251\198z\199\206\248\193E2\022\027t\024[\210\145R\208I\027d%\223\133!\150i\202\162\132Fx\011\175{\141\\\225Z\198D\243\241V\243*\153f\184\147P-\249\005\231\176?\023\254\208\180<\249\180\001=4\214t\173G\139\029\028\2446\165\254\022l\150\250\001\017\151/U\154\012x\156\165\140A\n\
     \1940\016\000\239y\197\222EI\218$\018\144\"\250\005?\176\155l4`\026I\183\224\243-\250\004\2318\003#\157\025h\012\t9b\014\020\173v\137\025\143C\2066EJ\142\200xoF&\147\021\174\242h\029.\237\158p\222_{Y\164\224\0127\020\236\173\172p\162o9\243\027\235\235\201\135\216\234\004\198Y\239\131\025\180\133\157\222P\155\173E\132\255\030)\225E\224\183S\031\135HA\131;x\156K\203,*.QH\203\204I\229\002\000\025\199\003\243<x\156+NM\206\207KQH\203\204I\229\002\000\0302\004G\163\007x\156340031QHd0\176\255\217x\164c\135\2086\197\248\218\237\147x\223n\222:w\150!D2\137A&\210\169\234\142\183B\148:o\127#\179\128\165g\210\243Y\221&\006@\160\144\204pa\147{nT\254\241=o\253\243~8n\127\206\164\191\150e\178\161\017X2\133!c\239\235\213\203\191H\253_\184j\207\211\224\2273$f86\174\004\000\188\152.\205\184\131\178>\246Ls\189Y\t\202\243Ya)\203\b\022\240}"
  in
  let index_contents =
    "\255tOc\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\001\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\002\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\004\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\000\000\000\005\028YBz\220K \
     Z'\r\143\129\003\0169Ib\231\154\1390?\249\129\196\136\184\018\182!_}\183\146\r\237\179\181\157\154\172_6\128\023\231<\172Y\156}\253w\1896\218+\129n\175\179\157\174\202\249\188@]\238\167/\244\220\189[\177f\019\235\031\210\239\140q\004\022\243\139\223n\132\135c\004\134\131\014\220l\127\137\244h\223\248\191\197\209\135\177\130\255\231\214\159\229\193_\201O\000\000\001,\000\000\001\024\000\000\000\012\000\000\001A\000\000\000\150\184\131\178>\246Ls\189Y\t\202\243Ya)\203\b\022\240}|\216\027\2209\007\228\001\230\145\203R\132\166\189w\220\235\1864"
  in
  let pack_name = "pack-b883b23ef64c73bd5909caf3596129cb0816f07d" in
  Expect_test_helpers.with_temp_dir (fun dir ->
    let%bind () = Writer.save (dir ^/ pack_name ^ ".pack") ~contents:pack_contents in
    let%bind () = Writer.save (dir ^/ pack_name ^ ".idx") ~contents:index_contents in
    let%bind () = For_testing.print_out_pack_file (dir ^/ pack_name) in
    let%bind () =
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
          (description "test commit\n")) |}]
    in
    Deferred.unit)
;;
