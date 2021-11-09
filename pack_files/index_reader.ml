open! Core
open! Async
open! Import

type t =
  { index_file : string
  ; fd : Fd.t
  ; file_size : int
  ; file_mmap : Bigstring.t
  ; offsets : Index_offsets.t
  }

let open_existing ~pack_file ~pack_file_mmap ~pack_file_size ~items_in_pack =
  let index_file = String.chop_suffix_exn pack_file ~suffix:".pack" ^ ".idx" in
  Util.with_file index_file ~f:(fun fd file_size file_mmap ->
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
        ; offsets = Index_offsets.create ~items_in_pack
        }
    in
    Deferred.return result)
;;
