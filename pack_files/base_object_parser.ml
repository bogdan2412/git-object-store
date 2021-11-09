open! Core
open! Async
open! Import

type 'Sha1_validation t =
  { object_parser : 'Sha1_validation Object_parser.t
  ; zlib_inflate : Zlib.Inflate.t
  ; mutable expected_sha1 : 'Sha1_validation
  }

let create (type a) (sha1_validation : a Sha1_validation.t) : a t =
  let object_parser =
    Object_parser.create
      ~on_blob_size:(fun (_ : int) -> ())
      ~on_blob_chunk:(fun (_ : Bigstring.t) ~pos:(_ : int) ~len:(_ : int) -> ())
      ~on_commit:(fun (_ : Commit.t) -> ())
      ~on_tree_line:(fun (_ : File_mode.t) (_ : Sha1.Raw.Volatile.t) ~name:(_ : string) ->
        ())
      ~on_tag:(fun (_ : Tag.t) -> ())
      ~on_error:Error.raise
      sha1_validation
  in
  let zlib_inflate =
    Zlib.Inflate.create_uninitialised ~on_data_chunk:(fun buf ~pos ~len ->
      Object_parser.append_data object_parser buf ~pos ~len)
  in
  match sha1_validation with
  | Do_not_validate_sha1 -> { object_parser; zlib_inflate; expected_sha1 = () }
  | Validate_sha1 ->
    { object_parser
    ; zlib_inflate
    ; expected_sha1 = Sha1.Hex.of_string "0000000000000000000000000000000000000000"
    }
;;

let reset_for_reading
      t
      object_type
      ~payload_length
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      expected_sha1
  =
  Zlib.Inflate.init_or_reset t.zlib_inflate;
  Object_parser.set_callback_and_reset_for_reading_object_type
    t.object_parser
    object_type
    ~payload_length
    ~on_blob_size
    ~on_blob_chunk
    ~on_commit
    ~on_tree_line
    ~on_tag;
  t.expected_sha1 <- expected_sha1
;;

let process t buf ~pos ~len = Zlib.Inflate.process t.zlib_inflate buf ~pos ~len

let finalise t =
  Zlib.Inflate.finalise t.zlib_inflate;
  Object_parser.finalise t.object_parser t.expected_sha1
;;
