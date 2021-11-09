module Commit = Git_core_types.Commit
module Crc32 = Git_bindings_crc32
module Expect_test_time_zone = Git_core_types.Expect_test_time_zone
module File_mode = Git_core_types.File_mode
module Object_header_writer = Git_object_files.Header_writer
module Object_reader = Git_object_files.Reader
module Object_type = Git_core_types.Object_type
module Sha1 = Git_bindings_sha1
module Sha1_validation = Git_core_types.Sha1_validation
module Tag = Git_core_types.Tag
module Zlib = Git_bindings_zlib

module Object_parser = struct
  include Git_object_files.Parser

  let set_callback_and_reset_for_reading_object_type
        t
        object_type
        ~payload_length
        ~on_blob_size
        ~on_blob_chunk
        ~on_commit
        ~on_tree_line
        ~on_tag
    =
    match (object_type : Object_type.t) with
    | Commit ->
      set_on_commit t on_commit;
      reset_for_reading_commit t ~payload_length
    | Tree ->
      set_on_tree_line t on_tree_line;
      reset_for_reading_tree t ~payload_length
    | Blob ->
      set_on_blob t ~on_size:on_blob_size ~on_chunk:on_blob_chunk;
      reset_for_reading_blob t ~payload_length
    | Tag ->
      set_on_tag t on_tag;
      reset_for_reading_tag t ~payload_length
  ;;
end
