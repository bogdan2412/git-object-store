module Find_result : Find_result.Public = Find_result
module Reader = Pack_reader
module Writer = Pack_writer

module Low_level = struct
  module Index_reader = Index_reader
  module Reverse_index_reader = Reverse_index_reader

  module Util = struct
    open Util

    let with_resource = with_resource
    let with_file = with_file

    module Make_sha1_binary_search = Make_sha1_binary_search
  end
end
