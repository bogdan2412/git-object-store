module Reader = Pack_reader
module Writer = Pack_writer

module Low_level = struct
  module Find_result = Find_result
  module Index_reader = Index_reader
  module Reverse_index_reader = Reverse_index_reader

  module Util = struct
    open Util

    let with_resource = with_resource
    let with_file = with_file

    module Make_sha1_binary_search = Make_sha1_binary_search
  end
end

module Find_result : Find_result.Public with type Volatile.t = Find_result.Volatile.t =
  Find_result
