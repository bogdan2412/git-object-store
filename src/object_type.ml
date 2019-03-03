open! Core

module T = struct
  type t =
    | Commit
    | Tree
    | Blob
    | Tag
  [@@deriving sexp]
end

include T
include Sexpable.To_stringable (T)
