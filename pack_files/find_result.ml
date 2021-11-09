open! Core
open! Async
open! Import
include Find_result_intf

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
