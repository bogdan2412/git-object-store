open! Core
open! Import

type t =
  | Non_executable_file
  | Executable_file
  | Link
  | Directory
  | Git_submodule
[@@deriving sexp]
