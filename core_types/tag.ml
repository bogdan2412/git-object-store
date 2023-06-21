(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2023  Bogdan-Cristian Tataroiu

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
open! Import

type t =
  { object_sha1 : Sha1.Hex.t
  ; object_type : Object_type.t
  ; tag : string
  ; tagger : Author_line.t option
  ; description : string
  }
[@@deriving sexp]

let split_first_line_exn string = String.lsplit2_exn ~on:'\n' string

let parse_git_object_payload_exn rest =
  let object_sha1, rest = split_first_line_exn rest in
  let object_sha1 =
    Sha1.Hex.of_string (String.chop_prefix_exn ~prefix:"object " object_sha1)
  in
  let object_type, rest = split_first_line_exn rest in
  let object_type =
    Object_type.of_string (String.chop_prefix_exn ~prefix:"type " object_type)
  in
  let tag, rest = split_first_line_exn rest in
  let tag = String.chop_prefix_exn ~prefix:"tag " tag in
  let tagger, rest =
    match String.chop_prefix ~prefix:"tagger " rest with
    | None -> None, rest
    | Some tagger_and_rest ->
      let tagger, rest = split_first_line_exn tagger_and_rest in
      Some (Author_line.parse_exn tagger), rest
  in
  let empty_line, description = split_first_line_exn rest in
  assert (String.is_empty empty_line);
  { object_sha1; object_type; tag; tagger; description }
;;

let format_as_git_object_payload t =
  String.concat
    ~sep:"\n"
    (List.filter_opt
       [ Some ("object " ^ Sha1.Hex.to_string t.object_sha1)
       ; Some ("type " ^ String.lowercase (Object_type.to_string t.object_type))
       ; Some ("tag " ^ t.tag)
       ; Option.map t.tagger ~f:(Author_line.format ~line_prefix:"tagger")
       ; Some ""
       ; Some t.description
       ])
;;

module For_testing = struct
  let example_git_object_payload =
    "object fd0b2091596e649f6ca4521262c3a0cadb0d042e\n\
     type commit\n\
     tag vtest\n\
     tagger Bogdan-Cristian Tataroiu <bogdan@example.com> 1547392527 +0000\n\n\
     test tag\n"
  ;;

  let example_tag = parse_git_object_payload_exn example_git_object_payload

  let%expect_test "round-trip" =
    Expect_test_time_zone.with_fixed_time_zone (fun () ->
      printf !"%{sexp: t}\n" example_tag;
      [%expect
        {|
           ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
            (tag vtest)
            (tagger
             (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
               (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC+0))))
            (description "test tag\n")) |}];
      let as_payload = format_as_git_object_payload example_tag in
      printf !"%s\n" as_payload;
      [%expect
        {|
           object fd0b2091596e649f6ca4521262c3a0cadb0d042e
           type commit
           tag vtest
           tagger Bogdan-Cristian Tataroiu <bogdan@example.com> 1547392527 +0000

           test tag |}];
      [%test_eq: string] example_git_object_payload as_payload)
  ;;
end
