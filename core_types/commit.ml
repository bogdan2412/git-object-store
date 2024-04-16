(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2024  Bogdan-Cristian Tataroiu

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
  { tree : Sha1.Hex.t
  ; parents : Sha1.Hex.t list
  ; author : Author_line.t
  ; committer : Author_line.t
  ; encoding : string option
  ; merge_tags : Tag.t list
  ; gpg_signature : string option
  ; description : string
  }
[@@deriving sexp]

let split_first_line_exn string = String.lsplit2_exn ~on:'\n' string

let rec parse_parent_lines_exn acc string =
  let line, rest = split_first_line_exn string in
  match String.chop_prefix line ~prefix:"parent " with
  | Some parent -> parse_parent_lines_exn (Sha1.Hex.of_string parent :: acc) rest
  | None -> List.rev acc, string
;;

let rec parse_multiline_field_loop acc string =
  let first_line, rest = split_first_line_exn string in
  match String.chop_prefix ~prefix:" " first_line with
  | None -> String.concat ~sep:"\n" (List.rev acc), string
  | Some line -> parse_multiline_field_loop (line :: acc) rest
;;

let rec parse_merge_tag_fields acc string =
  match String.chop_prefix ~prefix:"mergetag" string with
  | None -> List.rev acc, string
  | Some merge_tag_and_rest ->
    let merge_tag, rest = parse_multiline_field_loop [] merge_tag_and_rest in
    let merge_tag = Tag.parse_git_object_payload_exn merge_tag in
    parse_merge_tag_fields (merge_tag :: acc) rest
;;

let parse_git_object_payload_exn rest =
  let tree, rest = split_first_line_exn rest in
  let tree = Sha1.Hex.of_string (String.chop_prefix_exn ~prefix:"tree " tree) in
  let parents, rest = parse_parent_lines_exn [] rest in
  let author, rest = split_first_line_exn rest in
  let author = Author_line.parse_exn (String.chop_prefix_exn ~prefix:"author " author) in
  let committer, rest = split_first_line_exn rest in
  let committer =
    Author_line.parse_exn (String.chop_prefix_exn ~prefix:"committer " committer)
  in
  let encoding, rest =
    match String.chop_prefix ~prefix:"encoding " rest with
    | None -> None, rest
    | Some encoding_and_rest ->
      let encoding, rest = split_first_line_exn encoding_and_rest in
      Some encoding, rest
  in
  let merge_tags, rest = parse_merge_tag_fields [] rest in
  let gpg_signature, rest =
    match String.chop_prefix ~prefix:"gpgsig" rest with
    | None -> None, rest
    | Some gpgsig_and_rest ->
      let gpgsig, rest = parse_multiline_field_loop [] gpgsig_and_rest in
      Some gpgsig, rest
  in
  let empty_line, description = split_first_line_exn rest in
  assert (String.is_empty empty_line);
  { tree; parents; author; committer; encoding; merge_tags; gpg_signature; description }
;;

let format_as_git_object_payload t =
  String.concat
    ~sep:"\n"
    (List.concat
       [ [ "tree " ^ Sha1.Hex.to_string t.tree ]
       ; List.map t.parents ~f:(fun parent -> "parent " ^ Sha1.Hex.to_string parent)
       ; [ Author_line.format t.author ~line_prefix:"author" ]
       ; [ Author_line.format t.committer ~line_prefix:"committer" ]
       ; (match t.encoding with
          | None -> []
          | Some encoding -> [ "encoding " ^ encoding ])
       ; List.map t.merge_tags ~f:(fun tag ->
           let tag = Tag.format_as_git_object_payload tag in
           "mergetag " ^ String.concat ~sep:"\n " (String.split tag ~on:'\n'))
       ; (match t.gpg_signature with
          | None -> []
          | Some signature ->
            [ "gpgsig " ^ String.concat ~sep:"\n " (String.split signature ~on:'\n') ])
       ; [ ""; t.description ]
       ])
;;

module For_testing = struct
  let example_git_object_payload =
    "tree 2ee0644233b67fb9e83da4d4183cd65e076a1115\n\
     parent 46f17af77006c41c0e20556a949aa7fc4a14bed0\n\
     parent a9b129d414fcb4d596eba78b870e1f780b60b091\n\
     author Bogdan-Cristian Tataroiu <bogdan@example.com> 1543759434 -0500\n\
     committer Bogdan-Cristian Tataroiu <bogdan@example.com> 1543759434 -0500\n\
     encoding iso-8859-8\n\
     mergetag object fd0b2091596e649f6ca4521262c3a0cadb0d042e\n\
    \ type commit\n\
    \ tag vtest\n\
    \ tagger Bogdan-Cristian Tataroiu <bogdan@example.com> 1547392527 +0000\n\
    \ \n\
    \ test tag\n\
     gpgsig -----BEGIN PGP SIGNATURE-----\n\
    \ Version: GnuPG v1.4\n\
    \ \n\
    \ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n\
    \ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n\
    \ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n\
    \ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n\
    \ XXXXXXXX\n\
    \ -----END PGP SIGNATURE-----\n\n\
     Merge branch 'branch'\n"
  ;;

  let example_commit = parse_git_object_payload_exn example_git_object_payload

  let%expect_test "round-trip" =
    Expect_test_time_zone.with_fixed_time_zone (fun () ->
      printf !"%{sexp: t}\n" example_commit;
      [%expect
        {|
        ((tree 2ee0644233b67fb9e83da4d4183cd65e076a1115)
         (parents
          (46f17af77006c41c0e20556a949aa7fc4a14bed0
           a9b129d414fcb4d596eba78b870e1f780b60b091))
         (author
          ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
           (timestamp (2018-12-02 09:03:54.000000000-05:00)) (zone UTC-5)))
         (committer
          ((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
           (timestamp (2018-12-02 09:03:54.000000000-05:00)) (zone UTC-5)))
         (encoding (iso-8859-8))
         (merge_tags
          (((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e)
            (object_type Commit) (tag vtest)
            (tagger
             (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
               (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC+0))))
            (description "test tag"))))
         (gpg_signature
          ( "-----BEGIN PGP SIGNATURE-----\
           \nVersion: GnuPG v1.4\
           \n\
           \nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
           \nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
           \nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
           \nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\
           \nXXXXXXXX\
           \n-----END PGP SIGNATURE-----"))
         (description "Merge branch 'branch'\n")) |}];
      let as_payload = format_as_git_object_payload example_commit in
      printf !"%s\n" as_payload;
      [%expect
        {|
        tree 2ee0644233b67fb9e83da4d4183cd65e076a1115
        parent 46f17af77006c41c0e20556a949aa7fc4a14bed0
        parent a9b129d414fcb4d596eba78b870e1f780b60b091
        author Bogdan-Cristian Tataroiu <bogdan@example.com> 1543759434 -0500
        committer Bogdan-Cristian Tataroiu <bogdan@example.com> 1543759434 -0500
        encoding iso-8859-8
        mergetag object fd0b2091596e649f6ca4521262c3a0cadb0d042e
         type commit
         tag vtest
         tagger Bogdan-Cristian Tataroiu <bogdan@example.com> 1547392527 +0000

         test tag
        gpgsig -----BEGIN PGP SIGNATURE-----
         Version: GnuPG v1.4

         XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
         XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
         XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
         XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
         XXXXXXXX
         -----END PGP SIGNATURE-----

        Merge branch 'branch' |}];
      [%test_eq: string] example_git_object_payload as_payload)
  ;;
end
