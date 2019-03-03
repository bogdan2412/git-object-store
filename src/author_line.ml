(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019  Bogdan-Cristian Tataroiu

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

open Core

type t =
  { name : string
  ; email : string
  ; timestamp : Time_ns.t
  ; zone : Time_ns.Zone.t
  }
[@@deriving sexp_of]

let parse_git_time_zone_exn string =
  assert (String.length string = 5);
  assert (string.[0] = '+' || string.[0] = '-');
  let sign, rest = string.[0], String.sub string ~pos:1 ~len:4 in
  assert (String.for_all rest ~f:(Char.between ~low:'0' ~high:'9'));
  let hours =
    ((Char.to_int rest.[0] - Char.to_int '0') * 10)
    + (Char.to_int rest.[1] - Char.to_int '0')
  in
  Time_ns.Zone.of_string (sprintf "UTC%c%d" sign hours)
;;

let parse_exn line =
  let line, zone = String.rsplit2_exn ~on:' ' line in
  let zone = parse_git_time_zone_exn zone in
  let line, timestamp = String.rsplit2_exn ~on:' ' line in
  let timestamp =
    Time_ns.of_span_since_epoch (Time_ns.Span.of_int_sec (Int.of_string timestamp))
  in
  let line = String.chop_suffix_exn ~suffix:">" line in
  let name_email_split = String.substr_index_exn ~pattern:" <" line in
  let name = String.sub line ~pos:0 ~len:name_email_split in
  let email =
    String.sub
      line
      ~pos:(name_email_split + 2)
      ~len:(String.length line - name_email_split - 2)
  in
  { name; email; timestamp; zone }
;;

let format { name; email; timestamp; zone } ~line_prefix =
  let zone_str =
    let sec_offset = Time_ns.utc_offset timestamp ~zone |> Time_ns.Span.to_int_sec in
    let sign, sec_offset =
      if sec_offset >= 0 then "+", sec_offset else "-", -sec_offset
    in
    let min_offset = sec_offset / 60 in
    sprintf "%s%02d%02d" sign (min_offset / 60) (min_offset % 60)
  in
  sprintf
    "%s %s <%s> %d %s"
    line_prefix
    name
    email
    (Time_ns.Span.to_int_sec (Time_ns.to_span_since_epoch timestamp))
    zone_str
;;
