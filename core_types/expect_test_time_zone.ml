open! Core
open! Async
open! Import

let with_fixed_time_zone f =
  let old_zone = Time_ns.get_sexp_zone () in
  Time_ns.set_sexp_zone (Time_ns.Zone.of_string "America/New_York");
  let result = f () in
  Time_ns.set_sexp_zone old_zone;
  result
;;

let with_fixed_time_zone_async f =
  let old_zone = Time_ns.get_sexp_zone () in
  Time_ns.set_sexp_zone (Time_ns.Zone.of_string "America/New_York");
  let%map result = f () in
  Time_ns.set_sexp_zone old_zone;
  result
;;
