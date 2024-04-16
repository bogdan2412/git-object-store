(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2020-2024  Bogdan-Cristian Tataroiu

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
open! Async
open! Import

module Bitset : sig
  type t

  val create : capacity:int -> t
  val capacity : t -> int
  val get : t -> int -> bool
  val set : t -> int -> bool -> unit
end = struct
  type t = Bytes.t

  let create ~capacity = Bytes.make ((capacity + 7) / 8) '\000'
  let[@inline] capacity t = Bytes.length t lsl 3
  let[@inline] byte index = index lsr 3
  let[@inline] bit index = 1 lsl (index land 7)
  let[@inline] get t index = (Bytes.get t (byte index) |> Char.to_int) land bit index <> 0

  let[@inline] set t index what =
    let current = Bytes.get t (byte index) |> Char.to_int in
    let new_ =
      match what with
      | true -> current lor bit index
      | false -> current land lnot (bit index)
    in
    Bytes.set t (byte index) (Char.unsafe_of_int new_)
  ;;
end

module Bitset_tree : sig
  type t

  val create : capacity:int -> t
  val get : t -> int -> bool
  val set : t -> int -> bool -> unit
  val has_unused : t -> bool
  val next_unused_exn : t -> int
end = struct
  type t = Bitset.t

  let create ~capacity = Bitset.create ~capacity:(capacity * 2)
  let[@inline] capacity t = Bitset.capacity t lsr 1

  let rec get' t index pos left right =
    if left = right
    then (
      assert (index = left);
      Bitset.get t pos)
    else (
      let mid = (left + right) lsr 1 in
      let poslc = (pos lsl 1) + 1 in
      let posrc = (pos lsl 1) + 2 in
      if index <= mid
      then get' t index poslc left mid
      else get' t index posrc (mid + 1) right)
  ;;

  let get t index = get' t index 0 0 (capacity t - 1)

  let rec set' t index what pos left right =
    if left = right
    then (
      assert (index = left);
      Bitset.set t pos what)
    else (
      let mid = (left + right) lsr 1 in
      let poslc = (pos lsl 1) + 1 in
      let posrc = (pos lsl 1) + 2 in
      if index <= mid
      then set' t index what poslc left mid
      else set' t index what posrc (mid + 1) right;
      Bitset.set t pos (Bitset.get t poslc && Bitset.get t posrc))
  ;;

  let set t index what = set' t index what 0 0 (capacity t - 1)
  let has_unused t = not (Bitset.get t 0)

  let rec next_unused_exn' t pos left right =
    if left = right
    then (
      assert (not (Bitset.get t pos));
      left)
    else (
      let mid = (left + right) lsr 1 in
      let poslc = (pos lsl 1) + 1 in
      let posrc = (pos lsl 1) + 2 in
      if not (Bitset.get t poslc)
      then next_unused_exn' t poslc left mid
      else next_unused_exn' t posrc (mid + 1) right)
  ;;

  let next_unused_exn t = next_unused_exn' t 0 0 (capacity t - 1)
end

type 'a t =
  { mutable values : 'a array
  ; mutable used : Bitset_tree.t
  ; empty_value : 'a
  }

module Handle = struct
  type 'a t = int

  let to_int = Fn.id
  let unsafe_of_int = Fn.id
end

let create ~initial_capacity ~empty_value =
  { values = Array.create ~len:initial_capacity empty_value
  ; used = Bitset_tree.create ~capacity:initial_capacity
  ; empty_value
  }
;;

let double_capacity t =
  let length = Array.length t.values in
  let new_values =
    Array.init (length * 2) ~f:(fun index ->
      if index < length then t.values.(index) else t.empty_value)
  in
  let new_used = Bitset_tree.create ~capacity:(length * 2) in
  for index = 0 to length - 1 do
    Bitset_tree.set new_used index (Bitset_tree.get t.used index)
  done;
  t.values <- new_values;
  t.used <- new_used
;;

let[@inline] lease t value =
  if not (Bitset_tree.has_unused t.used) then double_capacity t;
  let handle = Bitset_tree.next_unused_exn t.used in
  Bitset_tree.set t.used handle true;
  t.values.(handle) <- value;
  handle
;;

let[@cold] assert_used_failed () =
  raise_s [%message "Tried to use a handle which was previously released"]
;;

let[@inline] assert_used t handle =
  if not (Bitset_tree.get t.used handle) then assert_used_failed ()
;;

let[@inline] get t handle =
  assert_used t handle;
  t.values.(handle)
;;

let[@inline] set t handle value =
  assert_used t handle;
  t.values.(handle) <- value
;;

let[@inline] release t handle =
  assert_used t handle;
  Bitset_tree.set t.used handle false
;;
