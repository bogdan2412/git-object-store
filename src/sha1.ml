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

module Hex = struct
  module T = struct
    type t = string [@@deriving compare, hash, sexp_of]

    let length = 40

    let of_string string =
      if String.length string <> length
      then
        raise_s
          [%message
            "Expected string of different length"
              ~expected_length:(length : int)
              ~actual_length:(String.length string : int)];
      if not
           (String.for_all string ~f:(fun char ->
              Char.between char ~low:'0' ~high:'9'
              || Char.between char ~low:'a' ~high:'f'))
      then failwith "Expected string made only out of characters [0-9a-f]";
      string
    ;;

    let to_string = Fn.id
    let t_of_sexp = Fn.compose of_string [%of_sexp: string]
  end

  include T
  include Hashable.Make (T)

  module Volatile = struct
    type t = bytes [@@deriving sexp_of]

    let create () = Bytes.create length
    let non_volatile t = of_string (Bytes.to_string t)
    let bytes = Fn.id

    let is_valid t =
      let valid = ref true in
      for idx = 0 to length - 1 do
        let char = Bytes.get t idx in
        if not
             (Char.between char ~low:'0' ~high:'9'
              || Char.between char ~low:'a' ~high:'f')
        then valid := false
      done;
      !valid
    ;;

    let to_string = Bytes.to_string
    let of_string = Fn.compose Bytes.of_string of_string
    let t_of_sexp = Fn.compose of_string [%of_sexp: string]
  end
end

module Raw = struct
  let length = 20

  module T = struct
    type t = string [@@deriving compare, sexp_of]

    let of_string string =
      if String.length string <> length
      then
        raise_s
          [%message
            "Expected string of different length"
              ~expected_length:(length : int)
              ~actual_length:(String.length string : int)];
      string
    ;;

    let to_string = Fn.id
    let t_of_sexp = Fn.compose of_string [%of_sexp: string]
  end

  include T
  include Comparable.Make (T)

  let char_hex n =
    Char.unsafe_of_int
      (n + if Int.(n < 10) then Char.to_int '0' else Char.to_int 'a' - 10)
  ;;

  let to_hex_volatile' idx t result =
    for i = 0 to length - 1 do
      let x = Char.to_int (idx t i) in
      Bytes.unsafe_set result (i * 2) (char_hex (x lsr 4));
      Bytes.unsafe_set result ((i * 2) + 1) (char_hex (x land 0x0f))
    done
  ;;

  let to_hex' idx t =
    let result = Hex.Volatile.create () in
    to_hex_volatile' idx t result;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:result
  ;;

  let digit_of_hex c =
    match c with
    | '0' .. '9' -> Char.to_int c - Char.to_int '0'
    | 'a' .. 'f' -> Char.to_int c - Char.to_int 'a' + 10
    | _ -> raise (Invalid_argument "Sha1.Raw.of_hex")
  ;;

  let byte_of_hex idx hex i =
    Char.unsafe_of_int ((digit_of_hex (idx hex i) lsl 4) + digit_of_hex (idx hex (i + 1)))
  ;;

  let of_hex' hex result =
    for i = 0 to length - 1 do
      Bytes.set result i (byte_of_hex String.get hex (2 * i))
    done
  ;;

  let of_hex_volatile' hex result =
    for i = 0 to length - 1 do
      Bytes.set result i (byte_of_hex Bytes.get hex (2 * i))
    done
  ;;

  module Volatile = struct
    type t = bytes [@@deriving sexp_of]

    let create () = Bytes.create length
    let non_volatile t = of_string (Bytes.to_string t)
    let bytes = Fn.id
    let to_hex_volatile = to_hex_volatile' Bytes.get
    let to_hex = to_hex' Bytes.get
    let of_hex hex t = of_hex' hex t
    let of_hex_volatile hex t = of_hex_volatile' hex t
    let to_string = Bytes.to_string
    let of_string = Fn.compose Bytes.of_string of_string
    let t_of_sexp = Fn.compose of_string [%of_sexp: string]
  end

  let to_hex_volatile = to_hex_volatile' String.get
  let to_hex = to_hex' String.get

  let of_hex hex =
    let result = Volatile.create () in
    of_hex' hex result;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:result
  ;;

  let of_hex_volatile hex =
    let result = Volatile.create () in
    of_hex_volatile' hex result;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:result
  ;;
end

module Compute = struct
  type _ t =
    { sha1_ctx : Digestif_native.ctx
    ; result_raw : Raw.Volatile.t
    ; result_hex : Hex.Volatile.t
    }

  let create_uninitialised () : [`Uninitialised] t =
    { sha1_ctx = Bytes.create (Digestif_native.SHA1.ctx_size ())
    ; result_raw = Raw.Volatile.create ()
    ; result_hex = Hex.Volatile.create ()
    }
  ;;

  let init_or_reset (t : _ t) =
    Digestif_native.SHA1.Bigstring.init t.sha1_ctx;
    (t :> [`Initialised] t)
  ;;

  let process (t : [`Initialised] t) buf ~pos ~len =
    Digestif_native.SHA1.Bigstring.update t.sha1_ctx buf pos len
  ;;

  let finalise (t : [`Initialised] t) =
    Digestif_native.SHA1.Bytes.finalize t.sha1_ctx t.result_raw 0;
    Raw.Volatile.to_hex_volatile t.result_raw t.result_hex;
    (t :> [`Finalised] t)
  ;;

  let get_raw (t : [`Finalised] t) = t.result_raw
  let get_hex (t : [`Finalised] t) = t.result_hex
end
