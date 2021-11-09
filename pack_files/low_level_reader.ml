open! Core
open! Async
open! Import

let rec skip_variable_length_integer buf ~pos =
  if Bigstring.get_uint8 buf ~pos land 128 = 0
  then pos + 1
  else skip_variable_length_integer buf ~pos:(pos + 1)
;;

let read_variable_length_integer =
  let rec read_loop ~acc ~shift buf ~pos =
    let byte = Bigstring.get_uint8 buf ~pos in
    if byte land 128 = 0
    then acc lor (byte lsl shift)
    else
      read_loop
        ~acc:(acc lor ((byte land 127) lsl shift))
        ~shift:(shift + 7)
        buf
        ~pos:(pos + 1)
  in
  fun buf ~pos -> read_loop ~acc:0 ~shift:0 buf ~pos
;;
