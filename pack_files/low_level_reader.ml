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

let read_variable_length_relative_offset =
  let rec read_loop ~acc buf ~pos =
    let byte = Bigstring.get_uint8 buf ~pos in
    let acc = ((acc + 1) lsl 7) lor (byte land 127) in
    if byte land 128 = 0 then acc else read_loop ~acc buf ~pos:(pos + 1)
  in
  fun buf ~pos ->
    let first_byte = Bigstring.get_uint8 buf ~pos in
    let acc = first_byte land 127 in
    if first_byte land 128 = 0 then acc else read_loop ~acc buf ~pos:(pos + 1)
;;

let object_type buf ~pos : Pack_object_type.packed =
  let type_int = (Bigstring.get_uint8 buf ~pos lsr 4) land 7 in
  match type_int with
  | 1 -> T Commit
  | 2 -> T Tree
  | 3 -> T Blob
  | 4 -> T Tag
  | 6 -> T Ofs_delta
  | 7 -> T Ref_delta
  | _ -> raise_s [%message "Invalid pack object type" (type_int : int)]
;;

let object_length buf ~pos =
  let first_byte = Bigstring.get_uint8 buf ~pos in
  if first_byte land 128 = 0
  then first_byte land 15
  else first_byte land 15 lor (read_variable_length_integer buf ~pos:(pos + 1) lsl 4)
;;
