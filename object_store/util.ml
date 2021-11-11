open! Core
open! Async
open! Import

let validate_preferred_pack ~pack_directory ~preferred_pack =
  Option.map preferred_pack ~f:(fun preferred_pack ->
    let preferred_pack =
      match Filename.is_posix_pathname_component preferred_pack with
      | true -> pack_directory ^/ preferred_pack
      | false -> preferred_pack
    in
    (match
       String.( = )
         (Filename_unix.realpath pack_directory)
         (Filename_unix.realpath (Filename.dirname preferred_pack))
     with
     | false ->
       raise_s
         [%message
           "Preferred pack file not within pack directory"
             (pack_directory : string)
             (preferred_pack : string)]
     | true -> ());
    match String.is_suffix ~suffix:".pack" preferred_pack with
    | false ->
      raise_s
        [%message
          "Expected preferred pack file to end with .pack extension"
            (preferred_pack : string)]
    | true -> preferred_pack)
;;
