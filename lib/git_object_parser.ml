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

module State : sig
  module View : sig
    type t = private
      | Before_type_read
      | Reading_blob of { mutable payload_length : int }
      | Reading_commit of { mutable payload_length : int }
      | Reading_tree of
          { mutable payload_length : int
          ; parser_state : Tree.Git_object_payload_parser.State.t
          }
      | Reading_tag of { mutable payload_length : int }
      | Done
      | Error of { mutable error : Error.t }
    [@@deriving sexp_of]
  end

  type t [@@deriving sexp_of]

  val create
    :  emit_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
    -> t

  val view : t -> View.t
  val set_before_type_read : t -> unit
  val set_reading_blob : t -> payload_length:int -> unit
  val set_reading_commit : t -> payload_length:int -> unit
  val set_reading_tree : t -> payload_length:int -> unit
  val set_reading_tag : t -> payload_length:int -> unit
  val set_done : t -> unit
  val set_error : t -> Error.t -> unit
end = struct
  module View = struct
    type t =
      | Before_type_read
      | Reading_blob of { mutable payload_length : int }
      | Reading_commit of { mutable payload_length : int }
      | Reading_tree of
          { mutable payload_length : int
          ; parser_state : Tree.Git_object_payload_parser.State.t
          }
      | Reading_tag of { mutable payload_length : int }
      | Done
      | Error of { mutable error : Error.t }
    [@@deriving sexp_of]

    let sexp_of_t = function
      | Error { error } -> [%message "Error" ~_:(error : Error.t)]
      | t -> sexp_of_t t
    ;;
  end

  type t =
    { mutable value : View.t
    ; reading_blob : View.t
    ; reading_commit : View.t
    ; reading_tree : View.t
    ; reading_tag : View.t
    ; error : View.t
    }

  let sexp_of_t t = View.sexp_of_t t.value

  let create ~emit_tree_line =
    let reading_blob = View.Reading_blob { payload_length = 0 } in
    let reading_commit = View.Reading_commit { payload_length = 0 } in
    let reading_tree =
      View.Reading_tree
        { payload_length = 0
        ; parser_state = Tree.Git_object_payload_parser.State.create ~emit_tree_line
        }
    in
    let reading_tag = View.Reading_tag { payload_length = 0 } in
    let error = View.Error { error = Error.of_string "uninitialised" } in
    { value = Before_type_read
    ; reading_blob
    ; reading_commit
    ; reading_tree
    ; reading_tag
    ; error
    }
  ;;

  let view t = t.value
  let set_before_type_read t = t.value <- Before_type_read

  let set_reading_blob t ~payload_length =
    (match t.reading_blob with
     | Reading_blob record -> record.payload_length <- payload_length
     | _ -> assert false);
    t.value <- t.reading_blob
  ;;

  let set_reading_commit t ~payload_length =
    (match t.reading_commit with
     | Reading_commit record -> record.payload_length <- payload_length
     | _ -> assert false);
    t.value <- t.reading_commit
  ;;

  let set_reading_tree t ~payload_length =
    (match t.reading_tree with
     | Reading_tree record ->
       record.payload_length <- payload_length;
       Tree.Git_object_payload_parser.State.reset record.parser_state
     | _ -> assert false);
    t.value <- t.reading_tree
  ;;

  let set_reading_tag t ~payload_length =
    (match t.reading_tag with
     | Reading_tag record -> record.payload_length <- payload_length
     | _ -> assert false);
    t.value <- t.reading_tag
  ;;

  let set_done t = t.value <- Done

  let set_error t error =
    (match t.error with
     | Error record -> record.error <- error
     | _ -> assert false);
    t.value <- t.error
  ;;
end

type t =
  { mutable buf : Bigstring.t
  ; mutable pos : int
  ; mutable len : int
  ; mutable total_payload_read : int
  ; state : State.t
  ; mutable on_blob_chunk : Bigstring.t -> pos:int -> len:int -> unit
  ; mutable on_commit : Commit.t -> unit
  ; mutable on_tree_line : File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit
  ; mutable on_tag : Tag.t -> unit
  ; on_error : Error.t -> unit
  }
[@@deriving fields]

let sexp_of_t
      { buf
      ; pos
      ; len
      ; total_payload_read
      ; state
      ; on_blob_chunk = _
      ; on_commit = _
      ; on_tree_line = _
      ; on_tag = _
      ; on_error = _
      }
  =
  [%sexp
    { data = (Bigstring.to_string ~pos ~len buf : string)
    ; total_payload_read : int
    ; state : State.t
    }]
;;

let create ~initial_buffer_size ~on_blob_chunk ~on_commit ~on_tree_line ~on_tag ~on_error
  =
  let rec state =
    lazy
      (let state =
         State.create ~emit_tree_line:(fun file_mode sha1 ~name ->
           (force t).on_tree_line file_mode sha1 ~name)
       in
       State.set_before_type_read state;
       state)
  and t =
    lazy
      { buf = Bigstring.create initial_buffer_size
      ; pos = 0
      ; len = 0
      ; total_payload_read = 0
      ; state = force state
      ; on_blob_chunk
      ; on_commit
      ; on_tree_line
      ; on_tag
      ; on_error
      }
  in
  force t
;;

let reset t =
  Fields.Direct.iter
    t
    ~buf:(fun _ _ _ -> ())
    ~pos:(fun _ t _ -> t.pos <- 0)
    ~len:(fun _ t _ -> t.len <- 0)
    ~total_payload_read:(fun _ t _ -> t.total_payload_read <- 0)
    ~state:(fun _ t _ -> State.set_before_type_read t.state)
    ~on_blob_chunk:(fun _ _ _ -> ())
    ~on_commit:(fun _ _ _ -> ())
    ~on_tree_line:(fun _ _ _ -> ())
    ~on_tag:(fun _ _ _ -> ())
    ~on_error:(fun _ _ _ -> ())
;;

let error t error =
  t.on_error error;
  State.set_error t.state error
;;

let set_state_reading_blob t ~payload_length =
  State.set_reading_blob t.state ~payload_length
;;

let set_state_reading_tree t ~payload_length =
  State.set_reading_tree t.state ~payload_length
;;

let set_state_reading_commit t ~payload_length =
  State.set_reading_commit t.state ~payload_length
;;

let set_state_reading_tag t ~payload_length =
  State.set_reading_tag t.state ~payload_length
;;

let read_header_payload_length t ~from_offset ~on_length_read =
  if from_offset < t.len
  then
    if Bigstring.get t.buf (t.pos + from_offset) = ' '
    then (
      let length = ref 0 in
      let pos = ref (t.pos + from_offset + 1) in
      let max_pos = t.pos + t.len - 1 in
      while
        !pos <= max_pos
        &&
        let char = Bigstring.get t.buf !pos in
        if Char.between char ~low:'0' ~high:'9'
        then (
          length := (!length * 10) + Char.to_int char - Char.to_int '0';
          true)
        else false
      do
        pos := !pos + 1
      done;
      if !pos <= max_pos
      then
        if Bigstring.get t.buf !pos = '\000' && !pos <> t.pos + from_offset + 1
        then (
          on_length_read t ~payload_length:!length;
          pos := !pos + 1;
          t.len <- t.len + t.pos - !pos;
          t.total_payload_read <- t.total_payload_read + t.pos - !pos;
          t.pos <- !pos)
        else error t (Error.create_s [%sexp "Invalid_type"]))
    else error t (Error.create_s [%sexp "Invalid_type"])
;;

let read_header t =
  if t.len >= 6
  then
    if Bigstring.get_uint32_le t.buf ~pos:t.pos = 1651469410
    then
      read_header_payload_length t ~from_offset:4 ~on_length_read:set_state_reading_blob
    else if Bigstring.get_uint32_le t.buf ~pos:t.pos = 1701147252
    then
      read_header_payload_length t ~from_offset:4 ~on_length_read:set_state_reading_tree
    else if Bigstring.get_uint32_le t.buf ~pos:t.pos = 1835888483
         && Bigstring.get_uint16_le t.buf ~pos:(t.pos + 4) = 29801
    then
      read_header_payload_length
        t
        ~from_offset:6
        ~on_length_read:set_state_reading_commit
    else if Bigstring.get_uint32_le t.buf ~pos:t.pos = 543646068
    then
      read_header_payload_length t ~from_offset:3 ~on_length_read:set_state_reading_tag
    else error t (Error.create_s [%sexp "Invalid_type"])
;;

let read_blob_chunk t =
  match State.view t.state with
  | Before_type_read | Reading_commit _ | Reading_tree _ | Reading_tag _ | Done | Error _
    -> ()
  | Reading_blob { payload_length = _ } ->
    if t.len <> 0
    then (
      t.on_blob_chunk t.buf ~pos:t.pos ~len:t.len;
      t.pos <- 0;
      t.len <- 0)
;;

let rec read_tree_line t =
  match State.view t.state with
  | Before_type_read | Reading_blob _ | Reading_commit _ | Reading_tag _ | Done | Error _
    -> ()
  | Reading_tree { parser_state; payload_length = _ } ->
    (try
       let consumed =
         Tree.Git_object_payload_parser.consume_payload_exn
           parser_state
           t.buf
           ~pos:t.pos
           ~len:t.len
       in
       if consumed <> 0
       then (
         t.pos <- t.pos + consumed;
         t.len <- t.len - consumed;
         read_tree_line t)
     with
     | exn -> error t (Error.of_exn exn))
;;

let read_full_commit_info t =
  match State.view t.state with
  | Before_type_read | Reading_blob _ | Reading_tree _ | Reading_tag _ | Done | Error _
    -> ()
  | Reading_commit { payload_length = _ } ->
    (try
       let commit =
         Commit.parse_git_object_payload_exn
           (Bigstring.To_string.sub t.buf ~pos:t.pos ~len:t.len)
       in
       t.on_commit commit;
       t.pos <- 0;
       t.len <- 0;
       State.set_done t.state
     with
     | exn -> error t (Error.of_exn exn))
;;

let read_full_tag_info t =
  match State.view t.state with
  | Before_type_read
  | Reading_blob _
  | Reading_commit _
  | Reading_tree _
  | Done
  | Error _ -> ()
  | Reading_tag { payload_length = _ } ->
    (try
       let tag =
         Tag.parse_git_object_payload_exn
           (Bigstring.To_string.sub t.buf ~pos:t.pos ~len:t.len)
       in
       t.on_tag tag;
       t.pos <- 0;
       t.len <- 0;
       State.set_done t.state
     with
     | exn -> error t (Error.of_exn exn))
;;

let rec move_state_forward t =
  let current_state_view = State.view t.state in
  (match current_state_view with
   | Before_type_read -> read_header t
   | Reading_blob { payload_length = _ } -> read_blob_chunk t
   | Reading_tree _ -> read_tree_line t
   | Reading_tag _ | Reading_commit _ | Done | Error _ -> ());
  if not (phys_equal current_state_view (State.view t.state)) then move_state_forward t
;;

let append_data t buf ~pos ~len =
  if t.pos + t.len + len > Bigstring.length t.buf && t.pos <> 0
  then (
    Bigstring.blit ~src:t.buf ~src_pos:t.pos ~len:t.len ~dst:t.buf ~dst_pos:0;
    t.pos <- 0);
  while t.pos + t.len + len > Bigstring.length t.buf do
    let new_buf = Bigstring.create (Bigstring.length t.buf * 2) in
    assert (t.pos = 0);
    Bigstring.blit ~src:t.buf ~src_pos:0 ~len:t.len ~dst:new_buf ~dst_pos:0;
    t.buf <- new_buf
  done;
  Bigstring.blit ~src:buf ~src_pos:pos ~len ~dst:t.buf ~dst_pos:(t.pos + t.len);
  t.len <- t.len + len;
  t.total_payload_read <- t.total_payload_read + len;
  move_state_forward t
;;

let finalise t =
  match State.view t.state with
  | Before_type_read -> error t (Error.create_s [%sexp "Invalid_type"])
  | Done | Error _ -> ()
  | Reading_blob { payload_length }
  | Reading_tree { payload_length; parser_state = _ } ->
    if t.total_payload_read = payload_length
    then
      if t.len = 0
      then State.set_done t.state
      else error t (Error.create_s [%sexp "Unparsed_data_left"])
    else error t (Error.create_s [%sexp "Incorrect_length"])
  | Reading_commit { payload_length } ->
    if t.total_payload_read = payload_length
    then read_full_commit_info t
    else error t (Error.create_s [%sexp "Incorrect_length"])
  | Reading_tag { payload_length } ->
    if t.total_payload_read = payload_length
    then read_full_tag_info t
    else error t (Error.create_s [%sexp "Incorrect_length"])
;;

let%expect_test "reading header" =
  let test string =
    let t =
      create
        ~initial_buffer_size:1
        ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> ())
        ~on_commit:(fun _ -> ())
        ~on_tree_line:(fun _ _ ~name:_ -> ())
        ~on_tag:(fun _ -> ())
        ~on_error:(fun _ -> ())
    in
    append_data t (Bigstring.of_string string) ~pos:0 ~len:(String.length string);
    printf !"%{sexp: t}\n" t
  in
  test "com";
  [%expect {| ((data com) (total_payload_read 3) (state Before_type_read)) |}];
  test "comm";
  [%expect {| ((data comm) (total_payload_read 4) (state Before_type_read)) |}];
  test "commit";
  [%expect {| ((data commit) (total_payload_read 6) (state Before_type_read)) |}];
  test "commit ";
  [%expect {| ((data "commit ") (total_payload_read 7) (state Before_type_read)) |}];
  test "commit 123";
  [%expect {| ((data "commit 123") (total_payload_read 10) (state Before_type_read)) |}];
  test "commit 123\000";
  [%expect
    {|
        ((data "") (total_payload_read 0)
         (state (Reading_commit (payload_length 123)))) |}];
  test "tre";
  [%expect {| ((data tre) (total_payload_read 3) (state Before_type_read)) |}];
  test "tree";
  [%expect {| ((data tree) (total_payload_read 4) (state Before_type_read)) |}];
  test "tree ";
  [%expect {| ((data "tree ") (total_payload_read 5) (state Before_type_read)) |}];
  test "tree 38";
  [%expect {| ((data "tree 38") (total_payload_read 7) (state Before_type_read)) |}];
  test "tree 38\000";
  [%expect
    {|
        ((data "") (total_payload_read 0)
         (state
          (Reading_tree (payload_length 38)
           (parser_state ((walked_without_finding_null 0)))))) |}];
  test "blo";
  [%expect {| ((data blo) (total_payload_read 3) (state Before_type_read)) |}];
  test "blob";
  [%expect {| ((data blob) (total_payload_read 4) (state Before_type_read)) |}];
  test "blob ";
  [%expect {| ((data "blob ") (total_payload_read 5) (state Before_type_read)) |}];
  test "blob 1234";
  [%expect {| ((data "blob 1234") (total_payload_read 9) (state Before_type_read)) |}];
  test "blob 1234\000";
  [%expect
    {|
        ((data "") (total_payload_read 0)
         (state (Reading_blob (payload_length 1234)))) |}];
  test "blob \000";
  [%expect
    {| ((data "blob \000") (total_payload_read 6) (state (Error Invalid_type))) |}];
  test "blob 1234a\000";
  [%expect
    {|
        ((data "blob 1234a\000") (total_payload_read 11)
         (state (Error Invalid_type))) |}];
  test "ta";
  [%expect {| ((data ta) (total_payload_read 2) (state Before_type_read)) |}];
  test "tag";
  [%expect {| ((data tag) (total_payload_read 3) (state Before_type_read)) |}];
  test "tag ";
  [%expect {| ((data "tag ") (total_payload_read 4) (state Before_type_read)) |}];
  test "tag 123";
  [%expect {| ((data "tag 123") (total_payload_read 7) (state Before_type_read)) |}];
  test "tag 123\000";
  [%expect
    {| ((data "") (total_payload_read 0) (state (Reading_tag (payload_length 123)))) |}]
;;

let%expect_test "read blob" =
  let blob_text = Bigstring.of_string "blob 16\0001a2a3a4a5a6a7a8a" in
  let new_t () =
    create
      ~initial_buffer_size:1
      ~on_blob_chunk:(fun buf ~pos ~len ->
        printf "Blob chunk: %s\n" (Bigstring.To_string.sub buf ~pos ~len))
      ~on_commit:(fun _ -> failwith "Expected blob")
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected blob")
      ~on_tag:(fun _ -> failwith "Expected blob")
      ~on_error:Error.raise
  in
  let t = new_t () in
  append_data t blob_text ~pos:0 ~len:(Bigstring.length blob_text);
  finalise t;
  printf !"%{sexp: t}\n" t;
  [%expect
    {|
        Blob chunk: 1a2a3a4a5a6a7a8a
        ((data "") (total_payload_read 16) (state Done)) |}];
  let t = new_t () in
  for i = 0 to Bigstring.length blob_text - 1 do
    append_data t blob_text ~pos:i ~len:1
  done;
  finalise t;
  printf !"%{sexp: t}\n" t;
  [%expect
    {|
        Blob chunk: 1
        Blob chunk: a
        Blob chunk: 2
        Blob chunk: a
        Blob chunk: 3
        Blob chunk: a
        Blob chunk: 4
        Blob chunk: a
        Blob chunk: 5
        Blob chunk: a
        Blob chunk: 6
        Blob chunk: a
        Blob chunk: 7
        Blob chunk: a
        Blob chunk: 8
        Blob chunk: a
        ((data "") (total_payload_read 16) (state Done)) |}]
;;

let%expect_test "read commit" =
  let commit_text =
    Bigstring.of_string
      (sprintf
         "commit %d\000%s"
         (String.length Commit.For_testing.example_git_object_payload)
         Commit.For_testing.example_git_object_payload)
  in
  let new_t () =
    create
      ~initial_buffer_size:1
      ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> failwith "Expected commit")
      ~on_commit:(fun commit -> printf !"%{sexp: Commit.t}\n" commit)
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected commit")
      ~on_tag:(fun _ -> failwith "Expected commit")
      ~on_error:Error.raise
  in
  let t = new_t () in
  append_data t commit_text ~pos:0 ~len:(Bigstring.length commit_text);
  finalise t;
  printf !"%{sexp: t}\n" t;
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
               (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC))))
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
         (description "Merge branch 'branch'\n"))
        ((data "") (total_payload_read 830) (state Done)) |}];
  let t = new_t () in
  for i = 0 to Bigstring.length commit_text - 1 do
    append_data t commit_text ~pos:i ~len:1
  done;
  [%expect {||}];
  finalise t;
  printf !"%{sexp: t}\n" t;
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
               (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC))))
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
         (description "Merge branch 'branch'\n"))
        ((data "") (total_payload_read 830) (state Done)) |}]
;;

let%expect_test "read tree" =
  let tree_text =
    Bigstring.of_string
      (sprintf
         "tree %d\000%s"
         (String.length Tree.For_testing.example_git_object_payload)
         Tree.For_testing.example_git_object_payload)
  in
  let new_t () =
    create
      ~initial_buffer_size:1
      ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> failwith "Expected tree")
      ~on_commit:(fun _ -> failwith "Expected tree")
      ~on_tree_line:(fun mode sha1 ~name ->
        printf
          !"Received tree line: %{sexp: File_mode.t} %{Sha1.Hex} %s\n"
          mode
          (Sha1.Raw.Volatile.to_hex sha1)
          name)
      ~on_tag:(fun _ -> failwith "Expected tree")
      ~on_error:Error.raise
  in
  let t = new_t () in
  append_data t tree_text ~pos:0 ~len:(Bigstring.length tree_text);
  finalise t;
  printf !"%{sexp: t}\n" t;
  [%expect
    {|
        Received tree line: Directory bec63e37d08c454ad3a60cde90b70f3f7d077852 dir
        Received tree line: Non_executable_file e2129701f1a4d54dc44f03c93bca0a2aec7c5449 file1
        Received tree line: Non_executable_file 6c493ff740f9380390d5c9ddef4af18697ac9375 file2
        Received tree line: Link dea97c3520a755e4db5694d743aa8599511bbe9c link
        ((data "") (total_payload_read 128) (state Done)) |}];
  let t = new_t () in
  for i = 0 to Bigstring.length tree_text - 1 do
    append_data t tree_text ~pos:i ~len:1
  done;
  finalise t;
  printf !"%{sexp: t}\n" t;
  [%expect
    {|
        Received tree line: Directory bec63e37d08c454ad3a60cde90b70f3f7d077852 dir
        Received tree line: Non_executable_file e2129701f1a4d54dc44f03c93bca0a2aec7c5449 file1
        Received tree line: Non_executable_file 6c493ff740f9380390d5c9ddef4af18697ac9375 file2
        Received tree line: Link dea97c3520a755e4db5694d743aa8599511bbe9c link
        ((data "") (total_payload_read 128) (state Done)) |}]
;;

let%expect_test "read tag" =
  let tag_text =
    Bigstring.of_string
      (sprintf
         "tag %d\000%s"
         (String.length Tag.For_testing.example_git_object_payload)
         Tag.For_testing.example_git_object_payload)
  in
  let new_t () =
    create
      ~initial_buffer_size:1
      ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> failwith "Expected tag")
      ~on_commit:(fun _ -> failwith "Expected tag")
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected tag")
      ~on_tag:(fun tag -> printf !"%{sexp: Tag.t}\n" tag)
      ~on_error:Error.raise
  in
  let t = new_t () in
  append_data t tag_text ~pos:0 ~len:(Bigstring.length tag_text);
  finalise t;
  printf !"%{sexp: t}\n" t;
  [%expect
    {|
        ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
         (tag vtest)
         (tagger
          (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
            (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC))))
         (description "test tag\n"))
        ((data "") (total_payload_read 150) (state Done)) |}];
  let t = new_t () in
  for i = 0 to Bigstring.length tag_text - 1 do
    append_data t tag_text ~pos:i ~len:1
  done;
  [%expect {||}];
  finalise t;
  printf !"%{sexp: t}\n" t;
  [%expect
    {|
        ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
         (tag vtest)
         (tagger
          (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
            (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC))))
         (description "test tag\n"))
        ((data "") (total_payload_read 150) (state Done)) |}]
;;

let create = create ~initial_buffer_size:(1 lsl 16)
