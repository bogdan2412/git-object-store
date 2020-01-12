(** Library for manipulating a git object store via OCaml.

    Copyright (C) 2019-2020  Bogdan-Cristian Tataroiu

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

module Raw_kernel = struct
  module State : sig
    module View : sig
      type t = private
        | Before_header
        | Reading_payload of { mutable payload_length : int }
        | Done
        | Error of { mutable error : Error.t }
      [@@deriving sexp_of]
    end

    type t [@@deriving sexp_of]

    val create : unit -> t
    val view : t -> View.t
    val set_before_header : t -> unit
    val set_reading_payload : t -> payload_length:int -> unit
    val set_done : t -> unit
    val set_error : t -> Error.t -> unit
  end = struct
    module View = struct
      type t =
        | Before_header
        | Reading_payload of { mutable payload_length : int }
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
      ; reading_payload : View.t
      ; error : View.t
      }

    let sexp_of_t t = View.sexp_of_t t.value

    let create () =
      let reading_payload = View.Reading_payload { payload_length = 0 } in
      let error = View.Error { error = Error.of_string "uninitialised" } in
      { value = Before_header; reading_payload; error }
    ;;

    let view t = t.value
    let set_before_header t = t.value <- Before_header

    let set_reading_payload t ~payload_length =
      (match t.reading_payload with
       | Reading_payload record -> record.payload_length <- payload_length
       | _ -> assert false);
      t.value <- t.reading_payload
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
    ; on_header : Object_type.t -> size:int -> unit
    ; on_payload_chunk : Bigstring.t -> pos:int -> len:int -> final:bool -> int
    ; on_error : Error.t -> unit
    }
  [@@deriving fields]

  let sexp_of_t
        { buf
        ; pos
        ; len
        ; total_payload_read
        ; state
        ; on_header = _
        ; on_payload_chunk = _
        ; on_error = _
        }
    =
    [%sexp
      { data = (Bigstring.to_string ~pos ~len buf : string)
      ; total_payload_read : int
      ; state : State.t
      }]
  ;;

  let create ~initial_buffer_size ~on_header ~on_payload_chunk ~on_error =
    { buf = Bigstring.create initial_buffer_size
    ; pos = 0
    ; len = 0
    ; total_payload_read = 0
    ; state = State.create ()
    ; on_header
    ; on_payload_chunk
    ; on_error
    }
  ;;

  let reset t =
    Fields.Direct.iter
      t
      ~buf:(fun _ _ _ -> ())
      ~pos:(fun _ t _ -> t.pos <- 0)
      ~len:(fun _ t _ -> t.len <- 0)
      ~total_payload_read:(fun _ t _ -> t.total_payload_read <- 0)
      ~state:(fun _ t _ -> State.set_before_header t.state)
      ~on_header:(fun _ _ _ -> ())
      ~on_payload_chunk:(fun _ _ _ -> ())
      ~on_error:(fun _ _ _ -> ())
  ;;

  let error t error =
    State.set_error t.state error;
    t.on_error error
  ;;

  let set_state_reading_blob t ~payload_length =
    State.set_reading_payload t.state ~payload_length;
    t.on_header Blob ~size:payload_length
  ;;

  let set_state_reading_commit t ~payload_length =
    State.set_reading_payload t.state ~payload_length;
    t.on_header Commit ~size:payload_length
  ;;

  let set_state_reading_tree t ~payload_length =
    State.set_reading_payload t.state ~payload_length;
    t.on_header Tree ~size:payload_length
  ;;

  let set_state_reading_tag t ~payload_length =
    State.set_reading_payload t.state ~payload_length;
    t.on_header Tag ~size:payload_length
  ;;

  let read_header_payload_length t ~from_offset ~on_length_read =
    if from_offset < t.len
    then
      if Char.( = ) (Bigstring.get t.buf (t.pos + from_offset)) ' '
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
          if Char.( = ) (Bigstring.get t.buf !pos) '\000'
          && !pos <> t.pos + from_offset + 1
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

  let rec read_payload t =
    if t.len <> 0
    then (
      let consumed = t.on_payload_chunk t.buf ~pos:t.pos ~len:t.len ~final:false in
      if consumed <> 0
      then (
        t.pos <- t.pos + consumed;
        t.len <- t.len - consumed;
        read_payload t))
  ;;

  let rec move_state_forward t =
    let current_state_view = State.view t.state in
    (match current_state_view with
     | Before_header -> read_header t
     | Reading_payload { payload_length = _ } -> read_payload t
     | Done | Error _ -> ());
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
    | Before_header -> error t (Error.create_s [%sexp "Invalid_type"])
    | Reading_payload { payload_length } ->
      if t.total_payload_read = payload_length
      then (
        let consumed = t.on_payload_chunk t.buf ~pos:t.pos ~len:t.len ~final:true in
        t.pos <- t.pos + consumed;
        t.len <- t.len - consumed;
        if t.len = 0
        then State.set_done t.state
        else error t (Error.create_s [%sexp "Unparsed_data_left"]))
      else error t (Error.create_s [%sexp "Incorrect_length"])
    | Done | Error _ -> ()
  ;;
end

module Raw = struct
  type _ t =
    | Do_not_validate_sha1 : { raw_kernel : Raw_kernel.t } -> unit t
    | Validate_sha1 :
        { raw_kernel : Raw_kernel.t
        ; sha1_context : [ `Initialised ] Sha1.Compute.t
        }
        -> Sha1.Hex.t t

  let raw_kernel (type a) : a t -> Raw_kernel.t = function
    | Do_not_validate_sha1 { raw_kernel } -> raw_kernel
    | Validate_sha1 { raw_kernel; sha1_context = _ } -> raw_kernel
  ;;

  let sexp_of_t _ t = [%sexp_of: Raw_kernel.t] (raw_kernel t)

  let create'
        ~initial_buffer_size
        ~on_header
        ~on_payload_chunk
        ~on_error
        (type a)
        (sha1_validation : a Sha1_validation.t)
    : a t
    =
    let raw_kernel =
      Raw_kernel.create ~initial_buffer_size ~on_header ~on_payload_chunk ~on_error
    in
    match sha1_validation with
    | Do_not_validate_sha1 -> Do_not_validate_sha1 { raw_kernel }
    | Validate_sha1 ->
      Validate_sha1
        { raw_kernel
        ; sha1_context = Sha1.Compute.init_or_reset (Sha1.Compute.create_uninitialised ())
        }
  ;;

  let create ~on_header ~on_payload_chunk ~on_error sha1_validation =
    create'
      ~initial_buffer_size:(1 lsl 16)
      ~on_header
      ~on_payload_chunk
      ~on_error
      sha1_validation
  ;;

  let append_data (type a) (t : a t) buf ~pos ~len =
    match t with
    | Do_not_validate_sha1 { raw_kernel } ->
      Raw_kernel.append_data raw_kernel buf ~pos ~len
    | Validate_sha1 { raw_kernel; sha1_context } ->
      Raw_kernel.append_data raw_kernel buf ~pos ~len;
      Sha1.Compute.process sha1_context buf ~pos ~len
  ;;

  let finalise (type a) (t : a t) (expected_sha1 : a) =
    match t with
    | Do_not_validate_sha1 { raw_kernel } -> Raw_kernel.finalise raw_kernel
    | Validate_sha1 { raw_kernel; sha1_context } ->
      let sha1_context = Sha1.Compute.finalise sha1_context in
      let actual_sha1 =
        Sha1.Hex.Volatile.non_volatile (Sha1.Compute.get_hex sha1_context)
      in
      if [%compare.equal: Sha1.Hex.t] actual_sha1 expected_sha1
      then Raw_kernel.finalise raw_kernel
      else
        Raw_kernel.error
          raw_kernel
          (Error.create_s
             [%message
               "Unexpected_sha1" (actual_sha1 : Sha1.Hex.t) (expected_sha1 : Sha1.Hex.t)])
  ;;

  let error t error = Raw_kernel.error (raw_kernel t) error

  let reset (type a) (t : a t) =
    match t with
    | Do_not_validate_sha1 { raw_kernel } -> Raw_kernel.reset raw_kernel
    | Validate_sha1 { raw_kernel; sha1_context } ->
      let (_ : [ `Initialised ] Sha1.Compute.t) =
        Sha1.Compute.init_or_reset sha1_context
      in
      Raw_kernel.reset raw_kernel
  ;;

  module Feed_sha1_context_header_data_function = struct
    let buf = Bigstring.create 32

    let impl (type a) (t : a t) object_type payload_length =
      match t with
      | Do_not_validate_sha1 { raw_kernel = _ } -> ()
      | Validate_sha1 { raw_kernel = _; sha1_context } ->
        let header_len =
          Git_object_header_writer.write_from_left
            buf
            ~pos:0
            object_type
            ~object_length:payload_length
        in
        Sha1.Compute.process sha1_context buf ~pos:0 ~len:header_len
    ;;
  end

  let feed_sha1_context_header_data = Feed_sha1_context_header_data_function.impl

  let reset_for_reading_blob t ~payload_length =
    reset t;
    Raw_kernel.set_state_reading_blob (raw_kernel t) ~payload_length;
    feed_sha1_context_header_data t Blob payload_length
  ;;

  let reset_for_reading_tree t ~payload_length =
    reset t;
    Raw_kernel.set_state_reading_tree (raw_kernel t) ~payload_length;
    feed_sha1_context_header_data t Tree payload_length
  ;;

  let reset_for_reading_commit t ~payload_length =
    reset t;
    Raw_kernel.set_state_reading_commit (raw_kernel t) ~payload_length;
    feed_sha1_context_header_data t Commit payload_length
  ;;

  let reset_for_reading_tag t ~payload_length =
    reset t;
    Raw_kernel.set_state_reading_tag (raw_kernel t) ~payload_length;
    feed_sha1_context_header_data t Tag payload_length
  ;;
end

module State : sig
  module View : sig
    type t = private
      | Before_header
      | Reading_blob
      | Reading_commit
      | Reading_tree of { parser_state : Tree.Git_object_payload_parser.State.t }
      | Reading_tag
    [@@deriving sexp_of]
  end

  type t [@@deriving sexp_of]

  val create
    :  emit_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
    -> t

  val view : t -> View.t
  val set_before_header : t -> unit
  val set_reading_blob : t -> unit
  val set_reading_commit : t -> unit
  val set_reading_tree : t -> unit
  val set_reading_tag : t -> unit
end = struct
  module View = struct
    type t =
      | Before_header
      | Reading_blob
      | Reading_commit
      | Reading_tree of { parser_state : Tree.Git_object_payload_parser.State.t }
      | Reading_tag
    [@@deriving sexp_of]
  end

  type t =
    { mutable value : View.t
    ; reading_tree : View.t
    }

  let sexp_of_t t = View.sexp_of_t t.value

  let create ~emit_tree_line =
    let reading_tree =
      View.Reading_tree
        { parser_state = Tree.Git_object_payload_parser.State.create ~emit_tree_line }
    in
    { value = Before_header; reading_tree }
  ;;

  let view t = t.value
  let set_before_header t = t.value <- Before_header
  let set_reading_blob t = t.value <- Reading_blob
  let set_reading_commit t = t.value <- Reading_commit

  let set_reading_tree t =
    (match t.reading_tree with
     | Reading_tree record ->
       Tree.Git_object_payload_parser.State.reset record.parser_state
     | _ -> assert false);
    t.value <- t.reading_tree
  ;;

  let set_reading_tag t = t.value <- Reading_tag
end

type 'Sha1_validation t =
  { raw : 'Sha1_validation Raw.t
  ; state : State.t
  ; mutable on_blob_size : int -> unit
  ; mutable on_blob_chunk : Bigstring.t -> pos:int -> len:int -> unit
  ; mutable on_commit : Commit.t -> unit
  ; mutable on_tree_line : File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit
  ; mutable on_tag : Tag.t -> unit
  ; on_error : Error.t -> unit
  }
[@@deriving fields]

let on_header t object_type ~size =
  match (object_type : Object_type.t) with
  | Blob ->
    State.set_reading_blob t.state;
    t.on_blob_size size
  | Commit -> State.set_reading_commit t.state
  | Tree -> State.set_reading_tree t.state
  | Tag -> State.set_reading_tag t.state
;;

let on_payload_chunk t buf ~pos ~len ~final =
  match State.view t.state with
  | Before_header -> failwith "[on_payload_chunk] called before [on_header]"
  | Reading_blob ->
    if len <> 0 then t.on_blob_chunk buf ~pos ~len;
    len
  | Reading_tree { parser_state } ->
    (try
       Tree.Git_object_payload_parser.consume_payload_exn parser_state buf ~pos ~len
     with
     | exn ->
       Raw.error t.raw (Error.of_exn exn);
       0)
  | Reading_commit ->
    if final
    then (
      (try
         let commit =
           Commit.parse_git_object_payload_exn (Bigstring.To_string.sub buf ~pos ~len)
         in
         t.on_commit commit
       with
       | exn -> Raw.error t.raw (Error.of_exn exn));
      len)
    else 0
  | Reading_tag ->
    if final
    then (
      (try
         let tag =
           Tag.parse_git_object_payload_exn (Bigstring.To_string.sub buf ~pos ~len)
         in
         t.on_tag tag
       with
       | exn -> Raw.error t.raw (Error.of_exn exn));
      len)
    else 0
;;

let sexp_of_t
      _
      { raw
      ; state
      ; on_blob_size = _
      ; on_blob_chunk = _
      ; on_commit = _
      ; on_tree_line = _
      ; on_tag = _
      ; on_error = _
      }
  =
  [%sexp { raw : _ Raw.t; state : State.t }]
;;

let create
      ~initial_buffer_size
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      ~on_error
      sha1_validation
  =
  let rec state =
    lazy
      (let state =
         State.create ~emit_tree_line:(fun file_mode sha1 ~name ->
           (force t).on_tree_line file_mode sha1 ~name)
       in
       State.set_before_header state;
       state)
  and raw =
    lazy
      (Raw.create'
         ~initial_buffer_size
         ~on_header:(fun object_type ~size -> on_header (force t) object_type ~size)
         ~on_payload_chunk:(fun buf ~pos ~len ~final ->
           on_payload_chunk (force t) buf ~pos ~len ~final)
         ~on_error
         sha1_validation)
  and t =
    lazy
      { raw = force raw
      ; state = force state
      ; on_blob_size
      ; on_blob_chunk
      ; on_commit
      ; on_tree_line
      ; on_tag
      ; on_error
      }
  in
  force t
;;

let set_on_blob t ~on_size ~on_chunk =
  t.on_blob_size <- on_size;
  t.on_blob_chunk <- on_chunk
;;

let append_data t buf ~pos ~len = Raw.append_data t.raw buf ~pos ~len
let finalise t = Raw.finalise t.raw

let reset t =
  Fields.Direct.iter
    t
    ~raw:(fun _ t _ -> Raw.reset t.raw)
    ~state:(fun _ t _ -> State.set_before_header t.state)
    ~on_blob_size:(fun _ _ _ -> ())
    ~on_blob_chunk:(fun _ _ _ -> ())
    ~on_commit:(fun _ _ _ -> ())
    ~on_tree_line:(fun _ _ _ -> ())
    ~on_tag:(fun _ _ _ -> ())
    ~on_error:(fun _ _ _ -> ())
;;

let reset_for_reading_blob t ~payload_length =
  Raw.reset_for_reading_blob t.raw ~payload_length
;;

let reset_for_reading_tree t ~payload_length =
  Raw.reset_for_reading_tree t.raw ~payload_length
;;

let reset_for_reading_commit t ~payload_length =
  Raw.reset_for_reading_commit t.raw ~payload_length
;;

let reset_for_reading_tag t ~payload_length =
  Raw.reset_for_reading_tag t.raw ~payload_length
;;

let%expect_test "reading header" =
  let test ?(after_finalise = false) string =
    let t =
      create
        ~initial_buffer_size:1
        ~on_blob_size:(fun _ -> ())
        ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> ())
        ~on_commit:(fun _ -> ())
        ~on_tree_line:(fun _ _ ~name:_ -> ())
        ~on_tag:(fun _ -> ())
        ~on_error:(fun _ -> ())
        Do_not_validate_sha1
    in
    append_data t (Bigstring.of_string string) ~pos:0 ~len:(String.length string);
    if after_finalise then finalise t ();
    printf !"%{sexp: _ t}\n" t
  in
  test "com";
  [%expect
    {|
    ((raw ((data com) (total_payload_read 3) (state Before_header)))
     (state Before_header)) |}];
  test "comm";
  [%expect
    {|
    ((raw ((data comm) (total_payload_read 4) (state Before_header)))
     (state Before_header)) |}];
  test "commit";
  [%expect
    {|
    ((raw ((data commit) (total_payload_read 6) (state Before_header)))
     (state Before_header)) |}];
  test "commit ";
  [%expect
    {|
    ((raw ((data "commit ") (total_payload_read 7) (state Before_header)))
     (state Before_header)) |}];
  test "commit 123";
  [%expect
    {|
    ((raw ((data "commit 123") (total_payload_read 10) (state Before_header)))
     (state Before_header)) |}];
  test "commit 123\000";
  [%expect
    {|
        ((raw
          ((data "") (total_payload_read 0)
           (state (Reading_payload (payload_length 123)))))
         (state Reading_commit)) |}];
  test "tre";
  [%expect
    {|
    ((raw ((data tre) (total_payload_read 3) (state Before_header)))
     (state Before_header)) |}];
  test "tree";
  [%expect
    {|
    ((raw ((data tree) (total_payload_read 4) (state Before_header)))
     (state Before_header)) |}];
  test "tree ";
  [%expect
    {|
    ((raw ((data "tree ") (total_payload_read 5) (state Before_header)))
     (state Before_header)) |}];
  test "tree 38";
  [%expect
    {|
    ((raw ((data "tree 38") (total_payload_read 7) (state Before_header)))
     (state Before_header)) |}];
  test "tree 38\000";
  [%expect
    {|
        ((raw
          ((data "") (total_payload_read 0)
           (state (Reading_payload (payload_length 38)))))
         (state (Reading_tree (parser_state ((walked_without_finding_null 0)))))) |}];
  test "blo";
  [%expect
    {|
    ((raw ((data blo) (total_payload_read 3) (state Before_header)))
     (state Before_header)) |}];
  test "blob";
  [%expect
    {|
    ((raw ((data blob) (total_payload_read 4) (state Before_header)))
     (state Before_header)) |}];
  test "blob ";
  [%expect
    {|
    ((raw ((data "blob ") (total_payload_read 5) (state Before_header)))
     (state Before_header)) |}];
  test "blob 1234";
  [%expect
    {|
    ((raw ((data "blob 1234") (total_payload_read 9) (state Before_header)))
     (state Before_header)) |}];
  test "blob 1234\000";
  [%expect
    {|
        ((raw
          ((data "") (total_payload_read 0)
           (state (Reading_payload (payload_length 1234)))))
         (state Reading_blob)) |}];
  test "blob \000";
  [%expect
    {|
      ((raw
        ((data "blob \000") (total_payload_read 6) (state (Error Invalid_type))))
       (state Before_header)) |}];
  test "blob 1234a\000";
  [%expect
    {|
        ((raw
          ((data "blob 1234a\000") (total_payload_read 11)
           (state (Error Invalid_type))))
         (state Before_header)) |}];
  test "ta";
  [%expect
    {|
    ((raw ((data ta) (total_payload_read 2) (state Before_header)))
     (state Before_header)) |}];
  test "tag";
  [%expect
    {|
    ((raw ((data tag) (total_payload_read 3) (state Before_header)))
     (state Before_header)) |}];
  test "tag ";
  [%expect
    {|
    ((raw ((data "tag ") (total_payload_read 4) (state Before_header)))
     (state Before_header)) |}];
  test "tag 123";
  [%expect
    {|
    ((raw ((data "tag 123") (total_payload_read 7) (state Before_header)))
     (state Before_header)) |}];
  test "tag 123\000";
  [%expect
    {|
      ((raw
        ((data "") (total_payload_read 0)
         (state (Reading_payload (payload_length 123)))))
       (state Reading_tag)) |}];
  test ~after_finalise:true "blob 5\0001234";
  [%expect
    {|
    ((raw ((data "") (total_payload_read 4) (state (Error Incorrect_length))))
     (state Reading_blob)) |}];
  test ~after_finalise:true "blob 5\00012345";
  [%expect
    {| ((raw ((data "") (total_payload_read 5) (state Done))) (state Reading_blob)) |}];
  test ~after_finalise:true "blob 5\000123456";
  [%expect
    {|
    ((raw ((data "") (total_payload_read 6) (state (Error Incorrect_length))))
     (state Reading_blob)) |}]
;;

let%expect_test "read blob" =
  let blob_text = Bigstring.of_string "blob 16\0001a2a3a4a5a6a7a8a" in
  let new_t () =
    create
      ~initial_buffer_size:1
      ~on_blob_size:(fun size -> printf "Blob size: %d\n" size)
      ~on_blob_chunk:(fun buf ~pos ~len ->
        printf "Blob chunk: %s\n" (Bigstring.To_string.sub buf ~pos ~len))
      ~on_commit:(fun _ -> failwith "Expected blob")
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected blob")
      ~on_tag:(fun _ -> failwith "Expected blob")
      ~on_error:Error.raise
      Do_not_validate_sha1
  in
  let t = new_t () in
  append_data t blob_text ~pos:0 ~len:(Bigstring.length blob_text);
  finalise t ();
  printf !"%{sexp: _ t}\n" t;
  [%expect
    {|
        Blob size: 16
        Blob chunk: 1a2a3a4a5a6a7a8a
        ((raw ((data "") (total_payload_read 16) (state Done))) (state Reading_blob)) |}];
  let t = new_t () in
  for i = 0 to Bigstring.length blob_text - 1 do
    append_data t blob_text ~pos:i ~len:1
  done;
  finalise t ();
  printf !"%{sexp: _ t}\n" t;
  [%expect
    {|
        Blob size: 16
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
        ((raw ((data "") (total_payload_read 16) (state Done))) (state Reading_blob)) |}]
;;

let%expect_test "read blob, check sha1" =
  let blob_text = Bigstring.of_string "blob 16\0001a2a3a4a5a6a7a8a" in
  let new_t () =
    create
      ~initial_buffer_size:1
      ~on_blob_size:(fun size -> printf "Blob size: %d\n" size)
      ~on_blob_chunk:(fun buf ~pos ~len ->
        printf "Blob chunk: %s\n" (Bigstring.To_string.sub buf ~pos ~len))
      ~on_commit:(fun _ -> failwith "Expected blob")
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected blob")
      ~on_tag:(fun _ -> failwith "Expected blob")
      ~on_error:Error.raise
      Validate_sha1
  in
  let t = new_t () in
  append_data t blob_text ~pos:0 ~len:(Bigstring.length blob_text);
  Expect_test_helpers.show_raise (fun () ->
    finalise t (Sha1.Hex.of_string "0000000000000000000000000000000000000000"));
  printf !"%{sexp: _ t}\n" t;
  [%expect
    {|
        Blob size: 16
        Blob chunk: 1a2a3a4a5a6a7a8a
        (raised (
          Unexpected_sha1
          (actual_sha1 33b65c6cea5fc7a2da684fbbedd1d3fe3ca9cec5)
          (expected_sha1 0000000000000000000000000000000000000000)))
        ((raw
          ((data "") (total_payload_read 16)
           (state
            (Error
             (Unexpected_sha1 (actual_sha1 33b65c6cea5fc7a2da684fbbedd1d3fe3ca9cec5)
              (expected_sha1 0000000000000000000000000000000000000000))))))
         (state Reading_blob)) |}];
  let t = new_t () in
  append_data t blob_text ~pos:0 ~len:(Bigstring.length blob_text);
  finalise t (Sha1.Hex.of_string "33b65c6cea5fc7a2da684fbbedd1d3fe3ca9cec5");
  printf !"%{sexp: _ t}\n" t;
  [%expect
    {|
    Blob size: 16
    Blob chunk: 1a2a3a4a5a6a7a8a
    ((raw ((data "") (total_payload_read 16) (state Done))) (state Reading_blob)) |}]
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
      ~on_blob_size:(fun _ -> failwith "Expected commit")
      ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> failwith "Expected commit")
      ~on_commit:(fun commit -> printf !"%{sexp: Commit.t}\n" commit)
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected commit")
      ~on_tag:(fun _ -> failwith "Expected commit")
      ~on_error:Error.raise
      Do_not_validate_sha1
  in
  let t = new_t () in
  append_data t commit_text ~pos:0 ~len:(Bigstring.length commit_text);
  finalise t ();
  printf !"%{sexp: _ t}\n" t;
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
        ((raw ((data "") (total_payload_read 830) (state Done)))
         (state Reading_commit)) |}];
  let t = new_t () in
  for i = 0 to Bigstring.length commit_text - 1 do
    append_data t commit_text ~pos:i ~len:1
  done;
  [%expect {||}];
  finalise t ();
  printf !"%{sexp: _ t}\n" t;
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
        ((raw ((data "") (total_payload_read 830) (state Done)))
         (state Reading_commit)) |}]
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
      ~on_blob_size:(fun _ -> failwith "Expected tree")
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
      Do_not_validate_sha1
  in
  let t = new_t () in
  append_data t tree_text ~pos:0 ~len:(Bigstring.length tree_text);
  finalise t ();
  printf !"%{sexp: _ t}\n" t;
  [%expect
    {|
        Received tree line: Directory bec63e37d08c454ad3a60cde90b70f3f7d077852 dir
        Received tree line: Non_executable_file e2129701f1a4d54dc44f03c93bca0a2aec7c5449 file1
        Received tree line: Non_executable_file 6c493ff740f9380390d5c9ddef4af18697ac9375 file2
        Received tree line: Link dea97c3520a755e4db5694d743aa8599511bbe9c link
        ((raw ((data "") (total_payload_read 128) (state Done)))
         (state (Reading_tree (parser_state ((walked_without_finding_null 0)))))) |}];
  let t = new_t () in
  for i = 0 to Bigstring.length tree_text - 1 do
    append_data t tree_text ~pos:i ~len:1
  done;
  finalise t ();
  printf !"%{sexp: _ t}\n" t;
  [%expect
    {|
        Received tree line: Directory bec63e37d08c454ad3a60cde90b70f3f7d077852 dir
        Received tree line: Non_executable_file e2129701f1a4d54dc44f03c93bca0a2aec7c5449 file1
        Received tree line: Non_executable_file 6c493ff740f9380390d5c9ddef4af18697ac9375 file2
        Received tree line: Link dea97c3520a755e4db5694d743aa8599511bbe9c link
        ((raw ((data "") (total_payload_read 128) (state Done)))
         (state (Reading_tree (parser_state ((walked_without_finding_null 0)))))) |}]
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
      ~on_blob_size:(fun _ -> failwith "Expected tag")
      ~on_blob_chunk:(fun _ ~pos:_ ~len:_ -> failwith "Expected tag")
      ~on_commit:(fun _ -> failwith "Expected tag")
      ~on_tree_line:(fun _ _ ~name:_ -> failwith "Expected tag")
      ~on_tag:(fun tag -> printf !"%{sexp: Tag.t}\n" tag)
      ~on_error:Error.raise
      Do_not_validate_sha1
  in
  let t = new_t () in
  append_data t tag_text ~pos:0 ~len:(Bigstring.length tag_text);
  finalise t ();
  printf !"%{sexp: _ t}\n" t;
  [%expect
    {|
        ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
         (tag vtest)
         (tagger
          (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
            (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC))))
         (description "test tag\n"))
        ((raw ((data "") (total_payload_read 150) (state Done))) (state Reading_tag)) |}];
  let t = new_t () in
  for i = 0 to Bigstring.length tag_text - 1 do
    append_data t tag_text ~pos:i ~len:1
  done;
  [%expect {||}];
  finalise t ();
  printf !"%{sexp: _ t}\n" t;
  [%expect
    {|
        ((object_sha1 fd0b2091596e649f6ca4521262c3a0cadb0d042e) (object_type Commit)
         (tag vtest)
         (tagger
          (((name "Bogdan-Cristian Tataroiu") (email bogdan@example.com)
            (timestamp (2019-01-13 10:15:27.000000000-05:00)) (zone UTC))))
         (description "test tag\n"))
        ((raw ((data "") (total_payload_read 150) (state Done))) (state Reading_tag)) |}]
;;

let create
      ~on_blob_size
      ~on_blob_chunk
      ~on_commit
      ~on_tree_line
      ~on_tag
      ~on_error
      validation
  =
  create
    ~initial_buffer_size:(1 lsl 16)
    ~on_blob_size
    ~on_blob_chunk
    ~on_commit
    ~on_tree_line
    ~on_tag
    ~on_error
    validation
;;
