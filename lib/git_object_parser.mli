open Core

type t [@@deriving sexp_of]

(** [create] returns a new parser that can be fed git object data incrementally and
    reused to parse multiple git objects. *)
val create
  :  on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> on_error:(Error.t -> unit)
  -> t

(** [append_data] should be called to feed the parser a chunk of the git object data
    for the currently parsed object. Some of the callbacks which were specified in
    [create] might be called. *)
val append_data : t -> Bigstring.t -> pos:int -> len:int -> unit

(** [finalise] should be called once the entire git object has been fed to the
    parser. Some of the callbacks which were specified in [create] might be called. *)
val finalise : t -> unit

(** [reset] should be called to start parsing a new object. *)
val reset : t -> unit

(** [set_state_reading_blob] can be called after [reset] to signal to the parser that
    what follows is a blob of the specified length. *)
val set_state_reading_blob : t -> payload_length:int -> unit

(** [set_state_reading_commit] can be called after [reset] to signal to the parser that
    what follows is a commit of the specified length. *)
val set_state_reading_commit : t -> payload_length:int -> unit

(** [set_state_reading_tree] can be called after [reset] to signal to the parser that
    what follows is a tree of the specified length. *)
val set_state_reading_tree : t -> payload_length:int -> unit

(** [set_state_reading_tag] can be called after [reset] to signal to the parser that
    what follows is a tag of the specified length. *)
val set_state_reading_tag : t -> payload_length:int -> unit

(** Change the callback that gets called while parsing [blob] objects. *)
val set_on_blob_chunk : t -> (Bigstring.t -> pos:int -> len:int -> unit) -> unit

(** Change the callback that gets called while parsing [commit] objects. *)
val set_on_commit : t -> (Commit.t -> unit) -> unit

(** Change the callback that gets called while parsing [tree] objects. *)
val set_on_tree_line
  :  t
  -> (File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> unit

(** Change the callback that gets called while parsing [tag] objects. *)
val set_on_tag : t -> (Tag.t -> unit) -> unit
