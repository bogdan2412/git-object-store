open! Core
open! Import

type _ t [@@deriving sexp_of]

(** [create] returns a new parser that can be fed git object data incrementally and
    reused to parse multiple git objects.

    If [Validate_sha1] is passed, then the [finalise] method will expect a [Sha1.Hex.t]
    to be passed in and the parser will assert that the passed in data matches the
    checksum provided.  Otherwise, if [Do_not_validate_sha1] is passed, [finalise] only
    expects a [unit] argument and does not spend time maintaining the [Sha1] sum of the
    data. *)
val create
  :  on_blob_size:(int -> unit)
  -> on_blob_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> on_commit:(Commit.t -> unit)
  -> on_tree_line:(File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> on_tag:(Tag.t -> unit)
  -> on_error:(Error.t -> unit)
  -> 'Sha1_validation Sha1_validation.t
  -> 'Sha1_validation t

(** [append_data] should be called to feed the parser a chunk of the git object data
    for the currently parsed object. Some of the callbacks which were specified in
    [create] might be called. *)
val append_data : _ t -> Bigstring.t -> pos:int -> len:int -> unit

(** [finalise] should be called once the entire git object has been fed to the
    parser. Some of the callbacks which were specified in [create] might be called. *)
val finalise : 'Sha1_validation t -> 'Sha1_validation -> unit

(** [reset] should be called to start parsing a new object. *)
val reset : _ t -> unit

(** [reset_for_reading_blob] should be called to start parsing a new blob object of the
    specified length. *)
val reset_for_reading_blob : _ t -> payload_length:int -> unit

(** [reset_for_reading_commit] should be called to start parsing a new tree object of the
    specified length. *)
val reset_for_reading_commit : _ t -> payload_length:int -> unit

(** [reset_for_reading_tree] should be called to start parsing a new tree object of the
    specified length. *)
val reset_for_reading_tree : _ t -> payload_length:int -> unit

(** [reset_for_reading_tag] should be called to start parsing a new tag object of the
    specified length. *)
val reset_for_reading_tag : _ t -> payload_length:int -> unit

(** Change the callback that gets called while parsing [blob] objects. *)
val set_on_blob
  :  _ t
  -> on_size:(int -> unit)
  -> on_chunk:(Bigstring.t -> pos:int -> len:int -> unit)
  -> unit

(** Change the callback that gets called while parsing [commit] objects. *)
val set_on_commit : _ t -> (Commit.t -> unit) -> unit

(** Change the callback that gets called while parsing [tree] objects. *)
val set_on_tree_line
  :  _ t
  -> (File_mode.t -> Sha1.Raw.Volatile.t -> name:string -> unit)
  -> unit

(** Change the callback that gets called while parsing [tag] objects. *)
val set_on_tag : _ t -> (Tag.t -> unit) -> unit

(** Lower-level module that allows one to obtain raw payload data rather than have it be
    parsed. *)
module Raw : sig
  type _ t

  (** [create] returns a new raw parser that can be fed git object data incrementally and
      reused to parse multiple git objects.

      The return value of [on_payload_chunk] should be the number of bytes from the
      input that have been read.  Any unconsumed data will be fed to the method on
      the next call.

      Whenever more payload data becomes available, the [on_payload_chunk] method will
      be repeatedly called until either no data is left or until no data is consumed.

      Upon [finalise] being called, the [on_payload_chunk] will be called a final time
      with a value of [~final:true]. This final call may have a [len] of 0.

      If [Validate_sha1] is passed, then the [finalise] method will expect a [Sha1.Hex.t]
      to be passed in and the parser will assert that the passed in data matches the
      checksum provided.  Otherwise, if [Do_not_validate_sha1] is passed, [finalise] only
      expects a [unit] argument and does not spend time maintaining the [Sha1] sum of the
      data. *)
  val create
    :  on_header:(Object_type.t -> size:int -> unit)
    -> on_payload_chunk:(Bigstring.t -> pos:int -> len:int -> final:bool -> int)
    -> on_error:(Error.t -> unit)
    -> 'Sha1_validation Sha1_validation.t
    -> 'Sha1_validation t

  val append_data : _ t -> Bigstring.t -> pos:int -> len:int -> unit
  val finalise : 'Sha1_validation t -> 'Sha1_validation -> unit
  val reset : _ t -> unit

  (** Change the callback that gets called after parsing the raw object header. *)
  val set_on_header : _ t -> (Object_type.t -> size:int -> unit) -> unit

  (** Change the callback that gets called on each raw payload chunk. *)
  val set_on_payload_chunk
    :  _ t
    -> (Bigstring.t -> pos:int -> len:int -> final:bool -> int)
    -> unit
end
