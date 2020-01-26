open Core

(** Adds an optional [-git-objects-directory] flag pointing to a repository's
    [.git/objects/] directory.  If the flag is not specified, then we try to infer the
    directory by walking up the file system starting with the current working directory
    looking for a [.git] directory or a [.git] file with a [gitdir: ...] redirection in
    it (used by e.g. git submodules). *)
val object_directory_param : string Command.Param.t
