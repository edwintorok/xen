type transaction

val none: transaction

val transaction_start: unit -> transaction
(*@ t = transaction_start () *)

val transaction_end: transaction -> bool -> bool
(*@ r = transaction_end t comit
    consumes t
 *)

type path
(* TODO: valid path constraint *)

val path: unit -> path
(*@ p = path () *)

(* ortac limitation: no variants or records yet *)
type perm = bool * bool

val directory: transaction -> path -> path list
(*@ entries = directory transaction path
    pure *)

type value

val value: unit -> value
(*@ v = value () *)

val read: transaction -> path -> value
(*@ v = read transaction path
    pure *)

type domid = int

type perms =
  { owner: domid
  ; others: perm
  (* ortac: list generation broken? Gen.list takes a size parameter, but not supplied *)
  (*; rest: (domid * perm) list *)
  }

val getperms: transaction -> path -> perms
(*@ perms = getperms transaction path
    pure *)

type token

val token: unit -> token
(* t = token () *)

val watch: path -> token -> unit
(*@ watch path token *)

val unwatch: path -> token -> unit
(*@ unwatch path token *)


val dom0: domid

val introduce: unit -> domid
(*@ domid = introduce ()
    ensures 0 < domid <= 0xFFFF *)

val release: domid -> unit
(*@ release domid
    requires domid > 0
    consumes domid *)

val resume: domid -> unit
(*@ resume domid *)

val getdomainpath: domid -> path
(*@ s = getdomainpath domid
    pure *)

val write: transaction -> path -> value -> unit
(*@ write transaction path value *)

val mkdir: transaction -> path -> unit
(*@ mkdir transaction path *)

val rm: transaction -> path -> unit
(*@ rm transaction path *)

val setperms: transaction -> path -> perms -> unit
(*@ setperms transaction path perms *)
