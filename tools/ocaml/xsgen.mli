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

(* ortac limitation: no variants yet, and if we make this a tuple
   then the type of S.perm will have weak type variable that fail to generalize *)
type perm = { r: bool; w: bool }

val directory: transaction -> path -> path list
(*@ entries = directory transaction path
    pure *)

type value

val value: unit -> value
(*@ v = value () *)

val read: transaction -> path -> value
(*@ v = read transaction path
    pure *)

type domid

val domid: int -> domid
(*@ d = domid i
    pure *)

val int_of_domid: domid -> int
(*@ i = int_of_domid d
    pure
    ensures (domid i) = d
    *)

(* ortac limitation: can't use a record here containing other types declared here,
   or their type variables will be weak in S.perms, failing to generalize.
   Instead use a constructor function that takes record fields *)
type perms

val perms: domid -> perm -> perms
(*@ p = perms domid perm
    pure *)

(*
  { owner: domid
  ; others: perm
  (* ortac: list generation broken? Gen.list takes a size parameter, but not supplied *)
  (*; rest: (domid * perm) list *)
  }
*)

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
    ensures 0 < int_of_domid domid <= 0xFFFF *)

val release: domid -> unit
(*@ release domid
    requires int_of_domid domid > 0
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
