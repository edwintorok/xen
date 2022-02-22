type connection

type transaction

val none: transaction

val number_of_transactions: connection -> int
(*@ n = number_of_transactions con
    pure
*)

(*@ gospel/ortac limitation: cannot be simply an int *)

val maxtransaction: connection -> int
(*@ n = maxtransaction connection
    pure *)

exception Quota

val transaction_start: connection -> transaction
(*@ t = transaction_start connection
    raises Quota -> number_of_transactions connection > maxtransaction connection
*)

val transaction_is_valid: transaction -> bool
(*@ b = transaction_is_valid t
    pure *)

(* ortac/monolith limitation: it doesn't check that we attempt to use a consumed t,
   so have to check validity ourselves for now
 *)
val transaction_end: transaction -> bool -> bool
(*@ r = transaction_end t comit
    requires transaction_is_valid t
    modifies t
    consumes t
    ensures not (transaction_is_valid t)
 *)

(*
type path
(* TODO: valid path constraint *)

val path: unit -> path
(*@ p = path () *)

exception Noent
val path_exists: transaction -> path -> bool
(*@ v = path_exists transaction path
    requires transaction_is_valid transaction
    pure *)

(* can't use as an invariant because 'directory' can return an empty path,
   perhaps the wrappers should append the results to the prefix and then we can use as invariant!
 *)
val path_is_valid : path -> bool
(*@ b = path_is_valid path
    pure *)


(* ortac limitation: no variants yet, and if we make this a tuple
   then the type of S.perm will have weak type variable that fail to generalize *)
type perm = { r: bool; w: bool }

val directory: transaction -> path -> path list
(*@ entries = directory transaction path
    requires transaction_is_valid transaction
    raises Noent -> not (path_exists transaction path)
    *)

type value

val value: unit -> value
(*@ v = value () *)

val read: transaction -> path -> value
(*@ v = read transaction path
    requires transaction_is_valid transaction
    raises Noent -> not (path_exists transaction path)
    *)

type domid

val domid_exists: domid -> bool
(*@ b = domid_exists domid
    pure *)

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
    requires transaction_is_valid transaction && path_is_valid path
    raises Noent -> not (path_exists transaction path)
    *)

type token

val token: unit -> token
(* t = token () *)

val watch: path -> token -> unit
(*@ watch path token
    requires path_is_valid path
 *)

val unwatch: path -> token -> unit
(*@ unwatch path token
    requires path_is_valid path
    *)


(*val dom0: domid*)

val introduce: unit -> domid
(*@ domid = introduce ()
    ensures domid_exists domid
*)
    (* ensures domid <> dom0 *)

val release: domid -> unit
(*@ release domid
    requires domid_exists domid
    modifies domid
    consumes domid
    *)

val resume: domid -> unit
(*@ resume domid
    requires domid_exists domid
 *)

val getdomainpath: domid -> path
(*@ s = getdomainpath domid
    requires domid_exists domid
    pure *)

val write: transaction -> path -> value -> unit
(*@ write transaction path value
    requires transaction_is_valid transaction && path_is_valid path
 *)

val mkdir: transaction -> path -> unit
(*@ mkdir transaction path
    requires transaction_is_valid transaction && path_is_valid path
 *)

val rm: transaction -> path -> unit
(*@ rm transaction path
    requires transaction_is_valid transaction && path_is_valid path
 *)

val setperms: transaction -> path -> perms -> unit
(*@ setperms transaction path perms
    requires transaction_is_valid transaction && path_is_valid path
    raises Noent -> not (path_exists transaction path)
 *)
*)
