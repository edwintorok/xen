type connection

val memory_reachable_bytes: connection -> int
(*@ n = memory_reachable_bytes con
    pure *)

val memory_calculated_bytes: connection -> int
(*@ n = memory_calculated_bytes con
    pure *)

val memory_check: connection -> bool
(*@ b = memory_check con
    pure
 *)

(* reachable words can be smaller due to sharing *)
(*@ predicate memory_calculation_ok(c:connection) =
      memory_reachable_bytes c <= memory_calculated_bytes c
  *)

type transaction

val connection: connection

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
exception Eexist

val transaction_start: connection -> transaction
(*@ t = transaction_start connection
    raises Quota -> number_of_transactions connection > maxtransaction connection
*)

val transaction_is_valid: connection -> transaction -> bool
(*@ b = transaction_is_valid con t
    pure *)

(* ortac/monolith limitation: it doesn't check that we attempt to use a consumed t,
   so have to check validity ourselves for now
 *)
val transaction_end: connection -> transaction -> bool -> bool
(*@ r = transaction_end con t comit
    requires transaction_is_valid con t
    modifies t
    consumes t
    ensures not (transaction_is_valid con t)
    ensures memory_check con
 *)

type path
(* TODO: valid path constraint *)

val path: unit -> path
(*@ p = path () *)

exception Noent
val path_exists: connection -> transaction -> path -> bool
(*@ v = path_exists con transaction path
    requires transaction_is_valid con transaction
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

(* TODO: should return valid paths.... prefix *)
val directory: connection -> transaction -> path -> path list
(*@ entries = directory con transaction path
    requires transaction_is_valid con transaction
    raises Noent -> not (path_exists con transaction path)
    *)

type value

val value: unit -> value
(*@ v = value () *)

val read: connection -> transaction -> path -> value
(*@ v = read con transaction path
    requires transaction_is_valid con transaction
    raises Noent -> not (path_exists con transaction path)
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

val getperms: connection -> transaction -> path -> perms
(*@ perms = getperms con transaction path
    requires transaction_is_valid con transaction && path_is_valid path
    raises Noent -> not (path_exists con transaction path)
    *)

type token

val token: unit -> token
(* t = token () *)

val has_watch: connection -> path -> bool
(*@ b = has_watch con path
    pure *)

(* TODO: also quota... *)

val watch: connection -> path -> token -> unit
(*@ watch con path token
    requires path_is_valid path
    ensures has_watch con path
    raises Eexist -> has_watch con path
 *)

(*val unwatch: connection -> path -> token -> unit*)
(* TODO: unwatch doens't work for some reason... raises EINVAL
  unwatch con path token
    requires path_is_valid path && has_watch con path
    ensures not (has_watch con path)
    *)

(*val dom0: domid*)

val introduce: connection -> domid
(*@ domid = introduce con
    ensures domid_exists domid
*)
    (* ensures domid <> dom0 *)

val release: connection -> domid -> unit
(*@ release con domid
    requires domid_exists domid
    modifies domid
    consumes domid
    *)

val resume: connection -> domid -> unit
(*@ resume con domid
    requires domid_exists domid
 *)

val getdomainpath: connection -> domid -> path
(*@ s = getdomainpath con domid
    requires domid_exists domid
    pure *)

val write: connection -> transaction -> path -> value -> unit
(*@ write con transaction path value
    requires transaction_is_valid con transaction && path_is_valid path
 *)

val mkdir: connection -> transaction -> path -> unit
(*@ mkdir con transaction path
    requires transaction_is_valid con transaction && path_is_valid path
 *)

val rm: connection -> transaction -> path -> unit
(*@ rm con transaction path
    requires transaction_is_valid con transaction && path_is_valid path
 *)

val setperms: connection -> transaction -> path -> perms -> unit
(*@ setperms con transaction path perms
    requires transaction_is_valid con transaction && path_is_valid path
    raises Noent -> not (path_exists con transaction path)
 *)
