(* cameleer sizetracker.ml  --batch --prover=alt-ergo
   If it fails get a counterexample from cvc4:
   cameleer sizetracker.ml --batch --prover=cvc4-ce
 *)

(* TODO: this should be added by the cameleer wrapper script, and loaded from sys.mli *)
module type Sys = sig
  (*@ function word_size : integer *)
  (*@ axiom ws: word_size = 32 || word_size = 64 *)
  (* The word size can currently only be 32 or 64, use an axiom to define this.
     It ensures that the counter-example uses more realistic values *)
end

module type Pervasives = sig
  val[@logic] min_int: int

  val[@logic] max_int: int

  (* GOSPEL doesn't support 0x yet, and although this can be defined using the Power module and ^
     that will slow down proof/counterexample search a lot (since ^ is defined using proof machinery)
   *)
  (*@ axiom int31mm: Sys.word_size = 32 -> min_int = -1073741824 && max_int = 1073741823 *)
  (*@ axiom int63mm: Sys.word_size = 64 -> min_int = -4611686018427387904 && max_int = 4611686018427387903 *)
end

(*@ predicate int_in_bounds(i: int) = Pervasives.min_int <= i <= Pervasives.max_int *)

(* We want arithmetic on sum to not overflow. We could either:
  * perform the + operation and check for overflow (e.g. adding 2 positive numbers turned
  negative)
  * or check the range of the operands and refuse to add if it'd overflow

  max_int/2 should be safe as an upper limit to avoid overflow on +,
  defined as safe_max_int below
  *)
let safe_max_int = Pervasives.max_int / 2
(*@ ensures 0 < result + result <= Pervasives.max_int
    ensures 2 * result + 1 = Pervasives.max_int
    ensures 0 < result
 *)

(* the @logic ensures this is usable in Cameleer specifications later.
   TODO: ensure this works on 4.02.3 or add/remove using preprocessor *)
let[@logic] is_safe_size i = i >= 0 && i <= safe_max_int

type t = { limit: int; sum: int }
(*@ invariant is_safe_size limit
    invariant 0 <= sum <= limit
    invariant int_in_bounds limit && int_in_bounds sum
 *)

let create_exn limit =
  if is_safe_size limit then { limit; sum = 0 }
  else invalid_arg "limit out of bounds"
  (* TODO: more details on what the limit was and what the bounds are *)
(** [create_exn limit] creates a new size tracker that ensures [ sum <= limit].
    If the limit is not valid (negative or too large risking overflow) then Invalid_argument is
    raised *)
(*@ t = create_exn limit
    raises Invalid_argument _ -> not is_safe_size limit
    ensures t.limit = limit && t.sum = 0
    *)

let add x t =
  let sum = t.sum + x in
  if 0 <= x && x <= t.limit && 0 <= sum && sum <= t.limit then
    Some { t with sum }
  else None
(** [add x t] will add [x] to [t] if doing so wouldn't cause limits to be exceeded.
    Returns None if limits were exceeded *)
(*@ ropt = add x t
    pure
    ensures (let sum = t.sum + x in match ropt with
      | None -> sum < t.sum || sum > t.limit
      | Some r -> r.sum = sum && r.limit = t.limit && r.sum >= t.sum )
  *)

let remove x t = {t with sum = t.sum - x}
(** [remove x t] removes [x] from [t]. It is the caller's responsibility to ensure that [x] was part
    of [t] to begin with, and thus this operation cannot overflow *)
(*@ r = remove x t
    requires 0 <= x <= t.sum
    pure
    ensures r.limit = t.limit
    ensures r.sum + x = t.sum
    *)

let clear t = { t with sum = 0 }
(*@ r = clear t
    pure
    ensures r.sum = 0
    ensures r.limit = t.limit *)
