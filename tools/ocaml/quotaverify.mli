(**
  Verify quota computations.

  Quotas are meant to safeguard against guest actions exhausting memory
  and causing the process to be OOM killed.
  They ensure that memory used by data structures is bounded.

  It can be difficult to accurately predict the amount of memory used when given certain input:
  it depends on data structure implementation details and the garbage collector.
  Therefore size computations and quotas are only approximate, however we need to test whether they are correct,
  and thus need a mathematical formula on what is acceptable in this approximation.

  The approximation used in oxenstored can be expressed by the following property:
    [∀Input, ∃ Bound < AvailableMemory: memory_used_bytes(Input) < Bound]

  where:
   * [Input] is a sequence of guest actions (e.g. xenstore packets), potentially unbounded
   * [memory_used_bytes(x)] is the amount of memory used when supplied the input [x]
   * [Bound] is the upper bound ensured by the quota system
   * [AvailableMemory] is the amount of memory the process is allowed to allocate
    before getting OOM killed. E.g. if OOM score adjust is set to 50% fo RAM, then 50% of RAM.

  The application is allowed to use less memory than the computed Bound, either
  due to optimization (e.g. sharing), or to account for inaccuracies in bound computation.
  However [Bound] is the upper bound used to ensure OOM safety.

  The condition [Bound < AvailableMemory] is important, otherwise [Bound = 2^64]
  would always be a valid value (memory allocation will have to fail once that is exceeded,
  because pointers cannot represent more than 64-bit values), but such a bound
  is not useful in practice: the application would've been OOM killed long before that.

  However the following property is desireable:
  [∀ Bound ≥ Baseline, ∃ Quota_i ≥ 0, Constant_i, Constant_i ≥ 1, Bound = Baseline + ∑ Quota_i * Constant_i]

  where:
  * [Baseline] is the baseline memory usage of oxenstored including any one-off
  memory usage that stays ~constant for its lifetime (e.g. the executable itself,
  initial heap size, and initial size of global data structures)
  Any one-off overhead from datastructures can also be included here as long as there is another quota
  limiting the number of such data structures, thus ensuring that any overhead has a constant upper bound

  * [Quota_i] is a user supplied configuration value, set to achieve the desired [AvailableMemory]
  * [Constant_i] is a constant factor to ensure linear memory usage.
  The application is of course allowed to optimize memory usage (e.g. via sharing) and use less memory
  than computed here, but that is already allowed by the initial equation.
  A small [Constant_i] is desireable, however an upper bound on this is eventually ensured by
  [Bound < AvailableMemory]

  * [∑] accounts for needing multiple quotas for different kinds of input

  There are similarities between this property and the Big-O notation used for
  algorithmic and space complexity, except we're not interested only in what happens
  when the input goes to infinity, we're also interested whether there are any
  large constant factors.
*)

open Xenbus

module Approximation : sig
  type 'a t = 'a -> Size_tracker.t -> Size_tracker.t * Size_tracker.t
  (** a memory usage approximation model [size_of_x -> (lb, ub)]
   Given a value [x] and an approximate size [size_of_x] compute bounds [lb, ub],
   such that [lb <= real_memory_usage(x) <= ub]
   *)

  val exact: 'a t
  (** [exact] is an exact model, [lb = ub = size_of_x].
    Used for primitive types.
  *)

  val linear: ?offset: Size_tracker.t -> ?multiplier:int -> Size_tracker.t seq -> 'a t
  (** [linear ?offset ?multiplier examples]
    [lb = offset], [ub = offset + multiplier * size_of_x].
    if [multiplier] is provided then it verifies that the formula works on the supplied examples,
    if any.
    if [multiplier] is not provided then [examples] must be, and [offset] and [multiplier] are computed as needed.
  *)

  val bounded_log: max_count:int -> 'a t -> 'a t
  (** [bounded_log ~max_count approx] is the approximation approx with its constant factors
    modified such that [log2(max_count)] is included.
  *)
end


type quota_error =
 { calculated: Size_tracker.t
 ; measured: Size_tracker.t
 ; msg: string
 }

val check: Approximation.t -> ('a -> Size_tracker.t) -> 'a -> (Size_tracker.t, quota_error) result
(** [check approximation size_of x] checks whether the computed [size_of x] matches the bounds computed 
     by the [approximation]
*)

type 'a t = 'a -> (Size_tracker.t, quota_error) result (** verify quota when supplied input of type ['a] *)

module Exact : sig
  val unboxed: 'a t
  (** [unboxed] is the size of unboxed OCaml types, i.e. just 1 word, no GC overhead *)

  val words: int -> 'a t
  (** [words size] asserts that [memory_size(x) = size+1] in words,
  where +1 accounts for the extra GC header before each value *)

  val string_like: int -> 'a t
  (** [string_like n] is the memory used by a [string] or [bytes] with [n] characters. *)
end

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] verifies the memory used by the pair of [a, b] *)

val linear : ?baseline:Size_tracker.t -> ?multiplier:int -> ('a seq -> 'b) -> 'a t -> (int -> 'b) t
(** [linear ?baseline ?multiplier gen element] asserts that the memory used by [N] elements follows a linear model:
  Confusion here between memory usage model and N* sequence.... lets separate
  linear bound!


  If [multiplier] is specified then this is required to be an exact match:
    [memory_used = baseline + N * multiplier * element]
  Otherwise it is allowed to be approximate and a suitable [baseline] and [multiplier] is determined automatically at startup.
    [memory_used(gen(n)) ≤ baseline + n * multiplier * element]
*)

val tree_like: max_count:int -> (int -> 'a) t -> (int -> 'a) t
(** [tree_like ~max_count elements] verifies that [elements] follows an [n*log2(n)] memory usage model,
    where [n ≤ max_count], a constant upper bound.
    This will in turn [log(n)] into a constant factor.
    (We could've also turned the entire [n*log(n)] into a constant factor, but may have been too large to be useful
    when we want more accurate memory usage models)

    [elements] can be specified using [linear].
*)

val bounded_by: bytes:int -> 'a t -> 'a t
(** [bounded_by ~bytes] verifies that the memory used by ['a] is bounded by [bytes].
  Typically this implies that some quota mechanism is used to limit the construction of ['a],
  so it doesn't become too big.
*)
