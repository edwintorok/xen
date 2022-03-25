(**
  Assigns owners to long lived values in memory.
  This can be used to introduce quotas per owner to reduce the likelihood that a single entity drives the
  application out of memory.

  We cannot guarantee that we won't get OOM killed (that depends on how much memory other processes
  in the system use), but we can ensure that our memory usage is bounded if:
    * all long lived values in memory are assigned an owner
    * when an owner exceeds its quota it is not allowed to consume more memory
    * set an OOM score adjustment to ensure we don't get killed just because we're the highest
    memory user (done by startup script) as long as we don't use more than X% of RAM
    * tweak the garbage collector to reduce max overhead

  Memory used temporarily by packet processing is not measured, this is assumed to be "small"
  compared to overall memory usage, and this can be tested by fuzzing to not exceed a certain upper
  bound.
  This module only takes care of the first part - tracking memory usage.

  Memory usage needs to be tracked efficiently (so we don't consume a lot more memory just to track
  it, and we don't degrade performance by walking all the values on each action).
    * all operations in this module are O(1), except removal of mixed containers
    * all memory used by long-lived values in this module are O(1)
    * doesn't rely on the Obj module: memory tracking is opt-in for data structures

  A given data structure can hold memory belonging to different owners (most obvious example: the
  xenstore tree itself), hence ownership isn't always hierarchical, but attached to individual
  values. However we avoid keeping an explicit list of values owned by each owner, and instead just
  update a counter by adding and subtracting sizes on each operation.

  To detect bugs there is also a "reference" implementation that is not O(1) that can be used
  by fuzzers to check that the optimized implementation here is correct.
*)

(** A module to compute the size of a data structure incrementally in O(1) time.

      Only types where the size can be computed in O(1) are implemented below.

      Not implemented:
        * exn (because it can contain different element types)
        * 'a list - there is a separate SizedList module that can be used instead
        * _ format6 - not typically stored in xenstored, size is implementation version dependent
   *)

type size_kind =
  [ `constant
  (** all values of this type have exactly the same size.
      This means they can be part of arrays and the size of the array will stay constant
      (even if the values change) *)

  | `immutable
  (** the size of this value doesn't change, but the size of different values of this type is not
      the same. Cannot be part of primitive arrays since we'd no longer be able to compute its
      size when elements are changed
    *)

  | `updatable
  (** this refers to a value that has a mutable component, but we are tracking updates to the size
      of the value, and this expression is updated too.
      Can still be nested inside other mutable data structures *)

  | `ephemeral
  (** this size expression is only valid at the current moment in time.
      It contains records with mutable fields where we cannot track updates to their size.
      This cannot be nested inside other mutable data structures.
      *)
  ]

(* helper types when we require a specific property *)
type array_compatible = [`constant]

type require_nestable = [`constant | `immutable | `updatable]
(** to require a parameter to be nestable use as [< require_nestable] *)

(* we declare various helper types for cases where it is easier to define what *not* to accept *)
type forbid_updates = [`constant | `immutable] (** to be used as [< forbid_updates] *)

type +'a t
(** A size expression for data structures.

    The type parameter declares how size updates propagate.

  *)

val add: 'a t -> 'a t -> 'a t
(** [add a b] is the size of [a] + size of [b].
    Updates to the sizes of either [a] or [b] will propagate to the size of their sum.
    The type parameters are unified, so ideally they should be open polymorphic variants
    ([> `...]).
    *)

val remove: 'a t -> 'a t -> 'a t
(** [remove a b] is the size of [a] - size of [b].
    Updates to the sizes of either [a] or [b] will propagate to the size of their difference.
    The type parameters are unified, so ideally they should be open polymorphic variants
    ([> `...]).
    *)

val tracker: _ t -> [> array_compatible] t
(** the overhead of a size tracker is constant *)

val bool: bool -> [> array_compatible] t
(** [bool] computes the size of a bool.

    O(1) complexity, it is a constant. *)

val char: char -> [> array_compatible] t
(** [bool] computes the size of a bool.

    O(1) complexity, it is a constant. *)

val float: float -> [> array_compatible] t
(** [float] computes the size of a float.

    O(1) complexity, it is a constant. *)

val variant: ('a -> 'b t) -> 'a -> 'b t
(** [variant t] is the size of [t] plus the overhead of a variant.

    O(1) complexity *)

val func: ('a -> 'b) -> [> array_compatible] t
(** [func] computes the size of a function reference.
    This assumes that the function already exists and is not a closure.
    However the type system can't verify that.

    O(1) complexity, it is a constant *)

val int: int -> [> array_compatible] t
(** [int] computes the size of an int.

    O(1) complexity, it is a constant. *)

val size_t: Sizeops.t -> [> array_compatible] t
(** [size_t] computes the size of a Size.t.

    O(1) complexity, it is a constant. *)

val int32: int32 -> [> array_compatible] t
(** [int32] computes the size of an int32.

    O(1) complexity, it is a constant. *)

val int64: int64 -> [> array_compatible] t
(** [int64] computes the size of an int32.

    O(1) complexity, it is a constant. *)

val nativeint: nativeint -> [> array_compatible] t
(** [nativeint] computes the size of a nativeint.

    O(1) complexity, it is a constant. *)

val unit: unit -> [> array_compatible] t
(** [unit] computes the size of a unit.

    O(1) complexity, it is a constant. *)

val bytes: bytes -> [> `immutable ] t
(** [bytes] computes the size of a [bytes value]. This is mutable but its size is constant.

    O(1) complexity since [bytes] store their size explicitly. *)

val bytes_n: int -> [> `immutable ] t
(** [bytes_n] computes the size of a [bytes] value without having access to it directly,
    just by knowing its length.

    O(1) complexity *)

val string: string -> [> `immutable] t
(** [string] computes the size of a string.

    O(1) complexity since strings store their size explicitly. *)

val option: ('a -> 'b t) -> 'a option -> 'b t
(** [option element_immutable_size_of x] computes the size of ['a option] using [element_immutable_size_of] to compute
    the size of its element.

    O(1) complexity if [element_immutable_size_of] is
    *)

val array: ('a -> [< array_compatible] t) -> 'a array -> [> array_compatible] t
(** [array size_of_element arr] computes the size of the array [arr],
    using [size_of_element].
    The elements must have a constant size regardless of their value. *)

type ('a, +'b) fields
(** size expression for record fields.
    'a is the type of the record
    'b is the [size_kind]
 *)

val record_start: 'a -> ('a, 'b) fields
(** [record_start r] begins computing the size of record [r].

    O(1) complexity.
 *)

val record_add_immutable: 'b t -> ('a, 'b) fields -> ('a, 'b) fields
(** [record_add_immutable field acc] adds the size of [field] to [acc].

    The field cannot be mutable, because if it was then
      we wouldn't be able to automatically track size of updates for it.
    However the type checker cannot ensure this.

    Note that this is unrelated to the *type* of the field:
      an immutable field can still point to a mutable type, which is fine.

    O(1) complexity.
 *)

val record_add_mutable_const: [< `constant] t -> ('a, [> `constant] as 'b) fields -> ('a, 'b) fields
(** [record_add_mutable_const field acc] adds the size of [field] to [acc] when the record field is
    mutable.
    Same as [record_add_immutable]: the size of the field is constant, so changing the value
    won't change its size.

    *)

val record_add_mutable: _ t -> ('a, _) fields -> ('a, [> `ephemeral]) fields
(** [record_add_mutable field acc] adds the size of [field] to [acc].

    The field is mutable.
    However the type checker cannot ensure this.

    O(1) complexity.
 *)

val record_end: ('a, 'b) fields -> 'b t
(** [record_end acc] finishes computing the size of a record.

  O(1) complexity.
 *)

val container_create: initial:[< forbid_updates] t -> item_overhead:[< forbid_updates] t -> [> `updatable] t
(** [container_create ~initial ~item_overhead] creates a new size update tracker, with [record]'s size as starting
    point and [item_overhead] overhead per element.

    O(1) complexity *)

val container_add_element: [< require_nestable] t -> [< `updatable] t -> unit
(** [container_add_element element container] adds [element]'s size to [container] and tracks
    updates to it. The size expression of the container will always reflect the size of all items
    elements. It includes the [item_overhad].

    O(1) complexity. *)

val container_remove_element: [< require_nestable] t -> [< `updatable] t -> unit
(** [container_remove_element element container] removes [element]'s size from the [container].
    It removes the [item_overhead] too.

    O(1) complexity. *)

val container_clear: [< `updatable] t -> unit
(** [container_clear t] sets [t]'s size back to its initial size.

    O(1) complexity. *)

val container_transfer: src:[< `updatable] t -> dst:[< `updatable] t -> unit
(** [container_transfer ~src ~dst] transfers all elements from [src] to [dst],
    while ensuring the [initial] size of the container is not double counted.

    O(1) complexity *)

val size_of: [< forbid_updates] t -> Sizeops.t
(** [size_of t] is the size of [t]. Can only be inspected directly when it is a constant *)

val size_of_bytes: _ t -> int
(** [size_of_bytes t] returns the size of [t] in bytes. This is only valid at the current moment,
    future updates to [t]'s size cannot be reflected in the return type (which is an integer).
    It will return [max_int] if the size tracking has overflown *or* underflown.

    O(1) complexity *)

val pp: Format.formatter -> _ t -> unit
(** [pp formatter size] pretty prints the size *)

val pp_dump: Format.formatter -> _ t -> unit
(** [pp formatter size] pretty prints the size *)
