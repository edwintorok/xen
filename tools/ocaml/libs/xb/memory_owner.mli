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

type top (** phantom type for top-level owner *)
type 'a nested

type 'a t
(** The entity owning various memory resources.
    Can be a domain in Xen, or more granular.
    It contains mutable state because we primarily deal with mutable data structures.
    *)

type id
(** A read-only top-level owner, so we can assign ownership explicitly.
    It cannot be used to modify the size directly to prevent accidentally updating
    the size used by an owner while the data structure is not yet fully linked up into its
    hierarchy.
 *)

type 'a printable = 'a -> (Format.formatter -> 'a -> unit)
(** a value and a pretty printer.
    This allows calling the pretty printer only when needing and avoids having to create lots of
    temporary strings that are never used *)

val create: 'a -> (Format.formatter -> 'a -> unit) -> top t
(** [create id pp_id] creates a new top-level owner.
    [pp_id id] will be used when pretty printing.

    O(1) complexity. *)

val id: top t -> id
(** [id t] is an identifier for the top-level owner.

    O(1) complexity.
  *)

val pp: Format.formatter -> top t -> unit
(** [pp formatter t] prints details about the [t] owner's memory usage.

    The format is implementation defined and subject to change between versions.
    It prints at least an identifier for the top-level owner and its size.

    Complexity:
      - O(1) when debug mode is not enabled
      - O(containers) when debug mode is enabled
  *)

val add_nested: _ t -> _ nested t -> unit
(** [add_nested owner container] sets [container]'s owner to be [owner].
    All size updates on [container] will now be forwarded to [owner],
    and it triggers an immediate size update with the container's current size.
    [owner] doesn't have to be a top-level container, and doesn't have to chain up to a top-level
    owner.

    [_] assigns a unique type parameter: owner and container have unrelated types.
    (could've also been written as 'a t -> 'b nested t)

    O(1) complexity.

      @raise Invalid_argument if the container already has an owner
  *)

val remove_nested: _ t -> _ nested t -> unit
(** [remove_nested owner container] marks the [container] as unowned,
    stops further size updates from it and removes its current size from the owner.

    O(1) complexity.

    @raise Invalid_argument if the container was not owned by [owner]
  *)

module TrackedSize: sig
  type 'a t  (* constraint 'a = [< `constant | `ephemeral] *)
  (** a size can be constant if the value is immutable,
      or ephemeral if the value is mutable and this just represents the size
      at the current moment.
      A special case is if the value is mutable but always the same size (e.g. int array)
      in which case constant is allowed.
      The distinction matters if this value is nested within another another data structure,
      we want to avoid ephemeral values inside immutable records for example.

      Size is tracked in words by [Sizeops.Size.t], so internally this uses words too.
      *)

  type ('a, 'b) immutable_size_of = 'a -> ([> `constant] as 'b) t
  (** A function to compute the size of ['a], which must be an immutable value,
    or containing only immutable values.

    ['b] is used only because polymorphic variants require it, otherwise we cannot unify `constant
    with `ephemeral in the add function below.

    The value must be immutable and the size computable in O(1).

    If this is a data structure use the [ImmutableContainer] module to keep a size value updated
    on-the-fly without having to walk the entire container to compute it.

    If this is a mutable data structure use the [Container] module to register it instead,
    and don't write a size computation function: size is not allowed to change on a value,
    if it does it invalidates Container size updates.

    Records with mutable fields, or immutable fields containing mutable data structures are not
    allowed.

    The type system cannot ensure this, but the debug mode will attempt to detect inconsistencies.
    *)

  val string: (string, _) immutable_size_of
  (** [string] computes the size of a string.

      O(1) complexity since strings store their size explicitly. *)

  val bytes: (bytes, _) immutable_size_of
  (** [bytes] computes the size of a [bytes value]. This is mutable but its size is constant.

      O(1) complexity since [bytes] store their size explicitly. *)

  val int: (int, _) immutable_size_of
  (** [string] computes the size of a string.

      O(1) complexity, it is a constant. *)

  val bool: (bool, _) immutable_size_of
  (** [bool] computes the size of a bool.

      O(1) complexity, it is a constant. *)

  val option: ('a, [<`constant]) immutable_size_of -> ('a option, 'b) immutable_size_of
  (** [option element_immutable_size_of] computes the size of ['a option] using [element_immutable_size_of] to compute
      the size of its element.

      O(1) complexity if [element_immutable_size_of] is
      *)

  val add: 'a t -> 'a t -> 'a t
  (** [add a b] adds the sizes of [a] and [b].

      O(1) complexity
   *)

  val size_of: [> `constant] t -> Sizeops.Size.t
  (** [size_of constant] returns the computed size for [constant].

      Access to the size of ephemeral values is not provided on purpose: these should be forwarded
      to a top-level owner.

    O(1) complexity
   *)

  val pp: Format.formatter -> _ t -> unit
  (** [pp formatter size] pretty prints the size *)
end

val size_of: _ t -> [> `ephemeral] TrackedSize.t
(** [size_of t] is the size of all values own by [t] currently.
    Note that no deduplication happens: if the same immutable value is referred to multiple times,
    then it'll be counted multiple times.
    This is on purpose: sharing of values is an optimization to reduce overall memory usage,
    but quota is only applied deterministically.

    O(1) complexity.
  *)

val owner_size_of_bytes: top t -> int
(** [owner_size_of_bytes t] is the size of all values owned by [t] currently.
    Returns [max_int] if the underlying size tracker has overflown or underflown.
    Size is tracked in words, but the value returned here is converted to bytes
    for convenient comparison against quotas.

    O(1) complexity.
    *)

module Container: sig
  (** Containers act as an implicit owner for values not otherwise owned by an explicit owner.
      They can be nested and forward size updates to their parent.
      The memory used by a top-level owner is only changed once a container is added to it
      via [add_container].
      It can efficiently add/remove nested containers without having to walk and recompute
      its elements.
      Containers don't have to chain up to a top-level owner, they can be temporary
      (in which case they don't yet take part in quota calculations

      It may contain values from different owners, however for best performance it should contain
      values from only a single owner. *)

  type nonrec 'a t = 'a nested t
  (** A data structure that contains values with tracked memory usage. *)

  val create: 'a printable -> ('b, [< `constant]) TrackedSize.immutable_size_of -> 'b t
  (** [create id pp_id size_of] creates a (nested) container.
      Once initialized it should be added using [add_nested] to an owner or another container.
      [pp_id id] will be used to pretty print in debug mode, and not otherwise stored.
      [size_of] will be used to compute the size of immutable elements.

      O(1) complexity.
    *)

  val add_immutable_item: ?owner:id -> 'a t -> 'a -> unit
  (** [add_immutable_item ?owner t item] adds [item] to [t]. Its size will be computed using
      the function provided at the time the container was created.
      [item] must be an immutable value, containing only immutable values and fields.
      In particular its size is not allowed to change, since that would cause [remove_item] to
      incorrectly update the count.
      [owner] defaults to the parent of this container, but can be manually specified.

      O(1) complexity
      *)

  val remove_immutable_item: ?owner: id -> 'a t -> 'a -> unit
  (** [remove_immutable_item ?owner t item] removes [item] from [t].
      [owner] must be specified and identical to [add_immutable_item] if it was specified there.

      @raise Invalid_argument if debug mode is enabled and the size of [item] has changed

      Complexity:
        - if all items have the same owner matching the container's - O(1)
        - if we have to walk all items due to mixed owners - O(items)
    *)
end
