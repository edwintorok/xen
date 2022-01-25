module type ImmutableContainer = sig
  type size = int (** bytes, or words, or count, defined by container. *)

  type element (** the type of elements in the container *)

  val element_size: element -> size
  (** [element_size el] is the size of [el] computed in O(1) *)
  (*@ n = element_size el
      pure
      ensures 0 <= n
   *)

  type t (** the type of the container *)

  val count: t -> int
  (** [count t] is the number of elements in [t] computed in O(1) *)
  (*@ n = count t
      pure
      ensures 0 <= n *)

  val add: t -> element -> t
  (*@ r = add t e
      pure
   *)

  val remove: t -> element -> t
  (*@ r = remove t e
      pure
    *)

  val fold: ('a -> element -> 'a) -> 'a -> t -> 'a
  (*@ r = fold f init t
      pure *)

  (*@ function container_size(t: t) : integer =
    fold (fun s e -> s + element_size e) 0 t *)
end

module Make(I: ImmutableContainer): sig
  type size = I.size
  type container = I.t
  type element = I.element

  type raw =
    { container: container
    ; count_limit: int
    ; size_limit: int
    ; cached_size: int
    }

  type t (* = private raw, but `ortac` doesn't support it yet *)

  val raw: t -> raw
  (*@ r = raw t
      pure *)

  val size: t -> size
  (** [size t] computes the size of the container in O(1) *)
  (*@ n = size t
      ensures n = I.container_size (raw t).container *)

  (* these invariants will apply to all functions that return a type t anywhere *)
  (*@ invariant count t <= (raw t).count_limit *)
  (*@ invariant size t <= (raw t).size_limit *)
  (*@ invariant size t = (raw t).cached_size *)

  val create: container -> count_limit:int -> size_limit:int -> t
  (** [create c ~count_limit ~size_limit] creates a bounded container out of
      the immutable container [c].
      It will accept at most [count_limit] elements of [size_limit] size in total.
      The meaning of the size unit (bytes, words, counts) is defined by the underlying container.
      Typically [c] will be the empty container.
      @raises Invalid_argument if the container already exceeds the quota
      *)
  (*@ t = create container ~count_limit ~size_limit
      checks I.count container <= count_limit
      checks I.container_size container <= size_limit
      ensures (raw t).container = container
      ensures (raw t).count_limit = count_limit
      ensures (raw t).size_limit = size_limit
   *)

  val add: t -> element -> t option
  (** [add t el] adds [el] to [t], returning None if a limit is exceeded *)
  (*@ r = add t e
      pure *)

  val remove: t -> element -> t
  (** [remove t el] removes [el] from [t] *)
  (*@ r = remove t e
      pure
   *)
end

(* for testing *)
module TestList : sig
  type size = int (** bytes, or words, or count, defined by container. *)

  type element = string (** the type of elements in the container *)

  val element_size: element -> size
  (** [element_size el] is the size of [el] computed in O(1) *)
  (*@ n = element_size el
      pure
      ensures 0 <= n
   *)

  type t (** the type of the container *)

  val count: t -> int
  (** [count t] is the number of elements in [t] computed in O(1) *)
  (*@ n = count t
      pure
      ensures 0 <= n *)

  val add: t -> element -> t
  (*@ r = add t e
      pure
   *)

  val remove: t -> element -> t
  (*@ r = remove t e
      pure
    *)

  val fold: ('a -> element -> 'a) -> 'a -> t -> 'a
  (*@ r = fold f init t
      pure *)

  (*@ function container_size(t: t) : integer =
    fold (fun s e -> s + element_size e) 0 t *)
end

module BoundedList : Make(TestList)
