type 'a size = 'a Memory_size.t
type require_nestable = Memory_size.require_nestable
type array_compatible = Memory_size.array_compatible
module Queue: sig
  type 'a t

  val create_sized : ('a -> [< Memory_size.require_nestable] size) -> 'a t

  val add: 'a -> 'a t -> unit

  val push: 'a -> 'a t -> unit

  val take: 'a t -> 'a

  val pop: 'a t -> 'a

  val peek: 'a t -> 'a

  val top: 'a t -> 'a

  val clear: 'a t -> unit

  val is_empty: 'a t -> bool

  val copy: 'a t -> 'a t

  val length: 'a t -> int

  val iter: ('a -> unit) -> 'a t -> unit

  val fold: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val transfer: 'a t -> 'a t -> unit

  val initial:  [> array_compatible] size
  val item_overhead: Sizeops.Size.t
  val size_of: 'a t -> [> `updatable] size
end

module Hashtbl : sig
  type ('a, 'b) t

  val create_sized: ('a -> [< require_nestable] size) -> ('b -> [< require_nestable] size) -> int -> ('a, 'b) t

  val reset: ('a, 'b) t -> unit

  val copy: ('a, 'b) t -> ('a, 'b) t

  val remove: ('a, 'b) t -> 'a -> unit

  val replace: ('a, 'b) t -> 'a -> 'b -> unit

  val find: ('a, 'b) t -> 'a -> 'b
  val find_all: ('a, 'b) t -> 'a -> 'b list

  val mem: ('a, 'b) t -> 'a -> bool

  val iter: ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  val fold: ('a ->  'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

  val length: ('a, 'b) t -> int

  val initial:  ('a, 'b) Hashtbl.t -> [> array_compatible] size
  val item_overhead: Sizeops.Size.t
  val size_of: ('a, 'b) t -> [> require_nestable] size
end

module SizedList: sig
  type 'a t

  val filter: ('a -> bool) -> 'a t -> 'a t
  val empty:  ('a -> [< require_nestable] size) -> 'a t

  val cons: 'a -> 'a t -> 'a t
  val length: 'a t -> int
  val hd: 'a t -> 'a
  val tl: 'a t -> 'a t
  val rev_append: 'a t -> 'a t -> 'a t

  val iter: ('a -> unit) -> 'a t -> unit
  val rev_map: ('b -> [< require_nestable] size) -> ('a -> 'b) -> 'a t -> 'b t
  val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val size_of: 'a t -> [> require_nestable] size

  val for_all: ('a -> bool) -> 'a t -> bool
  val exists: ('a -> bool) -> 'a t -> bool
  val find: ('a -> bool) -> 'a t -> 'a

  val rev: 'a t -> 'a t
  val is_empty: 'a t -> bool

  val initial:  [> array_compatible] size
  val item_overhead: [> array_compatible] size
end
