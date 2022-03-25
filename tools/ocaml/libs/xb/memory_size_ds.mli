type 'a size = 'a Memory_size.t
type forbid_updates = Memory_size.forbid_updates
type array_compatible = Memory_size.array_compatible

module Buffer: sig
  type t
  val create: int -> t
  val length: t -> int
  val add_char: t -> char -> unit
  val add_substring: t -> string -> int -> int -> unit
  val reset: t -> unit
  val size_of: t -> [> `updatable] size
  val contents: t -> string
end

module Ref : sig
  type 'a t
  val make: ('a -> [< Memory_size.require_nestable] size) -> 'a -> 'a t
  val set: 'a t -> 'a -> unit
  val get: 'a t -> 'a
  val size_of: 'a t -> [> `updatable] size
  val copy: ('a -> 'a) -> 'a t -> 'a t
end

module Queue: sig
  type 'a t

  val create_sized : ('a -> [< forbid_updates] size) -> 'a t

  val add: 'a -> 'a t -> unit

  val push: 'a -> 'a t -> unit

  val take: 'a t -> 'a

  val pop: 'a t -> 'a

  val peek: 'a t -> 'a

  val top: 'a t -> 'a

  val clear: 'a t -> unit

  val is_empty: 'a t -> bool

  val length: 'a t -> int

  val iter: ('a -> unit) -> 'a t -> unit

  val fold: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val transfer: 'a t -> 'a t -> unit

  val initial:  [> array_compatible] size
  val item_overhead: [> array_compatible] size
  val size_of: 'a t -> [> `updatable] size
end

module Hashtbl : sig
  type ('a, 'b) t

  val create_sized: ('a -> [< forbid_updates] size) -> ('b -> [< forbid_updates] size) -> int -> ('a, 'b) t

  val reset: ('a, 'b) t -> unit

  val remove: ('a, 'b) t -> 'a -> unit

  val replace: ('a, 'b) t -> 'a -> 'b -> unit

  val find: ('a, 'b) t -> 'a -> 'b
  val find_all: ('a, 'b) t -> 'a -> 'b list

  val mem: ('a, 'b) t -> 'a -> bool

  val iter: ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  val fold: ('a ->  'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

  val length: ('a, 'b) t -> int

  val initial:  ('a, 'b) Hashtbl.t -> [> array_compatible] size
  val item_overhead: [> array_compatible] size
  val size_of: ('a, 'b) t -> [> Memory_size.require_nestable] size
end

module SizedList: sig
  type 'a t

  val of_list : ('a -> [< forbid_updates] size) -> 'a list -> 'a t

  val filter: ('a -> bool) -> 'a t -> 'a t
  val empty:  ('a -> [< forbid_updates] size) -> 'a t
  val to_list: 'a t -> 'a list

  val cons: 'a -> 'a t -> 'a t
  val length: 'a t -> int
  val hd: 'a t -> 'a
  val tl: 'a t -> 'a t
  val rev_append: 'a t -> 'a t -> 'a t

  val iter: ('a -> unit) -> 'a t -> unit
  val rev_map: ('b -> [< forbid_updates] size) -> ('a -> 'b) -> 'a t -> 'b t
  val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val size_of: 'a t -> [> forbid_updates] size

  val for_all: ('a -> bool) -> 'a t -> bool
  val exists: ('a -> bool) -> 'a t -> bool
  val find: ('a -> bool) -> 'a t -> 'a

  val rev: 'a t -> 'a t
  val is_empty: 'a t -> bool

  val initial:  [> array_compatible] size
  val item_overhead: [> array_compatible] size
end
