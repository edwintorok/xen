module type Allocator = sig
  type t
  type item

  val init: unit -> t
  (** [init ()] initializes an allocator, or returns None if no more allocators are available.
      For xenstore an allocator would be a transaction, or a watchlist
   *)

  val size_min: t -> int
  (** [size_min t] is the minimum allocation size *)

  val size_max: t -> int
  (** [size_max t] is the maximum allocation size *)

  val finish: t -> unit
  (** [finish t] ends the use of allocator [t] *)

  val alloc: t -> int -> item Seq.t
  (** [alloc t n] allocates as many items of size [n] as possible,
      where [size_min <= n <= size_max].
      Allocations can be done in parallel but they must all be completed when the sequence completes
      to ensure we stay within our quota.
      @return an allocated item sequence that can be deallocated without deallocating other items
      *)

  val dealloc: t -> item Seq.t -> unit
  (** [dealloc t item] deallocates [item]. Can only be called once for a given [item].
      Deallocations can be done in parallel but they must all be completed before this function returns
      to ensure we stay within our quota.
   *)
end

module type InitializedAllocator = sig
  module A: Allocator
  val t : A.t
end

let multiple allocator_seq =
  let allocators = Array.of_seq allocator_seq in
  (module struct
    type t = (module InitializedAllocator) array
    type item = unit -> unit

    let do_init (module A: Allocator) =
      (module struct
        module A = A
        let t = A.init ()
      end: InitializedAllocator)

    let init () = Array.map do_init allocators

    let size_min t =
      Array.fold_left (fun m (module I: InitializedAllocator) ->
        min m (I.A.size_min I.t)
      ) max_int t

    let size_max t =
      Array.fold_left (fun m (module I: InitializedAllocator) ->
        max m (I.A.size_max I.t)
      ) min_int t

    let finish = Array.iter (fun (module I: InitializedAllocator) -> I.A.finish I.t)

    let alloc t n =
      (* allocate as much as possible from each allocator *)
      t |> Array.to_seq |> Seq.flat_map @@ fun (module I: InitializedAllocator) ->
      I.A.alloc I.t n
      |> Seq.map @@ fun item -> (fun () -> I.A.dealloc I.t (Seq.return item))

    let dealloc _t items =
      items |> Seq.iter @@ fun f -> f ()

  end: Allocator)

module type Strategy = sig
  type t
  val init: (module Allocator) -> t
  val run: t -> unit
end
