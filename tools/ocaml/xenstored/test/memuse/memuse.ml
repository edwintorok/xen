module type Allocator = sig
  type t

  type item

  val init : unit -> t
  (** [init ()] initializes an allocator, or returns None if no more allocators are available.
      For xenstore an allocator would be a transaction, or a watchlist
   *)

  val size_min : t -> int
  (** [size_min t] is the minimum allocation size *)

  val size_max : t -> int
  (** [size_max t] is the maximum allocation size *)

  val finish : t -> unit
  (** [finish t] ends the use of allocator [t] *)

  val alloc : t -> int -> item Seq.t
  (** [alloc t n] allocates as many items of size [n] as possible,
      where [size_min <= n <= size_max].
      Allocations can be done in parallel but they must all be completed when the sequence completes
      to ensure we stay within our quota.
      @return an allocated item sequence that can be deallocated without deallocating other items
      *)

  val dealloc : t -> item Seq.t -> unit
  (** [dealloc t item] deallocates [item]. Can only be called once for a given [item].
      Deallocations can be done in parallel but they must all be completed before this function returns
      to ensure we stay within our quota.
   *)

  val cycle_done: t -> unit
  (** [cycle_done t] is a debugging hook *)
end

module ProbeAllocator (A : sig
  type t

  type item

  val init : unit -> t
  (** [init ()] initializes the allocator.
     Min and max supported allocation sizes will be probed automatically.
   *)

  val alloc : t -> int -> item option
  (** [alloc t n] allocates an item of size [n] if possible *)

  val dealloc : t -> item -> unit
  (** [dealloc t item] deallocates exactly item [t] *)

  val finish : t -> unit
  (** [finish t] last call using [t] *)
end) =
struct
  type t = {allocator: A.t; size_min: int; size_max: int}

  type item = A.item

  let init () =
    let t = A.init () in
    let rec loop_min size =
      if size > Sys.max_array_length then
        invalid_arg "Allocator not working: all sizes failed" ;
      match A.alloc t size with
      | None ->
          (* probe sizes using powers of 2 until we succeed finding minimum *)
          loop_min (size * 2)
      | Some v ->
          A.dealloc t v ; size
    in
    let rec loop_max oksize size =
      match A.alloc t size with
      | None ->
          oksize
      | Some v ->
          A.dealloc t v ;
          (* probe sizes using powers of 2 until we fail allocating:
             the size just before the failed probe is the maximum supported *)
          loop_max size (size * 2)
    in
    let size_min = loop_min 1 in
    let size_max = loop_max size_min size_min in
    {allocator= t; size_min; size_max}

  let size_min t = t.size_min

  let size_max t = t.size_max

  let finish t = A.finish t.allocator

  let rec alloc t n () =
    match A.alloc t n with None -> Seq.Nil | Some v -> Seq.Cons (v, alloc t n)

  let dealloc t items = items |> Seq.iter (A.dealloc t)

  let cycle_done _t = ()
end

type 'a allocator = (module Allocator with type t = 'a)

type initialized = Alloc : 'a allocator * 'a -> initialized

(** [multiple allocators] combines multiple allocators into a single one
   that calls each allocator in turn *)
let multiple allocator_seq =
  let allocators = Array.of_seq allocator_seq in
  (module struct
    type t = initialized array

    type item = unit -> unit

    let init () =
      allocators
      |> Array.map @@ fun (module A : Allocator) -> Alloc ((module A), A.init ())

    let size_min t =
      Array.fold_left
        (fun m (Alloc ((module A), t)) -> min m (A.size_min t))
        max_int t

    let size_max t =
      Array.fold_left
        (fun m (Alloc ((module A), t)) -> max m (A.size_max t))
        min_int t

    let finish = Array.iter (fun (Alloc ((module A), t)) -> A.finish t)

    let alloc t n =
      (* allocate as much as possible from each allocator *)
      t
      |> Array.to_seq
      |> Seq.flat_map @@ fun (Alloc ((module A), t)) ->
         if n >= A.size_min t && n <= A.size_max t then
           A.alloc t n |> Seq.map @@ fun item () -> A.dealloc t (Seq.return item)
         else
           Seq.empty

    let dealloc _t items =
      (* TODO: deallocs could be grouped if we used a different type *)
      items |> Seq.iter @@ fun f -> f ()

    let cycle_done t =
      t |> Array.iter @@ fun (Alloc ((module A), t)) ->
      A.cycle_done t
  end: Allocator)

(** an attack strategy against an allocator *)
module type Strategy = sig
  type t

  module A : Allocator

  val cycle_prepare : ?minsize:int -> A.t -> t option

  val cycle_run : t -> unit

  val cycle_cleanup : t -> unit
end

module WorstCase (A : Allocator) : Strategy = struct
  module A = A
  (* References:
       "Upper Bounds for Dynamic Memory Allocation" Yusuf Hasan, Wei-Mei Chen, J. Morris Chang, and Bashar M. Gharaibeh
     J. M. Robson. "Bounds for Some Functions Concerning Dynamic Storage Allocation". Journal of the Association for Computing Machinery, Volume 21, Number 8, July 1974, pages 491-499.
     https://www.sqlite.org/malloc.html#_mathematical_guarantees_against_memory_allocation_failures
  *)

  type cycle = {
      size: int (* size of items allocated at this cycle *)
    ; mutable items: A.item option array
  }

  type t = {allocator: A.t; cycles: cycle array}

  let rec sizes size_max all size =
    if size > size_max then
      List.rev all
    else
      sizes size_max (size :: all) (size * 2)

  let cycle_prepare ?(minsize = 1) allocator =
    let minsize = max minsize (A.size_min allocator) in
    let cycles =
      Array.of_seq
        (sizes (A.size_max allocator) [] minsize
        |> List.to_seq
        |> Seq.map @@ fun size -> {size; items= [||]}
        )
    in
    if Array.length cycles = 0 then
      None
    else
      Some {allocator; cycles}

  let some x = Some x

  let cycle_run t =
    t.cycles
    |> Array.iteri @@ fun i0 cycle ->
        Logs.debug (fun m -> m "Cycle %d" i0);
       (* allocate as many items as possible in current cycle of the current size *)
       cycle.items <-
         A.alloc t.allocator cycle.size |> Seq.map some |> Array.of_seq ;
        Logs.debug (fun m -> m "Allocated");
       (* deallocate some of the previous items, such that the largest continuos gap is not
          enough to fulfil the next allocation size.
          In OCaml this won't immediately deallocate the item just make it possible for the GC
          to reuse the item in the future
       *)
       let deallocs = ref [] in
       for j0 = 0 to i0 do
         let a = t.cycles.(j0).items in
         a
         |> Array.iteri @@ fun k0 item ->
            (* formula in paper is k ∤ 2 ^ (i-j+1), using 1-based indexes.
               We use 0 based indexes in OCaml, hence the modified formula here
            *)
            if (k0 + 1) mod (1 lsl (i0 - j0 + 1)) <> 0 then
              match item with
              | Some dealloc ->
                assert (a.(k0) <> None) ;
                (* can only dealloc once *)
                a.(k0) <- None ;
                deallocs := dealloc :: !deallocs
              | None -> ()
       done ;
       A.dealloc t.allocator @@ List.to_seq !deallocs

  let cycle_cleanup t =
    A.cycle_done t.allocator;
    t.cycles
    |> Array.iter @@ fun cycle ->
       cycle.items
       |> Array.to_seq
       |> Seq.filter_map (fun x -> x)
       |> A.dealloc t.allocator ;
       cycle.items <- [||]
end

(* We are limited both in amount and count, so try allocating max number of items
   of max size and then try smaller sizes.
   This isn't about fragmentation, but about using the max amount of memory
   within the existing number and amount quotas
*)
module MaxAlloc (A : Allocator) : Strategy = struct
  module A = A

  type t = {allocator: A.t; mutable items: A.item array}

  let cycle_prepare ?minsize t =
    match minsize with
    | Some m when m <> A.size_min t ->
        None (* just one cycle *)
    | _ ->
        Some {allocator= t; items= [||]}

  let cycle_run t =
    let minsize = A.size_min t.allocator in
    let rec loop size =
      if size < minsize then
        Seq.empty
      else
        Seq.append (A.alloc t.allocator size) @@ loop (size / 2)
    in
    t.items <- loop (A.size_max t.allocator) |> Array.of_seq

  let cycle_cleanup t =
    A.cycle_done t.allocator;
    A.dealloc t.allocator (Array.to_seq t.items) ;
    t.items <- [||]
end

let run (module S : Strategy) =
  let alloc = S.A.init () in
  let rec loop minsize =
    match S.cycle_prepare ~minsize alloc with
    | None -> ()
    | Some cycle ->
        Logs.debug (fun m -> m "Starting cycle with size %d" minsize);
        S.cycle_run cycle ;
        S.cycle_cleanup cycle ;
        S.A.finish alloc;
        (* due to item count limit we can't achieve optimum fragmentation and space usage,
           so have to try higher and higher sizes to start with *)
        loop (minsize * 2)
  in
  loop (S.A.size_min alloc);
  S.A.finish alloc

module OCamlAllocator = struct
  let maxpacket = 4096

  let keysize = 1024

  let valuesize = 2048

  let maxentries = 8192

  let maxwatches = 512

  let headersize = 16

  let maxtokensize = maxpacket - keysize - headersize - 1

  let maxtxn = 10

  let word = Sys.word_size / 8

  let max_split_keysize = keysize / 2 * 2 * word

  let permdomid_digits = 3

  let max_split_perm_size =
    let perms =
      (maxpacket - keysize - headersize - 1) / (1 + 1 + permdomid_digits)
    in
    3 * word * perms

  let maxmem =
    ((keysize + valuesize + max_split_perm_size) * maxentries * (maxtxn + 1))
    + (maxwatches * (max_split_keysize + maxtokensize))

  let maxmem_controllable =
    ((keysize + valuesize) * (maxentries/2) * (maxtxn + 1))
    + (maxwatches * maxtokensize)

  let maxalloc_size = List.fold_left max maxtokensize [keysize; valuesize]

  let minalloc_size = 2 * word

  (* ignoring item count limits, this is to test the OCaml GC *)
  module NoCountLimit = struct
    type t = {mutable used: int}

    type item = string ref

    let init () =
      Logs.debug (fun m ->
          m "allocator initialized, maxmem_controllable = %.2fMiB"
            (float maxmem_controllable /. 1024. /. 1024.)
      ) ;
      {used= 0}

    let size_min _ = minalloc_size

    let size_max _ = maxalloc_size

    let cycle_done _t =
      let q = Gc.quick_stat () in
      Logs.debug (fun m -> m "running GC") ;
      (* needed to get accurate live_words count from stat *)
      Gc.full_major () ;
      let s = Gc.stat () in
      let num = q.Gc.heap_words in (* heap words prior to GC *)
      let denum = s.Gc.live_words in (* accurate live words only known after Gc *)
      Logs.info (fun m ->
        m "Ratio: %d (heap) / %d (live) = %.3f" num denum (float num /. float denum)
      );

      Logs.debug @@ fun m ->
      let num = s.Gc.top_heap_words in
      let denum = s.Gc.live_words in
      m "Ratio: %d (top) / %d (live) = %.3f" num denum (float num /. float denum)

    let finish t = assert (t.used = 0)

    let alloc t n =
      if n mod word <> 0 then Seq.empty
      else
      let avail = max 0 (maxmem_controllable - t.used) in
      let count = avail / n in
      if count = 0 then
        Seq.empty
      else
        let elements =
          Array.init count @@ fun _ ->
          let strsize = max 1 (((n/word - 1) * word) - word) in
          (* in reality this should be a unique value to ensure oxenstored can't
             optimize the duplicates away
          *)
          ref (String.make strsize 'x')
        in
        let result = Array.to_seq elements in
        (* ensure they are all in the major heap, this could be done e.g. by
           sending a bunch of noop packets *)
        Gc.minor () ;
        t.used <- t.used + count * n;
        Logs.debug (fun m -> m "used %d, count=%d, n=%d" t.used count n);
        result

    let dealloc t items =
      items |> Seq.iter @@ fun item ->
        if !item <> "" then
          t.used <- t.used - (word * Obj.reachable_words (Obj.repr !item));
        item := "";
  end

  module Strategy = WorstCase (NoCountLimit)

  let test () = run (module Strategy)
end
