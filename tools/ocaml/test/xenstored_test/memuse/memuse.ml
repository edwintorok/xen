module type Allocator = sig
  (** A memory allocator, either local or remote.
   Since we are testing the (fragmentation) behaviour of the GC
   the code in this module should minimize memory allocations
   outside of [init] and [finish].
  *)

  type t

  type item

  val empty : item
  (** [empty] is an item with minimal size. *)

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

  val alloc : t -> int -> item array
  (** [alloc t n] allocates as many items of size [n] as possible,
      where [size_min <= n <= size_max].
      Allocations can be done in parallel but they must all be completed when the sequence completes
      to ensure we stay within our quota.
      @return an allocated item sequence that can be deallocated without deallocating other items.
      The overhead of each item should be kept at a minimum, ideally it should be just the allocated
      item of size [n]
      *)

  val dealloc : t -> item -> unit
  (** [dealloc t item] deallocates [item]. Can only be called once for a given [item].
      The caller will set the array entry to [empty].
   *)

  val strategy_begin : t -> unit
  (** [strategy_begin t] is a debugging hook, called when an attack strategy starts,
      can be used to take a baseline memory measurement
   *)

  val strategy_done : t -> unit
  (** [strategy_done t] is a debugging hook, called when an attack cycle finishes,
      but before the memory is cleaned up (i.e. near peak fragmentation level).
   *)
end

module ProbeIndexedAllocator (A : sig
  (** Typically used with a remote system,
      so [alloc] here allocating an extra option is not a problem
   *)

  type t

  type item

  val empty : item
  (** [empty] is a minimal sized item *)

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
end) : Allocator = struct
  type t = {allocator: A.t; size_min: int; size_max: int}

  type item = A.item

  let empty = A.empty

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

  let alloc t n =
    let rec loop l =
      match A.alloc t.allocator n with
      | None ->
          Array.of_list l
      | Some v ->
          loop (v :: l)
    in
    loop []

  let dealloc t item = A.dealloc t.allocator item

  let strategy_begin _t = ()

  let strategy_done _t = ()
end

type 'a allocator = (module Allocator with type t = 'a)

type initialized = Alloc : 'a allocator * 'a -> initialized

(** [multiple allocators] combines multiple (remote) allocators into a single one
   that calls each allocator in turn *)
let multiple allocator_seq =
  let allocators = List.of_seq allocator_seq in
  (module struct
    type t = initialized list

    let empty () = ()

    type item = unit -> unit

    let init () =
      allocators
      |> List.map @@ fun (module A : Allocator) -> Alloc ((module A), A.init ())

    let size_min t =
      List.fold_left
        (fun m (Alloc ((module A), t)) -> min m (A.size_min t))
        max_int t

    let size_max t =
      List.fold_left
        (fun m (Alloc ((module A), t)) -> max m (A.size_max t))
        min_int t

    let finish = List.iter (fun (Alloc ((module A), t)) -> A.finish t)

    let alloc t n =
      (* allocate as much as possible from each allocator *)
      t
      |> List.map (fun (Alloc ((module A), t)) ->
             if n >= A.size_min t && n <= A.size_max t then
               A.alloc t n |> Array.map (fun item () -> A.dealloc t item)
             else
               [||]
         )
      |> Array.concat

    let dealloc _t item = item ()

    let strategy_begin t =
      t |> List.iter @@ fun (Alloc ((module A), t)) -> A.strategy_begin t

    let strategy_done t =
      t |> List.iter @@ fun (Alloc ((module A), t)) -> A.strategy_done t end
  : Allocator
  )

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
    ; mutable items: A.item array
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
      sizes (A.size_max allocator) [] minsize
      |> List.map (fun size -> {size; items= [||]})
      |> Array.of_list
    in
    if Array.length cycles = 0 then
      None
    else
      Some {allocator; cycles}

  let cycle_run t =
    A.strategy_begin t.allocator ;
    let () =
      t.cycles
      |> Array.iteri @@ fun i0 cycle ->
         Logs.debug (fun m -> m "Cycle %d" i0) ;
         (* allocate as many items as possible in current cycle of the current size *)
         cycle.items <- A.alloc t.allocator cycle.size ;
         Logs.debug (fun m -> m "Allocated") ;
         (* deallocate some of the previous items, such that the largest continuos gap is not
            enough to fulfil the next allocation size.
            In OCaml this won't immediately deallocate the item just make it possible for the GC
            to reuse the item in the future
         *)
         if i0 < Array.length t.cycles - 2 then
           for j0 = 0 to i0 do
             let a = t.cycles.(j0).items in
             a
             |> Array.iteri @@ fun k0 item ->
                (* formula in paper is k ∤ 2 ^ (i-j+1), using 1-based indexes.
                   We use 0 based indexes in OCaml, hence the modified formula here
                *)
                if (k0 + 1) mod (1 lsl (i0 - j0 + 1)) <> 0 && item != A.empty
                then (
                  a.(k0) <- A.empty ; A.dealloc t.allocator item
                )
           done
    in
    A.strategy_done t.allocator

  let cycle_cleanup t =
    t.cycles
    |> Array.iter @@ fun cycle ->
       Array.iter
         (fun item ->
           (* physical equality: do not deallocate empty! *)
           if item != A.empty then
             A.dealloc t.allocator item
         )
         cycle.items ;
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
    A.strategy_begin t.allocator ;
    let rec loop items size =
      if size < minsize then
        Array.concat items
      else
        loop (A.alloc t.allocator size :: items) (size / 2)
    in
    t.items <- loop [] (A.size_max t.allocator) ;
    A.strategy_done t.allocator

  let cycle_cleanup t =
    Array.iter
      (fun item ->
        if item != A.empty then
          A.dealloc t.allocator item
      )
      t.items ;
    t.items <- [||]
end

let run (module S : Strategy) =
  let alloc = S.A.init () in
  let rec loop minsize =
    match S.cycle_prepare ~minsize alloc with
    | None ->
        ()
    | Some cycle ->
        Logs.debug (fun m -> m "Starting cycle with size %d" minsize) ;
        S.cycle_run cycle ;
        S.cycle_cleanup cycle ;
        S.A.finish alloc ;
        (* due to item count limit we can't achieve optimum fragmentation and space usage,
           so have to try higher and higher sizes to start with *)
        loop (minsize * 2)
  in
  loop (S.A.size_min alloc) ;
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
    ((keysize + valuesize) * (maxentries / 2) * (maxtxn + 1))
    + (maxwatches * maxtokensize)

  let maxalloc_size = List.fold_left max maxtokensize [keysize; valuesize]

  let minalloc_size = 2 * word

  (* ignoring item count limits, this is to test the OCaml GC *)
  module NoCountLimit = struct
    type baseline = {top: int; live: int}

    type t = {
        mutable used: int
      ; mutable overhead: baseline
      ; mutable baseline_top_words: baseline option
    }

    type item = string

    let empty = ""

    let default = {top= 0; live= 0}

    let init () =
      Logs.debug (fun m ->
          m "allocator initialized, maxmem_controllable = %.2fMiB"
            (float maxmem_controllable /. 1024. /. 1024.)
      ) ;
      {used= 0; overhead= default; baseline_top_words= None}

    let size_min _ = minalloc_size

    let size_max _ = maxalloc_size

    let mib = 1024. *. 1024.

    let mib_of_words w = float (w * word) /. mib

    let strategy_begin t =
      match t.baseline_top_words with
      | Some _ ->
          ()
      | None ->
          Gc.compact () ;
          let q = Gc.quick_stat () in
          let baseline = {top= q.Gc.top_heap_words; live= q.Gc.live_words} in
          Logs.debug (fun m ->
              m "taken baseline GC measurement: %.2f MiB top, %.2f MiB live"
                (mib_of_words baseline.top)
                (mib_of_words baseline.live)
          ) ;
          t.baseline_top_words <- Some baseline

    let strategy_done t =
      t.overhead <- {t.overhead with top= max t.overhead.top t.overhead.live} ;
      let q = Gc.quick_stat () in
      Logs.debug (fun m -> m "running GC") ;
      (* needed to get accurate live_words count from stat *)
      Gc.full_major () ;
      let s = Gc.stat () in
      let baseline = Option.value ~default t.baseline_top_words in
      let baseline =
        {
          top= baseline.top + t.overhead.top
        ; live= baseline.live + t.overhead.live
        }
      in
      Logs.debug (fun m ->
          m "baseline top = %.2fMiB, live=%.2fMiB"
            (mib_of_words baseline.top)
            (mib_of_words baseline.live)
      ) ;
      (* heap=live when compacted in baseline *)
      let num = mib_of_words (q.Gc.heap_words - baseline.live) in
      (* heap words prior to GC *)
      let denum = mib_of_words (s.Gc.live_words - baseline.live) in
      (* accurate live words only known after Gc *)
      Logs.info (fun m ->
          m "Ratio: %.2f MiB (heap) / %.2f MiB (live) = %.3f" num denum
            (num /. denum)
      ) ;

      Logs.debug @@ fun m ->
      let num = mib_of_words (s.Gc.top_heap_words - baseline.top) in
      let denum = mib_of_words (s.Gc.live_words - baseline.live) in
      m "Ratio: %.2f MiB (top) / %.2f MiB (live) = %.3f" num denum (num /. denum)

    let finish t =
      assert (t.used = 0) ;
      t.overhead <- {t.overhead with live= 0}

    let alloc t n =
      if n mod word <> 0 then
        [||]
      else
        let avail = max 0 (maxmem_controllable - t.used) in
        let count = avail / n in
        if count = 0 then
          [||]
        else
          let elements =
            Array.init count @@ fun _ ->
            let strsize = max 1 ((((n / word) - 1) * word) - word) in
            (* in reality this should be a unique value to ensure oxenstored can't
               optimize the duplicates away
            *)
            String.make strsize 'x'
          in
          (* ensure they are all in the major heap, this could be done e.g. by
             sending a bunch of noop packets *)
          Gc.minor () ;
          t.used <- t.used + (count * n) ;
          (* the array itself is not part of what we're "allocating" *)
          let delta = 1 + Array.length elements in
          t.overhead <- {t.overhead with live= t.overhead.live + delta} ;
          Logs.debug (fun m -> m "used %d, count=%d, n=%d" t.used count n) ;
          elements

    let dealloc t item =
      if item <> "" then
        let n = word * (((String.length item + word) / word) + 1) in
        t.used <- t.used - n
  end

  module Strategy = WorstCase (NoCountLimit)

  let test () = run (module Strategy)
end
