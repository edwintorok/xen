(* Implementation constraints:
   
  - No dependencies outside of the libraries shipped with OCaml (makes it easier to use for unit tests during package builds)
  - It can optionally integrate with other libraries without depending on them (e.g. supply a logs reporter function)
  - Minimize memory allocation and CPU usage during normal operation
  - Limit maximum memory usage so that tracing can be left enabled for long running tests
  - Dump a debug trace when an error/exception is encountered, this one will necessarily be slow

  Note: exception: Atomic is a 4.12 feature, for older versions a backward compat
    https://github.com/c-cube/ocaml-atomic can be used
*)


module type TracedEvent = sig
  type t (** type of events to store in the trace ring *)
  val empty: t (** the empty event *)
  val to_string: t -> string
end

module StringEvent = struct
  type t = string
  let empty = ""
  let to_string t = t
end

module ExnEvent = struct
  type t = Printexc.raw_backtrace * exn
  let empty = Printexc.get_raw_backtrace (), Not_found
  let to_string (bt, exn) =
    (* don't allow any exceptions to escape *)
    let str =
      try Printexc.to_string exn
      with _ -> Printexc.to_string_default exn
    in
    str ^ "\n" ^ (Printexc.raw_backtrace_to_string bt)

  let get e = Printexc.get_raw_backtrace (), e
end

module GcEvent = struct
  type t = Gc.stat

  let to_string q =
    Printf.sprintf "major GC end: %d/%d heap words (top %d), compactions %d"
      q.Gc.live_words q.Gc.heap_words q.Gc.top_heap_words q.Gc.compactions

  let get = Gc.quick_stat
  let empty = get ()
end

type events = (float * string) list

module Make(E: TracedEvent)(Config: sig
  val limit_log2: int (** number of events to store = [2**limit_log2] *)
end) = struct
  (* avoid allocation: unpack fields into separate arrays *)
  let limit =
    assert (Config.limit_log2 > 0);
    1 lsl Config.limit_log2

  let mask = limit - 1

  let enabled = Atomic.make true

  let index = Atomic.make 0

  let events = Array.make limit E.empty

  let timestamps = Times.create limit

  let start () = Atomic.set enabled true
  let stop () = Atomic.set enabled false

  let reset () =
    stop ();
    (* race condition here: some [event] functions may still be running,
       but caller should ensure that doesn't happen
     *)
    Atomic.set index 0;
    Array.fill events 0 (Array.length events) E.empty;
    Times.fill timestamps;
    start ()

  let record_internal ev =
      (* assumes power of 2 size *)
      let events_idx = Atomic.fetch_and_add index 1 land mask in
      (Times.record [@ocaml.inlined]) timestamps events_idx;
      (* mask ensures we are within bounds *)
      Array.unsafe_set events events_idx ev
  (* avoid inlining this itself to prevent making callers too large *)

  (* performance critical: avoid allocation, and minimize function calls
      when disabled *)
  let record ev =
    if (Atomic.get [@ocaml.inlined]) enabled then
      record_internal ev
    [@@ocaml.inline]

  let recordf f =
    if (Atomic.get [@ocaml.inlined]) enabled then
      record_internal (f ())
    [@@ocaml.inline]

  (* end performance critical *)

  let rec getall lst idx =
    let i = (idx + limit) land mask in
    let timestamp = Times.get timestamps i in
    if Times.is_valid timestamp then
      let timestamp = Times.to_s timestamp
      and event =
        try E.to_string events.(i)
        with e ->
          "exception formatting: " ^ (ExnEvent.to_string (ExnEvent.get e))
      in
      getall ((timestamp, event) :: lst) (idx - 1)
    else
      (* we constructed the list by going backwards through the ring,
         so the list is in the correct order and we only traversed as much of
         the ring that had valid entries *)
      lst

  let dump () =
    stop ();
    let idx = Atomic.get index - 1 in
    let r = getall [] idx in
    reset ();
    r
end

let sort_timestamp (t0, _) (t1, _) = Float.compare t0 t1

(* 4.14: we could use Seq.sorted_merge *)
let sorted all =
  let a = all |> List.map Array.of_list |> Array.concat in
  Array.stable_sort sort_timestamp a;
  a

let dump all f =
  let print last (timestamp, event) = 
    let delta = if Float.is_nan last then 0. else timestamp -. last in
    Printf.ksprintf f "[%.9f](+%.8f) %s" timestamp delta event;
    timestamp
  in
  let (_:float) = all |> sorted |> Array.fold_left print Float.nan in
  ()

let register_gc ?(limit_log2=9) () =
  let module R = Make(GcEvent)(struct let limit_log2 = limit_log2 end) in
  let on_major_cycle_end () =
    R.record (GcEvent.get ());
  in
  let (_:Gc.alarm) = Gc.create_alarm on_major_cycle_end in
  ()

(* TODO: get access to this... *)
