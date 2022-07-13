(* Implementation constraints:
   
  - No dependencies outside of the libraries shipped with OCaml (makes it easier to use for unit tests during package builds)
  - It can optionally integrate with other libraries without depending on them (e.g. supply a logs reporter function)
  - Minimize memory allocation and CPU usage during normal operation
  - Limit maximum memory usage so that tracing can be left enabled for long running tests
  - Dump a debug trace when an error/exception is encountered, this one will necessarily be slow

  Note: exception: Atomic is a 4.12 feature, for older versions a backward compat
    https://github.com/c-cube/ocaml-atomic can be used
*)
open Tracedebug_times
let pid = Unix.getpid ()
module type TracedEvent = sig
  type t

  val empty: t

  val pp: Format.formatter -> t -> unit
end

module StringEvent = struct
  type t = string
  let empty = ""
  let pp ppf s =
    Format.pp_print_string ppf s
end

module ExnEvent = struct
  type t = Printexc.raw_backtrace * exn
  let empty = Printexc.get_raw_backtrace (), Not_found

  let pp_backtrace ppf bt =
    Printexc.raw_backtrace_to_string bt
    |> Format.pp_print_text ppf

  let pp ppf (bt, exn) =
    (* don't allow any exceptions to escape *)
    let str =
      try Printexc.to_string exn
      with _ -> Printexc.to_string_default exn
    in
    Format.fprintf ppf "@[<v>Exception: %s@,%a@]" str
      pp_backtrace bt

  let get e = Printexc.get_raw_backtrace (), e
end

module GcEvent = struct
  type t = Gc.stat

  let pp ppf q =
    Format.fprintf ppf "major GC end: %d/%d heap words (top %d), compactions %d"
      q.Gc.live_words q.Gc.heap_words q.Gc.top_heap_words q.Gc.compactions

  let get = Gc.quick_stat
  let empty = get ()
end

type events = (int64 * (Format.formatter -> unit)) list

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
      Times.record timestamps events_idx;
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
      let timestamp = Times.to_ns timestamp
      and event ppf =
        try E.pp ppf events.(i)
        with e ->
          let ee = ExnEvent.get e in
          Format.fprintf ppf "@,Exception formatting: %a" ExnEvent.pp ee
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
    getall [] idx
    (* do not reset here, formatting is delayed *)
end

let sort_timestamp (t0, _) (t1, _) = Int64.compare t0 t1

(* 4.14: we could use Seq.sorted_merge *)
let sorted all =
  let a = all |> List.map Array.of_list |> Array.concat in
  Array.stable_sort sort_timestamp a;
  a

let time_frac_scale =
  10. ** (float (9 - Times.precision)) |> Int64.of_float

let pp_timestamp ppf i =
  let open Monotonic_clock in
  let s = Int64.unsigned_div i nsec_per_s
  and ns = Int64.unsigned_rem i nsec_per_s in
  let frac = Int64.unsigned_div ns time_frac_scale in
  Format.fprintf ppf "%Lu.%0*Lus" s Times.precision frac

let get_overhead () =
  let n = 10000 in
  let a = Times.create n in
  for i = 0 to n-1 do
    Times.record a i
  done;
  let t = Array.init n (fun i -> Times.get a i |> Times.to_ns) in
  let avg = Int64.unsigned_div
    (Int64.sub t.(n-1) t.(0))
    (Int64.of_int (n-1)) in
  let t = Array.mapi (fun i ti ->
    if i > 0 then Int64.sub ti t.(i-1)
    else 0L
  ) t in 
  let t = Array.sub t 1 (n-1) in
  Array.fold_left (fun acc dt ->
    (* ignore 0 values, that is just precision limit, it is not really
       0 overhead *)
    if dt > 0L then Int64.min acc dt else acc)
    Int64.max_int t,
  avg,
  Array.fold_left Int64.max Int64.min_int t

let dump ppf all =
  let print last (timestamp, pp_event) = 
    let delta = if Int64.compare last Int64.min_int = 0 then 0L else Int64.sub timestamp last in
    Format.fprintf ppf "[pid %d][%a](+%a) " pid pp_timestamp timestamp pp_timestamp delta;
    pp_event ppf;
    Format.pp_print_cut ppf ();
    timestamp
  in
  let o_min, o_avg, o_max = get_overhead () in
  Format.fprintf ppf "Using clock: %s. Overhead: ~%Luns [%Luns, %Luns]@," Times.id o_avg o_min o_max;
  let (_:int64) = all |> sorted |> Array.fold_left print Int64.min_int in
  Format.pp_print_flush ppf ()

let register_gc ?(limit_log2=9) () =
  let module R = Make(GcEvent)(struct let limit_log2 = limit_log2 end) in
  let on_major_cycle_end () =
    R.record (GcEvent.get ());
  in
  let (_:Gc.alarm) = Gc.create_alarm on_major_cycle_end in
  ()

(* TODO: get access to this... *)
