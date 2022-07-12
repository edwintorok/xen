(* Implementation constraints:
   
  - No dependencies outside of the libraries shipped with OCaml (makes it easier to use for unit tests during package builds)
  - It can optionally integrate with other libraries without depending on them (e.g. supply a logs reporter function)
  - Minimize memory allocation and CPU usage during normal operation
  - Limit maximum memory usage so that tracing can be left enabled for long running tests
  - Dump a debug trace when an error/exception is encountered, this one will necessarily be slow

  Note: exception: Atomic is a 4.12 feature, for older versions a backward compat
    https://github.com/c-cube/ocaml-atomic can be used
*)


module Event = struct
  type arg = string
  let empty_arg = ""

  type fn = unit -> string
  let empty_fn () = ""

  module Times0 = struct
    type t = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t

    (* caller ensures idx is within bounds by applying a mask based on ring size *)
    external record: t -> int -> unit = "stub_clock_record" [@@noalloc]

    let fill t = Bigarray.Array1.fill t 0L
    let create n =
      let t = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout n in
      fill t;
      t
    let is_valid t idx =
      Bigarray.Array1.get t idx <> 0L
    let get t idx =
      Bigarray.Array1.get t idx

    let to_s i = Int64.to_float i /. 1e9
  end

  module Times = struct
    open Ocaml_intrinsics.Perfmon

    let calibrate _ =
      let t0 = Unix.gettimeofday () in
      let tsc0 = rdtsc () in
      Unix.sleepf 0.03;
      let t1 = Unix.gettimeofday () in
      let tsc1 = rdtsc () in
      (t1 -. t0) /. Int64.(sub tsc1 tsc0 |> to_float)

    let tsc_min_freq, tsc_max_freq, tsc_to_s =
      let a = Array.init 10 calibrate in
      1. /. Array.fold_left Float.max Float.min_float a,
      1. /. Array.fold_left Float.min Float.max_float a,
      Array.fold_left (+.) 0. a /. float (Array.length a)

    let to_s i =
      float i *. tsc_to_s

    type t = int array
    let record t idx =
      (* this is noalloc *)
      t.(idx) <- Int64.to_int (rdtsc ())

    let fill t = Array.fill t 0 (Array.length t) 0
    let create n = Array.make n 0
    let is_valid t idx = t.(idx) <> 0
    let get t idx = t.(idx)
  end
end

(* avoid allocation: unpack fields into separate arrays *)
type t =
  { event_fns: Event.fn array
  ; event_args: Event.arg array 
  ; event_times: Event.Times.t
  ; events_idx: int Atomic.t
  ; enabled: bool Atomic.t
  }

let create ?(limit_pow2=9) () =
  let limit = 1 lsl limit_pow2 in
  { event_fns = Array.make limit Event.empty_fn
  ; event_args = Array.make limit Event.empty_arg
  ; event_times = Event.Times.create limit
  ; events_idx = Atomic.make 0
  ; enabled = Atomic.make true
  }

let start t = Atomic.set t.enabled true
let stop t = Atomic.set t.enabled false

let fillall a v = Array.fill a 0 (Array.length a) v

let reset t =
  stop t;
  (* race condition here: some [event] functions may still be running,
     but caller should ensure that doesn't happen
   *)
  Atomic.set t.events_idx 0;
  fillall t.event_fns Event.empty_fn;
  fillall t.event_args Event.empty_arg;
  Event.Times.fill t.event_times;
  start t

let record t fn msg =
  if Atomic.get t.enabled then
    (* assumes power of 2 size *)
    let events_idx = Atomic.fetch_and_add t.events_idx 1 land (Array.length t.event_fns - 1) in
    Event.Times.record t.event_times events_idx;
    t.event_fns.(events_idx) <- fn;
    t.event_args.(events_idx) <- msg

let event t msg = record t Event.empty_fn msg
let event1 t f x = record t (fun () -> f x) ""
let event2 t f x y = record t (fun () -> f x y) ""

let fmt_exn exn bt =
  (* don't allow any exceptions to escape *)
  let str =
    try Printexc.to_string exn
    with _ -> Printexc.to_string_default exn
  in
  str ^ "\n" ^ (Printexc.raw_backtrace_to_string bt)

let event_exn t exn =
  let bt = Printexc.get_raw_backtrace () in
  record t (fun () -> fmt_exn exn bt) "exception: "

let eventf t f =
  try
    record t Event.empty_fn (f ())
  with e ->
    event_exn t e

let dump t f =
  event t "dumping trace ring";
  event t (Printf.sprintf "TSC frequency: %.3fGHz (%.6f GHz - %.6f GHz)"
    (1e-9 /. Event.Times.tsc_to_s)
    (Event.Times.tsc_min_freq *. 1e-9)
    (Event.Times.tsc_max_freq *. 1e-9));
  stop t;
  let idx = Atomic.get t.events_idx in
  let last = ref nan in
  for i = idx to idx + Array.length t.event_args do
    let idx = i land (Array.length t.event_fns - 1) in
    if Event.Times.is_valid t.event_times idx then begin
      let timestamp = Event.Times.to_s (Event.Times.get t.event_times idx)
      and prefix = t.event_args.(idx)
      and suffix =
        try t.event_fns.(idx) ()
        with e ->
          let msg = fmt_exn e (Printexc.get_raw_backtrace ()) in
          "exception formatting: " ^ msg
      in
      if Float.is_nan !last then last := timestamp;
      Printf.ksprintf f "[%.9f](+%.9f) %s%s" timestamp (timestamp -. !last) prefix suffix;
      last := timestamp
    end
  done

let fmt_gc q =
  Printf.sprintf "major GC end: %d/%d heap words (top %d), compactions %d"
    q.Gc.live_words q.Gc.heap_words q.Gc.top_heap_words q.Gc.compactions

let register_gc t =
  let on_major_cycle_end () =
    event1 t fmt_gc (Gc.quick_stat ())
  in
  let (_:Gc.alarm) = Gc.create_alarm on_major_cycle_end in
  ()
