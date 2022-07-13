open Ocaml_intrinsics.Perfmon

let measure_tsc_to_ns () =
  let t0 = Monotonic_clock.get_ns () in
  let tsc0 = rdtsc () in
  Unix.sleepf 0.1; (* delay doesn't need to be accurate, time is measured *)
  let t1 = Monotonic_clock.get_ns () in
  let tsc1 = rdtsc () in
  let delta_ns = Int64.sub t1 t0 |> Int64.to_float in
  let delta_tsc = Int64.sub tsc1 tsc0 |> Int64.to_float in
  delta_ns /. delta_tsc

let tsc_to_ns =
  let env = "TRACEDEBUG_TSC_TO_NS" in
  (* ensure children use consistent values for calibration,
     so that timestamps from parents and children are sortable *)
  match Sys.getenv_opt env with
  | None ->
      let r = measure_tsc_to_ns () in
      Unix.putenv env (Printf.sprintf "%h" r);
      r
  | Some s -> Float.of_string s

type t = int array

(* records timestamps using the processor's TSC.
   assumes constant tsc rate independent of CPU frequency adjustments
   and that TSCs across CPUs are synchronized
 *)
let record t idx =
  (* this is noalloc *)
  Array.unsafe_set t idx (Int64.to_int (rdtsc ()))
  [@@ocaml.inline]

let to_ns i =
  float i *. tsc_to_ns |> Int64.of_float

let precision = 8 (* ~20ns overhead *)

let fill t = Array.fill t 0 (Array.length t) 0
let create n = Array.make n 0
let is_valid idx = idx <> 0
let get t idx = t.(idx)
