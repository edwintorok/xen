open Ocaml_intrinsics.Perfmon

let measure_tsc_to_ns () =
  let t0 = Monotonic_clock.get_ns () in
  let tsc0 = rdtsc () in
  Unix.sleepf 0.2; (* delay doesn't need to be accurate, time is measured *)
  let t1 = Monotonic_clock.get_ns () in
  let tsc1 = rdtsc () in
  let delta_ns = (Int64.sub t1 t0) |> Int64.to_float in
  let delta_tsc = (Int64.sub tsc1 tsc0) |> Int64.to_float in
  (* round value to avoid this changing every time the program is run *)
  1e4 /. (1e4 *. delta_tsc /. delta_ns |> Float.round)

let cycle_duration_ns =
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
  float i *. cycle_duration_ns |> Float.round |> Int64.of_float

let precision = 8 (* ~10-50ns overhead, drop 1 digit *)

let fill t = Array.fill t 0 (Array.length t) 0
let create n = Array.make n 0
let is_valid idx = idx <> 0
let get t idx = t.(idx)

let id = Printf.sprintf "RDTSC, frequency: %.4f GHz" (1. /. cycle_duration_ns) 
