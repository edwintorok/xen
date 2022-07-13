open Ocaml_intrinsics.Perfmon

type c = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t
(* caller ensures idx is within bounds by applying a mask based on ring size *)
external clock_record: c -> int -> unit = "stub_clock_record" [@@noalloc]

let calibrate _ =
  let t = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout 2 in
  clock_record t 0;
  let tsc0 = rdtsc () in
  Unix.sleepf 0.1;
  clock_record t 1;
  let tsc1 = rdtsc () in
  let delta_ns = Int64.sub t.{1} t.{0} |> Int64.to_float in
  let delta_tsc = Int64.sub tsc1 tsc0 |> Int64.to_float in
  delta_ns /. delta_tsc

let tsc_min_freq, tsc_max_freq, tsc_to_ns =
  let a = Array.init 10 calibrate in
  1. /. Array.fold_left Float.max Float.min_float a,
  1. /. Array.fold_left Float.min Float.max_float a,
  Array.fold_left (+.) 0. a /. float (Array.length a)

let () =
  Printf.eprintf "%.9f, %.9f\n" tsc_min_freq tsc_max_freq

let to_ns i =
  float i *. tsc_to_ns |> Int64.of_float

type t = int array
let record t idx =
  (* this is noalloc *)
  Array.unsafe_set t idx (Int64.to_int (rdtsc ()))
  [@@ocaml.inline]

let fill t = Array.fill t 0 (Array.length t) 0
let create n = Array.make n 0
let is_valid idx = idx <> 0
let get t idx = t.(idx)
