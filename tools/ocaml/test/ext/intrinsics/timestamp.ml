type t = Uint62.t

let now () = Uint62.of_int64 @@ Ocaml_intrinsics.Perfmon.rdtsc ()

let calibrate () =
  let t0 = Tracer_monotonic.Timestamp.now () in
  let tsc0 = now () in
  Unix.sleepf 0.1 ;
  let t1 = Tracer_monotonic.Timestamp.now () in
  let tsc1 = now () in
  match Tracer_monotonic.Timestamp.sub_ns ~t1 ~t0 with
  | None ->
      invalid_arg "Timestamp source is non monotonic"
  | Some delta_ns ->
      let dtsc = Int64.sub (Uint62.to_int64 tsc1) (Uint62.to_int64 tsc0) in
      let ns_per_cycles = Int64.to_float delta_ns /. Int64.to_float dtsc
      and offset =
        Int64.sub
          (Tracer_monotonic.Timestamp.to_epoch_ns t0)
          (Uint62.to_int64 tsc0)
      in
      (ns_per_cycles, offset)

(* call separate function to be able to diferentiate in stack profiles *)
let ns_per_tsc_cycles, offset = calibrate ()

let to_ns t =
  ns_per_tsc_cycles *. Int64.to_float (Uint62.to_int64 t)
  |> Float.round (* OK to use 4.08 features due to lib deps *)
  |> Int64.of_float

let sub_ns ~t1 ~t0 =
  let i = Int64.sub (to_ns t1) (to_ns t0) in
  if Int64.compare i 0L < 0 then
    None (* TSC went backwards? *)
  else
    Some i

let to_epoch_ns t = Int64.add offset @@ to_ns t

let id =
  Printf.sprintf "RDTSC (freq: %.4f GHz, ref: %s)" (1. /. ns_per_tsc_cycles)
    Tracer_monotonic.Timestamp.id
