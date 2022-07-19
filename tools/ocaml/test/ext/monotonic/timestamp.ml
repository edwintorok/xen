type t = Uint62.t

let now () =
  (* could also use bechamel's monotonic clock and avoid allocation *)
  Mclock.elapsed_ns () |> Uint62.of_int64

let sub_ns ~t1 ~t0 =
  let i = Int64.sub Uint62.(to_int64 t1) Uint62.(to_int64 t0) in
  if Int64.compare i 0L < 0 then
    None (* shouldn't happen: clock went backwards, let caller handle *)
  else
    Some i

let offset =
  let s, ps = Pclock.now_d_ps () in
  let m = Mclock.elapsed_ns () in
  Int64.(sub (add (mul 1_000_000_000L @@ Int64.of_int s) (div ps 1000L)) m)

let to_epoch_ns m = Int64.add offset Uint62.(to_int64 m)

let id =
  Printf.sprintf "mirage-clock-unix (offset %Ldns = pclock - mclock)" offset
