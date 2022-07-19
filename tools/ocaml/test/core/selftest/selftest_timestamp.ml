open Tracer_core

let () =
  let t = Array.init 100 (fun _ -> Timestamp.now ()) in
  Int64.div
  (Timestamp.sub_ns ~t1:t.(99) ~t0:t.(0) |> Option.get) 100L
  |> Printf.printf "overhead: %Ld ns\n"

let test () =
  Format.printf "Testing %s@." Timestamp.id ;
  let t0 = Timestamp.now () in
  let t1 = Timestamp.now () in
  let is_monotonic = Timestamp.sub_ns ~t1 ~t0 <> None in
  if is_monotonic then (
    assert (Timestamp.sub_ns ~t1:t0 ~t0 = Some 0L) ;
    assert (Timestamp.sub_ns ~t1 ~t0:t1 = Some 0L) ;
    assert (Timestamp.sub_ns ~t1 ~t0 <> None)
  ) ;
  assert (Timestamp.to_epoch_ns t0 = Timestamp.to_epoch_ns t0) ;
  assert (Timestamp.to_epoch_ns t1 = Timestamp.to_epoch_ns t1) ;
  assert (Timestamp.to_epoch_ns t1 >= Timestamp.to_epoch_ns t0) ;

  let t2 = Timestamp.now () in
  Unix.sleepf 0.01 ;
  (* 10ms *)
  let t3 = Timestamp.now () in

  (* checks that clock is not permanently stuck,
     we should get at least 10ms resolution
  *)
  let i3 = Timestamp.to_epoch_ns t3 and i2 = Timestamp.to_epoch_ns t2 in
  if i2 >= i3 then
    failwith (Printf.sprintf "expected %Ld < %Ld" i2 i3) ;
  if is_monotonic then
    match Timestamp.sub_ns ~t1:t3 ~t0:t2 with
    | None ->
        assert false
    | Some dt ->
        assert (dt >= 10_000_000L)
