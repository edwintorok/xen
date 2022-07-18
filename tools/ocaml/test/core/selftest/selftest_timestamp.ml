open Tracer_core

module Stuck = struct
  type t = Uint62.t

  let now () = Uint62.of_int64 (-42L)

  let to_epoch_ns = Uint62.to_int64

  let id = "stuck"
end

module Bad = struct
  type t = Uint62.t

  let cnt = ref 0

  let elapsed () =
    decr cnt ;
    Uint62.of_int64 (Int64.of_int !cnt)

  let now = elapsed

  let to_ns = Uint62.to_int64

  let to_epoch_ns t = Int64.add 420L (to_ns t)

  let id = "bad"
end

module Good = struct
  type t = Uint62.t

  let cnt = ref 0

  let elapsed () =
    incr cnt ;
    Uint62.of_int64 (Int64.of_int !cnt)

  let now = elapsed

  let to_ns = Uint62.to_int64

  let to_epoch_ns t = Int64.add 42L (to_ns t)

  let id = "good"
end

module NM = Timestamp.MakeNonMonotonic (Stuck)
module MBad = Timestamp.Make (Stuck) (Bad)
module MGood = Timestamp.Make (Bad) (Good)

let test () =
  let t0 = NM.now () in
  let t1 = NM.now () in
  assert (NM.sub_ns ~t1 ~t0 = None) ;
  assert (NM.to_epoch_ns t0 = NM.to_epoch_ns t1) ;

  let t0 = MBad.now () in
  let t1 = MBad.now () in
  assert (MBad.sub_ns ~t1 ~t0 = None) ;
  assert (MBad.sub_ns ~t1:t0 ~t0 = Some 0L) ;
  assert (MBad.sub_ns ~t1 ~t0:t1 = Some 0L) ;
  assert (MBad.to_epoch_ns t0 = Int64.add (MBad.to_epoch_ns t1) 1L) ;

  let t0 = MGood.now () in
  let t1 = MGood.now () in
  assert (MGood.sub_ns ~t1 ~t0 = Some 1L) ;
  assert (MGood.sub_ns ~t1:t0 ~t0 = Some 0L) ;
  assert (MGood.sub_ns ~t1 ~t0:t1 = Some 0L) ;
  assert (MGood.to_epoch_ns t0 = Int64.sub (MGood.to_epoch_ns t1) 1L) ;
  assert (MGood.to_epoch_ns t0 = MGood.to_epoch_ns t0)
