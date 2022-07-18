module type Absolute = sig
  (** absolute clock raw value *)
  type t = private Uint62.t

  val now : unit -> t
  (** [now ()] retrieves the current absolute clock value.
    Avoids conversion and allocation where possible.
    May be subject to NTP adjustments, including discontinuos time jumps.
   *)

  val to_epoch_ns : t -> int64
  (** [to_epoch_ns t] converts [t] to nanoseconds since the Unix epoch,
  not called in a fastpath. *)

  val id : string
  (** [id] describes this timestamp source *)
end

module type Monotonic = sig
  (** monotonic clock raw value *)
  type t = private Uint62.t

  val elapsed : unit -> t
  (** [elapsed ()] retrieves the current monotonic clock value
  measuring elapsed time since an arbitrary fixed point.
  Forked processes use the same reference point, but other processes,
  and fork+execed processes may not.
  Avoids conversion and allocation where possible.
  May be subject to NTP adjustments, but not discontinuos time jumps.
  *)

  val to_ns : t -> int64
  (** [to_ns t] converts [t] to nanoseconds, not called in a fastpath. *)

  val id : string
  (** [id] describes this clock *)
end

module type Clock = sig
  include Absolute

  val sub_ns : t1:t -> t0:t -> int64 option
  (** [sub_ns ~t1 ~t0] calculates [t1 - t0].
    Only implemented for monotonic clocks. *)
end

(** a timestamp *)
type +'a t = 'a

module MakeNonMonotonic (A : Absolute) : Clock with type t = A.t t = struct
  include A

  let sub_ns ~t1:_ ~t0:_ = None (* not a monotonic clock *)
end

module Make (A : Absolute) (M : Monotonic) : Clock with type t = M.t t = struct
  type nonrec t = M.t t

  let now = M.elapsed

  let sub_ns ~t1 ~t0 =
    let delta = Int64.sub (M.to_ns t1) (M.to_ns t0) in
    if Int64.compare delta 0L < 0 then
      None (* shouldn't happen, clock went backwards, let caller handle it *)
    else
      Some delta

  let offset =
    let a0 = A.now () in
    let m0 = M.elapsed () in
    Int64.sub (A.to_epoch_ns a0) (M.to_ns m0)

  let to_epoch_ns t = Int64.add offset (M.to_ns t)

  let id = Printf.sprintf "%s (offset %Ld, reference: %s)" M.id offset A.id
end
