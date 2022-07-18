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
type +'a t = private 'a

module MakeNonMonotonic (A : Absolute) : Clock with type t = A.t t

module Make (A : Absolute) (M : Monotonic) : Clock with type t = M.t t
