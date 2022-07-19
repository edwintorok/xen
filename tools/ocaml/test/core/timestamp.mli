(** timestamp *)
type t = private Uint62.t

val now : unit -> t
(** [now ()] retrieves the current timestamp.
  Avoids conversion and allocation where possible.
  May be subject to NTP adjustments, including discontinuos time jumps.
 *)

val to_epoch_ns : t -> int64
(** [to_epoch_ns t] converts [t] to nanoseconds since the Unix epoch.
  Not called in a fastpath. *)

val sub_ns : t1:t -> t0:t -> int64 option
(** [sub_ns ~t1 ~t0] calculates [t1 - t0].
  Only implemented for monotonic clocks. *)

val id : string
(** [id] describes this timestamp source *)
