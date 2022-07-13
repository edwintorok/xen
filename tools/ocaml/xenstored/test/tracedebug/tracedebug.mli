(**
  Tracedebug is a tracing module that keeps track of the most recent
  N events and can be told to dump these when something goes wrong.
  This avoids flooding the console with debug messages during normal operation

  It is especially suitable for tracing high-volume events, e.g. debugging
  ring-buffer communication, recording events while fuzzing, where it is
  intended to be a low-overhead "always-on" tracer.

  When tracing is turned off it will have a constant memory and performance overhead.
*)

module type TracedEvent = sig
  type t (** type of events to store in the trace ring, must be immutable *)
  val empty: t (** the empty event *)

  val to_string: t -> string
  (** [to_string event] formats the event as a string.
    This will get called only when the events are dumped, much later than when
    the event got created, hence the immutability requirement on the event
  *)
end

module StringEvent: TracedEvent with type t = string
module ExnEvent: sig
  include TracedEvent
  val get: exn -> t
end

type events

module Make(E: TracedEvent)(C: sig
  val limit_log2: int (** number of events to store = [2**limit_log2] *)
end) : sig

  val start: unit -> unit
  (** [start ()] starts tracing. idempotent.
     Starting the tracer doesn't wipe already recorded events.
  *)

  val stop: unit -> unit
  (** [stop ()] stops tracing. idempotent.
     Stopping the tracer doesn't discard already recorded events.
  *)

  val reset: unit -> unit
  (** [reset ()] forgets all events, and then inserts a reset event.
    Shouldn't be called concurrently with other [event] functions, or it can't guarantee
    to delete all events.
    *)

  val dump: unit -> events
  (** [dump t f] causes all events in [t] to be formatted as strings and returned.
      The event trace is then [reset].
      Note that the strings may contain embedded newlines (e.g. for stacktraces)
  *)

  val record: E.t -> unit
  (** [record event] records the event [event] and the current timestamp.
  *)

  val recordf: (unit -> E.t) -> unit
  (** [recordf f] is equivalent to [record @@ f()] when tracing is turned on,
    but avoids calling [f] when tracing is turned off. *)

end

val dump: events list -> (string -> unit) -> unit
(** [dump events f] prints all [events] using [f]. *)

val register_gc: ?limit_log2:int -> unit -> unit
(** [register_gc t] registers a GC alarm to log some quick GC stats on major cycles. *)
