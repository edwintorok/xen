(**
  Tracedebug is a tracing module that keeps track of the most recent
  N events and can be told to dump these when something goes wrong.
  This avoids flooding the console with debug messages during normal operation

  It is especially suitable for tracing high-volume events, e.g. debugging
  ring-buffer communication, recording events while fuzzing, where it is
  intended to be a low-overhead "always-on" tracer.

  When tracing is turned off it will have a constant memory and performance overhead.
*)

type t (** an event tracer *)

val create : ?limit_pow2:int -> unit -> t
(** [create ?limit_pow2 ()] creates a new tracer, and starts tracing.
  [limit_pow2] controls how many past events to remember.
  *)

val start: t -> unit
(** [start t] starts tracing. idempotent.
   Starting the tracer doesn't wipe already recorded events.
*)

val stop: t -> unit
(** [stop t] stops tracing. idempotent.
   Stopping the tracer doesn't discard already recorded events.
*)

val reset: t -> unit
(** [reset t] forgets all events, and then inserts a reset event.
  Shouldn't be called concurrently with other [event] functions, or it can't guarantee
  to delete all events.
  *)

val dump: t -> (string -> unit) -> unit
(** [dump t f] causes all events in [t] to be formatted as strings and printed using [f].
    The event trace is then [reset].
    Note that the strings may contain embedded newlines (e.g. for stacktraces)
*)

val register_gc: t -> unit
(** [register_gc t] registers a GC alarm to log some quick GC stats on major cycles. *)

val event: t -> string -> unit
(** [event t msg] records the event [msg].
  [msg] should be a string that is already computed even when tracing is otherwise off.
  If that is not the case consider using [event1], [event2], of [eventf] instead
  to avoid the overhead of computing the string when not needed.
*)

val event_exn: t -> exn -> unit
(** [event_exn t exn] logs the occurence of [exn].
  It is recommended to call this as soon as possible, before the backtrace is overwritten.
  This won't on its own trigger an event trace dump!
*)

val eventf: t -> (unit -> string) -> unit
(** [eventf t f] is like [event t @@ f ()] when tracing is enabled, but potentially faster.
  If [f] raises any exceptions it is caught and logged as an event, but doesn't
  otherwise let the exception escape the function.
  When tracing is enabled [f] is called immediately, so it is suitable for formatting mutable
  data structures.
*)

val event1: t -> ('a -> string) -> 'a -> unit
(** [event1 t f a] is like [eventf @@ fun () -> f a] but evaluates [f] only when
  the trace is dumped. Thus [a] has to be an immutable data structure. *)

val event2: t -> ('a -> 'b -> string) -> 'a -> 'b -> unit
(** [event2 t f a b] is like [eventf @@ fun () -> f a b] but evaluates [f] only when
  the trace is dumped. Thus both [a] and [b] have to be immutable data structures. *)
