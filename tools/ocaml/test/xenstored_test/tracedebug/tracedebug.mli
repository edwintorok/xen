(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(**
  Tracedebug is a tracing module that keeps track of the most recent
  N events and can be told to dump these when something goes wrong.
  This avoids flooding the console with debug messages during normal operation

  It is especially suitable for tracing high-volume events, e.g. debugging
  ring-buffer communication, recording events while fuzzing, where it is
  intended to be a low-overhead "always-on" tracer.

  When tracing is turned on it will have a constant memory and performance overhead.
*)

module type TracedEvent = sig
  (** type of events to store in the trace ring, must be immutable *)
  type t

  val empty : t
  (** the empty event *)

  val pp : Format.formatter -> t -> unit
  (** [pp formatter event] formats the event on [formatter].
    This will get called only when the events are dumped, much later than when
    the event got created, hence the immutability requirement on the event.
    Exceptions raised by this function are caught and dumped.
  *)
end

module StringEvent : TracedEvent with type t = string

module ExnEvent : sig
  include TracedEvent

  val get : exn -> t
end

module GcEvent : sig
  include TracedEvent

  val get : unit -> t
end

(** the type of an event tracer *)
type 'a t

(** the type of events *)
type events

val create : ?limit_log2:int -> 'a -> 'a t
(** [create ?limit_log2 empty] creates an event tracer with a ring
  of size [2**limit_log2]. When the ring is full old events will be overwritten.
  The ring stores events of the type of [empty].
  [empty] is an event that is never generated. *)

val start : _ t -> unit
(** [start t] starts tracing. idempotent.
   Starting the tracer doesn't wipe already recorded events.
*)

val stop : _ t -> unit
(** [stop t] stops tracing. idempotent.
   Stopping the tracer doesn't discard already recorded events.
*)

val reset : _ t -> unit
(** [reset t] forgets all events, and then inserts a reset event.
  Shouldn't be called concurrently with other [event] functions, or it can't guarantee
  to delete all events.
  *)

val dump : (Format.formatter -> 'a -> unit) -> 'a t -> events
(** [dump printer t] causes all events in [t] to be formatted using [printer],
  and returned as [events] *)

val record : 'a t -> 'a -> unit
(** [record t event] records the event [event] and the current timestamp.
*)

val recordf : 'a t -> (unit -> 'a) -> unit
(** [recordf t f] is equivalent to [record t @@ f()] when tracing is turned on,
  but avoids calling [f] when tracing is turned off.
  Examples for string formatting:
  [recordf t (fun () -> Printf.sprintf ...)] (* allocates a string *)
  [recordf t (fun () -> Format.asprintf ...)] (* not recommended, this allocates a buffer and a string *)

  If all the arguments are immutable values then using [record1] is better:
  it delays the formatting itself, and only formats when the ring is dumped
  *)

val record1 : 'a t -> ('b -> 'a) -> 'b -> unit
(** [record1 t f x] is like [recordf t @@ fun () -> f x], but
  avoids an allocation when tracing is off. *)

val pp_events : Format.formatter -> events list -> unit
(** [dump formatter events] prints all [events] using [formatter],
    sorted chronogically *)

val register_gc : ?limit_log2:int -> unit -> GcEvent.t t
(** [register_gc t] registers a GC alarm to log some quick GC stats on major cycles. *)
