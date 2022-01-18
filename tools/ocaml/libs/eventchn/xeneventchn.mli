(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
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

(** Event channel bindings: see tools/libxc/include/xenctrl.h *)

(*@ open Stdlib *)
(*@ function evtchn_is_fifo: bool *)
(*@ function max_evtchns: integer =
  if evtchn_is_fifo then 131072 else Sys.word_size * Sys.word_size *)

type t
(** A local event channel. *)
(*@ model port: int
    invariant 0 <= port < max_evtchns
 *)

(*@ type port_state =
  { notified: bool
  ; masked: bool
  }
*)

type handle
(** An initialised event channel interface. *)
(*@ model fd: int
    mutable model used_ports: t set
    mutable model ports: port_state array
    invariant Array.length ports = max_evtchns
    invariant Set.cardinal used_ports < max_evtchns
 *)

(* An event channel is modeled as having a file descriptor and a set of used ports,
   allocating a new one every time a virq or remote port is bound.
   The predicates below are helpers used in the specifications below to ensure
   that functions get called only with valid ports, and that newly allocated ports are unique.
*)

(*@ predicate is_port_used(h: handle)(port: t) = Set.mem port h.used_ports *)
(*@ predicate port_unmodified(h: handle)(port: t) =
  h.ports[port.port] = (old h).ports[port.port] *)

(*@ predicate modify_port(h: handle)(port: t)(f: port_state -> port_state) =
      is_port_used h port ->
        Set.for_all (fun p -> port_unmodified h p) (Set.remove port h.used_ports)
        && h.ports[port.port] = f (old h).ports[port.port] *)

(*@ predicate allocated_port(h: handle)(port: t) =
      h.used_ports = Set.add port (old h).used_ports
      && not is_port_used (old h) port
      && modify_port h port (fun x -> { masked = false; notified = false })
  *)
(* we allocate a port that was previously unused, initialize its state,
   and declare that all other ports are unaltered *)

(*@ function count_ports(h: handle) : integer = Set.cardinal h.used_ports *)

type virq_t =
  | Timer        (* #define VIRQ_TIMER      0 *)
  | Debug        (* #define VIRQ_DEBUG      1 *)
  | Console      (* #define VIRQ_CONSOLE    2 *)
  | Dom_exc      (* #define VIRQ_DOM_EXC    3 *)
  | Tbuf         (* #define VIRQ_TBUF       4 *)
  | Reserved_5   (* Do not use this value as it's not defined *)
  | Debugger     (* #define VIRQ_DEBUGGER   6 *)
  | Xenoprof     (* #define VIRQ_XENOPROF   7 *)
  | Con_ring     (* #define VIRQ_CON_RING   8 *)
  | Pcpu_state   (* #define VIRQ_PCPU_STATE 9 *)
  | Mem_event    (* #define VIRQ_MEM_EVENT  10 *)
  | Xc_reserved  (* #define VIRQ_XC_RESERVED 11 *)
  | Enomem       (* #define VIRQ_ENOMEM     12 *)
  | Xenpmu       (* #define VIRQ_XENPMU     13 *)


val to_int: t -> int
(*@ n = to_int t
    ensures n = t.port *)

val of_int: int -> t
(*@ t = of_int port
    requires 0 <= port < max_evtchns
    ensures t.port = port *)

val init: unit -> handle
(** Return an initialised event channel interface. On error it
    will throw a Failure exception. *)
(*@ h = init ()
    ensures h.used_ports = Set.empty
    raises Failure _ -> true *)

(* 'raises Failure -> true' means that Failure can be raised and there isn't any
   specific precondition failure that causes it, i.e. it can be raised at any time.
   If this line is ommitted then Failure wouldn't be allowed to be raised.
   It does NOT mean that Failure is always raised by this call, it is an implication,
   essentially equivalent to not (raises Failure) || true. *)

val fd: handle -> Unix.file_descr
(** Return a file descriptor suitable for Unix.select. When
    the descriptor becomes readable, it is safe to call 'pending'.
    On error it will throw a Failure exception. *)
(*@ n = fd h
    ensures n = h.fd
    raises Failure _ -> h.fd = -1 *)

val notify : handle -> t -> unit
(** Notify the given event channel. On error it will throw a
    Failure exception. *)
(*@ notify h port
    requires is_port_used h port
    modifies h.ports
    ensures modify_port h port (fun t -> { notified = true; masked = t.masked })
    raises Failure _ -> true *)

val bind_interdomain : handle -> int -> int -> t
(** [bind_interdomain h domid remote_port] returns a local event
    channel connected to domid:remote_port. On error it will
    throw a Failure exception. *)
(*@ t = bind_interdomain h domid remote_port
    modifies h.used_ports
    modifies h.ports
    ensures allocated_port h t
    raises Failure _ -> true
*)

val bind_dom_exc_virq : handle -> t
(** Binds a local event channel to the VIRQ_DOM_EXC
    (domain exception VIRQ). On error it will throw a Failure
    exception. *)
(*@ t = bind_dom_exc_virq h
    modifies h.used_ports
    modifies h.ports
    ensures allocated_port h t
    raises Failure _ -> true *)

val bind_virq: handle -> virq_t -> t
(** Binds a local event channel to the specific VIRQ type.
    On error it will throw a Failure exception. *)
(*@ t = bind_virq h v
    modifies h.used_ports
    modifies h.ports
    ensures allocated_port h t
    raises Failure _ -> true *)

val unbind : handle -> t -> unit
(** Unbinds the given event channel. On error it will throw a
    Failure exception. *)
(*@ unbind h t
    requires is_port_used h t
    modifies h.used_ports
    consumes t
    ensures h.used_ports = Set.remove t (old h).used_ports
    raises Failure _ -> true
  *)
(* although t is only an integer, it shouldn't be used after unbinding,
   because it would imply operation on an unbound port, hence the 'consumes' directive. *)

val pending : handle -> t
(** Returns the next event channel to become pending. On error it
    will throw a Failure exception. *)
(*@ t = pending h
    requires not Set.is_empty h.used_ports
    modifies h.ports
    ensures is_port_used h t && modify_port h t (fun t -> { masked = true; notified = t.notified })
    raises Failure _ -> true
 *)

val unmask : handle -> t -> unit
(** Unmasks the given event channel. On error it will throw a
    Failure exception. *)
(*@ unmask h t
    requires is_port_used h t
    modifies h.ports
    ensures is_port_used h t && modify_port h t (fun t -> { masked = false; notified = t.notified })
    raises Failure _ -> true *)
