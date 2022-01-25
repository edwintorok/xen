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

type handle = Unix.file_descr * int ref

let devnull = Unix.openfile "/dev/null" [] 0

let init () = (devnull, ref 0)

let fd (h, _) = h

type t = int

type virq_t =
  | Timer (* #define VIRQ_TIMER      0 *)
  | Debug (* #define VIRQ_DEBUG      1 *)
  | Console (* #define VIRQ_CONSOLE    2 *)
  | Dom_exc (* #define VIRQ_DOM_EXC    3 *)
  | Tbuf (* #define VIRQ_TBUF       4 *)
  | Reserved_5 (* Do not use this value as it's not defined *)
  | Debugger (* #define VIRQ_DEBUGGER   6 *)
  | Xenoprof (* #define VIRQ_XENOPROF   7 *)
  | Con_ring (* #define VIRQ_CON_RING   8 *)
  | Pcpu_state (* #define VIRQ_PCPU_STATE 9 *)
  | Mem_event (* #define VIRQ_MEM_EVENT  10 *)
  | Xc_reserved (* #define VIRQ_XC_RESERVED 11 *)
  | Enomem (* #define VIRQ_ENOMEM     12 *)
  | Xenpmu (* #define VIRQ_XENPMU     13 *)

let notify _h _ = ()

let bind_interdomain (_h, port) _domid _remote_port = incr port ; !port

let bind_virq (_h, port) _ = incr port ; !port

let bind_dom_exc_virq handle = bind_virq handle Dom_exc

let unbind _ _ = ()

let pending (_h, port) = !port

let unmask _ _ = ()

let to_int x = x

let of_int x = x
