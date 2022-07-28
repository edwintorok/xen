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

type handle = {
    fd_send: Unix.file_descr
  ; fd_recv: Unix.file_descr
  ; client_send: Unix.file_descr
  ; client_recv: Unix.file_descr
  ; port: int ref
}

let init () =
  let pipe1_rd, pipe1_wr = Unix.pipe ~cloexec:false () in
  let pipe2_rd, pipe2_wr = Unix.pipe ~cloexec:false () in
  {
    fd_send= pipe2_wr
  ; fd_recv= pipe1_rd
  ; client_send= pipe1_wr
  ; client_recv= pipe2_rd
  ; port= ref 0
  }

let fd t = t.fd_recv

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

let notify t _ =
  let (_ : int) = Unix.single_write_substring t.fd_send "," 0 1 in
  ()

let bind_interdomain t _domid _remote_port = incr t.port ; !(t.port)

let bind_virq t _ = incr t.port ; !(t.port)

let bind_dom_exc_virq handle = bind_virq handle Dom_exc

let unbind _ _ = ()

let tmp = Bytes.make 1 ' '

let pending t =
  (* TODO: could use char to distinguish between "domains" *)
  let (n : int) = Unix.read t.fd_recv tmp 0 1 in
  if n = 0 then
    failwith "Eventchn.pending: EOF" ;
  (* TODO: should simulate mask/unmask *)
  !(t.port)

let unmask _ _ = ()

let to_int x = x

let of_int x = x
