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

(** *)
type domid = int

(* ** xenctrl.h ** *)

type domaininfo = {
    domid: domid
  ; dying: bool
  ; shutdown: bool
  ; shutdown_code: int
}

exception Error of string

type handle = unit

let interface_open () = ()

let interface_close () = ()

let domain_getinfo () domid =
  {domid; dying= false; shutdown= false; shutdown_code= 0}

let devzero = Unix.openfile "/dev/zero" [] 0

let nullmap () = Xenmmap.mmap devzero Xenmmap.RDWR Xenmmap.PRIVATE 4096 0

let map_foreign_range _ _ _ _ = nullmap ()
