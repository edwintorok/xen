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

type mmap_interface
(*@ model mapped_len: integer *)

type mmap_prot_flag = RDONLY | WRONLY | RDWR
type mmap_map_flag = SHARED | PRIVATE

(*@ function pagesize: integer *)

external mmap : Unix.file_descr -> mmap_prot_flag -> mmap_map_flag -> int -> int
             -> mmap_interface = "stub_mmap_init"
(*@ m = mmap fd prot mapflag len offset
    requires len > 0 && exists k >= 0. k * pagesize = offset
    ensures m.mapped_len = len
    raises Failure _ -> true *)

external unmap : mmap_interface -> unit = "stub_mmap_final"
(*@ unmap m
    consumes m *)

external read : mmap_interface -> int -> int -> string = "stub_mmap_read"
(*@ s = read m start len
    requires 0 <= start && 0 <= len
    checks start <= len && start + len <= m.mapped_len
*)

external write : mmap_interface -> string -> int -> int -> unit
               = "stub_mmap_write"
(*@ write m data start len
    requires 0 <= start && 0 <= len < String.length data
    checks start <= len && start + len <= m.mapped_len
*)

external getpagesize : unit -> int = "stub_mmap_getpagesize"
(*@ n = getpagesize ()
    ensures n = pagesize *)
