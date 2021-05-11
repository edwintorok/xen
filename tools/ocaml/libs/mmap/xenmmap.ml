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
type t = mmap_interface * (mmap_interface -> unit)


type mmap_prot_flag = RDONLY | WRONLY | RDWR
type mmap_map_flag = SHARED | PRIVATE

(* mmap: fd -> prot_flag -> map_flag -> length -> offset -> interface *)
external mmap': Unix.file_descr -> mmap_prot_flag -> mmap_map_flag
		-> int -> int -> mmap_interface = "stub_mmap_init"
(* read: interface -> start -> length -> data *)
external read: mmap_interface -> int -> int -> string = "stub_mmap_read"
(* write: interface -> data -> start -> length -> unit *)
external write: mmap_interface -> string -> int -> int -> unit = "stub_mmap_write"
(* getpagesize: unit -> size of page *)
external unmap': mmap_interface -> unit = "stub_mmap_final"
(* getpagesize: unit -> size of page *)
let make ?(unmap=unmap') interface = interface, unmap
external getpagesize: unit -> int = "stub_mmap_getpagesize"

let to_interface (intf, _) = intf
let mmap fd prot_flag map_flag length offset =
	let map = mmap' fd prot_flag map_flag length offset in
	make map ~unmap:unmap'
let unmap (map, do_unmap) = do_unmap map
