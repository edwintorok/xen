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

exception Limit_reached
exception Data_too_big
exception Transaction_opened

let warn fmt = Logging.warn "quota" fmt
let activate = ref true
let maxent = ref (10000)
let maxsize = ref (4096)

module OCamlList = List
open Xenbus.Memory_tracker
open Xenbus.Sizeops

type entry =
	{ entries: int
	; size: Tracker.t
	}

let entry_to_string () e =
  let size = match Size.to_int_opt e.size with
  | None -> max_int
  | Some v -> v in
  Printf.sprintf "%d (%u words)" e.entries size

let entry_add a b =
  { entries = a.entries + b.entries
  ; size = Tracker.add a.size b.size
  }

let entry_sub a b =
  { entries = a.entries - b.entries
  ; size = Tracker.remove a.size b.size
  }

let fields_of_entry = Size.of_int 2
let size_of_entry _ = fields_of_entry

  type t = {
	  maxent: int;               (* max entities per domU *)
	  maxsize: int;              (* max size of data store in one node *)
	  cur: (Xenctrl.domid, entry) Hashtbl.t; (* current domains quota *)
  }

  let fields_of_t = Size.of_int 3
  let size_of t =
    Size.(fields_of_t
	 + Hashtbl.size_of t.cur)

let to_string quota domid =
	if Hashtbl.mem quota.cur domid
	then Printf.sprintf "dom%i quota: %a/%i" domid entry_to_string (Hashtbl.find quota.cur domid) quota.maxent
	else Printf.sprintf "dom%i quota: not set" domid

let create () =
	{ maxent = !maxent; maxsize = !maxsize; cur = Hashtbl.create_sized size_of_int size_of_entry 100; }

let copy quota = { quota with cur = (Hashtbl.copy quota.cur) }

let del quota id = Hashtbl.remove quota.cur id

let _check quota id size =
	if size > quota.maxsize then (
		warn "domain %u err create entry: data too big %d" id size;
		raise Data_too_big
	);
	if id > 0 && Hashtbl.mem quota.cur id then
		let entry = Hashtbl.find quota.cur id in
		if entry.entries >= quota.maxent then (
			warn "domain %u cannot create entry: quota reached" id;
			raise Limit_reached
		)

let check quota id size =
	if !activate then
		_check quota id size

let get_entry quota id = Hashtbl.find quota.cur id

let set_entry quota id nb =
	if nb.entries = 0
	then Hashtbl.remove quota.cur id
	else
		Hashtbl.replace quota.cur id nb

let size_of_path path v =
	let path_size = OCamlList.fold_left (fun acc s -> Size.(acc + size_of_string s)) value path in
	let vsize = size_of_string v in
	{ entries = 1; size = Size.(path_size + vsize) }

let del_entry quota id path v =
	try
		let nb = get_entry quota id in
		set_entry quota id (entry_sub nb @@ size_of_path path v)
	with Not_found -> ()

let entries_0 = { entries = 0; size = Tracker.empty }
let add_entry quota id path v =
	let nb = try get_entry quota id with Not_found -> entries_0 in
	set_entry quota id (entry_add nb @@ size_of_path path v)

let add quota diff =
	Hashtbl.iter (fun id nb -> set_entry quota id (entry_add (get_entry quota id) nb)) diff.cur

let get_entry_0 quota id = try get_entry quota id with Not_found -> entries_0

let merge orig_quota mod_quota dest_quota =
	  Hashtbl.iter (fun id nb ->
				let diff = entry_sub nb @@ get_entry_0 orig_quota id in
				if diff <> entries_0 then
					set_entry dest_quota id (entry_add (get_entry_0 dest_quota id) diff)) mod_quota.cur
