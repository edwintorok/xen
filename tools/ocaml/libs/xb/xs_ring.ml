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

module Server_feature = struct
	type t =
	| Reconnection
		| Error (** connection error reporting *)

		let to_int = function
			| Reconnection -> 1
			| Error -> 2

		let all = [Reconnection; Error]
end

module Server_features = Set.Make(struct
	type t = Server_feature.t
	let compare = compare
end)

let all = Server_features.of_list Server_feature.all

external read: Xenmmap.mmap_interface -> bytes -> int -> int = "ml_interface_read"
external write: Xenmmap.mmap_interface -> bytes -> int -> int = "ml_interface_write"

external _internal_set_server_features: Xenmmap.mmap_interface -> int -> unit = "ml_interface_set_server_features" [@@noalloc]
external _internal_get_server_features: Xenmmap.mmap_interface -> int = "ml_interface_get_server_features" [@@noalloc]

let write_substring mmap buff len =
	write mmap (Bytes.unsafe_of_string buff) len

let get_server_features mmap =
	(* NB only one feature currently defined above *)
	let x = _internal_get_server_features mmap in
	Server_features.filter (fun feature ->
		(Server_feature.to_int feature) land x <> 0
	) all

let set_server_features mmap set =
	(* NB only one feature currently defined above *)
	let x = Server_features.fold (fun feature acc ->
				acc lor (Server_feature.to_int feature)) set 0 in
	_internal_set_server_features mmap x

(** [set_error intf errorcode] sets the connection error code for [intf].
		See docs/misc/xenstore-ring.txt
 *)
external set_error: Xenmmap.mmap_interface -> int -> unit = "ml_interface_set_error"

(** [get_error intf] retrieves the connection error code for [intf] *)
external get_error: Xenmmap.mmap_interface -> int = "ml_interface_get_error"

external close: Xenmmap.mmap_interface -> unit = "ml_interface_close" [@@noalloc]
