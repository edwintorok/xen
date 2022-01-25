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

module Op = struct include Op end
module Packet = struct include Packet end

module BoundedQueue : sig
	type 'a t

	(** [create ~capacity] creates a queue with maximum [capacity] elements. *)
	val create: capacity:int -> 'a t

	(** [clear q] discards all elements from [q] *)
	val clear: 'a t -> unit

	(** [can_push q] when [length q < capacity].	*)
	val can_push: 'a t -> bool

	(** [push e q] adds [e] at the end of queue [q] if [can_push q], or returns [None]. *)
	val push: 'a -> 'a t -> unit option

	(** [pop q] removes and returns first element in [q], or raises [Queue.Empty]. *)
	val pop: 'a t -> 'a

	(** [peek q] returns the first element in [q], or raises [Queue.Empty].  *)
	val peek : 'a t -> 'a

	(** [length q] returns the current number of elements in [q] *)
	val length: 'a t -> int
end = struct
	type 'a t = { q: 'a Queue.t; capacity: int }

	let create ~capacity =
		{ capacity; q = Queue.create () }

	let can_push t = Queue.length t.q < t.capacity
	let push e t =
		if can_push t then
			Some (Queue.push e t.q)
		else
			None

	let clear t = Queue.clear t.q
	let pop t = Queue.pop t.q
	let peek t = Queue.peek t.q
	let length t = Queue.length t.q
end


exception End_of_file
exception Eagain
exception Noent
exception Invalid
exception Reconnect

let _ =
  Callback.register_exception "Xb.Reconnect" Reconnect

type backend_mmap =
{
	mmap: Xenmmap.mmap_interface;     (* mmaped interface = xs_ring *)
	eventchn_notify: unit -> unit; (* function to notify through eventchn *)
	mutable work_again: bool;
}

type backend_fd =
{
	fd: Unix.file_descr;
}

type backend = Fd of backend_fd | Xenmmap of backend_mmap

type partial_buf = HaveHdr of Partial.pkt | NoHdr of int * bytes

type t =
{
	backend: backend;
	pkt_out: Packet.t Queue.t;
	mutable partial_in: partial_buf;
	mutable partial_out: string;
}

let init_partial_in () = NoHdr
	(Partial.header_size (), Bytes.make (Partial.header_size()) '\000')

let reconnect t = match t.backend with
	| Fd _ ->
		(* should never happen, so close the connection *)
		raise End_of_file
	| Xenmmap backend ->
		Xs_ring.close backend.mmap;
		backend.eventchn_notify ();
		(* Clear our old connection state *)
		Queue.clear t.pkt_out;
		t.partial_in <- init_partial_in ();
		t.partial_out <- ""

let queue con pkt = Queue.push pkt con.pkt_out

let read_fd back _con b len =
	let rd = Unix.read back.fd b 0 len in
	if rd = 0 then
		raise End_of_file;
	rd

let read_mmap back _con b len =
	let s = Bytes.make len '\000' in
	let rd = Xs_ring.read back.mmap s len in
	Bytes.blit s 0 b 0 rd;
	back.work_again <- (rd > 0);
	if rd > 0 then
		back.eventchn_notify ();
	rd

let read con b len =
	match con.backend with
	| Fd backfd     -> read_fd backfd con b len
	| Xenmmap backmmap -> read_mmap backmmap con b len

let write_fd back _con b len =
	Unix.write_substring back.fd b 0 len

let write_mmap back _con s len =
	let ws = Xs_ring.write_substring back.mmap s len in
	if ws > 0 then
		back.eventchn_notify ();
	ws

let write con s len =
	match con.backend with
	| Fd backfd     -> write_fd backfd con s len
	| Xenmmap backmmap -> write_mmap backmmap con s len

(* NB: can throw Reconnect *)
let output con =
	(* get the output string from a string_of(packet) or partial_out *)
	let s = if String.length con.partial_out > 0 then
			con.partial_out
		else if Queue.length con.pkt_out > 0 then
			Packet.to_string (Queue.pop con.pkt_out)
		else
			"" in
	(* send data from s, and save the unsent data to partial_out *)
	if s <> "" then (
		let len = String.length s in
		let sz = write con s len in
		let left = String.sub s sz (len - sz) in
		con.partial_out <- left
	);
	(* after sending one packet, partial is empty *)
	con.partial_out = ""

(* NB: can throw Reconnect *)
let input con =
	let to_read =
		match con.partial_in with
		| HaveHdr partial_pkt -> Partial.to_complete partial_pkt
		| NoHdr   (i, _)    -> i in

	(* try to get more data from input stream *)
	let b = Bytes.make to_read '\000' in
	let sz = if to_read > 0 then read con b to_read else 0 in

	(
	match con.partial_in with
	| HaveHdr partial_pkt ->
		(* we complete the data *)
		if sz > 0 then
			Partial.append partial_pkt (Bytes.to_string b) sz;
		if Partial.to_complete partial_pkt = 0 then (
			let pkt = Packet.of_partialpkt partial_pkt in
			con.partial_in <- init_partial_in ();
			Some pkt
		) else None
	| NoHdr (i, buf)      ->
		(* we complete the partial header *)
		if sz > 0 then
			Bytes.blit b 0 buf (Partial.header_size () - i) sz;
		con.partial_in <- if sz = i then
			HaveHdr (Partial.of_string (Bytes.to_string buf)) else NoHdr (i - sz, buf);
		None
	)

let newcon backend = {
	backend = backend;
	pkt_out = Queue.create ();
	partial_in = init_partial_in ();
	partial_out = "";
	}

let open_fd fd = newcon (Fd { fd = fd; })

let open_mmap mmap notifyfct =
	(* Advertise XENSTORE_SERVER_FEATURE_RECONNECTION *)
	Xs_ring.set_server_features mmap (Xs_ring.Server_features.singleton Xs_ring.Server_feature.Reconnection);
	newcon (Xenmmap {
		mmap = mmap;
		eventchn_notify = notifyfct;
		work_again = false; })

let close con =
	match con.backend with
	| Fd backend   -> Unix.close backend.fd
	| Xenmmap backend -> Xenmmap.unmap backend.mmap

let is_fd con =
	match con.backend with
	| Fd _   -> true
	| Xenmmap _ -> false

let is_mmap con = not (is_fd con)

let output_len con = Queue.length con.pkt_out
let has_new_output con = Queue.length con.pkt_out > 0
let has_old_output con = String.length con.partial_out > 0

let has_output con = has_new_output con || has_old_output con

let peek_output con = Queue.peek con.pkt_out

let has_partial_input con = match con.partial_in with
	| HaveHdr _ -> true
	| NoHdr (n, _) -> n < Partial.header_size ()
let has_more_input con =
	match con.backend with
	| Fd _         -> false
	| Xenmmap backend -> backend.work_again

let is_selectable con =
	match con.backend with
	| Fd _   -> true
	| Xenmmap _ -> false

let get_fd con =
	match con.backend with
	| Fd backend -> backend.fd
	| Xenmmap _     -> raise (Failure "get_fd")
