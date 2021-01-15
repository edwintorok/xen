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

let enable = ref false
let xs_daemon_database = Paths.xen_run_stored ^ "/db"

let error fmt = Logging.error "disk" fmt

(* unescape utils *)
exception Bad_escape

let is_digit c = match c with '0' .. '9' -> true | _ -> false

let undec c =
	match c with
	| '0' .. '9' -> (Char.code c) - (Char.code '0')
	| _          -> raise (Failure "undecify")

let unhex c =
	let c = Char.lowercase c in
	match c with
	| '0' .. '9' -> (Char.code c) - (Char.code '0')
	| 'a' .. 'f' -> (Char.code c) - (Char.code 'a') + 10
	| _          -> raise (Failure "unhexify")

let string_unescaped s =
	let len = String.length s
	and i = ref 0 in
	let d = Buffer.create len in

	let read_escape () =
		incr i;
		match s.[!i] with
		| 'n'  -> '\n'
		| 'r'  -> '\r'
		| '\\' -> '\\'
		| '\'' -> '\''
		| '"'  -> '"'
		| 't'  -> '\t'
		| 'b'  -> '\b'
		| 'x'  ->
			let v = (unhex s.[!i + 1] * 16) + unhex s.[!i + 2] in
			i := !i + 2;
			Char.chr v
		| c    ->
			if is_digit c then (
				let v = (undec s.[!i]) * 100 +
					(undec s.[!i + 1]) * 10 +
					(undec s.[!i + 2]) in
				i := !i + 2;
				Char.chr v
			) else
				raise Bad_escape
	in

	while !i < len
	do
		let c = match s.[!i] with
		| '\\' -> read_escape ()
		| c    -> c in
		Buffer.add_char d c;
		incr i
	done;
	Buffer.contents d

(* file -> lines_of_file *)
let file_readlines file =
	let channel = open_in file in
	let rec input_line_list channel =
		let line = try input_line channel with End_of_file -> "" in
		if String.length line > 0 then
			line :: input_line_list channel
		else (
			close_in channel;
			[]
		) in
	input_line_list channel

let rec map_string_list_range l s =
	match l with
	| [] -> []
	| (a,b) :: l -> String.sub s a (b - a) :: map_string_list_range l s

let is_digit c =
	try ignore (int_of_char c); true with _ -> false

let rec parse_perm s =
	let len = String.length s in
	if len = 0 then
		[]
	else
		let i = ref 1 in
		while !i < len && is_digit s.[!i] do incr i done;
		let x = String.sub s 0 !i
		and lx = String.sub s !i len in
		x :: parse_perm lx

let read store =
	(* don't let the permission get on our way, full perm ! *)
	let v = Store.get_ops store Perms.Connection.full_rights in

	(* a line is : path{perm} or path{perm} = value *)
	let parse_line s =
		let path, perm, value =
			let len = String.length s in
			let si = if String.contains s '=' then
					String.index s '='
				else
					len - 1 in
			let pi = String.rindex_from s si '{' in
			let epi = String.index_from s pi '}' in

			if String.contains s '=' then
				let ss = map_string_list_range [ (0, pi);
				                                 (pi + 1, epi);
				                                 (si + 2, len); ] s in
				(List.nth ss 0, List.nth ss 1, List.nth ss 2)
			else
				let ss = map_string_list_range [ (0, pi);
				                                 (pi + 1, epi);
				                               ] s in
				(List.nth ss 0, List.nth ss 1, "")
			in
		let path = Store.Path.of_string path in
		v.Store.write path (string_unescaped value);
		v.Store.setperms path (Perms.Node.of_strings (parse_perm perm)) in
	try
		let lines = file_readlines xs_daemon_database in
		List.iter (fun s -> parse_line s) lines
	with exc ->
		error "caught exn %s" (Printexc.to_string exc)

let write store =
	if !enable then
	try
		let tfile = Printf.sprintf "%s#" xs_daemon_database in
		let channel = open_out_gen [ Open_wronly; Open_creat; Open_trunc; ]
		                           0o600 tfile in
		Store.dump store channel;
		flush channel;
		close_out channel;
		Unix.rename tfile xs_daemon_database
	with exc ->
		error "caught exn %s" (Printexc.to_string exc)

	module BinaryOut = struct
		let version = 0x1
		let endian = 1
		let padding = String.make 7 '\x00'

		let write_header ch =
			(* for testing endian order *)
			output_binary_int ch 0x78656e73;
			output_binary_int ch 0x746f7265;
			output_binary_int ch version;
			output_binary_int ch endian;
			ch

		let w8 = output_char
		let w16 ch i =
			assert (i >= 0 && i lsr 16 = 0);
			output_byte ch (i lsr 8);
			output_byte ch i

		let w32 ch v =
			assert (v >= 0 && v <= 0xFFFF_FFFF);
			output_binary_int ch v

		let pos = pos_out
		let wpad ch =
			let padto = 8 in
			let padby = (padto - pos ch mod padto) mod padto in
			if padby > 0 then
				output_substring ch padding 0 padby

		let wstring = output_string
	end

	module BinaryIn = struct
		type t = in_channel

		let read_header t =
			let h = Bytes.make 8 '\x00' in
			really_input t h 0 (Bytes.length h);
			let ver = input_binary_int t in
			let endian = input_binary_int t in
			if Bytes.to_string h <> "xenstore" then
				failwith "Header doesn't begin with 'xenstore'";
			if ver <> BinaryOut.version then
				failwith "Incompatible version";
			if endian <> BinaryOut.endian then
				failwith "Incompatible endianness"

		let r8 = input_char

		let r16 t = 
			let r0 = input_byte t in
			let r1 = input_byte t  in
			(r0 lsl 8) lor r1

		let r32 t =
			(* read unsigned 32-bit int *)
			let r = input_binary_int t land 0xFFFF_FFFF in
			assert (r >= 0);
			r

		let rstring = really_input_string

		let rpad t =
			let padto = 8 in
			let padby = (padto - pos_in t mod padto) mod padto in
			if padby > 0 then
				ignore (really_input_string t padby)
	end

module FD : sig
     type t = Unix.file_descr
     val of_int: int -> t
     val to_int : t -> int
end = struct
    type t = Unix.file_descr
    (* This is like Obj.magic but just for these types,
       and relies on Unix.file_descr = int *)
    external to_int : t -> int = "%identity"
    external of_int : int -> t = "%identity"
end

module LiveRecord = struct
	(* See docs/designs/xenstore-migration.md for binary format *)
	module Type : sig
		type t = private int
		val end_ : t
		val global_data : t
		val connection_data : t
		val watch_data : t
		val transaction_data : t
		val node_data: t
	end = struct
		type t = int
		let end_ = 0x0
		let global_data = 0x01
		let connection_data = 0x02
		let watch_data = 0x03
		let transaction_data = 0x04
		let node_data = 0x05
	end

	module I = BinaryIn
	module O = BinaryOut

	let write_expect msg expected actual =
		if expected <> actual then
			let m = Printf.sprintf "expected %d <> %d: %s" expected actual msg in
			invalid_arg m

	let write_record t (typ: Type.t) len f =
		assert (O.pos t mod 8 = 0);
		O.w32 t (typ :> int);
		O.w32 t len;
		let p0 = O.pos t in
		f t;
		let p1 = O.pos t in
		write_expect "position and length" len (p1-p0);
		O.wpad t

	let write_end t =
		write_record t Type.end_ 0 ignore

	let read_expect t msg expected actual =
		if expected <> actual then
			let pos = pos_in t in
			let m = Printf.sprintf "expected %d <> %d at ~%d: %s" expected actual pos msg in
			invalid_arg m

	let read_end t ~len f =
		read_expect t "end" 0 len;
		f ()

	let write_global_data t ~rw_sock =
		write_record t Type.global_data 8 @@ fun b ->
		O.w32 b (FD.to_int rw_sock);
                (* TODO: this needs a unit test/live update test too! *)
		O.w32 b 0xFFFF_FFFF

	let read_global_data t ~len f =
		read_expect t "global_data" 8 len;
		let rw_sock = FD.of_int (I.r32 t) in
		let _ = FD.of_int (I.r32 t) in
		f ~rw_sock

	let conn_shared_ring = 0x0
	let conn_socket = 0x1
	let domid_invalid = 0x7FF4

	(* oxenstored doesn't support readonly sockets yet *)
	let flags_connection_readonly = 0x1l

	type dom = { id: int; target: int; remote_port: int }
	type conn = Socket of Unix.file_descr | Domain of dom

	let write_connection_data t ~conid ~conn xb_pktin xb_partialout xb_pktout =
		let in_data_len = Buffer.length xb_pktin in
		let out_resp_len = String.length xb_partialout in
		let out_data_len = Buffer.length xb_pktout in
		let data_len = in_data_len + out_data_len in

		write_record t Type.connection_data (32 + data_len) @@ fun b ->
		assert (conid > 0);
		O.w32 b conid;
		O.w32 b (match conn with
		| Socket _ -> conn_socket
		| Domain _ -> conn_shared_ring
		);
		let flags = 0x0 in
		O.w32 b flags;

		(match conn with
		| Socket fd ->
			O.w32 b (FD.to_int fd);
			O.w32 b 0 (* pad *)
		| Domain dom ->
			O.w16 b dom.id;
			O.w16 b dom.target;
			O.w32 b dom.remote_port
			);

		O.w32 b in_data_len;
		O.w32 b out_resp_len;
		O.w32 b out_data_len;
		Buffer.output_buffer b xb_pktin;
		O.wstring b xb_partialout;
		Buffer.output_buffer b xb_pktout

	let read_connection_data t ~len f =
		let conid = I.r32 t in
		assert (conid > 0);
		let kind = I.r32 t in
		let flags = I.r32 t in
		read_expect t "flags" 0 flags;
		let conn = (match kind with
		| x when x = conn_socket ->
			let fd = FD.of_int (I.r32 t) in
			I.r32 t |> ignore;
			Socket fd
		| x when x = conn_shared_ring ->
			let id = I.r16 t in
			let target = I.r16 t in
			let remote_port = I.r32 t in
			Domain {id; target; remote_port }
		| x ->
			invalid_arg (Printf.sprintf "Unknown connection kind %x" x)
		) in
		let in_data_len = I.r32 t in
		let out_resp_len = I.r32 t in
		let out_data_len = I.r32 t in
		let in_data = really_input_string t in_data_len in
		let out_data = really_input_string t out_data_len in
		f ~conid ~conn ~in_data ~out_data ~out_resp_len


	let write_watch_data t ~conid ~wpath ~token =
		let wpath_len = String.length wpath in
		let token_len = String.length token in

		write_record t Type.watch_data (12+wpath_len+token_len) @@ fun b ->
		O.w32 b conid;
		O.w32 b (String.length wpath);
		O.w32 b (String.length token);
		O.wstring b wpath;
		O.wstring b token

	let read_watch_data t ~len f =
		let conid = I.r32 t in
		let wpathlen = I.r32 t in
		let tokenlen = I.r32 t in
		let wpath = I.rstring t wpathlen in
		let token = I.rstring t tokenlen in
		f ~conid ~wpath ~token

	let write_transaction_data t ~conid ~txid =
		write_record t Type.transaction_data 8 @@ fun b ->
		O.w32 b conid;
		O.w32 b txid

	let read_transaction_data t ~len f =
		read_expect t "transaction" 8 len;
		let conid = I.r32 t in
		let txid = I.r32 t in
		f ~conid ~txid

	type access = R | W | RW | Del

	let write_node_data t ~txidaccess ~path ~value ~perms =
		let path_len = String.length path in
		let value_len = String.length value in
		let perms = Perms.Node.acls perms in
		let len = 24 + (List.length perms)*4 + path_len + value_len in

		write_record t Type.node_data len @@ fun b ->
		O.w32 b (match txidaccess with None -> 0 | Some (conid, _, _) -> conid);
		O.w32 b (match txidaccess with None -> 0 | Some (_, txid, _) -> txid);
		O.w32 b path_len;
		O.w32 b value_len;
		O.w32 b (match txidaccess with
		| None -> 0x0
		| Some (_, _, Del) -> 0x0
		| Some (_, _, R) -> 0x1
		| Some (_, _, W) -> 0x2
		| Some (_, _, RW) -> 0x3
		);
		O.w32 b (List.length perms);
		List.iter (fun (domid, permty) ->
			O.w8 b (Perms.char_of_permty permty);
			O.w8 b '\x00';
			O.w16 b domid;
		) perms;
		O.wstring b path;
		O.wstring b value

	let read_node_data t ~len f =
		let conid = I.r32 t in
		let txid = I.r32 t in
		let path_len = I.r32 t in
		let value_len = I.r32 t in
		let txaccess = match conid, I.r32 t with
		| 0, _ -> None
		| _, 0 -> Some (conid, txid, Del)
		| _, 1 -> Some (conid, txid, R)
		| _, 2 -> Some (conid, txid, W)
		| _, 3 -> Some (conid, txid, RW)
		| _ -> invalid_arg "invalid access flag"
		in
		let a = Array.init (I.r32 t) (fun _ ->
					let perm = Perms.permty_of_char (I.r8 t) in
					I.r8 t |> ignore;
					let domid = I.r16 t in
					domid, perm
		) in
		let perms = match Array.to_list a with
		| [] -> invalid_arg "Permission list cannot be empty";
		| (owner, other) :: acls ->
			Perms.Node.create owner other acls
		in
		let path = I.rstring t path_len in
		let value = I.rstring t value_len in
		f ~txaccess ~perms ~path ~value

	let read_record t ~on_end ~on_global_data ~on_connection_data ~on_watch_data ~on_transaction_data ~on_node_data =
		I.rpad t; (* if we fail to process a record (e.g. callback raises, ensure we resume at right place *)
		let typ = I.r32 t in
		let len = I.r32 t in
		let p0 = pos_in t in
		(match typ with
		| x when x = (Type.end_ :> int) -> read_end t ~len on_end
		| x when x = (Type.global_data :> int) -> read_global_data t ~len on_global_data
		| x when x = (Type.connection_data :> int) -> read_connection_data t ~len on_connection_data
		| x when x = (Type.watch_data :> int) -> read_watch_data t ~len on_watch_data
		| x when x = (Type.transaction_data :> int) -> read_transaction_data t ~len on_transaction_data
		| x when x = (Type.node_data :> int) -> read_node_data t ~len on_node_data
		| x -> failwith (Printf.sprintf "Unknown record type: %x" x));
		let p1 = pos_in t in
		read_expect t "record length" len (p1-p0)
end
