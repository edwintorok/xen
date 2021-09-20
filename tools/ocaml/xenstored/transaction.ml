(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 * Author Thomas Gazagnaire <thomas.gazagnaire@citrix.com>
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
let error fmt = Logging.error "transaction" fmt

open Stdext

let none = 0
let test_eagain = ref false
let do_coalesce = ref true

let check_parents_perms_identical root1 root2 path =
	let hierarch = Store.Path.get_hierarchy path in
	let permdiff = List.fold_left (fun acc path ->
		let n1 = Store.Path.get_node root1 path
		and n2 = Store.Path.get_node root2 path in
		match n1, n2 with
		| Some n1, Some n2 ->
			not (Perms.equiv (Store.Node.get_perms n1) (Store.Node.get_perms n2)) || acc
		| _ ->
			true || acc
	) false hierarch in
	(not permdiff)

let get_lowest path1 path2 =
	match path2 with
	| None       -> Some path1
	| Some path2 -> Some (Store.Path.get_common_prefix path1 path2)

let test_coalesce oldroot currentroot optpath =
	match optpath with
	| None      -> true
	| Some path ->
		let oldnode = Store.Path.get_node oldroot path
		and currentnode = Store.Path.get_node currentroot path in

		match oldnode, currentnode with
		| (Some oldnode), (Some currentnode) ->
			if oldnode == currentnode then (
				check_parents_perms_identical oldroot currentroot path
			) else (
				false
			)
		| None, None -> (
			(* ok then it doesn't exists in the old version and the current version,
			   just sneak it in as a child of the parent node if it exists, or else fail *)
			let pnode = Store.Path.get_node currentroot (Store.Path.get_parent path) in
			match pnode with
			| None       -> false (* ok it doesn't exists, just bail out. *)
			| Some _     -> true
			)
		| _ ->
			false

let can_coalesce oldroot currentroot path =
	if !do_coalesce then
		try test_coalesce oldroot currentroot path with _ -> false
	else
		false

type ty = No | Full of (
	int *          (* Transaction id *)
	Store.t *      (* Original store *)
	Store.t        (* A pointer to the canonical store: its root changes on each transaction-commit *)
)

type t = {
	ty: ty;
	start_count: int64;
	store: Store.t; (* This is the store that we change in write operations. *)
	quota: Quota.t;
	mutable must_fail: bool;
	oldroot: Store.Node.t;
	mutable paths: (Xenbus.Xb.Op.operation * Store.Path.t) list;
	mutable operations: (Packet.request * Packet.response) list;
	mutable read_lowpath: Store.Path.t option;
	mutable write_lowpath: Store.Path.t option;
}
let get_id t = match t.ty with No -> none | Full (id, _, _) -> id
let mark_failed t = t.must_fail <- true
let counter = ref 0L
let failed_commits = ref 0L
let failed_commits_no_culprit = ref 0L
let reset_conflict_stats () =
	failed_commits := 0L;
	failed_commits_no_culprit := 0L

(* Scope for optimisation: different data-structure and functions to search/filter it *)
let short_running_txns = ref []

let oldest_short_running_transaction () =
	let rec last = function
		| [] -> None
		| [x] -> Some x
		| _ :: xs -> last xs
	in last !short_running_txns

let trim_short_running_transactions txn =
	let cutoff = Unix.gettimeofday () -. !Define.conflict_max_history_seconds in
	let keep = match txn with
		| None -> (function (start_time, _) -> start_time >= cutoff)
		| Some t -> (function (start_time, tx) -> start_time >= cutoff && tx != t)
	in
	short_running_txns := List.filter
		keep
		!short_running_txns

let invalid_op = Xenbus.Xb.Op.Invalid, []

let make ?(internal=false) id store =
	let ty = if id = none then No else Full(id, Store.copy store, store) in
	let txn = {
		ty = ty;
		start_count = !counter;
		store = if id = none then store else Store.copy store;
		quota = Quota.copy store.Store.quota;
		oldroot = Store.get_root store;
		paths = [];
		operations = [];
		read_lowpath = None;
		write_lowpath = None;
		must_fail = false;
	} in
	if id <> none && not internal then (
		let now = Unix.gettimeofday () in
		short_running_txns := (now, txn) :: !short_running_txns
	);
	txn

let get_store t = t.store
let get_paths t = t.paths

let is_read_only t = t.paths = [] && not t.must_fail
let get_root t = Store.get_root t.store

let add_wop t ty path = t.paths <- (ty, path) :: t.paths
let clear_wops t = t.paths <- []
let add_operation ~perm t request response =
	if !Define.maxrequests >= 0
		&& not (Perms.Connection.is_dom0 perm)
		&& List.length t.operations >= !Define.maxrequests
		then raise Quota.Limit_reached;
	t.operations <- (request, response) :: t.operations
let get_operations t = List.rev t.operations
let set_read_lowpath t path = t.read_lowpath <- get_lowest path t.read_lowpath
let set_write_lowpath t path =
	 Logging.debug "transaction" "set_writelowpath (%d) %s" (get_id t)  (Store.Path.to_string path);
	 t.write_lowpath <- get_lowest path t.write_lowpath

let path_exists t path = Store.path_exists t.store path

let write t perm path value =
	let path_exists = path_exists t path in
	Store.write t.store perm path value;
	if path_exists
	then set_write_lowpath t path
	else set_write_lowpath t (Store.Path.get_parent path);
	add_wop t Xenbus.Xb.Op.Write path

let mkdir ?(with_watch=true) t perm path =
	Store.mkdir t.store perm path;
	set_write_lowpath t (Store.Path.get_parent path);
	if with_watch then
		add_wop t Xenbus.Xb.Op.Mkdir path

let setperms t perm path perms =
	Store.setperms t.store perm path perms;
	set_write_lowpath t path;
	add_wop t Xenbus.Xb.Op.Setperms path

let rm t perm path =
	Store.rm t.store perm path;
	set_write_lowpath t (Store.Path.get_parent path);
	add_wop t Xenbus.Xb.Op.Rm path

let ls t perm path =
	let r = Store.ls t.store perm path in
	set_read_lowpath t path;
	r

let read t perm path =
	let r = Store.read t.store perm path in
	set_read_lowpath t path;
	r

let getperms t perm path =
	let r = Store.getperms t.store perm path in
	set_read_lowpath t path;
	r

let commit ~con t =
	let has_write_ops = List.length t.paths > 0 in
	let has_coalesced = ref false in
	let has_commited =
	match t.ty with
	| No                         -> true
	| Full (id, oldstore, cstore) ->       (* "cstore" meaning current canonical store *)
		let commit_partial oldroot cstore store =
			(* get the lowest path of the query and verify that it hasn't
			   been modified by others transactions. *)
			if can_coalesce oldroot (Store.get_root cstore) t.read_lowpath
			&& can_coalesce oldroot (Store.get_root cstore) t.write_lowpath then (
				maybe (fun p ->
					let n = Store.get_node store p in

					(* it has to be in the store, otherwise it means bugs
					   in the lowpath registration. we don't need to handle none. *)
					maybe (fun n -> Store.set_node cstore p n t.quota store.Store.quota) n;
					Logging.write_coalesce ~tid:(get_id t) ~con (Store.Path.to_string p);
				) t.write_lowpath;
				maybe (fun p ->
					Logging.read_coalesce ~tid:(get_id t) ~con (Store.Path.to_string p)
					) t.read_lowpath;
				has_coalesced := true;
				Store.incr_transaction_coalesce cstore;
				true
			) else (
				(* cannot do anything simple, just discard the queries,
				   and the client need to redo it later *)
				Store.incr_transaction_abort cstore;
				false
			)
			in
		let try_commit oldroot cstore store =
			if oldroot == Store.get_root cstore then (
				(* move the new root to the current store, if the oldroot
				   has not been modified *)
				if has_write_ops then (
					Store.set_root cstore (Store.get_root store);
					Store.set_quota cstore (Store.get_quota store)
				);
				true
			) else
				(* we try a partial commit if possible *)
				commit_partial oldroot cstore store
			in
		if t.must_fail then begin
			Logging.info "transaction" "Transaction %d was marked to fail (by live-update)" id;
			false
		end else
		if !test_eagain && Random.int 3 = 0 then
			false
		else
			try_commit (Store.get_root oldstore) cstore t.store
		in
	Logging.info "transaction" "has_commited: %b" has_commited;
	if has_commited && has_write_ops then
		Disk.write t.store;
	if not has_commited
	then Logging.conflict ~tid:(get_id t) ~con
	else if not !has_coalesced
	then Logging.commit ~tid:(get_id t) ~con;
	has_commited

module LR = Disk.LiveRecord

(* here instead of Store.ml to avoid dependency cycle *)
let write_node ch txidaccess path node =
	let value = Store.Node.get_value node in
	let perms = Store.Node.get_perms node in
	let path = Store.Path.of_path_and_name path (Symbol.to_string node.Store.Node.name) |> Store.Path.to_string in
	LR.write_node_data ch ~txidaccess ~path ~value ~perms

let split limit c s =
	let limit = match limit with None -> 8 | Some x -> x in
	String.split ~limit c s
	
exception Invalid_Cmd_Args
let split_one_path data conpath =
	let args = split (Some 2) '\000' data in
	match args with
	| path :: "" :: [] -> Store.Path.create path conpath
	| _                -> raise Invalid_Cmd_Args
	
let dump base conpath ~conid txn ch =
	(* TODO: implicit paths need to be converted to explicit *)
	let txid = get_id txn in
	LR.write_transaction_data ch ~conid ~txid;
	let store = get_store txn in
	let write_node_mkdir path =
   let perms, value =	match Store.get_node store path with
  | None -> Perms.Node.default0, "" (* need to dump mkdir anyway even if later deleted due to implicit path creation *)
  | Some node -> Store.Node.get_perms node, Store.Node.get_value node (* not always "", e.g. on EEXIST *) in
  LR.write_node_data ch ~txidaccess:(Some (conid, txid, LR.W)) ~path:(Store.Path.to_string path) ~value ~perms
in
	maybe (fun path ->
		(* if there were any reads make sure the tree matches, remove all contents and write out subtree *)
		match Store.get_node store path with
		| None -> (* we've only read nodes that we ended up deleting, nothing to do *) ()
		| Some node ->
			write_node ch (Some (conid, txid, LR.Del)) (Store.Path.get_parent path) node;
			let path = Store.Path.get_parent path in
			Store.traversal node @@ fun path' node ->
			write_node ch (Some (conid,txid, LR.R)) (List.append path path') node
	) txn.read_lowpath;
	(* we could do something similar for write_lowpath, but that would become 
	 	 complicated to handle correctly wrt to permissions and quotas if there are nodes
		 owned by other domains in the subtree.
	*)
	let ops = get_operations txn in
	if ops <> [] then
		(* mark that we had some operation, these could be failures, etc.
			 we want to fail the transaction after a live-update,
			 unless it is completely a no-op
		 *)
		let perms = Store.getperms store Perms.Connection.full_rights [] in
		let value = Store.get_root store |> Store.Node.get_value in
  	LR.write_node_data ch ~txidaccess:(Some (conid, txid, LR.R)) ~path:"/" ~value ~perms;
	ListLabels.iter (fun (req, reply) ->
		Logging.debug "transaction" "dumpop %s" (Xenbus.Xb.Op.to_string req.Packet.ty); 
		let data = req.Packet.data in
		let open Xenbus.Xb.Op in
		match reply with
		| Packet.Error _ -> ()
		| _ ->
		try match req.Packet.ty with
| Debug
| Watch
| Unwatch
| Transaction_start
| Transaction_end
| Introduce
| Release
| Watchevent
| Getdomainpath
| Error
| Isintroduced
| Resume
| Set_target
| Reset_watches
| Invalid
| Directory
| (Read|Getperms) -> ()
| (Write|Setperms) ->
		(match (split (Some 2) '\000' data) with
		| path :: _ :: _ ->
	let path = Store.Path.create path conpath in
	if req.Packet.ty = Write then
  write_node_mkdir (Store.Path.get_parent path);(* implicit mkdir *)
	(match Store.get_node store path with
	| None -> ()
	| Some node ->
	write_node ch (Some (conid, txid, LR.W)) (Store.Path.get_parent path) node)
	| _ -> raise Invalid_Cmd_Args)
| Mkdir ->
	let path = split_one_path data conpath in
  write_node_mkdir  path;
| Rm ->
	let path = split_one_path data conpath |> Store.Path.to_string in
	LR.write_node_data ch ~txidaccess:(Some (conid, txid, LR.Del)) ~path ~value:"" ~perms:Perms.Node.default0
	with Invalid_Cmd_Args|Define.Invalid_path|Not_found-> ()
	 ) ops
