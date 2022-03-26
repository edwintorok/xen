(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 * Author Thomas Gazagnaire <thomas.gazagnaire@eu.citrix.com>
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
open Stdext


module SymbolMap = struct
  open Xenbus
  open Xenbus.Sizeops
  type 'a size = 'a Memory_size.t

  module M = Map.Make(Symbol)
  type 'a t =
    { map: 'a M.t
    ; size: [`constant | `immutable] size
    ; get_size: 'a -> [`constant | `immutable] size
    }

  let initial = Memory_size.unit ()
  let empty get_size = { map = M.empty; size = initial; get_size }

  let add k v t =
    let ksize = Memory_size.string (Symbol.to_string k) in
    let vsize = t.get_size v in
    { map = M.add k v t.map
    ; size = Memory_size.add t.size (Memory_size.add ksize vsize)
    ; get_size = t.get_size
    }

  let remove k t =
    let ksize = Memory_size.string (Symbol.to_string k) in
    let size = match M.find_opt k t.map with
    | None -> t.size
    | Some vold -> Memory_size.remove t.size (Memory_size.add ksize @@ t.get_size vold)
    in
    { map = M.remove k t.map
    ; size
    ; get_size = t.get_size
    }

  let mem x t = M.mem x t.map
  let find x t = M.find x t.map
  let find_opt x t = M.find_opt x t.map

    let update k f t =
            let r = find_opt k t in
            let r' = f r in
            match r, r' with
            | None, None -> t
            | Some _, None -> remove k t
            | Some r, Some r' when r == r' -> t
            | _, Some r' -> add k r' t

  let size_of t = (t.size : [< `constant | `immutable] size :> [> `constant | `immutable] size)

  let iter f t = M.iter f t.map
  let fold f t init = M.fold f t.map init

  let map f t =
    (* this is a map that preserves type, so we don't need a new get_size *)
    let map = M.map f t.map in
    let size = M.fold (fun k v acc ->
      Memory_size.add acc @@ Memory_size.add (Memory_size.string @@ Symbol.to_string k) @@ t.get_size v) t.map initial in
    { map
    ; size
    ; get_size = t.get_size
    }
end

module Node = struct

type t = {
	name: Symbol.t;
	perms: Perms.Node.t;
	value: string;
	children: t SymbolMap.t;
}

open Xenbus.Memory_size

let size_of t =
  record_start t
  |> record_add_immutable @@ string @@ Symbol.to_string t.name
  |> record_add_immutable @@ unit () (* TODO: perms node size *)
  |> record_add_immutable @@ string t.value
  |> record_add_immutable @@ SymbolMap.size_of t.children
  |> record_end

let create _name _perms _value =
	{ name = Symbol.of_string _name; perms = _perms; value = _value; children = SymbolMap.empty size_of; }

let get_owner node = Perms.Node.get_owner node.perms
let get_children node = node.children
let get_value node = node.value
let get_perms node = node.perms
let get_name node = Symbol.to_string node.name

let set_value node nvalue =
	if node.value = nvalue
	then node
	else { node with value = nvalue }

let set_perms node nperms = { node with perms = nperms }

let add_child node child =
	let children = SymbolMap.add child.name child node.children in
	{ node with children }

let exists node childname =
	let childname = Symbol.of_string childname in
	SymbolMap.mem childname node.children

let find node childname =
	let childname = Symbol.of_string childname in
	SymbolMap.find childname node.children

let replace_child node child nchild =
	{ node with
	  children = SymbolMap.update child.name
			(function None -> None | Some _ -> Some nchild)
			node.children
	}

let del_childname node childname =
	let sym = Symbol.of_string childname in
	{ node with children =
		SymbolMap.update sym
			(function None -> raise Not_found | Some _ -> None)
			node.children
	}

let del_all_children node =
	{ node with children = SymbolMap.empty size_of}

(* check if the current node can be accessed by the current connection with rperm permissions *)
let check_perm node connection request =
	Perms.check connection request node.perms

(* check if the current node is owned by the current connection *)
let check_owner node connection =
	if not (Perms.check_owner connection node.perms)
	then begin
		Logging.info "store|node" "Permission denied: Domain %d not owner" (get_owner node);
		raise Define.Permission_denied;
	end

let rec recurse fct node = fct node; SymbolMap.iter (fun _ -> recurse fct) node.children

(** [recurse_map f tree] applies [f] on each node in the tree recursively *)
let recurse_map f =
	let rec walk node =
          (* TODO: could check for physical equality *)
		f { node with children = SymbolMap.map walk node.children }
	in
	walk

let unpack node = (Symbol.to_string node.name, node.perms, node.value)

end

module Path = struct

(* represent a path in a store.
 * [] -> "/"
 * [ "local"; "domain"; "1" ] -> "/local/domain/1"
 *)
type t = string list

let char_is_valid c =
	(c >= 'a' && c <= 'z') ||
	(c >= 'A' && c <= 'Z') ||
	(c >= '0' && c <= '9') ||
	c = '_' || c = '-' || c = '@'

let name_is_valid name =
	name <> "" && String.fold_left (fun accu c -> accu && char_is_valid c) true name

let is_valid path =
	List.for_all name_is_valid path

let of_string s =
	if s.[0] = '@'
	then [s]
	else if s = "/"
	then []
	else match String.split '/' s with
		| "" :: path when is_valid path -> path
		| _ -> raise Define.Invalid_path

let of_path_and_name path name =
	match path, name with
	| [], "" -> []
	| _ -> path @ [name]

let create path connection_path =
	of_string (Utils.path_validate path connection_path)

let to_string t =
	"/" ^ (String.concat "/" t)

let to_string_list x = x

let get_parent t =
	if t = [] then [] else List.rev (List.tl (List.rev t))

let get_hierarchy path =
	Utils.get_hierarchy path

let get_common_prefix p1 p2 =
	let rec compare l1 l2 =
		match l1, l2 with
		| h1 :: tl1, h2 :: tl2 ->
			if h1 = h2 then h1 :: (compare tl1 tl2) else []
		| _, [] | [], _ ->
			(* if l1 or l2 is empty, we found the equal part already *)
			[]
		in
	compare p1 p2

let rec lookup_modify node path fct =
	match path with
	| []      -> raise (Define.Invalid_path)
	| h :: [] -> fct node h
	| h :: l  ->
		let (n, c) =
			if not (Node.exists node h) then
				raise (Define.Lookup_Doesnt_exist h)
			else
				(node, Node.find node h) in
		let nc = lookup_modify c l fct in
		Node.replace_child n c nc

let apply_modify rnode path fct =
	lookup_modify rnode path fct

let rec lookup_get node path =
	match path with
	| []      -> raise (Define.Invalid_path)
	| h :: [] ->
		(try
			Node.find node h
		with Not_found ->
			raise Define.Doesnt_exist)
	| h :: l  -> let cnode = Node.find node h in lookup_get cnode l

let get_node rnode path =
	if path = [] then
		Some rnode
	else (
		try Some (lookup_get rnode path) with Define.Doesnt_exist -> None
	)

(* get the deepest existing node for this path, return the node and a flag on the existence of the full path *)
let rec get_deepest_existing_node node = function
	| [] -> node, true
	| h :: t ->
		try get_deepest_existing_node (Node.find node h) t
		with Not_found -> node, false

let set_node rnode path nnode =
	if path = [] then
		nnode
	else
		let set_node node name =
			try
				let ent = Node.find node name in
				Node.replace_child node ent nnode
			with Not_found ->
				Node.add_child node nnode
			in
		apply_modify rnode path set_node

(* read | ls | getperms use this *)
let rec lookup node path fct =
	match path with
	| []      -> raise (Define.Invalid_path)
	| h :: [] -> fct node h
	| h :: l  -> let cnode = Node.find node h in lookup cnode l fct

let apply rnode path fct =
	lookup rnode path fct

let introduce_domain = "@introduceDomain"
let release_domain = "@releaseDomain"
let specials = List.map of_string [ introduce_domain; release_domain ]

end

module CamlBuffer = Buffer
open Xenbus.Memory_size_ds
(* The Store.t type *)
type t =
{
	mutable stat_transaction_coalesce: int;
	mutable stat_transaction_abort: int;
	root: Node.t Ref.t;
	quota: Quota.t Ref.t;
}

let size_of t =
  let open Xenbus.Memory_size in
  record_start t
  |> record_add_mutable_const @@ int t.stat_transaction_coalesce
  |> record_add_mutable_const @@ int t.stat_transaction_abort
  |> record_add_immutable @@ Ref.size_of t.root
  |> record_add_immutable @@ Ref.size_of t.quota
  |> record_end

let get_root store = Ref.get store.root
let set_root store root = Ref.set store.root root

let get_quota store = Ref.get store.quota
let set_quota store quota = Ref.set store.quota quota

(* modifying functions *)
let path_mkdir store perm path =
	let do_mkdir node name =
		try
			let ent = Node.find node name in
			Node.check_perm ent perm Perms.WRITE;
			raise Define.Already_exist
		with Not_found ->
			Node.check_perm node perm Perms.WRITE;
			Node.add_child node (Node.create name node.Node.perms "") in
	if path = [] then
		(get_root store)
	else
		Path.apply_modify (get_root store) path do_mkdir

let path_write store perm path value =
	let node_created = ref false in
	let do_write node name =
		try
			let ent = Node.find node name in
			Node.check_perm ent perm Perms.WRITE;
			let nent = Node.set_value ent value in
			Node.replace_child node ent nent
		with Not_found ->
			node_created := true;
			Node.check_perm node perm Perms.WRITE;
			Node.add_child node (Node.create name node.Node.perms value) in
	if path = [] then (
		Node.check_perm (get_root store) perm Perms.WRITE;
		Node.set_value (get_root store) value, false
	) else
		let root = Path.apply_modify (get_root store) path do_write in
		root, !node_created

let path_rm store perm path =
	let do_rm node name =
		try
			let ent = Node.find node name in
			Node.check_perm ent perm Perms.WRITE;
			Node.del_childname node name
		with Not_found ->
			raise Define.Doesnt_exist in
	if path = [] then (
		Node.check_perm (get_root store) perm Perms.WRITE;
		Node.del_all_children (get_root store)
	) else
		Path.apply_modify (get_root store) path do_rm

let path_setperms store perm path perms =
	if path = [] then (
		Node.check_perm (get_root store) perm Perms.WRITE;
		Node.set_perms (get_root store) perms
	) else
		let do_setperms node name =
			let c = Node.find node name in
			Node.check_owner c perm;
			Node.check_perm c perm Perms.WRITE;
			let nc = Node.set_perms c perms in
			Node.replace_child node c nc
		in
		Path.apply_modify (get_root store) path do_setperms

(* accessing functions *)
let get_node store path =
	Path.get_node (get_root store) path

let get_deepest_existing_node store path =
	Path.get_deepest_existing_node (get_root store) path

let read store perm path =
	let do_read node name =
		let ent = Node.find node name in
		Node.check_perm ent perm Perms.READ;
		ent.Node.value
	in
	if path = [] then (
		let ent = (get_root store) in
		Node.check_perm ent perm Perms.READ;
		ent.Node.value
	) else
		Path.apply (get_root store) path do_read

let ls store perm path =
	let children =
		if path = [] then (
			Node.check_perm (get_root store) perm Perms.READ;
			Node.get_children (get_root store)
		) else
			let do_ls node name =
				let cnode = Node.find node name in
				Node.check_perm cnode perm Perms.READ;
				cnode.Node.children in
			Path.apply (get_root store) path do_ls in
	SymbolMap.fold (fun k _ accu -> Symbol.to_string k :: accu) children []

let getperms store perm path =
	if path = [] then (
		Node.check_perm (get_root store) perm Perms.READ;
		Node.get_perms (get_root store)
	) else
		let fct n name =
			let c = Node.find n name in
			Node.check_perm c perm Perms.READ;
			c.Node.perms in
		Path.apply (get_root store) path fct

let path_exists store path =
	if path = [] then
		true
	else
		try
			let check_exist node name =
				ignore(Node.find node name);
				true in
			Path.apply (get_root store) path check_exist
		with Not_found -> false


(* others utils *)
let traversal root_node f =
	let rec _traversal path node =
		f path node;
		let node_path = Path.of_path_and_name path (Symbol.to_string node.Node.name) in
		SymbolMap.iter (fun _ -> _traversal node_path) node.Node.children
		in
	_traversal [] root_node

let dump_store_buf root_node =
	let buf = CamlBuffer.create 8192 in
	let dump_node path node =
		let pathstr = String.concat "/" path in
		Printf.bprintf buf "%s/%s{%s}" pathstr (Symbol.to_string node.Node.name)
		               (String.escaped (Perms.Node.to_string (Node.get_perms node)));
		if String.length node.Node.value > 0 then
			Printf.bprintf buf " = %s\n" (String.escaped node.Node.value)
		else
			Printf.bprintf buf "\n";
		in
	traversal root_node dump_node;
	buf

let dump_store chan root_node =
	let buf = dump_store_buf root_node in
	output_string chan (CamlBuffer.contents buf);
	CamlBuffer.reset buf

let dump_fct store f = traversal (get_root store) f
let dump store out_chan = dump_store out_chan (get_root store)
let dump_stdout store = dump_store stdout (get_root store)
let dump_buffer store = dump_store_buf (get_root store)


(* modifying functions with quota udpate *)
let set_node store path node orig_quota mod_quota =
	let root = Path.set_node (get_root store) path node in
        set_root store root;
	Quota.merge orig_quota mod_quota (get_quota store)

let write store perm path value =
	let node, existing = get_deepest_existing_node store path in
	let owner = Node.get_owner node in
	if existing || (Perms.Connection.is_dom0 perm) then
		(* Only check the string length limit *)
		Quota.check (get_quota store) (-1) (String.length value)
	else
		(* Check the domain entries limit too *)
		Quota.check (get_quota store) owner (String.length value);
	let root, node_created = path_write store perm path value in
        set_root store root;
	if node_created
	then Quota.add_entry (get_quota store) owner path value

let mkdir store perm path =
	let node, existing = get_deepest_existing_node store path in
	let owner = Node.get_owner node in
	(* It's upt to the mkdir logic to decide what to do with existing path *)
	if not (existing || (Perms.Connection.is_dom0 perm)) then Quota.check (get_quota store) owner 0;
	set_root store @@ path_mkdir store perm path;
	if not existing then
	Quota.add_entry (get_quota store) owner path ""

let rm store perm path =
	let rmed_node = Path.get_node (get_root store) path in
	match rmed_node with
	| None -> raise Define.Doesnt_exist
	| Some rmed_node ->
		set_root store @@ path_rm store perm path;
		Node.recurse (fun node -> Quota.del_entry (get_quota store) (Node.get_owner node) path (Node.get_value node)) rmed_node

let setperms store perm path nperms =
	match Path.get_node (get_root store) path with
	| None -> raise Define.Doesnt_exist
	| Some node ->
		let old_owner = Node.get_owner node in
		let new_owner = Perms.Node.get_owner nperms in
		if not ((old_owner = new_owner) || (Perms.Connection.is_dom0 perm)) then
			raise Define.Permission_denied;
		set_root store @@ path_setperms store perm path nperms;
                let value = Node.get_value node in
		Quota.del_entry (get_quota store) old_owner path value;
		Quota.add_entry (get_quota store) new_owner path value

let reset_permissions store domid =
	Logging.info "store|node" "Cleaning up xenstore ACLs for domid %d" domid;
	set_root store @@ Node.recurse_map (fun node ->
		let perms = Perms.Node.remove_domid ~domid node.perms in
		if perms <> node.perms then
			Logging.debug "store|node" "Changed permissions for node %s" (Node.get_name node);
		{ node with perms }
	) (get_root store)

type ops = {
	store: t;
	write: Path.t -> string -> unit;
	mkdir: Path.t -> unit;
	rm: Path.t -> unit;
	setperms: Path.t -> Perms.Node.t -> unit;
	ls: Path.t -> string list;
	read: Path.t -> string;
	getperms: Path.t -> Perms.Node.t;
	path_exists: Path.t -> bool;
}

let get_ops store perms = {
	store = store;
	write = write store perms;
	mkdir = mkdir store perms;
	rm = rm store perms;
	setperms = setperms store perms;
	ls = ls store perms;
	read = read store perms;
	getperms = getperms store perms;
	path_exists = path_exists store;
}

let create () = {
	stat_transaction_coalesce = 0;
	stat_transaction_abort = 0;
	root = Ref.make Node.size_of @@ Node.create "" Perms.Node.default0 "";
	quota = Ref.make Quota.size_of @@ Quota.create ();
}
let copy store = {
	stat_transaction_coalesce = store.stat_transaction_coalesce;
	stat_transaction_abort = store.stat_transaction_abort;
	root = Ref.copy (fun x -> x) store.root;
	quota = Ref.copy Quota.copy store.quota
}

let incr_transaction_coalesce store =
	store.stat_transaction_coalesce <- store.stat_transaction_coalesce + 1
let incr_transaction_abort store =
	store.stat_transaction_abort <- store.stat_transaction_abort + 1

let stats store =
	let nb_nodes = ref 0 in
	traversal (get_root store) (fun _path _node ->
		incr nb_nodes
	);
	!nb_nodes, store.stat_transaction_abort, store.stat_transaction_coalesce
