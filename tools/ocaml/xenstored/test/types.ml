(*
 * Copyright (C) Citrix Systems Inc.
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

let domid_first_reserved = 0x7FF0

type 'a eq = 'a -> 'a -> bool

let hashtable_equal (eq : 'a eq) h h' =
  Hashtbl.length h = Hashtbl.length h'
  && Hashtbl.fold
       (fun k v acc ->
         acc
         && match Hashtbl.find_opt h' k with Some x -> eq v x | None -> false)
       h true

let list_equal (eq : 'a eq) l l' =
  try List.for_all2 eq l l' with Invalid_argument _ -> false

let queue_equal eq q q' =
  Queue.length q = Queue.length q'
  &&
  let list_of_queue q = Queue.fold (fun acc e -> e :: acc) [] q in
  list_equal eq (list_of_queue q) (list_of_queue q')

let pp_process_status ppf = function
  | Unix.WEXITED code ->
      Fmt.pf ppf "exited with code %d" code
  | Unix.WSIGNALED osig ->
      Fmt.pf ppf "killed by signal %a" Fmt.Dump.signal osig
  | Unix.WSTOPPED osig ->
      Fmt.pf ppf "stopped by signal %a" Fmt.Dump.signal osig

let pp_dump_ref dump =
  Fmt.using ( ! ) Fmt.(dump |> Fmt.braces |> prefix (const string "ref"))

let pp_file_descr = Fmt.using Disk.FD.to_int Fmt.int

module Quota = struct
  open Quota

  let pp_dump =
    let open Fmt in
    Dump.record
      [ Dump.field "maxent" (fun q -> q.maxent) int
      ; Dump.field "maxsize" (fun q -> q.maxsize) int
      ; Dump.field "cur" (fun q -> q.cur) @@ Dump.hashtbl int int ]

  let drop_dom0 h =
    (* Quota is ignored for Dom0 and will be wrong in some situations:
       - when domains die any nodes owned by them get inherited by Dom0
       - the root node is owned by Dom0, if its ownership is changed Dom0's quota will be off-by-one
      Since Dom0's quota is not actually used, just drop it when comparing
     *)
    let h' = Hashtbl.copy h in
    Hashtbl.remove h' 0;
    h'

  let equal q q' =
    q.maxent = q'.maxent && q.maxsize = q'.maxsize
    && hashtable_equal Int.equal (drop_dom0 q.cur) (drop_dom0 q'.cur)
end
let pp_dump_quota = Quota.pp_dump
let equal_quota = Quota.equal

module Store = struct
  open Store

  module Node = struct
    open Node

    let pp_dump ppf t =
      let buf = dump_store_buf t in
      Fmt.lines ppf (Buffer.contents buf)

    let rec equal n n' =
      Symbol.equal n.name n'.name
      && Perms.equiv n.perms n'.perms
      && String.equal n.value n'.value
      && SymbolMap.equal equal n.children n'.children
  end

  module Path = struct
    open Path

    let pp_dump = Fmt.using to_string Fmt.string

    let equal p p' = list_equal String.equal p p'

    let hash (p : t) = Hashtbl.hash p

    let compare (p : t) (p' : t) = compare p p'
  end

  let pp_dump =
    let open Fmt in
    (* only print relevant fields, expected to stay same during live-update. *)
    Dump.record
      [ Dump.field "stat_transaction_coalesce"
          (fun t -> t.stat_transaction_coalesce)
          int
      ; Dump.field "stat_transaction_abort"
          (fun t -> t.stat_transaction_coalesce)
          int
      ; Dump.field "store" (fun t -> t.root) Node.pp_dump
      ; Dump.field "quota" (fun t -> t.quota) Quota.pp_dump ]

  let equal s s' =
    (* ignore stats *)
    Node.equal s.root s'.root && Quota.equal s.quota s'.quota
end

let pp_dump_store = Store.pp_dump
let equal_store = Store.equal

module Xb = struct
  open Xenbus.Xb

  module Op = struct
    open Xenbus.Op

    let pp_dump = Fmt.of_to_string to_string

    let equal (op : t) (op' : t) = op = op'
  end

  module Packet = struct
    open Xenbus.Packet

    let pp_dump =
      let open Fmt in
      Dump.record
        [ Dump.field "tid" get_tid int
        ; Dump.field "rid" get_rid int
        ; Dump.field "ty" get_ty Op.pp_dump
        ; Dump.field "data" get_data Dump.string ]

    let equal (p : t) (p' : t) =
      (* ignore TXID, it can be different after a live-update *)
      p.rid = p'.rid && p.ty = p'.ty && p.data = p'.data
  end

  module Partial = struct
    open Xenbus.Partial

    let pp_dump =
      let open Fmt in
      Dump.record
        [ Dump.field "tid" (fun p -> p.tid) int
        ; Dump.field "rid" (fun p -> p.rid) int
        ; Dump.field "ty" (fun p -> p.ty) Op.pp_dump
        ; Dump.field "len" (fun p -> p.len) int
        ; Dump.field "buf" (fun p -> p.buf) Fmt.buffer ]

    let equal p p' =
      p.tid = p'.tid && p.rid = p'.rid && p.ty = p'.ty
      && Buffer.contents p.buf = Buffer.contents p'.buf
  end

  let pp_dump_partial_buf ppf = function
    | HaveHdr pkt ->
        Fmt.pf ppf "HaveHdr %a" Partial.pp_dump pkt
    | NoHdr (i, b) ->
        Fmt.pf ppf "NoHdr(%d, %S)" i (Bytes.to_string b)

  let equal_partial_buf buf buf' =
    match (buf, buf') with
    | HaveHdr pkt, HaveHdr pkt' ->
        Partial.equal pkt pkt'
    | NoHdr (i, b), NoHdr (i', b') ->
        i = i' && b = b'
    | HaveHdr _, NoHdr _ | NoHdr _, HaveHdr _ ->
        false

  let pp_backend ppf = function
    | Fd {fd} ->
        Fmt.prefix (Fmt.const Fmt.string "Fd ") pp_file_descr ppf fd
    | Xenmmap _ ->
        Fmt.const Fmt.string "Xenmmap _" ppf ()

  let equal_backend b b' =
    match (b, b') with
    | Fd fd, Fd fd' ->
        fd = fd'
    | Xenmmap _, Xenmmap _ ->
        true (* can't extract the FD to compare *)
    | Fd _, Xenmmap _ | Xenmmap _, Fd _ ->
        false

  let pp_dump =
    let open Fmt in
    Dump.record
      [ Dump.field "backend" (fun x -> x.backend) pp_backend
      ; Dump.field "pkt_in" (fun x -> x.pkt_in) @@ Dump.queue Packet.pp_dump
      ; Dump.field "pkt_out" (fun x -> x.pkt_out) @@ Dump.queue Packet.pp_dump
      ; Dump.field "partial_in" (fun x -> x.partial_in) pp_dump_partial_buf
      ; Dump.field "partial_out" (fun x -> x.partial_out) Dump.string ]

  let equal_pkts xb xb' =
    let queue_eq = queue_equal Packet.equal in
    queue_eq xb.pkt_in xb'.pkt_in
    && queue_eq xb.pkt_out xb'.pkt_out
    && xb.partial_in = xb'.partial_in
    && xb.partial_out = xb'.partial_out

  let equal xb xb' = equal_backend xb.backend xb'.backend && equal_pkts xb xb'
end

let pp_dump_packet = Xb.Packet.pp_dump
let pp_dump_xb = Xb.pp_dump
let equal_xb = Xb.equal
let equal_xb_pkts = Xb.equal_pkts

module Packet = struct
  open Packet

  let pp_dump_request =
    let open Fmt in
    Dump.record
      [ Dump.field "tid" (fun t -> t.tid) int
      ; Dump.field "rid" (fun t -> t.rid) int
      ; Dump.field "ty" (fun t -> t.ty) Xb.Op.pp_dump
      ; Dump.field "data" (fun t -> t.data) Dump.string ]

  let equal_req r r' =
    r.tid = r'.tid && r.rid = r'.rid && r.ty = r'.ty && r.data = r'.data

  let pp_dump_response ppf = function
    | Reply str ->
        Fmt.pf ppf "Reply %S" str
    | Error str ->
        Fmt.pf ppf "Error %S" str
    | Ack _ ->
        Fmt.string ppf "Ack"

  let equal_response = response_equal
end

module Transaction = struct
  open Transaction

  let pp_dump_ty ppf = function
    | Transaction.No ->
        Fmt.string ppf "No"
    | Full (id, orig, canonical) ->
        Fmt.pf ppf "Full @[(%d, %a, %a)@]" id Store.pp_dump orig Store.pp_dump
          canonical

  let equal_ty t t' =
    match (t, t') with
    | Transaction.No, Transaction.No ->
        true
    | Transaction.Full _, Transaction.Full _ ->
        (* We expect the trees not to be identical, so we ignore any differences here.
           The reply comparison tests will find any mismatches in observable transaction state
        *)
        true
    | Transaction.No, Transaction.Full _ | Transaction.Full _, Transaction.No ->
        false

  let equal_pathop (op, path) (op', path') =
    op = op' && Store.Path.equal path path'

  let pp_dump_op = Fmt.pair Packet.pp_dump_request Packet.pp_dump_response

  let equal_op (req, reply) (req', reply') =
    Packet.equal_req req req' && Packet.equal_response reply reply'

  let pp_dump =
    let open Fmt in
    let open Transaction in
    Dump.record
      [ Dump.field "ty" (fun t -> t.ty) pp_dump_ty
      ; Dump.field "start_count" (fun t -> t.start_count) int64
      ; Dump.field "store" (fun t -> t.store) Store.pp_dump
      ; Dump.field "quota" (fun t -> t.quota) Quota.pp_dump
      ; Dump.field "must_fail" (fun t -> t.must_fail) Fmt.bool
      ; Dump.field "paths" (fun t -> t.paths)
        @@ Dump.list (pair Xb.Op.pp_dump Store.Path.pp_dump)
      ; Dump.field "operations" (fun t -> t.operations)
        @@ list (pair Packet.pp_dump_request Packet.pp_dump_response)
      ; Dump.field "read_lowpath" (fun t -> t.read_lowpath)
        @@ option Store.Path.pp_dump
      ; Dump.field "write_lowpath" (fun t -> t.write_lowpath)
        @@ option Store.Path.pp_dump ]

  let equal t t' =
    equal_ty t.ty t'.ty
    (* ignored: quota at start of transaction, not relevant
       && Quota.equal t.quota t'.quota *)
    (*&& list_equal equal_pathop t.paths t'.paths *)
    (*&& list_equal equal_op t.operations t'.operations*)
    && t.must_fail = t'.must_fail
    (* ignore lowpath, impossible to recreate from limited migration info *)
    (*&& Option.equal Store.Path.equal t.read_lowpath t'.read_lowpath
    && Option.equal Store.Path.equal t.write_lowpath t'.write_lowpath *)
end

module Connection = struct
  open Connection

  let pp_dump_watch =
    let open Fmt in
    Dump.record
      [ Dump.field "token" (fun w -> w.token) Dump.string
      ; Dump.field "path" (fun w -> w.path) Dump.string
      ; Dump.field "base" (fun w -> w.base) Dump.string
      ; Dump.field "is_relative" (fun w -> w.is_relative) Fmt.bool ]

  let pp_dump =
    let open Fmt in
    Dump.record
      [ Dump.field "xb" (fun c -> c.xb) Xb.pp_dump
      ; Dump.field "transactions" (fun c -> c.transactions)
        @@ Dump.hashtbl int Transaction.pp_dump
      ; Dump.field "next_tid" (fun t -> t.next_tid) int
      ; Dump.field "nb_watches" (fun c -> c.nb_watches) int
      ; Dump.field "anonid" (fun c -> c.anonid) int
      ; Dump.field "watches" (fun c -> c.watches)
        @@ Dump.hashtbl Dump.string (Dump.list pp_dump_watch)
      ; Dump.field "perm" (fun c -> c.perm)
        @@ Fmt.using Perms.Connection.to_string Fmt.string ]

  let equal c c' =
    let watch_equal w w' =
      (* avoid recursion, these must be physically equal *)
      w.con == c && w'.con == c' && w.token = w'.token && w.path = w'.path
      && w.base = w'.base
      && w.is_relative = w'.is_relative
    in
    Xb.equal c.xb c'.xb
    && hashtable_equal Transaction.equal c.transactions c'.transactions
    (* next_tid ignored, not preserved *)
    && hashtable_equal (list_equal watch_equal) c.watches c'.watches
    && c.nb_watches = c'.nb_watches
    (* anonid ignored, not preserved *)
    (* && c.anonid = c'.anonid *) && c.perm = c'.perm

  let equal_watch w w' =
    equal w.con w'.con && w.token = w'.token && w.path = w'.path
    && w.base = w'.base
    && w.is_relative = w'.is_relative
end

module Trie = struct
  open Trie

  let pp_dump dump_elt =
    Fmt.Dump.iter_bindings Trie.iter (Fmt.any "trie") Fmt.string
      Fmt.(option dump_elt)

  let plus1 _ _ acc = acc + 1

  let length t = fold plus1 t 0

  (* Trie.iter doesn't give full path so we can't compare the paths/values exactly.
     They will be compared as part of the individual connections
  *)
  let equal _eq t t' = length t = length t'
end

module Connections = struct
  open Connections

  let pp_dump =
    let open Fmt in
    Dump.record
      [ Dump.field "anonymous" (fun t -> t.anonymous)
        @@ Dump.hashtbl Fmt.(any "") Connection.pp_dump
      ; Dump.field "domains" (fun t -> t.domains)
        @@ Dump.hashtbl Fmt.int Connection.pp_dump
      ; Dump.field "ports" (fun t -> t.ports)
        @@ Dump.hashtbl
             (Fmt.using Xeneventchn.to_int Fmt.int)
             Connection.pp_dump
      ; Dump.field "watches" (fun t -> t.watches)
        @@ Trie.pp_dump (Dump.list Connection.pp_dump_watch) ]

  let equal c c' =
    hashtable_equal Connection.equal c.anonymous c'.anonymous
    && hashtable_equal Connection.equal c.domains c'.domains
    (* TODO: local port changes for now *)
    (*&& hashtable_equal Connection.equal c.ports c'.ports *)
    && Trie.equal (list_equal Connection.equal_watch) c.watches c'.watches
end

let pp_dump_connections = Connections.pp_dump
let equal_connections = Connections.equal

module Domain = struct
  open Domain

  let pp_dump =
    let open Fmt in
    Dump.record
      [ Dump.field "id" Domain.get_id int
      ; Dump.field "remote_port" Domain.get_remote_port int
      ; Dump.field "bad_client" Domain.is_bad_domain bool
      ; Dump.field "io_credit" Domain.get_io_credit int
      ; Dump.field "conflict_credit" (fun t -> t.conflict_credit) float
      ; Dump.field "caused_conflicts" (fun t -> t.caused_conflicts) int64 ]

  (* ignore stats fields *)
  let equal t t' = t.id = t'.id && t.remote_port = t'.remote_port
end

module Domains = struct
  open Domains

  let pp_dump =
    let open Fmt in
    Dump.record
      [ Dump.field "table" (fun t -> t.table)
        @@ Dump.hashtbl Fmt.int Domain.pp_dump
      ; Dump.field "doms_conflict_paused" (fun t -> t.doms_conflict_paused)
        @@ Dump.queue (pp_dump_ref @@ Dump.option Domain.pp_dump)
      ; Dump.field "doms_with_conflict_penalty" (fun t ->
            t.doms_with_conflict_penalty)
        @@ Dump.queue (pp_dump_ref @@ Dump.option Domain.pp_dump)
      ; Dump.field "n_paused" (fun t -> t.n_paused) int
      ; Dump.field "n_penalised" (fun t -> t.n_penalised) int ]

  (* ignore statistic fields *)
  let equal t t' = hashtable_equal Domain.equal t.table t'.table
end
let pp_dump_domains = Domains.pp_dump
let equal_domains = Domains.equal
