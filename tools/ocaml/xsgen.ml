exception Noent = Xenbus.Xb.Noent
open Xenstored_test
open Xenstore
type transaction = int
type connection = Xenstore.Xsraw.con * Connection.t
let none = 0

open Sizedebug
let operation_debug = v ~exact:true (fun _ -> Xenbus.Size_tracker.record_field) "operation"
let packet_debug =
  let open Xenbus in
  record "Packet.t"
  [ field "tid" int Packet.get_tid
  ; field "rid" int Packet.get_rid
  ; field "ty" operation_debug Packet.get_ty
  ; field "data" string Packet.get_data
  ]

let packet_request_debug =
  let open Packet in
  record "Packet.request"
  [ field "tid" int (fun t -> t.tid)
  ; field "rid" int (fun t -> t.rid)
  ; field "ty" operation_debug (fun t -> t.ty)
  ; field "data" string (fun t -> t.data)
  ]

let closure = v (fun _ -> Xenbus.Size_tracker.record_field) "closure"

let packet_response_debug =
  let open Packet in
  variant
  [ case "Ack" closure (function Ack _ -> Some () | _ -> None)
  ; case "Reply" string (function Reply s -> Some s | _ -> None)
  ; case "Error" string (function Error e -> Some e | _ -> None)
  ] "Packet.response"

let mmap_interface = v (fun _ -> Xenbus.Size_tracker.record_field) "mmap_interface"
let event = v (fun _ -> Xenbus.Size_tracker.record_field) "event"
let unit = v (fun _ -> Xenbus.Size_tracker.empty) "unit"

let backend_debug =
  let open Xenbus.Xb in
  let file_descr = v (fun _ -> Xenbus.Size_tracker.empty) "file_descr" in
  let fd = record "backend_fd"
  [ field "fd" file_descr (fun t -> t.fd)
  ]
  in
  let closure = v (fun _ -> Xenbus.Size_tracker.record_field) "closure" in
  let mmap = record "backend_mmap"
  [ field "mmap" mmap_interface (fun t -> t.mmap)
  ; field "eventchn_notify" closure (fun t -> t.eventchn_notify)
  ; field "work_again" bool (fun t -> t.work_again)
  ]
  in
  variant
  [ case "Fd" fd (function Fd fd -> Some fd | _ -> None)
  ; case "Xenmmap" mmap (function Xenmmap m -> Some m | _ -> None)

  ] "backend"

let pkt_debug =
  let open Xenbus.Partial in
  let buf =
    v (fun t ->
      (* only approximate because there is also an initial buffer size here, and it may grow.. *)
      Xenbus.Size_tracker.of_string_length @@ Buffer.length t) "Buffer"
  in
  record "Partial.pkt"
  [ field "tid" int (fun t -> t.tid)
  ; field "rid" int (fun t -> t.rid)
  ; field "ty" operation_debug (fun t -> t.ty)
  ; field "len" int (fun t -> t.len)
  ; field "buf" buf (fun t -> t.buf)
  ]

let partial_buf_debug =
  let open Xenbus.Xb in
  variant
  [ case "HaveHdr" pkt_debug (function HaveHdr h -> Some h | _ -> None)
  ; case "NoHdr" (pair int bytes) (function NoHdr (a,b) -> Some (a,b) | _ -> None)
  ] "partial_buf"

let sized_queue_to_seq q =
  Xenbus.Sized_queue.fold (fun a b -> b :: a) [] q
  |> List.to_seq

let sized_queue el = seq sized_queue_to_seq el

let xb_debug =
  let open Xenbus.Xb in
  let packets = sized_queue packet_debug "PacketQueue" in
  record "Xb.t"
  [ field "backend" backend_debug (fun t ->
    let backend, _, _, _, _ = debug_view t in
    backend)
  ; field "pkt_in" packets
  (fun t ->
    let _, pkt_in, _, _, _ = debug_view t in
    pkt_in)
  ; field "pkt_out" packets
  (fun t ->
    let _, _, pkt_out, _, _ = debug_view t in
    pkt_out)
  ; field "partial_in" partial_buf_debug (fun t ->
    let _, _, _, partial_buf, _ = debug_view t in
    partial_buf
  )
  ; field "partial_out" string (fun t ->
    let _, _, _, _, partial_out = debug_view t in
    partial_out
  )
  ]

let option el =
  variant
  [ case "Some" el (fun x -> x)
  ; case "None" int (function None -> Some 0 | Some _ -> None)
  ] "option"

let domain_debug =
  let open Domain in
  record "domain"
  [ field "id" int (fun t -> t.id)
  ; field "mfn" nativeint (fun t -> t.mfn)
  ; field "interface"  mmap_interface (fun t -> t.interface)
  ; field "eventchn" event (fun t -> t.eventchn)
  ; field "remote_port" int (fun t -> t.remote_port)
  ; field "bad_client" bool (fun t -> t.bad_client)
  ; field "io_credit" int (fun t -> t.io_credit)
  ; field "conflict_credit" float (fun t -> t.conflict_credit)
  ; field "caused_conflicts" int64 (fun t -> t.caused_conflicts)
  ]

let node = v (fun  _ -> Xenbus.Size_tracker.record_field) "node"
let quota = v (fun  _ -> Xenbus.Size_tracker.record_field) "quota"

let rec store_debug =
  let open Store in
  record "store"
  [ field "stat_transaction_coalesce" int (fun t -> t.stat_transaction_coalesce)
  ; field "stat_transaction_abort" int (fun t -> t.stat_transaction_abort)
  ; field "root" node (fun t -> t.root) (* TODO: this is counted by the quota system instead.. *)
  ; field "quota" quota (fun t -> t.quota)
  ]

let transaction_ty =
  let open Transaction in
  let full =
    pair store_debug store_debug
  in
  variant
  [ case "No" unit (function No -> Some () | _ -> None)
  ; case "Full" full (function Full (id, orig, canon) -> Some (orig, canon) | _ -> None)
  ] "ty"

let path = seq List.to_seq string "path"

let path_queue = sized_queue (pair operation_debug path) "path_queue"
let operation_queue = sized_queue (pair packet_request_debug packet_response_debug) "operations_queue"

let transaction_debug =
  let open Transaction in
  record "transaction"
  [ field "ty" transaction_ty (fun t -> t.ty)
  ; field "start_count" int64 (fun t -> t.start_count)
  ; field "store" store_debug (fun t -> t.store)
  ; field "quota" quota (fun t -> t.quota)
  ; field "oldroot" node (fun t -> t.oldroot)
  ; field "paths" path_queue (fun t -> t.paths)
  ; field "operations" operation_queue (fun t -> t.operations)
  ; field "read_lowpath" (option path) (fun t -> t.read_lowpath)
  ; field "write_lowpath" (option path) (fun t -> t.write_lowpath)
  ]

let noop_conn = v (fun _ -> Xenbus.Size_tracker.record_field) "conn" (* avoid infinite rec *)

let watch_debug =
  let open Connection in
  record "watch"
  [ field "con" noop_conn (fun t -> t.con)
  ; field "token" string (fun t -> t.token)
  ; field "path" string (fun t -> t.path)
  ; field "base" string (fun t -> t.base)
  ; field "is_relative" bool (fun t -> t.is_relative)
  ]

let perm_debug = v (fun _ -> Xenbus.Size_tracker.record_field) "perm"

let connection_debug =
  let list el = seq List.to_seq el in
  record "connection"
  [ field "xb" xb_debug (fun t -> t.Connection.xb)
  ; field "dom" (option domain_debug) (fun t -> t.Connection.dom)
  ; field "transactions" (pair_seq int transaction_debug Hashtbl.to_seq "transactions") (fun t -> t.Connection.transactions)
  ; field "next_tid" int (fun t -> t.Connection.next_tid)
  ; field "watches" (pair_seq string (list watch_debug "watches") Hashtbl.to_seq "watches") (fun t -> t.Connection.watches)
  ; field "nb_watches" int (fun t -> t.Connection.nb_watches)
  ; field "anonid" int (fun t -> t.Connection.anonid)
  ; field "stat_nb_ops" int (fun t -> t.Connection.stat_nb_ops)
  ; field "perm" perm_debug (fun t -> t.Connection.perm)
  ]

let memory_calculated_bytes (_, con) =
  con |> Connection.size |> Xenbus.Size_tracker.to_byte_count

let memory_reachable_bytes (_, con) =
  Obj.reachable_words (Obj.repr con) * Sys.word_size / 8

let memory_check con =
  let bytes = memory_reachable_bytes con in
  let calculated = memory_calculated_bytes con in
  if bytes <= calculated then true
  else failwith (Printf.sprintf "calculated: %d bytes, actual: %d bytes" calculated bytes)

let is_output_devnull = Unix.stat "/dev/null" = Unix.fstat Unix.stdout
(* during AFL print nothing *)

let monolith_write ?(level=Logging.Debug) s =
  Monolith.dprintf "(* %s %s *)\n" (Logging.string_of_level level) s

let monolith_logger =
  Logging.{ stop = ignore
               ; restart = ignore
               ; rotate = ignore
               ; write = monolith_write }

let () =
  (* by default we don't print xenstore access/debug logs *)
  if not is_output_devnull (* && Sys.getenv_opt "XSLOG" <> None *) then begin
    Printexc.record_backtrace true;
    Logging.set_xenstored_logger monolith_logger;
    Logging.access_logger := Some monolith_logger;
    Logging.access_log_special_ops := true;
    Logging.access_log_transaction_ops := true ;
    Logging.xenstored_log_level := Logging.Debug
  end

let cons = Connections.create ()
let store = Store.create ()
let eventchn = Event.init ()
let doms = Domains.init eventchn ignore

let server_thread fd =
  let n = ref 0 in
  let t = ref @@ Unix.gettimeofday () in
  try while true do
    let is_peaceful c =
      match Connection.get_domain c with
                        | None -> true (* Treat socket-connections as exempt, and free to conflict. *)
                        | Some dom -> not (Domain.is_paused_for_conflict dom)
    in
    let mw = Connections.has_more_work cons in
    let peaceful_mw = List.filter is_peaceful mw in
    List.iter
                      (fun c ->
                        match Connection.get_domain c with
                       | None -> () | Some d -> Domain.incr_io_credit d)
                      peaceful_mw;
    let inset, outset = Connections.select cons in
    let rset, wset, _ = Poll.poll_select inset outset [] 5. in
    incr n;
    let now = Unix.gettimeofday () in
    if now > !t +. 1.  then begin
      Printf.eprintf "Requests: %d in %.2f\n" !n (!t -. now);
      (* TODO: detect if no requests: means stuck processing and bug *)
      flush stderr;
      n := 0;
      t := now
    end;
    if List.length rset = 0 && List.length wset = 0 then begin
      prerr_endline "Packet processing stuck: no more input available\n";
      exit 4
    end;
    List.iter (fun fd -> Process.do_input store cons doms @@ Connections.find cons fd) rset;
    List.iter (fun fd -> Process.do_output store cons doms @@ Connections.find cons fd) wset
  done
  with e ->
    Printexc.print_backtrace stderr;
    prerr_endline (Printexc.to_string e);
    flush stderr;
    Unix.close fd

let connection =
  let client, server = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Connections.add_anonymous cons server;
  let (_:Thread.t) = Thread.create server_thread server in
  Xsraw.open_fd client, Connections.find cons server

exception Quota

let maxtransaction _ = !Define.maxtransaction
let number_of_transactions (_, connection) = Connection.number_of_transactions connection

let transaction_start (con, _) =
  try Xsraw.transaction_start con
  with Xenbus.Xb.Packet.Error "EQUOTA" -> raise Quota

let transaction_is_valid (_, connection) tid =
  try let _:Transaction.t = Connection.get_transaction connection tid in true
  with Not_found -> false

let transaction_end (con, _) tid commit = Xsraw.transaction_end tid commit con

type path = string

let path_is_valid p = String.length p > 0 (* for now *)

let path () = "/TODO"

type perm = { r: bool; w: bool }

let directory (con, _) tid path =
  List.rev_map (function "" -> path | entry -> Filename.concat path entry) @@ Xsraw.directory tid path con

let path_exists (_, connection) tid path =
  let store = tid |> Connection.get_transaction connection |> Transaction.get_store in
  Store.Node.exists store.root path

let read (con, _) tid path = Xsraw.read tid path con

type domid = int

let domid (x:int) = x
let int_of_domid (x:int) = x

type perms =
  { owner: domid
  ; others: perm
  }

let perms owner others = { owner; others }

let map_perm = function
  | Xsraw.PERM_NONE -> { r = false; w = false }
  | Xsraw.PERM_READ -> { r = true; w = false }
  | Xsraw.PERM_WRITE ->{ r = false; w = true }
  | Xsraw.PERM_RDWR -> {r = true; w = true}

let rev_map_perm = function
  | {r =false; w= false} -> Xsraw.PERM_NONE
  | {r=true; w=false} -> Xsraw.PERM_READ
  | {r=false; w=true} -> Xsraw.PERM_WRITE
  | {r=true; w=true} -> Xsraw.PERM_RDWR

let getperms (con, _) tid path =
  let owner, others, _rest = Xsraw.getperms tid path con in
  { owner; others = map_perm others; (*rest = List.map (fun (domid, perm) -> domid, map_perm perm) rest*) }


type token = string
let token () = "TODO"
let value () = "TODO"

type value = string

let has_watch (_, connection) path = Connection.get_watches connection path <> []

exception Eexist

let watch (con, _) path token =
  try Xsraw.watch path token con
  with Xenbus.Packet.Error "EEXIST" -> raise Eexist

let unwatch (con, _) path token = Xsraw.unwatch path token con

let dom0 = 0

(* candidate and reference run same queries: must not interfere *)
let introduce =
  let counter = ref 0 in
  fun (con, _) ->
    incr counter;
    let domid = !counter in
    Xsraw.introduce domid 0n 0 con;
    domid

let domid_exists domid =
  try let (_:Domain.t) =  Domains.find doms domid in true
  with Not_found -> false

let release (con, _) domid =
  assert (domid > 0);
  Xsraw.release domid con

let resume (con, _) domid = Xsraw.resume domid con

let getdomainpath (con, _) domid = Xsraw.getdomainpath domid con

let write (con, _) tid path value = Xsraw.write tid path value con

let mkdir (con, _) tid path = Xsraw.mkdir tid path con

let rm (con, _) tid path = Xsraw.rm tid path con


let setperms (con, _) tid path perms =
  (*let rest = List.map (fun (domid, perm) -> domid, rev_map_perm perm ) perms.rest in*)
  Xsraw.setperms tid path (perms.owner, rev_map_perm perms.others, []) con
