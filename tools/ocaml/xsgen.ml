exception Noent = Xenbus.Xb.Noent
open Xenstored_test
open Xenstore
type transaction = int
type connection = Xenstore.Xsraw.con * Connection.t
let none = 0

let memory_calculated_bytes (_, con) =
  con |> Connection.size_of |> Xenbus.Memory_size.size_of_bytes

let memory_reachable_bytes (_, con) =
  Obj.reachable_words (Obj.repr con) * Sys.word_size / 8

let memory_check con =
  let words = memory_reachable_bytes con in
  let calculated = memory_calculated_bytes con in
  if words <= calculated then true
  else failwith (Printf.sprintf "calculated: %d bytes, actual: %d bytes" calculated words)

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
  let open Xenbus.Memory_size_ds in
  let store = tid |> Connection.get_transaction connection |> Transaction.get_store in
  Store.Node.exists (Ref.get store.root) path

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
  let owner, others, rest = Xsraw.getperms tid path con in
  { owner; others = map_perm others; (*rest = List.map (fun (domid, perm) -> domid, map_perm perm) rest*) }


type token = string
let token () = "TODO"
let value () = "TODO"

type value = string

let has_watch (_, connection) path = not @@ Xenbus.Memory_size_ds.SizedList.is_empty @@ Connection.get_watches connection path

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
