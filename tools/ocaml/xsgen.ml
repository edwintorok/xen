exception Noent = Xenbus.Xb.Noent
open Xenstored_test
open Xenstore
type transaction = int
type connection = Xenstore.Xsraw.con
let none = 0

let is_output_devnull = Unix.stat "/dev/null" = Unix.fstat Unix.stdout
(* during AFL print nothing *)

let () =
  if not is_output_devnull then begin
    Printexc.record_backtrace true;
    Logging.xenstored_log_destination := Logging.File "/dev/stderr";
    Logging.access_log_destination := Logging.File "/dev/stderr";
    Logging.access_log_special_ops := true;
    Logging.init_xenstored_log ();
    Logging.init_access_log ignore
  end

let cons = Connections.create ()
let store = Store.create ()
let eventchn = Event.init ()
let doms = Domains.init eventchn ignore

let server_thread fd =
  try while true do
    let rset, wset, _ = Poll.poll_select [fd] [fd] [] 1. in
    List.iter (fun fd -> Process.do_input store cons doms @@ Connections.find cons fd) rset;
    List.iter (fun fd -> Process.do_output store cons doms @@ Connections.find cons fd) wset
  done
  with e ->
    Printexc.print_backtrace stderr;
    prerr_endline (Printexc.to_string e);
    Unix.close fd

let con, connection =
  let client, server = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Connections.add_anonymous cons server;
  let (_:Thread.t) = Thread.create server_thread server in
  Xsraw.open_fd client, Connections.find cons server

exception Quota

let maxtransaction _ = !Define.maxtransaction
let number_of_transactions _ = Connection.number_of_transactions connection

let transaction_start con =
  try Xsraw.transaction_start con
  with Xenbus.Xb.Packet.Error "EQUOTA" -> raise Quota

let transaction_is_valid tid =
  try let _:Transaction.t = Connection.get_transaction connection tid in true
  with Not_found -> false

let transaction_end tid commit = Xsraw.transaction_end tid commit con

type path = string

let path_is_valid p = String.length p > 0 (* for now *)

let path () = "/TODO"

type perm = { r: bool; w: bool }

let directory tid path = Xsraw.directory tid path con

let path_exists tid path =
  let store = tid |> Connection.get_transaction connection |> Transaction.get_store in
  Store.Node.exists store.root path

let read tid path = Xsraw.read tid path con

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

let getperms tid path =
  let owner, others, rest = Xsraw.getperms tid path con in
  { owner; others = map_perm others; (*rest = List.map (fun (domid, perm) -> domid, map_perm perm) rest*) }


type token = string
let token () = "TODO"
let value () = "TODO"

type value = string

let watch path token = Xsraw.watch path token con

let unwatch path token = Xsraw.unwatch path token con

let dom0 = 0

(* candidate and reference run same queries: must not interfere *)
let introduce =
  let counter = ref 0 in
  fun () ->
    incr counter;
    let domid = !counter in
    Xsraw.introduce domid 0n 0 con;
    domid

let domid_exists domid =
  try let (_:Domain.t) =  Domains.find doms domid in true
  with Not_found -> false

let release domid =
  assert (domid > 0);
  Xsraw.release domid con

let resume domid = Xsraw.resume domid con

let getdomainpath domid = Xsraw.getdomainpath domid con

let write tid path value = Xsraw.write tid path value con

let mkdir tid path = Xsraw.mkdir tid path con

let rm tid path = Xsraw.rm tid path con


let setperms tid path perms =
  (*let rest = List.map (fun (domid, perm) -> domid, rev_map_perm perm ) perms.rest in*)
  Xsraw.setperms tid path (perms.owner, rev_map_perm perms.others, []) con
