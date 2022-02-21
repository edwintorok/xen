open Xenstored_internal
open Xenstore
type transaction = int

let none = 0

let server_thread =
  let cons = Connections.create () in
  let store = Store.create () in
  let eventchn = Event.init () in
  let doms = Domains.init eventchn ignore in
  fun fd ->
  Connections.add_anonymous cons fd;
  let rset, wset, _ = Poll.poll_select [fd] [fd] [] 1. in
  List.iter (fun fd -> Process.do_input store cons doms @@ Connections.find cons fd) rset;
  List.iter (fun fd -> Process.do_output store cons doms @@ Connections.find cons fd) wset

let con =
  let client, server = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let (_:Thread.t) = Thread.create server_thread server in
  Xsraw.open_fd client

let transaction_start () = Xsraw.transaction_start con

let transaction_end tid commit = Xsraw.transaction_end tid commit con

type path = string

let path () = "TODO"

type perm = { r: bool; w: bool }

let directory tid path = Xsraw.directory tid path con

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
let token () = failwith "TODO"
let value () = failwith "TODO"

type value = string

let watch path token = Xsraw.watch path token con

let unwatch path token = Xsraw.unwatch path token con

let dom0 = 0

let introduce =
  let counter = ref 0 in
  fun () ->
    incr counter;
    let domid = !counter in
    Xsraw.introduce domid 0n 0 con;
    domid

let release domid = Xsraw.release domid con

let resume domid = Xsraw.resume domid con

let getdomainpath domid = Xsraw.getdomainpath domid con

let write tid path value = Xsraw.write tid path value con

let mkdir tid path = Xsraw.mkdir tid path con

let rm tid path = Xsraw.rm tid path con


let setperms tid path perms =
  (*let rest = List.map (fun (domid, perm) -> domid, rev_map_perm perm ) perms.rest in*)
  Xsraw.setperms tid path (perms.owner, rev_map_perm perms.others, []) con
