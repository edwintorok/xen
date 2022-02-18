open Xenstore
type transaction = int

let none = 0

let con = failwith "TODO"
let transaction_start () = Xsraw.transaction_start con

let transaction_end tid commit = Xsraw.transaction_end tid commit con

type path = string

let path () = failwith "TODO"

type perm = bool * bool

let directory tid path = Xsraw.directory tid path con

let read tid path = Xsraw.read tid path con

type domid = int

type perms =
  { owner: domid
  ; others: perm
  }

let map_perm = function
  | Xsraw.PERM_NONE -> false, false
  | Xsraw.PERM_READ -> true, false
  | Xsraw.PERM_WRITE -> false, true
  | Xsraw.PERM_RDWR -> true, true

let rev_map_perm = function
  | false, false -> Xsraw.PERM_NONE
  | true, false -> Xsraw.PERM_READ
  | false, true -> Xsraw.PERM_WRITE
  | true, true -> Xsraw.PERM_RDWR

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

let string = "foo"
