open Xs_protocol

(* Conventions:
Aim for correctness, use simplest data structure that unambigously represents state.

E.g.:
* a list when duplicates are allowed, order matters and the empty list is a valid value
* a set when elements appearing multiple time have the same semantic meaning as them appearing once,
and the order is unspecified or sorted
* a map when a single key is mapped to a single value, and order is unspecified or sorted

When we must retain the original order for queries, but semantically it doesn't matter
then store both a canonical representation and the original order.

*)

let rec string_for_all_from s f pos =
  pos = String.length s || (f s.[pos] && (string_for_all_from s f @@ (pos + 1)))

type error = [`Msg of string]

module Path : sig
  (** a valid xenstore path *)
  type t

  val root : t

  val of_string : string -> t option
  (** [of_string path] parses [path].
      @return [None] if the path is syntactically not valid *)

  val to_string : t -> string
  (** [to_string path] converts path to string. *)

  (** [is_child parent child] returns true if [child] is a child of [parent].
      A path is considered to be a child of itself *)
  val is_child : t -> t -> bool
end = struct
  type t = string list

  let is_valid_char = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' | '/' | '_' | '@' ->
        true
    | _ ->
        false

  let root = [""]

  let nonempty s = String.length s > 0

  let of_string s =
    let n = String.length s in
    if
      n > 0 (* empty path is not permitted *)
      && n < 1024
      (* paths cannot exceed 1024 chars, FIXME: relative vs absolute *)
      && string_for_all_from s is_valid_char 0
    then
      match String.split_on_char '/' s with
      | [] ->
          assert false
      | [""; ""] ->
          Some root
      | _ :: tl as path ->
          if List.for_all nonempty tl then Some path else None
    else None

  let to_string = String.concat "/"

  let rec is_child p c =
    match (p, c) with
    | [], [] ->
        true (* a path is a child of itself *)
    | [], _ ->
        true
    | phd :: ptl, chd :: ctl when phd = chd ->
        is_child ptl ctl
    | _ ->
        false
end

module PathMap = Map.Make (String)
module DomidSet = Set.Make (Int)
module DomidMap = Map.Make (Int)

let preserve_order = ref true

module CanonicalACL = struct
  module RW = struct
    type t = {read: bool; write: bool}

    let of_perm = function
      | ACL.NONE ->
          {read= false; write= false}
      | ACL.WRITE ->
          {read= false; write= true}
      | ACL.READ ->
          {read= true; write= false}
      | ACL.RDWR ->
          {read= true; write= true}

    let to_perm = function
      | {read= false; write= false} ->
          ACL.NONE
      | {read= false; write= true} ->
          ACL.WRITE
      | {read= true; write= false} ->
          ACL.READ
      | {read= true; write= true} ->
          ACL.RDWR

    let full = {read= true; write= true}
  end

  module RWMap = struct
    type t = {fallback: RW.t; map: RW.t DomidMap.t}

    let lookup t domid =
      (* other=RDWR can be overriden by explicitly revoking
         permissions for a domain, so a read=false,write=false
         in the DomidMap is not necessarily redundant
      *)
      DomidMap.find_opt domid t.map |> Option.value ~default:t.fallback

    let create fallback owner =
      (* owner always has full permissions, and cannot be overriden *)
      {fallback; map= DomidMap.singleton owner RW.full}

    let override t (domid, perm) =
      let rw = RW.of_perm perm in
      (* first entry wins, see perms.ml, also entries that are same as the fallback are
         not necessarily redundant: (b1,b2,r2) means that domid 2 has rdwr,
         but if we remove the seemingly redundant `b2` entry then the override would make it
         just read which would be wrong. *)
      if DomidMap.mem domid t.map then t
      else {t with map= DomidMap.add domid rw t.map}
  end

  type t = {original: ACL.t; owner: ACL.domid; acl: RWMap.t}

  let can_read t domid = (RWMap.lookup t.acl domid).read

  let can_write t domid = (RWMap.lookup t.acl domid).write

  let owner t = t.owner

  let of_acl original =
    let fallback = RW.of_perm original.ACL.other in
    let owner = original.ACL.owner in
    let acl =
      let init = RWMap.create fallback owner in
      List.fold_left RWMap.override init original.ACL.acl
    in
    {original; owner; acl}

  let to_acl t =
    if !preserve_order then t.original
    else
      ACL.
        { owner= t.owner
        ; other= RW.to_perm t.acl.fallback
        ; acl= t.acl.map |> DomidMap.map RW.to_perm |> DomidMap.bindings }
end

module Store = struct
  type node = {value: string; children: string list; acl: CanonicalACL.t}

  type t = {paths: node PathMap.t}

  let create () = {paths= PathMap.empty}

  let parent x = failwith "TODO"

  let add t key value =
    let paths = PathMap.add key value t.paths in
    {paths}

  let remove t key =
    let paths = PathMap.remove key t.paths in
    {paths}
end

type t = Store.t

let create () = Store.create ()

let reply_enoent = Response.Error "ENOENT"

let reply_eexist = Response.Error "EEXIST"

let with_node_read t path f =
  ( t
  , match PathMap.find_opt path t.paths with
    | None ->
        reply_enoent
    | Some n ->
        f n )

(* TODO: perm check *)
let perform_path t domid path = function
  | Request.Read ->
      with_node_read t path @@ fun n -> Response.Read n.value
  | Request.Directory ->
      with_node_read t path @@ fun n -> Response.Directory n.children
  | Request.Directory_part _ ->
      (t, Response.Error "ENOTSUP")
  | Request.Getperms ->
      with_node_read t path @@ fun n -> Response.Getperms n.acl
  | Request.Write value -> (
    (* TODO: implicit mkdir *)
    match PathMap.find_opt path t.paths with
    | Some _ ->
        (t, reply_eexist)
    | None ->
        let acl = ACL.{owner= domid; other= NONE; acl= []} in
        let n = {value; children= []; acl} in
        ({t with paths= PathMap.add path n t.paths}, Response.Write) )
  | Request.Setperms acl -> (
    match PathMap.find_opt path t.paths with
    | Some _ ->
        (t, reply_enoent)
    | None ->
        let update_node = function
          | None ->
              None
          | Some n ->
              Some {n with acl}
        in
        ( {t with paths= PathMap.update path update_node t.paths}
        , Response.Setperms ) )
  | Request.Mkdir -> (
    (* TODO: implicit mkdir *)
    match PathMap.find_opt path t.paths with
    | Some _ ->
        (t, reply_eexist)
    | None ->
        let acl = ACL.{owner= domid; other= NONE; acl= []} in
        let n = {value= ""; children= []; acl} in
        ({t with paths= PathMap.add path n t.paths}, Response.Mkdir) )
  | Request.Rm -> (
    match PathMap.find_opt path t.paths with
    | None ->
        (t, reply_enoent)
    | Some _ ->
        ({t with paths= PathMap.remove path t.paths}, Response.Rm) )

let perform t domid = function
  | Request.PathOp (path, op) ->
      perform_path t domid path op
  | Request.Getdomainpath domid ->
      (t, Response.Getdomainpath (Printf.sprintf "/local/domain/%d" domid))
  | _ ->
      failwith "TODO"
