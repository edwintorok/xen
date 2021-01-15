module type S = sig
  type cmd

  type state

  val init_state : state

  val next_state : cmd -> state -> state

  val precond : cmd -> state -> bool
end

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

module Pickable (K : sig
  include Map.OrderedType

  include Hashtbl.HashedType with type t := t
end) =
struct
  (* allow picking a random value from a changing map keys.
     Store a random value (hash of key) as first element of key,
     then use find_first to pick an item related to the random element if any.
     This should be more efficient than converting to a list and using List.nth to pick
  *)
  module Key = struct
    type t = int * K.t

    let of_key k = (K.hash k, k)

    let compare (h, k) (h', k') =
      match Int.compare h h' with 0 -> K.compare k k' | r -> r
  end

  module M = Map.Make (Key)

  type 'a t = 'a M.t

  let empty = M.empty

  let singleton k v = M.singleton (Key.of_key k) v

  let add k v m = M.add (Key.of_key k) v m

  let find_opt k m = M.find_opt (Key.of_key k) m

  let mem k m = M.mem (Key.of_key k) m

  let remove k m = M.remove (Key.of_key k) m

  let merge f m m' = M.merge f m m'

  let is_empty = M.is_empty

  let update k f m = M.update (Key.of_key k) f m

  let choose rnd m =
    (* function needs to be monotonic, so the hash has to be part of the key *)
    let gte (keyhash, _) = Int.compare keyhash rnd >= 0 in
    match M.find_first_opt gte m with
    | Some ((_, k), _) ->
        k
    | None ->
        snd @@ fst @@ M.min_binding m
end

module PickablePath = Pickable (struct
  type t = string

  let hash = Hashtbl.hash

  let compare = String.compare

  let equal = String.equal
end)

module PickableInt = Pickable (struct
  include Int

  let hash = Hashtbl.hash
end)

module PathObserver = struct
  type state =
    { seen: unit PickablePath.t
    ; dom_txs: unit PickableInt.t PickableInt.t
    ; next_tid: int }

  let choose_path t rnd = PickablePath.choose rnd t.seen

  let choose_domid t rnd = PickableInt.choose rnd t.dom_txs

  let choose_txid_opt t domid rnd =
    match PickableInt.find_opt domid t.dom_txs with
    | None ->
        0
    | Some txs ->
        if PickableInt.is_empty txs then 0 else PickableInt.choose rnd txs

  let new_domid domid = PickableInt.singleton domid PickableInt.empty

  let both _ _ _ = Some ()

  let merge_txs _ s s' =
    let s = Option.value ~default:PickableInt.empty s in
    let s' = Option.value ~default:PickableInt.empty s' in
    Some (PickableInt.merge both s s')

  let init_state =
    {seen= PickablePath.singleton "/" (); dom_txs= new_domid 0; next_tid= 1}

  let with_path path t = {t with seen= PickablePath.add path () t.seen}

  let split0 str =
    match Process.split (Some 2) '\000' str with
    | [x; y] ->
        (x, y)
    | _ ->
        invalid_arg str

  let next_state (domid, cmd) t =
    let open Xenbus.Xb in
    match cmd with
    | {Xenbus.Packet.ty= Transaction_start; _} ->
        let update = function
          | None ->
              None
          | Some txs ->
              Some (PickableInt.add t.next_tid () txs)
        in
        { t with
          dom_txs= PickableInt.update domid update t.dom_txs
        ; next_tid= t.next_tid + 1 }
    | { Xenbus.Packet.ty=
          Op.(
            ( Rm
            | Read
            | Directory
            | Getperms
            | Setperms
            | Unwatch
            | Reset_watches
            | Getdomainpath
            | Isintroduced
            | Set_target
            | Debug ))
      ; _ } ->
        t
    | {Xenbus.Packet.ty= Op.(Watchevent | Error | Resume | Invalid); _} ->
        assert false
    | {Xenbus.Packet.ty= Op.Transaction_end; tid; _} ->
        let update = function
          | None ->
              None
          | Some txs ->
              Some (PickableInt.remove tid txs)
        in
        {t with dom_txs= PickableInt.update domid update t.dom_txs}
    | {Xenbus.Packet.ty= Op.(Write | Mkdir | Watch); data} ->
        let path, _ = split0 data in
        with_path path t
    | {Xenbus.Packet.ty= Introduce; data} ->
        let domidstr, _ = split0 data in
        let domid' = int_of_string domidstr in
        if domid = 0 then
          { t with
            dom_txs= PickableInt.merge merge_txs t.dom_txs (new_domid domid') }
        else t
    | {Xenbus.Packet.ty= Release; data} ->
        let domidstr, _ = split0 data in
        let domid = int_of_string domidstr in
        {t with dom_txs= PickableInt.remove domid t.dom_txs}

  let precond (domid, cmd) t =
    ( match PickableInt.find_opt domid t.dom_txs with
    | None ->
        false
    | Some txs ->
        let tid = cmd.Xenbus.Packet.tid in
        tid = 0 || PickableInt.mem tid txs )
    && Testable.Command.precond cmd t

  let pp =
    let open Fmt in
    Dump.record
      [ Dump.field "domid" fst Fmt.int
      ; Dump.field "cmd" snd Testable.Command.pp_dump ]
end
