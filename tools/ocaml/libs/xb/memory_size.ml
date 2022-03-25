(* See MLI for comments on how this works and the complexity requirements *)

type size_kind =
  [ `constant
    | `immutable
    | `updatable
    | `ephemeral
  ]

module rec T : sig
  type t =
    { mutable size: Sizeops.t
    ; mutable parent: parent
    ; item_overhead: Sizeops.t (* TODO: only for updatable! *)
    ; container_initial: Sizeops.t (* TODO: only for updatable! *)
    }

  and parent = Immutable | UpdatableUnset | UpdatableParent of TW.t

  val equal : t -> t -> bool

  val hash: t -> int
end = struct
  type t =
    { mutable size: Sizeops.t
    ; mutable parent: parent
    ; item_overhead: Sizeops.t (* TODO: only for updatable! *)
    ; container_initial: Sizeops.t (* TODO: only for updatable! *)
    }

  and parent = Immutable | UpdatableUnset | UpdatableParent of TW.t

  let equal a b = a == b
  (* we don't want to send updates to the same physically equal parent more than once,
     but other than that each parent is independent from each other and needs updates sent *)

  let hash t = Hashtbl.hash (t.item_overhead, t.container_initial)
end

and TW : Weak.S with type data = T.t = Weak.Make(T)

open T

type array_compatible = [`constant]

type require_nestable = [`constant | `immutable | `updatable]

type forbid_updates = [`constant | `immutable] (** to be used as [< forbid_updates] *)

type +'a t = T.t

let unboxed = Sizeops.of_int 0 (* no extra space taken up beyond that for the field itself *)
let boxed = Sizeops.of_words 1
let make size = { size; parent = Immutable; item_overhead = unboxed; container_initial = unboxed }

let record_field = Sizeops.of_words 0

let boxed4 = Sizeops.(boxed + boxed + boxed + boxed)

let tracker _ = make boxed4

let bool _ = make unboxed
let char _ = make unboxed
let int _ = make unboxed
let size_t _ = make unboxed
let unit () = make unboxed

let float _ = make boxed

let func _ = make boxed
let int32 _ = make boxed
let int64 _ = make boxed
let nativeint _ = make boxed

let bytes_n n = n |> Sizeops.of_bytes |> make
let bytes b = b |> Bytes.length |> bytes_n
let string s = s |> String.length |> bytes_n

let set_parent t ~parent =
  match t.parent with
  | Immutable -> false (* no updates possible *)
  | UpdatableUnset ->
      let tw = TW.create 1 in
      TW.add tw parent;
      t.parent <- UpdatableParent tw;
      true
  | UpdatableParent tw ->
      TW.add tw parent;
      true

let unset_parent t ~parent =
  match t.parent with
  | Immutable -> () (* no updates possible *)
  | UpdatableParent tw ->
      TW.remove tw parent;
      if TW.count tw = 0 then
        t.parent <- UpdatableUnset
  | UpdatableUnset ->
      ()
      (* invalid_arg "expression's parent already removed"*)

let add a b =
  let size = Sizeops.(a.size + b.size) in
  match a.parent, b.parent with
  | Immutable, Immutable -> make size
  | _ ->
      let t = { size; parent = Immutable; item_overhead = unboxed; container_initial = unboxed } in
      let updatable_a = set_parent a ~parent:t in
      let updatable_b = set_parent b ~parent:t in
      if updatable_a || updatable_b then
        { t with parent = UpdatableUnset }
      else t

let remove a b =
  let size = Sizeops.(a.size - b.size) in
  match a.parent, b.parent with
  | Immutable, Immutable -> make size
  | _ ->
      let t = { size; parent = Immutable; item_overhead = unboxed; container_initial = unboxed } in
      let updatable_a = set_parent a ~parent:t in
      let updatable_b = set_parent b ~parent:t in
      if updatable_a || updatable_b then
        { t with parent = UpdatableUnset }
      else t

let variant f x = add (make boxed) (f x)

let option size_of = variant @@ function
  | None -> unit ()
  | Some e -> size_of e

let array size_of a =
  if Array.length a = 0 then
    make boxed
  else
  make @@ match Sizeops.to_int_opt (size_of a.(0)).size with
  | None -> Sizeops.invalid
  | Some e ->
      (* TODO: multiplication should be part of sizeops? *)
      let n = Array.length a in
      let res = n * e in
      if res < n || res < e then Sizeops.Size.invalid
      else Sizeops.Size.of_int res

type ('a, 'b) fields = 'b t

(* important to keep records as immutable, because we may have constant or immutable sized
   record size expression as globals, and we want to use that immutable expression in multiple
   places.
   But if we set this to UpdatableUnset from the beginning then even immutable record sizes will
   have the restriction that we can only use them in a single expression (because they may change
   size then) *)
let record_start _t = { size = boxed; parent = Immutable; item_overhead = unboxed; container_initial = unboxed }

let record_add_immutable field t =
  if set_parent field ~parent:t && t.parent = Immutable then
    t.parent <- UpdatableUnset;
  t.size <- Sizeops.Size.(t.size + record_field + field.size);
  t

let record_add_mutable_const field t = record_add_immutable field t

let record_add_mutable field t : (_, [> `ephemeral]) fields =
  (* no need to set up size updates, the results are ephemeral anyway *)
  t.size <- Sizeops.Size.(t.size + record_field + field.size);
  t

let record_end t = t

let container_create ~initial ~item_overhead =
  { size = initial.size; parent = UpdatableUnset; item_overhead = item_overhead.size
  ; container_initial = initial.size }

let rec container_add t n =
  t.size <- Sizeops.Size.(t.size + n);
  match t.parent with
  | UpdatableParent tw ->
      tw |> TW.iter @@ fun p -> container_add p n
  | UpdatableUnset | Immutable -> ()

let rec container_sub t n =
  t.size <- Sizeops.Size.(t.size - n);
  match t.parent with
  | UpdatableParent tw ->
      tw |> TW.iter @@ fun p -> container_sub p n
  | UpdatableUnset | Immutable -> ()

let container_clear t =
  container_sub t Sizeops.Size.(t.size - t.container_initial)

let container_remove_element e t =
  unset_parent e ~parent:t;
  container_sub t Sizeops.Size.(e.size + t.item_overhead)

let container_transfer ~src ~dst =
  let delta = Sizeops.Size.(src.size - src.container_initial ) in
  container_add dst delta;
  container_clear src

let size_of t = t.size

let size_of_bytes t =
  match Sizeops.Size.to_int_opt t.size with
  | None -> max_int
  | Some x -> x * Sys.word_size / 8 (* TODO: this could still overflow *)

let pp ppf t =
  match Sizeops.Size.to_int_opt t.size with
  | None -> Format.pp_print_string ppf "OVERFLOW"
  | Some x -> Format.pp_print_int ppf x

let pp_parent ppf p =
  Format.pp_print_string ppf @@ match p with
  | Immutable -> "immutable"
  | UpdatableUnset -> "updatable"
  | UpdatableParent _ -> "updatable(parent)"

let pp_dump ppf t =
  Format.fprintf ppf "(size: %a, parents: %a)" pp t pp_parent t.parent

let container_add_element e t =
  let (_:bool) = set_parent e ~parent:t in
  container_add t Sizeops.Size.(e.size + t.item_overhead)

