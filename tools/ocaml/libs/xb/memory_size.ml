(* See MLI for comments on how this works and the complexity requirements *)

type size_kind =
  [ `constant
    | `immutable
    | `updatable
    | `ephemeral
  ]

type tracker =
  { mutable size: Sizeops.Size.t
    ; mutable parent: parent
    ; item_overhead: Sizeops.Size.t (* TODO: only for updatable! *)
    ; container_initial: Sizeops.Size.t (* TODO: only for updatable! *)
  }

and parent = Immutable | UpdatableUnset | UpdatableParent of tracker


type ephemeral = [`ephemeral]

type updatable = [`updatable | ephemeral ]

type immutable = [`immutable | updatable]

type constant = [`constant | immutable ]

type array_compatible = [`constant]

type require_nestable = [`constant | `immutable | `updatable]

type forbid_updates = [`constant | `immutable] (** to be used as [< forbid_updates] *)
type +'a t = tracker

let unboxed = Sizeops.Size.of_int 0 (* no extra space taken up beyond that for the field itself *)
let boxed = Sizeops.Size.of_words 1
let make size = { size; parent = Immutable; item_overhead = unboxed; container_initial = unboxed }

let record_field = Sizeops.Size.of_words 0

let boxed4 = Sizeops.Size.(boxed + boxed + boxed + boxed)

let tracker _ = make boxed4

let bool _ = make unboxed
let char _ = make unboxed
let int _ = make unboxed
let unit () = make unboxed

let float _ = make boxed
let func _ = make boxed
let int32 _ = make boxed
let int64 _ = make boxed
let nativeint _ = make boxed

let bytes b = b |> Bytes.length |> Sizeops.Size.of_bytes |> make
let string s = s |> String.length |> Sizeops.Size.of_bytes |> make

let set_parent t ~parent =
  match t.parent with
  | Immutable -> () (* no updates possible *)
  | UpdatableUnset ->
      t.parent <- UpdatableParent parent
  | UpdatableParent _ ->
      invalid_arg "expression already has a parent set"

let unset_parent t =
  match t.parent with
  | Immutable -> () (* no updates possible *)
  | UpdatableParent _ ->
      t.parent <- UpdatableUnset
  | UpdatableUnset ->
      invalid_arg "expression's parent already removed"

let add a b =
  let size = Sizeops.Size.(a.size + b.size) in
  match a.parent, b.parent with
  | Immutable, Immutable -> make size
  | _ ->
      let t = { size; parent = UpdatableUnset; item_overhead = unboxed; container_initial = unboxed } in
      set_parent a ~parent:t;
      set_parent b ~parent:t;
      t

let remove a b =
  let size = Sizeops.Size.(a.size - b.size) in
  match a.parent, b.parent with
  | Immutable, Immutable -> make size
  | _ ->
      let t = { size; parent = UpdatableUnset; item_overhead = unboxed; container_initial = unboxed } in
      set_parent a ~parent:t;
      set_parent b ~parent:t;
      t

let option size_of = function
  | None -> make boxed
  | Some e -> add (make boxed) @@ size_of e

let array size_of a =
  make @@ match Sizeops.Size.to_int_opt (size_of a.(0)).size with
  | None -> Sizeops.Size.invalid
  | Some e ->
      (* TODO: multiplication should be part of sizeops? *)
      let n = Array.length a in
      let res = n * e in
      if res < n || res < e then Sizeops.Size.invalid
      else Sizeops.Size.of_int res

type ('a, 'b) fields = 'b t

let record_start _t = { size = boxed; parent = UpdatableUnset; item_overhead = unboxed; container_initial = unboxed }

let record_add_immutable field t =
  set_parent field ~parent:t;
  t.size <- Sizeops.Size.(t.size + record_field + field.size);
  t

let record_add_mutable field t : (_, [> `ephemeral]) fields =
  (* no need to set up size updates, the results are ephemeral anyway *)
  t.size <- Sizeops.Size.(t.size + record_field + field.size);
  t

let record_end t = t

let container_create ~initial ~item_overhead =
  { size = initial.size; parent = UpdatableUnset; item_overhead = item_overhead
  ; container_initial = initial.size }

let rec container_add t n =
  t.size <- Sizeops.Size.(t.size + n);
  match t.parent with
  | UpdatableParent p -> container_add p n
  | UpdatableUnset | Immutable -> ()

let container_add_element t e =
  set_parent e ~parent:t;
  container_add t Sizeops.Size.(e.size + t.item_overhead)

let rec container_sub t n =
  t.size <- Sizeops.Size.(t.size - n);
  match t.parent with
  | UpdatableParent p -> container_sub p n
  | UpdatableUnset | Immutable -> ()

let container_clear t =
  container_sub t Sizeops.Size.(t.size - t.container_initial)

let container_remove_element t e =
  unset_parent e;
  container_sub t Sizeops.Size.(e.size + t.item_overhead)

let container_transfer ~src ~dst =
  let delta = Sizeops.Size.(src.size - src.container_initial ) in
  container_add dst delta;
  container_sub src delta

let size_of t = t.size

let size_of_bytes t =
  match Sizeops.Size.to_int_opt t.size with
  | None -> max_int
  | Some x -> x * Sys.word_size / 8 (* TODO: this could still overflow *)

let pp ppf t =
  match Sizeops.Size.to_int_opt t.size with
  | None -> Format.pp_print_string ppf "OVERFLOW"
  | Some x -> Format.pp_print_int ppf x

