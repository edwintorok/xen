(* See MLI for comments on how this works and the complexity requirements *)

type top

type 'a printable = 'a -> (Format.formatter -> 'a -> unit)

type 'a t =
  { mutable size: Sizeops.Size.t
  ; idstr: string
  ; nesting: 'a nesting
  }

and _ nesting =
  | Top: top nesting
  | Nested: 'a nested -> 'a nested nesting

and 'a nested =
  { item_size_of: 'a -> Sizeops.Size.t
  ; mutable parent: parent
  }

and parent =
  | Unset: parent
  | Parent: _ t -> parent (* hide type parameter *)

type id = top t

let make idstr nesting = { size = Sizeops.Size.of_words 0; idstr; nesting }

let create id pp_id =
  (* top-level owners are long-lived, it is worth formatting the id now *)
  pp_id Format.str_formatter id;
  let idstr = Format.flush_str_formatter () in
  make idstr Top

let id x = x

let pp_size ppf size =
  match Sizeops.Size.to_int_opt size with
  | None -> Format.pp_print_string ppf "OVERFLOW"
  | Some x -> Format.pp_print_int ppf x

let pp ppf t =
  Format.fprintf ppf "%s: %a" t.idstr pp_size t.size

let rec update_size_add: 'a. 'a t -> Sizeops.Size.t -> unit = fun (type a) (t:a t) delta ->
  t.size <- Sizeops.Size.(t.size + delta);
  match t.nesting with
  | Top -> ()
  | Nested { parent = Unset } -> ()
  | Nested { parent = Parent parent } -> update_size_add parent delta

let rec update_size_sub: 'a. 'a t -> Sizeops.Size.t -> unit = fun (type a) (t:a t) delta ->
  (* we cannot use + here with -n
     because that'd fail due to the underflow detection in
     Sizeops: all sizes must be >= 0 *)
  t.size <- Sizeops.Size.(t.size - delta);
  match t.nesting with
  | Top -> ()
  | Nested { parent = Unset } -> ()
  | Nested { parent = Parent parent } -> update_size_add parent delta

let set_parent_exn (type a) (t: a t) ~parent =
  match t.nesting with
  | Top -> invalid_arg "Top-level owners cannot have a parent: it'd create a loop"
  | Nested n -> n.parent <- parent

let add_nested t nested =
  set_parent_exn nested ~parent:(Parent t);
  update_size_add t nested.size

let remove_nested t nested =
  set_parent_exn nested ~parent:Unset;
  update_size_sub t nested.size

module TrackedSize = struct
  (* TODO: could use GADT variants *)
  type 'a t =
    | Constant: Sizeops.Size.t -> ([> `constant]) t
    | Ephemeral: Sizeops.Size.t ref -> [> `ephemeral ] t
   (*constraint 'a = [<`constant | `ephemeral ]*)

  type ('a, 'b) immutable_size_of = 'a -> ([> `constant] as 'b) t

  open Sizeops
  let constant x = Constant x
  let string s = constant @@ Size.of_bytes (String.length s)
  let bytes s = constant @@ Size.of_bytes (Bytes.length s)
  let value = Size.of_int 0
  let int (_:int) = constant @@ value
  let bool (_:bool) = constant @@ value
  let option (size_of_element: 'a -> [<`constant] t) = function
    | None -> value
    | Some x ->
        match size_of_element x with
        | Constant s -> Size.(value + s)

  (* TODO: ensure updates are tracked *)
  let add (type a) (a:a t) (b:a t) =
    match a, b with
    | Constant c, Constant d -> Constant Size.(c + d)
    | Ephemeral x, Constant y -> Ephemeral (ref Size.(!x + y))
    | Constant y, Ephemeral x -> Ephemeral (ref Size.(!x + y))
    | Ephemeral x, Ephemeral y -> Ephemeral (ref Size.(!x + !y))

  let size_of t = t.size

  let pp ppf t = pp_size ppf t.size
end

let size_of t = TrackedSize.Ephemeral (ref t.size)

let owner_size_of_bytes t =
  match Sizeops.Size.to_int_opt t.size with
  | None -> max_int
  | Some x -> x * Sys.word_size / 8 (* TODO: this could still overflow *)

module Container = struct
  type nonrec 'a t = 'a nested t

  let create _id _pp_id item_size_of =
    (* in non-debug mode here, ignore pp arguments *)
    let nesting = { item_size_of; parent = Unset } in
    make "container" (Nested nesting)

  let add_immutable_item ?owner t item =
    (* TODO: explicit owner support *)
    match t.nesting with
    | Nested { item_size_of; _ } ->
        let size = item_size_of item in
        update_size_add t size

  let remove_immutable_item ?owner t item =
    (* TODO: explicit owner support *)
    match t.nesting with
    | Nested { item_size_of; _ } ->
      let size = item_size_of item in
      update_size_sub t size
end
