module M = Map.Make(String)
type 'a t = { data: 'a; children: 'a t M.t }

type 'a tree = 'a t
let of_data data = { data; children = M.empty }

let update key f t = { t with children = M.update key f t.children }
let set t data = { t with data }

module Cursor = struct
  type 'a t = { tree: 'a tree; up: ('a t * M.key) option }

  let of_tree tree = { tree; up = None }

  let create parent key tree = { tree; up = Some (parent, key) }

  let down cur k =
    M.find_opt k cur.tree.children |> Option.map @@ create cur k

  let down_implicit_create ~implicit cur k =
    match down cur k with
    | Some r -> r
    | None -> cur.tree.data |> implicit |> of_data |> create cur k

  let rec to_tree t = match t.up with
    | None -> t.tree
    | Some (parent, key) ->
        to_tree { parent with tree = update key (fun _ -> Some t.tree) parent.tree }

  let set cur data = { cur with tree = set cur.tree data }
  let get cur = cur.tree.data

  let rm_child cur key = { cur with tree = update key (fun _ -> None) cur.tree}

  (* TODO: down with implicit create *)
end



let rec map f t = { data = f t.data; children = M.map (map f) t.children }
