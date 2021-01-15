open QCheck
open Store

type tree = Leaf | Nodes of (string * tree) list

let nodes children = Nodes children
let gen_tree = QCheck.Gen.(sized @@ fix
  (fun self n ->
    let children = frequency [1, pure 0; 2, int_bound n] >>= fun m ->
    match m with
    | 0 -> pure []
    | _ -> list_repeat m (pair string (self (n/m)))
    in
    frequency
     [ 1, pure Leaf
     ; 2, map nodes children
    ]
    ))

let rec paths_of_tree (acc, path) = function
| Leaf -> acc
| Nodes l ->
  List.fold_left (fun acc (k, children) ->
    let path = k :: path in
    paths_of_tree (Store.Path.to_string (List.rev path) :: acc, path) children
  ) acc l

let gen_paths_choices =
  Gen.map (fun tree ->
  tree |> paths_of_tree ([], []) |> Array.of_list
  ) gen_tree

(*let arb_name = Gen.small_string

let arb_permty = let open Perms in oneofl [ READ; WRITE; RDWR; NONE ]

let arb_domid = oneofl [ 0; 1; 0x7FEF]

let arb_perms =
   map (fun (domid, other, acls) -> Perms.Node.create domid other acls)
   ~rev:(fun n -> Perms.Node.get_owner n, Perms.Node.get_other n, Perms.Node.get_acl n)
   @@ triple arb_domid arb_permty (small_list (pair arb_domid arb_permty))*)

let arb_name = Gen.small_string
let arb_value = Gen.small_string

let node_of name value children =
  List.fold_left (fun c acc -> Node.add_child acc c)
  (Node.create name Perms.Node.default0 value ) children

let g = QCheck.Gen.(sized @@ fix
  (fun self n ->
      frequency [1, pure 0; 2, int_bound n] >>= fun m ->
      let children = match m with
      | 0 -> pure []
      | _ -> list_repeat m (self (n/m))
      in
      map3 node_of arb_name arb_value children
    ))

let paths_of_tree t =
  let paths = ref [] in
  Store.traversal t (fun path node ->
    paths := (Store.Path.of_path_and_name path (Node.get_name node) |> Store.Path.to_string) :: !paths
  );
  !paths
