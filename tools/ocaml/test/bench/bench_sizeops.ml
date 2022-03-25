open Bechamel
open Xenbus

let sum_sizeop size k v =
  Sizeops.(size
           + Sizeops.of_int (String.length k)
           + Sizeops.of_int (String.length v))

let impl_sizeop = Sizeops.of_int 0, sum_sizeop

let half = max_int / 2

let add_opt a b =
  match a, b with
  | Some x, Some y when (x >= 0 && x < half && y >= 0 && y < half) -> Some (x+y)
  | _ -> None

let sum_intopt size k v =
  add_opt size @@ add_opt (Some (String.length k)) (Some (String.length v))

let impl_intopt = None, sum_intopt

let sum_int_unsafe size k v = size + String.length k + String.length v
let impl_int_unsafe = 0, sum_int_unsafe

(* based on a single VM's realistic quotas *)
let entries = 8192
let keymax = 1024
let valuemax = 2048

module StringMap = Map.Make(String)

let data =
  let key_template = String.make (keymax-20) 'k' in
  let value_template = String.make (valuemax-20) 'v' in
  Array.init entries @@ fun i ->
  Printf.sprintf "%020d%s" i key_template,
  Printf.sprintf "%020d%s" i value_template

let tree =
  Array.fold_left (fun map (k,v) ->
      StringMap.add k v map
  ) StringMap.empty data

let test_with (start, f) () =
  StringMap.fold (fun k v size -> f size k v) tree start

let test_reachable () =
  (* simulate querying Obj.reachable_words at each step,
     as if we'd check quota then *)
  StringMap.fold (fun _ _ _ -> Obj.reachable_words (Obj.repr tree)) tree 0

let all =
  Test.make_grouped ~name:"N elements incremental size update"
    [ Test.make ~name:"sizeops (private int)" (Staged.stage @@ test_with impl_sizeop)
    ; Test.make ~name:"int option" (Staged.stage @@ test_with impl_intopt)
    ; Test.make ~name:"int (unsafe, assuming no overflow)" (Staged.stage @@ test_with impl_int_unsafe)
    (*; Test.make ~name:"Obj.reachable_words)" (Staged.stage @@ test_reachable)*)
  ]
