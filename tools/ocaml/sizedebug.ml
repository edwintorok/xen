type 'a t =
  { pp: 'a Fmt.t
  ; size_of: 'a -> Xenbus.Size_tracker.t
  ; exact: bool
  ; name: string
  }

type 'a case = 'a t

let real_size_bytes v = Obj.reachable_words (Obj.repr v) * Sys.word_size / 8

let check_exn t v =
  let real_bytes = real_size_bytes v in
  let computed_bytes = t.size_of v |> Xenbus.Size_tracker.to_byte_count in
  if real_bytes > computed_bytes || (t.exact && real_bytes <> computed_bytes) then
    Fmt.failwith "Size computation incorrect for %s: %a"
    t.name t.pp v

let pp_size_of size_of =
  let conv v =size_of v |> Xenbus.Size_tracker.to_byte_count in
  Fmt.(using conv int)

let v ?(exact=false) size_of name =
  let pp ppf v =
    let real_bytes = real_size_bytes v in
    Fmt.pf ppf "%s(%a calc, %d real)" name (pp_size_of size_of) v real_bytes
  in
  { size_of; exact; name; pp }

let same a b =
  let size_of v =
    let a_size = a.size_of v in
    let b_size = b.size_of v in
    check_exn a v;
    check_exn b v;
    if a.exact then a_size else b_size
  in
  let dup x = x, x in
  let pp = Fmt.(braces @@ using dup @@ Dump.pair a.pp b.pp)
  in
  let exact = a.exact && b.exact in
  { size_of; exact; name = a.name; pp}

type 'a field = Field of 'a t

let field name el get =
  let pp = Fmt.Dump.field name get el.pp in
  let size_of v =
    let open Xenbus.Size_tracker in
    add record_field @@ el.size_of (get v)
  in
  Field { size_of; exact=el.exact; name; pp }

let pp_of (Field t) = t.pp
let is_exact (Field t) = t.exact

let record name fields =
  let pp = Fmt.Dump.record (List.map pp_of fields) in
  let size_of r =
    fields |> List.to_seq |> Seq.map (fun (Field field) -> field.size_of r)
    |> Seq.fold_left Xenbus.Size_tracker.add Xenbus.Size_tracker.empty
  in
  { size_of; pp; name; exact = List.for_all is_exact fields }

open Xenbus.Size_tracker
let primitive name size = v ~exact:true (fun _ -> size) name
let bool = primitive "bool" empty
let int = primitive "int" empty
let int32 = primitive "int32" record_field
let int64 = primitive "int64" record_field
let nativeint=  primitive "nativeint" record_field
let float=  primitive "float" record_field
let string = v ~exact:true string "string"
let bytes = v ~exact:true bytes "bytes"

let seq to_seq t name =
  let size_of s =
    s |> to_seq |> Seq.map (fun v -> t.size_of v)
    |> Seq.fold_left add empty
  in
  let pp = Fmt.using to_seq Fmt.Dump.(seq t.pp)
  in
  { size_of; pp; name; exact = t.exact }


let pair a b =
  let pp = Fmt.Dump.pair a.pp b.pp in
  let size_of (l, r) =
    add (a.size_of l) (b.size_of r)
  in
  { pp; size_of ; name = "pair"; exact = a.exact && b.exact }

let pair_seq pp_k pp_v to_pair_seq =
  seq to_pair_seq @@ pair pp_k pp_v

let case name t is =
  let size_of v =
    match is v with
    | None -> empty
    | Some e -> add record_field (t.size_of e)
  in
  let pp =
    Fmt.using is @@ Fmt.option Fmt.(const string name ++ parens t.pp)
  in
  { pp; size_of; name; exact = t.exact }

let variant cases name =
  let size_of v =
    cases |> List.map (fun case -> case.size_of v)
    |> List.fold_left add empty
  in
  let pp ppf v =
    List.iter (fun case -> case.pp ppf v) cases
  in
  { pp; size_of; exact = List.for_all (fun t -> t.exact) cases; name}

