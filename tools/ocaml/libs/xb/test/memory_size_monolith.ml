open Xenbus
open Monolith

type 'a updatable_rec =
  { s: string
  ; q: 'a
  }

type 'a updatable_nested_rec =
  { s: string
  ; h: 'a
  }

module C = struct
  include Memory_size_ds
end

module R = struct
  let container_add ~item_overhead el_size_of x acc =
    let open Memory_size in
    add item_overhead @@ add acc @@ el_size_of x

  module Buffer = Buffer
  module Queue = struct
    include Queue
    let create_sized _ = create ()

    let size_of_fold fold el_size_of t =
      fold (fun acc el ->
        container_add ~item_overhead:C.Queue.item_overhead el_size_of el acc) C.Queue.initial t

    let size_of el_size_of t = size_of_fold Queue.fold el_size_of t

  end

  module Hashtbl = struct
    include Hashtbl
    let create_sized _ _ n = create n
    let size_of_fold fold initial key_size_of value_size_of t =
      fold (fun k v acc ->
        acc
        |> container_add ~item_overhead:C.Hashtbl.item_overhead key_size_of k
        |> container_add ~item_overhead:(Memory_size.int 0) value_size_of v) t @@ initial t

    let size_of k v t = size_of_fold Hashtbl.fold C.Hashtbl.initial k v t

  end

  module SizedList = struct
    include List

    let empty _ = []
    let rev_map _ f t = rev_map f t

    let size_of el_size_of t =
      List.fold_left (fun acc e ->
        container_add ~item_overhead:C.SizedList.item_overhead el_size_of e acc) C.SizedList.initial t
  end
end

let str = deconstructible Print.string

(* don't waste many random bits on chars, their contents doesn't matter for buffers, just their count *)
let buf_gen_char () = Gen.closed_interval (Char.code 'a') (Char.code 'b') () |> Char.chr
let buf_char = easily_constructible buf_gen_char Print.char

let buf_gen_string = Gen.string (Gen.closed_interval 0 10) buf_gen_char
let buf_string =
  let neg = easily_constructible buf_gen_string Print.string in
  ifpol neg str

let small_int = easily_constructible (Gen.closed_interval 0 100) Print.int

type size_t = Memory_size.size_kind Memory_size.t
type size_spec = (size_t, size_t) spec


let size_of_bytes = (Memory_size.size_of_bytes :> size_t -> int)

let wrap_size_of f x =
  Memory_size.size_of_bytes @@ f x

let declare_size_of name typ refs candidate =
  declare (name ^ ".size_of") (typ ^> int) (wrap_size_of refs) (wrap_size_of candidate)

let zero _ = 0

let size_t = deconstructible @@ fun s -> s |> Sizeops.Size.to_int_opt |> Print.option Print.int

let declare_size_of_reachable name typ candidate =
  let check_size t actual_bytes =
    let expected_bytes = Obj.reachable_words (Obj.repr t) * Sys.word_size / 8 in
    if expected_bytes <= actual_bytes then
      Valid actual_bytes
    else
      Invalid (fun d ->
        let open PPrint in
        Print.assert_ (Print.int expected_bytes ^^ PPrint.arbitrary_string " <= " ^^ d) ^^
        Print.candidate_finds @@ Print.int actual_bytes)
  in
  declare (name ^ ".size_of") (typ ^?> int) check_size (wrap_size_of candidate)

let declare_sized_type ~var ref_size_of candidate_size_of =
  let call_size_of = PPrint.arbitrary_string (var ^ ".size_of") in
  let check ref =
    let reference = wrap_size_of ref_size_of ref in
    let code =
      let open PPrint in
      Print.parens @@
      separate space
      [ arbitrary_string "fun t -> "
      ; Print.assert_ @@
        separate space
        [ Print.apply call_size_of [ arbitrary_string "t" ]
        ; arbitrary_string "="
        ; Print.int reference
        ]
      ]
    in
    (fun candidate ->
      let candidate = wrap_size_of candidate_size_of candidate in
      if reference = candidate then ()
      else begin
        let code = Print.candidate_finds @@ Print.int candidate in
        Monolith.dprintf "(* check: %a *)\n" PPrint.ToBuffer.compact code;
      end;
      assert (reference = candidate)
    ), document code
  in
  declare_abstract_type ~check ~var ()

let () =
  let buffer = declare_abstract_type ~var:"buffer" () in
  declare "Buffer.create" (small_int ^> buffer) R.Buffer.create C.Buffer.create;
  declare "Buffer.length" (buffer ^> int) R.Buffer.length C.Buffer.length;
  declare "Buffer.add_char" (buffer ^> buf_char ^> unit) R.Buffer.add_char C.Buffer.add_char;
  declare "Buffer.add_substring" (buffer ^> buf_string ^> small_int ^> small_int ^!> unit) R.Buffer.add_substring C.Buffer.add_substring;
  declare "Buffer.reset" (buffer ^> unit) R.Buffer.reset C.Buffer.reset;
  declare "Buffer.contents" (buffer ^> str) R.Buffer.contents C.Buffer.contents;
  declare_size_of_reachable "Buffer" buffer C.Buffer.size_of

let queue_tests el el_size_of =
  let queue = declare_sized_type ~var:"queue" (R.Queue.size_of el_size_of) C.Queue.size_of in
  (* TODO: code printed here is not exactly right, because we've done the currying ourselves *)
  declare "Queue.create_sized" (unit ^> queue)
    (fun () -> R.Queue.create_sized el_size_of)
    (fun () -> C.Queue.create_sized el_size_of);
  declare "Queue.add" (el ^> queue ^> unit) R.Queue.add C.Queue.add;
  declare "Queue.push" (el ^> queue ^> unit) R.Queue.push C.Queue.push;
  declare "Queue.take" (queue ^!> el) R.Queue.take C.Queue.take;
  declare "Queue.pop" (queue ^!> el) R.Queue.pop C.Queue.pop;
  declare "Queue.peek" (queue ^!> el) R.Queue.peek C.Queue.peek;
  declare "Queue.top" (queue ^!> el) R.Queue.top C.Queue.top;
  declare "Queue.clear" (queue ^> unit) R.Queue.clear C.Queue.clear;
  declare "Queue.is_empty" (queue ^> bool) R.Queue.is_empty C.Queue.is_empty;
  declare "Queue.length" (queue ^> int) R.Queue.length C.Queue.length;
  declare "Queue.transfer" (queue ^> queue ^> unit) R.Queue.transfer C.Queue.transfer;
  queue

let fixed_item =
  let neg =
    easily_constructible (Gen.choose ["foo"; "bar2"; "foobar"]) Print.string
  in
  ifpol neg str

let declare_spec d = let (_: (_,_) spec) = d in ()

let () = declare_spec @@ queue_tests fixed_item Memory_size.string

let hashtbl_tests el el_size_of_ref el_size_of_cand v v_size_of_ref v_size_of_cand =
  let hashtbl = declare_sized_type ~var:"hashtbl" (R.Hashtbl.size_of el_size_of_ref v_size_of_ref) C.Hashtbl.size_of in
  declare "Hashtbl.create_sized" (small_int ^> hashtbl)
    (R.Hashtbl.create_sized el_size_of_ref v_size_of_ref)
    (C.Hashtbl.create_sized el_size_of_cand v_size_of_cand);
  declare "Hashtbl.reset" (hashtbl ^> unit) R.Hashtbl.reset C.Hashtbl.reset;
  declare "Hashtbl.remove" (hashtbl ^> el ^> unit) R.Hashtbl.remove C.Hashtbl.remove;
  declare "Hashtbl.replace" (hashtbl ^> el ^> v ^> unit) R.Hashtbl.replace C.Hashtbl.replace;
  declare "Hashtbl.find" (hashtbl ^> el ^!> v) R.Hashtbl.find C.Hashtbl.find;
  declare "Hashtbl.find_all" (hashtbl ^> el ^> list v) R.Hashtbl.find_all C.Hashtbl.find_all;
  declare "Hashtbl.mem" (hashtbl ^> el ^> bool) R.Hashtbl.mem C.Hashtbl.mem;
  declare "Hashtbl.length" (hashtbl ^> int) R.Hashtbl.length C.Hashtbl.length;
  hashtbl

let () = declare_spec @@ hashtbl_tests fixed_item Memory_size.string Memory_size.string fixed_item Memory_size.string Memory_size.string

open Memory_size_ds

type immutable =
  | B of bool
  | C of char
  | F of float
  | I of int
  | I32 of int32
  | I64 of int64
  | NI of nativeint
  | Unit of unit
  | Bytes of bytes
  | String of string
  | Option of string option
  | Array of int64 array

let size_of_immutable =
  let open Memory_size in
  variant @@ function
    | B b -> bool b
    | C c -> char c
    | F f -> float f
    | I i -> int i
    | I32 i -> int32 i
    | I64 i -> int64 i
    | NI ni -> nativeint ni
    | Unit () -> unit ()
    | Bytes b -> bytes b
    | String s -> string s
    | Option x -> option string x
    | Array a -> array int64 a

let gen_length = Gen.closed_interval 0 16
let gen_string () =
  let n = gen_length () in
  String.make n 'x'
let gen_bytes () =
  let n = gen_length () in
  Bytes.make n 'x'

let print_immutable =
    let open PPrint.OCaml in
    let v constructor printer v =
      variant "immutable" constructor 4 @@ [printer v]
    in
    function
      | B b -> v "B" bool b
      | C c -> v "C" Print.char c
      | F f -> v "F" float f
      | I i -> v "I" Print.int i
      | I32 i -> v "I32" int32 i
      | I64 i -> v "I64" int64 i
      | NI i -> v "NI" nativeint i
      | Unit () -> v "Unit" (unknown "()") ()
      | Bytes b -> v "Bytes" string (Bytes.to_string b)
      | String s -> v "String" string s
      | Option x -> v "Option" (option string) x
      | Array a -> v "Array" (array int64) a

let gen_immutable () =
  (* for size it doesn't matter what the char is, so don't waste random bits on it *)
  match Gen.closed_interval 0 5 () with
  | 0 -> Bytes (gen_bytes ())
  | 1 -> String (gen_string ())
  | 2 -> Option (if Gen.bool () then None else Some (gen_string ()))
  | 3 -> Array (Gen.array gen_length (fun () -> 10L) ())
  | _ -> Gen.choose
    [ B true
    ; C 'x'
    ; F 4.0
    ; I 1
    ; I32 5l
    ; I64 6L
    ; NI 7n
    ; Unit ()
    ] ()

let immutable =
  let neg = easily_constructible gen_immutable print_immutable in
  let pos = deconstructible print_immutable in
  ifpol neg pos

type immutable_rec =
  { v: immutable
  ; f: unit -> unit
  ; mutable x: int32 (* constant size, so still immutable overall size *)
  }

let immutable_rec =
  let print t =
    let open PPrint.OCaml in
    record "immutable_rec"
    [ "v", print_immutable t.v
    ; "f", unknown "unit->unit" t.f
    ; "x", int32 t.x
    ]
  in
  let gen () =
    { v = gen_immutable ()
    ; f = ignore
    ; x = 27l
    }
  in
  let neg = easily_constructible gen print in
  let pos = deconstructible print in
  ifpol neg pos

let size_of_immutable_rec r =
  let open Memory_size in
  record_start r
  |> record_add_immutable @@ size_of_immutable r.v
  |> record_add_immutable @@ func r.f
  |> record_add_mutable_const @@ int32 r.x
  |> record_end

let () =
  declare_size_of_reachable "immutable_rec" immutable_rec size_of_immutable_rec

let make_updatable_rec s q = { s; q }

let size_of_updatable_rec size_of_q r =
  let open Memory_size in
  record_start r
  |> record_add_immutable @@ string r.s
  |> record_add_immutable @@ size_of_q r.q
  |> record_end

let updatable_rec =
  let t = declare_abstract_type ~var:"updatable_rec" () in
  let queue = queue_tests buf_string Memory_size.string in
  declare "make_updatable_rec" (buf_string ^> queue ^> t) make_updatable_rec make_updatable_rec;
  (* TODO: store and read later multiple times.. *)
  declare "size_of_updatable_rec" (t ^> int)
    (wrap_size_of @@ size_of_updatable_rec @@ R.Queue.size_of Memory_size.string)
    (wrap_size_of @@ size_of_updatable_rec Queue.size_of);
  t

let () =
  declare_size_of_reachable "updatable_rec" updatable_rec (size_of_updatable_rec Queue.size_of)

let size_of_updatable_nested_rec h_size_of r =
  let open Memory_size in
  record_start r
  |> record_add_immutable @@ string r.s
  |> record_add_immutable @@ h_size_of r.h
  |> record_end

let make_updatable_nested_rec s h = { s; h }

(*let () =
  let t = declare_abstract_type ~var:"updatable_nested_rec" () in
  let q_size_of = R.Queue.size_of Memory_size.string in
  let updatable_size_of_ref = size_of_updatable_rec q_size_of in
  let updatable_size_of_cand = size_of_updatable_rec Queue.size_of in
  let h = hashtbl_tests buf_string Memory_size.string Memory_size.string updatable_rec updatable_size_of_ref updatable_size_of_cand in
  declare "make_updatable_nested_rec" (buf_string ^> h ^> t) make_updatable_nested_rec make_updatable_nested_rec;
  (* TODO: store and read later multiple times.. *)
  declare "size_of_updatable_rec" (t ^> int)
    (wrap_size_of @@ size_of_updatable_nested_rec @@ R.Hashtbl.size_of Memory_size.string updatable_size_of_ref)
    (wrap_size_of @@ size_of_updatable_nested_rec Hashtbl.size_of)*)

type ephemeral_rec =
  { x: string
  ; mutable ms: string
  }

let make_ephemeral_rec x ms = {x;ms}

let size_of_ephemeral_rec r =
  let open Memory_size in
  record_start r
  |> record_add_immutable @@ string r.x
  |> record_add_mutable @@ string r.ms
  |> record_end

let mutate_ephemeral_rec t ms = t.ms <- ms

let ephemeral_rec =
  let t = declare_abstract_type ~var:"ephemeral_rec" () in
  declare "make_ephemeral_rec" (buf_string ^> buf_string ^> t) make_ephemeral_rec make_ephemeral_rec;
  declare "mutate_ephemeral_rec" (t ^> buf_string ^> unit) mutate_ephemeral_rec mutate_ephemeral_rec;
  declare "size_of_ephemeral_rec" (t ^> int) (wrap_size_of size_of_ephemeral_rec) (wrap_size_of size_of_ephemeral_rec);
  t

let () =
  declare_size_of_reachable "ephemeral_rec" ephemeral_rec size_of_ephemeral_rec

let prologue () =
  Monolith.dprintf "          #require \"xen.bus\";;\n";
  Monolith.dprintf "          open Xenbus;;\n";
  Monolith.dprintf "          open Memory_size_ds;;\n"

let () =
  at_exit (fun () -> print_endline "Done");
  let fuel = 100 in
  main ~prologue fuel
