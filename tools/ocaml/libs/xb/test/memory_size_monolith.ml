open Xenbus
open Monolith
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

    let size_of el_size_of t =
      Queue.fold (fun acc el ->
        container_add ~item_overhead:C.Queue.item_overhead el_size_of el acc) C.Queue.initial t

  end

  module Hashtbl = struct
    include Hashtbl
    let create_sized _ _ n = create n
    let size_of key_size_of value_size_of t =
      Hashtbl.fold (fun k v acc ->
        acc
        |> container_add ~item_overhead:C.Hashtbl.item_overhead key_size_of k
        |> container_add ~item_overhead:(Memory_size.int 0) value_size_of v) t @@ C.Hashtbl.initial t
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
  let queue = declare_abstract_type ~var:"queue" () in
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
  declare_size_of "Queue" queue (R.Queue.size_of el_size_of) C.Queue.size_of

let fixed_item =
  let neg =
    easily_constructible (Gen.choose ["foo"; "bar2"; "foobar"]) Print.string
  in
  ifpol neg str

let () = queue_tests fixed_item Memory_size.string

let hashtbl_tests el el_size_of v v_size_of =
  let hashtbl = declare_abstract_type ~var:"hashtbl" () in
  declare "Hashtbl.create_sized" (small_int ^> hashtbl)
    (R.Hashtbl.create_sized el_size_of v_size_of)
    (C.Hashtbl.create_sized el_size_of v_size_of);
  declare "Hashtbl.reset" (hashtbl ^> unit) R.Hashtbl.reset C.Hashtbl.reset;
  declare "Hashtbl.remove" (hashtbl ^> el ^> unit) R.Hashtbl.remove C.Hashtbl.remove;
  declare "Hashtbl.replace" (hashtbl ^> el ^> v ^> unit) R.Hashtbl.replace C.Hashtbl.replace;
  declare "Hashtbl.find" (hashtbl ^> el ^!> v) R.Hashtbl.find C.Hashtbl.find;
  declare "Hashtbl.find_all" (hashtbl ^> el ^> list v) R.Hashtbl.find_all C.Hashtbl.find_all;
  declare "Hashtbl.mem" (hashtbl ^> el ^> bool) R.Hashtbl.mem C.Hashtbl.mem;
  declare "Hashtbl.length" (hashtbl ^> int) R.Hashtbl.length C.Hashtbl.length;
  declare_size_of "Hashtbl" hashtbl (R.Hashtbl.size_of el_size_of v_size_of) C.Hashtbl.size_of

let () = hashtbl_tests fixed_item Memory_size.string fixed_item Memory_size.string

let () =
  at_exit (fun () -> print_endline "Done");
  let fuel = 100 in
  main fuel
