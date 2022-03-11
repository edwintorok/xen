open Xenbus
open Monolith
module C = struct
  include Memory_size_ds
end

module R = struct
  module Buffer = struct
    include Buffer

    let size_of (t:Buffer.t) =
      Sizeops.Size.of_words @@ Obj.reachable_words (Obj.repr t)
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

let wrap_size_of f x =
  Memory_size.size_of_bytes @@ f x

let declare_size_of name typ refs candidate =
  let size x = Option.value ~default:max_int @@ Sizeops.Size.to_int_opt @@ refs x in
  declare (name ^ ".size_of") (typ ^> int) size (wrap_size_of candidate)

let zero _ = 0

let size_t = deconstructible @@ fun s -> s |> Sizeops.Size.to_int_opt |> Print.option Print.int

let declare_size_of_reachable name typ candidate =
  let check_size t actual =
    let expected = Sizeops.Size.of_words @@ Obj.reachable_words (Obj.repr t) in
    let expected = Option.value ~default:max_int @@ Sizeops.Size.to_int_opt expected in
    let expected = expected * Sys.word_size / 8 in
    if expected <= actual then
      Valid actual
    else
      Invalid (fun d ->
        let open PPrint in
        Print.assert_ (d ^^ Print.string " <= " ^^ Print.int actual) ^^
        Print.candidate_finds d)
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

let () =
  let fuel = 100 in
  main fuel
