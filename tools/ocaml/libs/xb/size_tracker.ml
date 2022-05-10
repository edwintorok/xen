type t = int

let words_to_bytes = Sys.word_size / 8

let of_string_length n =
  (* https://dev.realworldocaml.org/runtime-memory-layout.html#scrollNav-6 *)
  (* padding is calculated such that the result is word aligned, so an empty string
     always uses at least 1 + 1(GC header) words = 16 bytes on x86-64.
     Although of course there is only one empty string so this could be shared with other empty
     strings.
     *)
  (n + words_to_bytes) / words_to_bytes + 1

let record_field = 1

let string s = s |> String.length |> of_string_length

let bytes b = b |> Bytes.length |> of_string_length

let to_byte_count x = x * words_to_bytes

let add a b = a + b

let remove a b =
  assert (b >= 0);
  let r = a - b in
  assert (r >= 0);
  r

let mul x c =
  assert (c > 0);
  x * c

let empty = 0
