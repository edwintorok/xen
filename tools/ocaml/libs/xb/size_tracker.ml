type t = int
type 'a size_of = 'a -> t

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

let queue size_of queue =
  let fold acc item =
    add acc @@ size_of item
  in
  Queue.fold fold 0 queue

let hashtbl key_size_of value_size_of tbl =
  let fold_kv k v acc =
    add acc @@
    add (key_size_of k) (value_size_of v)
  in
  let stats = Hashtbl.stats tbl in
  (* Even an empty hash table will use memory depending on the initial table size,
     so a hashtable with 0 elements can still use a significant amount of memory,
     especially if we'd nest lots of small hashtables.
     Also just deleting elements from a hashtable won't cause it to shrink its number of buckets,
     so having a large number of items in the past and deleting them doesn't mean our usage is
     nearly 0 *)
  add (mul record_field stats.Hashtbl.num_buckets)
  (Hashtbl.fold fold_kv tbl empty)
