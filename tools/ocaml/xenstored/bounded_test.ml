open Bounded

let word_size_bytes = Sys.word_size / 8

(* only used in test code, not production code *)
let actual_size_bytes t = Obj.reachable_words (Obj.repr t) * word_size_bytes

(* TODO: we should use ortac here to generate some [t]s and check the invariant *)
module SizedTest(X: Sized) = struct

  let check t =
    assert (1 <= X.max_overhead_words);
    assert (X.max_overhead_words < Sys.max_string_length);
    let n = X.size_bytes t in
    assert (0 <= n);
    assert (n <= Sys.max_string_length);
    let expected = n + X.max_overhead_words * word_size_bytes in
    assert (actual_size_bytes t <= expected)
end
