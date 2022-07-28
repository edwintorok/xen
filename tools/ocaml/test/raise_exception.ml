open Printf

let _ =
  try Xenlight.test_raise_exception ()
  with Xenlight.Error (err, fn) ->
    printf "Caught Exception: %s: %s\n" (Xenlight.string_of_error err) fn
