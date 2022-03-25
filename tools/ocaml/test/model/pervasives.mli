val[@logic] min_int: int

val[@logic] max_int: int

(* GOSPEL doesn't support 0x yet, and although this can be defined using the Power module and ^
   that will slow down proof/counterexample search a lot (since ^ is defined using proof machinery)
 *)
(*@ axiom int31mm: Sys.word_size = 32 -> min_int = -1073741824 && max_int = 1073741823 *)
(*@ axiom int63mm: Sys.word_size = 64 -> min_int = -4611686018427387904 && max_int = 4611686018427387903 *)
