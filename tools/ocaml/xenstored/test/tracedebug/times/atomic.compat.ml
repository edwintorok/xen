(* compat module for Atomic for OCaml <4.12 *)

type 'a t = 'a ref

let make = ref

let set t x = t := x [@@inline]

let get t = !t [@@inline]

let fetch_and_add t n =
  (* doesn't allocate, so thread safe on < OCaml 5.0 *)
  let old = !t in
  t := old + n ;
  old
  [@@inline]
