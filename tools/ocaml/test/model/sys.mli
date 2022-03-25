val[@logic] word_size: int (* axiom ws: word_size = 32 || word_size = 64 *)
(*@ axiom ws: word_size = 32 *) (* TODO! both! *)
(* The word size can currently only be 32 or 64, use an axiom to define this.
   It ensures that the counter-example uses more realistic values *)
