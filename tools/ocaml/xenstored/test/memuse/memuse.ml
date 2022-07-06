(* See
    https://www.sqlite.org/malloc.html#_mathematical_guarantees_against_memory_allocation_failures
  J. M. Robson. "Bounds for Some Functions Concerning Dynamic Storage Allocation". Journal of the Association for Computing Machinery, Volume 21, Number 8, July 1974, pages 491-499.

  See also "Upper Bounds for Dynamic Memory Allocation Yusuf Hasan, Wei-Mei Chen, Member, IEEE, J. Morris Chang, Senior Member, IEEE, and Bashar M. Gharaibeh"

  There is a theoretical bound on how much fragmentation overhead an allocator has, independently of its algorithm (i.e. even for a yet undiscovered optimal algorithm).
  And the paper also describes an attacking algorithm that triggers the worst case described in that theoretical bound.

  A first-fit allocator can achieve close to this theoretical bound, other algorithms such as best fit or next fit can have much worse worst cases (and there are papers describing an attacking algorithm
   to trigger the worst case: that is the way these allocator algorithms are designed: the proof of their worst case fragmentation complexity is constructive, i.e. provides an attacking algorithm

  Note that these worst cases only apply to allocators that cannot move memory (i.e. 'malloc'), a GC is not bound by these worst cases since it can move memory, although these attacks are still
  useful as they may trigger more CPU usage in GCs by triggering compactions a lot more often.
  
  They also seem useful in measuring how well the space overhead setting in the GC is reflected in worst case memory usage in practice.

  For oxenstored the smallest memory allocation on a 64-bit system is 8 bytes (one word), and the largest 4096+16 (a xenstored packet as a string),
  which means we'd use 5.5x as much memory than allocated due to fragmentation.
  This memory is not wasted: it can be reused by other operations/domains, or compacted and entirely recovered if we exceed the GC space overhead.
  *)


let words_to_bytes = Sys.word_size / 8
let keysize = 1024

(* lets try the exact Robson attack with powers of 2 *)

let empty = ""
let entries = 8192

let sizeof (s:(_,_,_) Bigarray.Array1.t) = Obj.reachable_words (Obj.repr s)

let alloc n =
  let strsize = max 2 ((n-1) * words_to_bytes - words_to_bytes) in
  let str = String.make strsize 'x' in
  str

let dealloc p k l = p.(l).(k) <- empty

let allocn p n =
  Array.iteri (fun i _ -> if (i> 0) then p.(i) <- alloc n) p

let view_cycle p i =
  Printf.printf "cycle %d: " i;
  Array.iteri  (fun row pi ->
    if row > 0 then
  Array.iteri (fun i e ->
    if (i > 0) then
    Printf.printf "%d," (Obj.reachable_words (Obj.repr e))
  ) pi;
  Printf.printf "\n"
  ) p;
  Printf.printf "\n"

let dofragment m n l =
  assert (1 lsl l * n = m);

  (* TODO: 1 based indexing to 0 based indexing *)
  let p = Array.init (l+1) (fun i ->
    let p_i = entries * m / ((1 lsl i) * n) in
    Array.make (1+p_i) empty
  ) in
  p.(1) <- Array.init (m/n+1) (fun i -> if (i>0) then alloc n else empty);
  Array.iteri (fun k _ -> 
    if k > 0 then
    if k mod 2 <> 0 then begin
      dealloc p k 1
    end
  ) p.(1);
  (* view_cycle p 1; *)
  for i = 2 to l do
    let size = (1 lsl (i-1)) * n in
    allocn p.(i) size;
    if i < l then
    for j = 1 to i do
      Array.iteri (fun k _ ->
        if (k>0) then
          if k mod (1 lsl (i - j + 1)) <> 0 then
            dealloc p k j
      ) p.(j);
    done;
    (* view_cycle p i; *)
  done;
  p
  (* view_cycle p (l-1) *)

let () =
  Printexc.record_backtrace true;
  let p = dofragment (keysize) words_to_bytes 7 in
  Gc.print_stat stdout;
  let (_:int) = Sys.command (Printf.sprintf "pmap -p %d" (Unix.getpid ())) in
  Gc.full_major ();
  Gc.print_stat stdout;
  let gc = Gc.stat () in
  Printf.printf "ratio:%f\n" (float gc.Gc.top_heap_words /. float gc.Gc.live_words);
  Printf.printf "%d\n" (Array.length p)
