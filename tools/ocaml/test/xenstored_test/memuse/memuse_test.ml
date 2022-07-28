open Memuse
open Bos_setup

module ObservingAllocator = struct
  type t = {
      allocated: int array array
    ; mutable used: int
    ; mutable overall: int
    ; mutable finished: bool
  }

  let empty = (0, 0)

  type item = int * int

  let m = 16

  let size_min _ = 1

  let size_max _ = m

  let init () =
    {used= 0; overall= 0; finished= false; allocated= Array.make (m + 1) [||]}

  let finish t =
    assert (t.used = 0) ;
    t.allocated |> Array.iteri (fun i _ -> t.allocated.(i) <- [||])

  let debug t =
    let sizes = t.allocated |> Array.to_seq |> Seq.flat_map Array.to_seq in
    Logs.debug (fun m -> m "sizes: %a" Fmt.(Dump.seq int) sizes)

  let alloc t n =
    debug t ;
    if t.finished then
      [||]
    else
      let avail = m - t.used in
      let count = avail / n in
      if count = 0 then
        [||]
      else
        let items =
          Array.init count (fun _ ->
              t.used <- t.used + n ;
              t.overall <- t.overall + n ;
              n
          )
        in
        t.allocated.(n) <- items ;
        items |> Array.mapi (fun i n -> (i, n))

  let dealloc t (idx, itemsize) =
    let tbl = t.allocated.(itemsize) in
    assert (tbl.(idx) = itemsize) ;
    tbl.(idx) <- 0 ;
    t.used <- t.used - itemsize
  (* t.overall is not updated, it is used to measure total usage *)

  let strategy_begin _t = ()

  let strategy_done t =
    if not t.finished then (
      Logs.debug (fun m ->
          m "cycle done, overall usage = %d, usage=%d" t.overall t.used
      ) ;
      debug t ;
      assert (t.overall = 40) ;
      assert (t.used > 0) ;
      t.finished <- true
    )
end

module SelfTest = WorstCase (ObservingAllocator)

let () =
  Logs.set_level (Some Logs.Debug) ;
  run (module SelfTest) ;
  OCamlAllocator.test ()
