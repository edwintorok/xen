let unique_string =
  let template = String.init 120 Char.chr in
  let counter = ref 0 in
  fun () ->
  let b = Buffer.create 128 in
  incr counter;
  Buffer.add_int64_ne b (Int64.of_int !counter);
  Buffer.add_string b template;
  Buffer.contents b

let keys =
  Array.init 8192 (fun _ -> unique_string ()) |> Array.to_list

module M = Map.Make(String)

let vm_keys i =
  keys |> List.map (fun k -> Printf.sprintf "%d/" i ^ k)

let do_one_vm_loop m i =
  vm_keys i |> List.fold_left (fun acc k ->
    let v = unique_string () in
    M.add k v acc) m

let remove_one_vm_loop m i =
  let m = vm_keys i |> List.fold_left (fun acc k ->
    M.remove k acc) m in
  (* in the real code we'd use size_of to query the actual size that got freed *)
  (*let _:int = Gc.major_slice 262144 in*)
  m

let do_n_vms n m =
  Array.init n (fun i -> i) |>
  Array.fold_left do_one_vm_loop m

let replace_n_vms n m =
  let vms = Array.init n (fun _ -> Random.int n) in
  let m = Array.fold_left remove_one_vm_loop m vms in
  Array.fold_left do_one_vm_loop m vms

let rec repeat_fold n f acc =
  if n <= 0 then acc
  else repeat_fold (n-1) f (f acc)

let run () =
  let (_:'a M.t) = repeat_fold 14 (replace_n_vms 16) (do_n_vms 224 M.empty) in
  ()

let top = ref 0
let gc_major_alarm () =
  let stat = Gc.quick_stat () in
  top := max !top stat.Gc.heap_words

let switch_settings space_overhead max_overhead () =
  let stat = Gc.quick_stat () in
  if stat.Gc.heap_words > 100*1024*1024 then begin
    Gc.set { (Gc.get ()) with space_overhead; max_overhead }
  end else begin
    Gc.set { (Gc.get ()) with space_overhead=80; max_overhead=500 }
  end

let timeit space_overhead max_overhead f x =
  let a = Gc.create_alarm (switch_settings space_overhead max_overhead) in
  Gc.compact ();
  top := 0;
  let alarm = Gc.create_alarm gc_major_alarm in
  let t0 = Unix.gettimeofday () in
  let () = f x in
  Gc.delete_alarm alarm;
  Gc.full_major ();
  let t1 = Unix.gettimeofday () in
  Gc.delete_alarm a;
  let words_in_mib = 1048576. /. float (Sys.word_size / 8) in
  let mib = float !top /. words_in_mib in
  let stat = Gc.stat() in
  let mibnow = float stat.Gc.heap_words /. words_in_mib in
  let compactions = stat.Gc.compactions in
  Printf.printf "space_overhead=%d%%, max_overhead=%d%%, %.2fMiB, end: %.2fMiB, compactions: %u in %gs\n%!" space_overhead max_overhead mib mibnow compactions (t1 -. t0)

let () =
    let () = Random.self_init () in
    timeit 80 500 run ();
    timeit 60 65 run ();
    timeit 40 45 run ();
    timeit 40 80 run ();
    timeit 80 90 run ();
    (* doesn't make sense to go much below *)
    timeit 80 1 run ();
    timeit 40 80 run ();
    timeit 40 45 run ();
    timeit 40 1 run ();
    timeit 60 65 run ();
    timeit 60 10 run ();
    timeit 10 80 run ();
    timeit 10 15 run ();
    timeit 10 1 run ();
    timeit 1 80 run ()
