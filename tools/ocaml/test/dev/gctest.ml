module Symbol = struct
  module WeakTable = Weak.Make (struct
    type t = string

    let equal (x : string) (y : string) = x = y

    let hash = Hashtbl.hash
  end)

  type t = WeakTable.data

  let t = WeakTable.create 1024

  let compare a b = String.compare b a

  let of_string s = WeakTable.merge t s
end

module Tree = struct
  module M = Map.Make (Symbol)

  type t = {children: t M.t; value: Symbol.t}

  let empty = {children= M.empty; value= ""}

  let rec add t path value =
    match path with
    | [] ->
        {t with value}
    | hd :: tl ->
        let key = Symbol.of_string hd in
        let children =
          M.update key
            (fun old ->
              let old = Option.value ~default:empty old in
              Some (add old tl value)
            )
            t.children
        in
        {t with children}

  let rec remove t = function
    | [] ->
        None
    | hd :: tl ->
        let key = Symbol.of_string hd in
        let children =
          M.update key
            (function None -> None | Some old -> remove old tl)
            t.children
        in
        Some {t with children}

  let remove t path = Option.value ~default:empty (remove t path)
end

(* generate a string with minimum amount of allocation *)
let gen_string =
  let template = String.init 255 Char.chr in
  let counter = ref 0 in
  fun lo hi ->
    let lo = max lo 4 in
    let length = lo + Random.int (hi - lo + 1) in
    let b = Buffer.create length in
    incr counter ;
    Buffer.add_int32_ne b (Int32.of_int !counter) ;
    Buffer.add_substring b template 0 (length - 4) ;
    Buffer.contents b

let gen_path () =
  let elements = 2 + Random.int 19 in
  List.init elements (fun _ -> gen_string 1 40)

let gen_value () = gen_string 1 135

let entries_per_vm = 8192

let keys_values = List.init entries_per_vm (fun _ -> (gen_path (), gen_value ()))

let do_one_vm_loop tree i =
  let prefix = [string_of_int i; "domain"; "local"] in
  keys_values
  |> List.fold_left
       (fun acc (k, v) ->
         (* 70% that we've got unique values *)
         let v = if Random.int 100 > 30 then gen_value () else v in
         Tree.add acc (List.rev_append prefix k) (Symbol.of_string v)
       )
       tree

let after_domain_destroy () =
  (* in the real code we'd use size_of to query the actual size that got freed *)
  let (_ : int) = Gc.major_slice 262144 in
  ()

let remove_one_vm_loop tree i =
  let tree = Tree.remove tree ["local"; "domain"; string_of_int i] in
  after_domain_destroy () ; tree

let do_n_vms n m = Array.init n (fun i -> i) |> Array.fold_left do_one_vm_loop m

let replace_n_vms n m =
  let vms = Array.init n (fun _ -> Random.int n) in
  let m = Array.fold_left remove_one_vm_loop m vms in
  Array.fold_left do_one_vm_loop m vms

let rec repeat_fold n f acc =
  if n <= 0 then
    acc
  else
    repeat_fold (n - 1) f (f acc)

let run () =
  (* 224 *)
  let n = 224 in
  let (_ : Tree.t) = repeat_fold 2 (replace_n_vms 16) (do_n_vms n Tree.empty) in
  ()

let top = ref 0

let gc_major_alarm () =
  let stat = Gc.quick_stat () in
  top := max !top stat.Gc.heap_words

let switch_settings space_overhead max_overhead () =
  let stat = Gc.quick_stat () in
  if stat.Gc.heap_words > 100 * 1024 * 1024 then
    Gc.set {(Gc.get ()) with space_overhead; max_overhead}
(* else begin
     Gc.set { (Gc.get ()) with space_overhead=80; max_overhead=500 }
   end *)

let timeit space_overhead max_overhead f x =
  let a = Gc.create_alarm (switch_settings space_overhead max_overhead) in
  Gc.compact () ;
  top := 0 ;
  let alarm = Gc.create_alarm gc_major_alarm in
  let t0 = Unix.gettimeofday () in
  let () = f x in
  Gc.delete_alarm alarm ;
  Gc.full_major () ;
  let t1 = Unix.gettimeofday () in
  Gc.delete_alarm a ;
  let words_in_mib = 1048576. /. float (Sys.word_size / 8) in
  let mib = float !top /. words_in_mib in
  let stat = Gc.stat () in
  let mibnow = float stat.Gc.heap_words /. words_in_mib in
  let compactions = stat.Gc.compactions in
  Printf.printf
    "space_overhead=%d%%, max_overhead=%d%%, %.2fMiB, end: %.2fMiB, \
     compactions: %u in %gs\n\
     %!"
    space_overhead max_overhead mib mibnow compactions (t1 -. t0)

let () =
  let () = Random.init 42 in
  (* deterministic *)
  (*Memtrace.trace_if_requested ();*)
  timeit 80 500 run () ;
  timeit 80 100 run () ;
  timeit 60 65 run () ;
  timeit 40 45 run () ;
  timeit 40 80 run () ;
  timeit 80 90 run () ;
  (* doesn't make sense to go much below *)
  timeit 80 1 run () ;
  timeit 40 80 run () ;
  timeit 40 45 run () ;
  timeit 40 1 run () ;
  timeit 60 65 run () ;
  timeit 60 10 run () ;
  timeit 10 80 run () ;
  timeit 10 15 run () ;
  timeit 10 1 run () ;
  timeit 1 80 run ()
