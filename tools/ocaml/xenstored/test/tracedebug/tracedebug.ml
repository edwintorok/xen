(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Implementation constraints:

   - has to build on OCaml 4.02.3+, no dependencies on libraries not shipped with it
   - can optionally use functionality provided by newer versions or external libraries by using
     dune's select mechanism (in separate modules)
   - Minimize memory allocation and CPU usage during normal operation
   - Limit maximum memory usage so that tracing can be left enabled for long running tests
   - Dump a debug trace when an error/exception is encountered, this one can be a slowpath
*)
open Tracedebug_times

let pid = Unix.getpid ()

module type TracedEvent = sig
  type t

  val empty : t

  val pp : Format.formatter -> t -> unit
end

module StringEvent = struct
  type t = string

  let empty = ""

  let pp ppf s = Format.pp_print_string ppf s
end

module ExnEvent = struct
  type t = Printexc.raw_backtrace * exn

  let empty = (Printexc.get_raw_backtrace (), Not_found)

  let pp_backtrace ppf bt =
    Printexc.raw_backtrace_to_string bt |> Format.pp_print_text ppf

  let pp ppf (bt, exn) =
    (* don't allow any exceptions to escape *)
    let exc =
      try `Ok (Printexc.to_string exn)
      with e -> (
        (* Printexc.to_string_default is 4.09, can't use here *)
        let bt = Printexc.get_raw_backtrace () in
        try `Error (Printexc.to_string e, bt) with _ -> `Error ("??", bt)
      )
    in
    match exc with
    | `Ok str ->
        Format.fprintf ppf "@[<v>Exception: %s@,%a@]" str pp_backtrace bt
    | `Error (str', bt') ->
        Format.fprintf ppf
          "@[<v>Exception: ?@,%a@,Exception formatting: %s@,%a@]" pp_backtrace
          bt str' pp_backtrace bt'

  let get e = (Printexc.get_raw_backtrace (), e)
end

module GcEvent = struct
  type t = Gc.stat

  let pp ppf q =
    Format.fprintf ppf "major GC end: %d/%d heap words (top %d), compactions %d"
      q.Gc.live_words q.Gc.heap_words q.Gc.top_heap_words q.Gc.compactions

  let get = Gc.quick_stat

  let empty = get ()
end

type events = (int64 * int * (Format.formatter -> unit)) list

let order = Atomic.make 0
(* we have multiple event rings based on the type of the event,
   and we want to always be able to sort them in chronological order.
   Using a high resolution timestamp helps, however if that is not available
   we could have multiple events with the same timestamp.
   Within a single process we can use this global to ensure consistent ordering.
   [dump] will use this together with the timestamp for sorting.
   This won't help in disambiguating order of events between multiple processes,
   so a high resolution timestamp is still recommended.
*)

module Make
    (E : TracedEvent) (Config : sig
      val limit_log2 : int
      (** number of events to store = [2**limit_log2] *)
    end) =
struct
  let limit =
    assert (Config.limit_log2 > 0) ;
    1 lsl Config.limit_log2

  let mask = limit - 1

  let enabled = Atomic.make true

  let index = Atomic.make 0

  (* avoid allocation: unpack fields into separate arrays of equal size *)
  let events = Array.make limit E.empty

  let ordering = Array.make limit 0

  let timestamps = Times.create limit

  let start () = Atomic.set enabled true

  let stop () = Atomic.set enabled false

  let reset () =
    stop () ;
    (* race condition here: some [record] functions may still be running,
       but caller should ensure that doesn't happen
    *)
    Array.fill events 0 (Array.length events) E.empty ;
    Times.fill timestamps ;
    Atomic.set index 0 ;
    start ()

  (* do not request inlining this to prevent making callers too large *)
  let record_internal ev =
    (* assumes power of 2 size *)
    let events_idx = Atomic.fetch_and_add index 1 land mask
    and order_idx = Atomic.fetch_and_add order 1 in
    Times.record timestamps events_idx ;
    (* mask ensures we are within bounds,
       and some versions of times do perform bound checks
       that can be used while testing *)
    Array.unsafe_set events events_idx ev ;
    Array.unsafe_set ordering events_idx order_idx

  (* performance critical: avoid allocation, and minimize function calls
      when disabled *)
  let record ev =
    if Atomic.get enabled then
      record_internal ev
    [@@ocaml.inline]

  let recordf f =
    if Atomic.get enabled then
      record_internal (f ())
    [@@ocaml.inline]

  (* end performance critical *)

  let rec getall lst count idx =
    if count >= limit then
      lst (* avoid infinite loop on a full ring *)
    else
      let i = (idx + limit) land mask in
      match Times.get_as_ns timestamps i with
      | Some timestamp ->
          let event ppf =
            try E.pp ppf events.(i)
            with e ->
              let ee = ExnEvent.get e in
              Format.fprintf ppf "@,Exception formatting: %a" ExnEvent.pp ee
          in
          getall ((timestamp, ordering.(i), event) :: lst) (count + 1) (idx - 1)
      | None ->
          (* we constructed the list by going backwards through the ring,
             so the list is in the correct order and we only traversed as much of
             the ring that had valid entries *)
          lst

  let dump () =
    stop () ;
    let idx = Atomic.get index - 1 in
    getall [] 0 idx
  (* do not reset here, formatting is delayed *)
end

let sort_timestamp (t0, o0, _) (t1, o1, _) =
  match Int64.compare t0 t1 with 0 -> Int.compare o0 o1 | r -> r

(* 4.14: we could use Seq.sorted_merge *)
let sorted all =
  let a = all |> List.map Array.of_list |> Array.concat in
  Array.stable_sort sort_timestamp a ;
  a

let time_frac_scale = 10. ** float (9 - Times.precision) |> Int64.of_float

let nsec_per_s = 1_000_000_000L

let pp_timestamp ppf i =
  let i = Int64.logand i (Int64.sub Int64.max_int 1L) in
  (* 4.08+ would have unsigned div/rem, for now
     just mask off the sign bit *)
  let s = Int64.div i nsec_per_s and ns = Int64.rem i nsec_per_s in
  let frac = Int64.div ns time_frac_scale in
  Format.fprintf ppf "%Lu.%0*Lus" s Times.precision frac

let get_overhead () =
  let n = 10000 in
  let a = Times.create n in
  for i = 0 to n - 1 do
    Times.record a i
  done ;
  let t =
    Array.init n (fun i ->
        match Times.get_as_ns a i with
        | None ->
            assert false (* we've filled all elements *)
        | Some i ->
            i
    )
  in
  let avg = Int64.div (Int64.sub t.(n - 1) t.(0)) (Int64.of_int (n - 1)) in
  let t =
    Array.mapi
      (fun i ti ->
        if i > 0 then
          Int64.sub ti t.(i - 1)
        else
          0L
      )
      t
  in
  let t = Array.sub t 1 (n - 1) in
  ( Array.fold_left
      (fun acc dt ->
        (* ignore 0 values, that is just precision limit, it is not really
           0 overhead *)
        if dt > 0L then min acc dt else acc
      )
      Int64.max_int t
  , avg
  , Array.fold_left max Int64.min_int t
  )

(* TODO: helper function to sort parent/child output based on timestamp, ordering,
   for now we can do dune runtest --profile=release | sort
*)

let dump ppf all =
  let print last (timestamp, _, pp_event) =
    let delta =
      if Int64.compare last Int64.min_int = 0 then
        0L
      else
        Int64.sub timestamp last
    in
    (* timestamp first in case this needs to be sorted again *)
    (* TODO: replace with <hov 2> once we no longer rely on external sorting *)
    Format.fprintf ppf "[%a][pid %d](+%a) @[<h>" pp_timestamp timestamp pid
      pp_timestamp delta ;
    pp_event ppf ;
    Format.fprintf ppf "@]@," ;
    timestamp
  in
  let o_min, o_avg, o_max = get_overhead () in
  Format.fprintf ppf "@[<v>Using clock: %s. Overhead: ~%Luns [%Luns, %Luns]@,"
    Times.id o_avg o_min o_max ;
  let (_ : int64) = all |> sorted |> Array.fold_left print Int64.min_int in
  Format.fprintf ppf "@]@."

let register_gc ?(limit_log2 = 9) () =
  let module R = Make (GcEvent) (struct let limit_log2 = limit_log2 end) in
  let on_major_cycle_end () = R.record (GcEvent.get ()) in
  let (_ : Gc.alarm) = Gc.create_alarm on_major_cycle_end in
  R.dump
