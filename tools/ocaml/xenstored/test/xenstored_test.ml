open Testable
open Generator
module Cb = Crowbar

let random_path = Cb.list Cb.bytes

let value = Cb.bytes

let token = Cb.bytes

let permty =
  [Perms.READ; Perms.WRITE; Perms.RDWR; Perms.NONE]
  |> List.map Cb.const |> Cb.choose

let new_domid = Cb.range ~min:1 Types.domid_first_reserved

let port = Cb.range 0xFFFF_FFFF (*uint32_t*)

let arb_cmd =
  let open Command in
  let path =
    Cb.choose
      [ Cb.map [Cb.int] (fun rnd state -> PathObserver.choose_path state rnd)
      ; Cb.map [random_path] (fun x _ -> Store.Path.to_string x) ]
  in
  let domid =
    Cb.map [Cb.int] (fun rnd state -> PathObserver.choose_domid state rnd)
  in
  let perms =
    Cb.map [domid; permty; Cb.pair domid permty |> Cb.list]
    @@ fun idgen owner other state ->
    let other = List.map (fun (idgen, ty) -> (idgen state, ty)) other in
    Perms.Node.create (idgen state) owner other
  in
  let guard' ~f gen state =
    let v = gen state in
    Cb.guard (f v) ;
    v
  in
  let cmd =
    let open Testable.Command in
    Cb.choose
      [ Cb.map [path] cmd_read
      ; Cb.map [path; value] cmd_write
      ; Cb.map [path] cmd_mkdir
      ; Cb.map [path] (fun p -> cmd_rm @@ guard' ~f:(fun p -> p <> "/") p)
      ; Cb.map [path] cmd_directory
      ; Cb.map [path] cmd_getperms
      ; Cb.map [path; perms] cmd_setperms
      ; Cb.map [path; token] cmd_watch
      ; Cb.map [path; token] cmd_unwatch
      ; Cb.const cmd_reset_watches
      ; Cb.const cmd_transaction_start
      ; Cb.map [Cb.bool] cmd_transaction_end
      ; Cb.map [new_domid; port] cmd_introduce
      ; Cb.map [domid] (fun idgen ->
            cmd_release @@ guard' ~f:(fun id -> id <> 0) idgen)
      ; Cb.map [domid] cmd_getdomainpath
      ; Cb.map [domid] cmd_isintroduced
      ; Cb.map [domid; domid] cmd_set_target
      ; Cb.const cmd_liveupdate ]
  in
  Cb.map [domid; Cb.int; cmd] (fun this rnd cmd state ->
      let this = this state in
      let txid = PathObserver.choose_txid_opt state this rnd in
      let cmd = cmd txid state in
      (this, cmd))

(* based on QCSTM *)
module Make (Spec : sig
  include Testable.S

  val arb_cmd : (state -> cmd) Crowbar.gen
end) =
struct
  let arb_cmds =
    Crowbar.with_printer (Fmt.Dump.list Spec.pp)
    @@ Crowbar.map [Crowbar.list1 Spec.arb_cmd] (fun cmdgens ->
           let cmds, _ =
             List.fold_left
               (fun (cmds, s) f ->
                 let cmd = f s in
                 Crowbar.check (Spec.precond cmd s) ;
                 (cmd :: cmds, Spec.next_state cmd s))
               ([], Spec.init_state) cmdgens
           in
           List.rev cmds)

  let interp_agree sut cs =
    List.fold_left
      (fun s cmd ->
        Crowbar.check
          ( try Spec.run_cmd cmd s sut
            with Failure msg -> Crowbar.failf "%a" Fmt.lines msg ) ;
        Spec.next_state cmd s)
      Spec.init_state cs

  let agree_prop cs =
    let on_exn e bt logs =
      List.iter prerr_endline logs ;
      Printexc.raise_with_backtrace e bt
    in
    Testable.with_logger ~on_exn (fun () ->
        let sut = Spec.init_sut () in
        Stdext.finally (fun () -> 
        let (_ : Spec.state) = interp_agree sut cs in ())
        (fun () -> 
        Spec.cleanup sut))

  let agree_test ~name = Crowbar.add_test ~name [arb_cmds] agree_prop
end

module LU = Make (struct
  include PathObserver

  type cmd = int * Testable.Command.t

  type sut = Testable.t ref * Testable.t ref

  let arb_cmd = arb_cmd

  let init_sut () =
    let sut1 = Testable.create () in
    Testable.init sut1 ;
    let sut2 = Testable.create ~live_update:true () in
    Testable.init sut2 ;
    let sut1 = ref sut1 in
    let sut2 = ref sut2 in
    (sut1, sut2)

  let cleanup (sut1, sut2) =
    Testable.cleanup !sut1 ; Testable.cleanup !sut2

  let run_cmd cmd state (sut1, sut2) =
    Testable.run2 state.next_tid sut1 sut2 cmd ;
    true
end)

let () =
  (* Crowbar runs at_exit, and after bisect's coverage dumper,
     registering an at_exit here would run *before* Crowbar starts,
     hence the nested at_exit which puts the bisect dumper in the proper place
     to dump coverage *after* crowbar is finished.
   *)
  (* at_exit (fun () -> at_exit Bisect.Runtime.write_coverage_data);*)
  print_endline "";
  LU.agree_test ~name:"live-update-agree";
