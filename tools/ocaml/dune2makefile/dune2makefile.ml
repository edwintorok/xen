open Printf
open Lexing
open Makefile

let print_position ppf lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf ppf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let process_rules rules = Printer.printf stdout rules

let parse_list_exn (terminated_by : MakefileLexer.t) (f : MakefileLexer.t -> 'b)
    (lexbuf : Lexing.lexbuf) : 'b list =
  let rec loop aux =
    match (aux, MakefileLexer.line lexbuf) with
    | _, MakefileLexer.EOF -> List.rev aux
    | _, r when r = terminated_by -> List.rev aux
    | [], EOL ->
        (* blank/empty lines are comments, ignore them
           this rule needs to come after terminated_by,
           which could itself be EOL
        *)
        loop aux
    | _, x -> loop (f x :: aux)
  in
  loop []

let raise_unexpected_token expected actual =
  let actual_str =
    match actual with
    | MakefileLexer.EOL -> "EOL"
    | COLON -> ":"
    | TARGET t -> Printf.sprintf "target(%s)" t
    | COMMAND (p, cmd) ->
        Printf.sprintf "command(%c%s)" (Option.value ~default:' ' p) cmd
    | EOF -> "EOF"
  in
  failwith
    (Printf.sprintf "unexpected token: <%s>, expected: <%s>" actual_str expected)

let parse_target_exn = function
  | MakefileLexer.TARGET t -> Target t
  | t -> raise_unexpected_token "target" t

let rec getall aux f x =
  match f x with None -> List.rev aux | Some arg -> getall (arg :: aux) f x

let parse_command_exn = function
  | MakefileLexer.COMMAND (_prefix, cmd) ->
      let lexbuf = Lexing.from_string cmd in
      let tokens =
        try getall [] ShellLexer.token lexbuf
        with Failure msg ->
          Printf.eprintf "Failed to parse commandline (%s):\n%s\n" msg cmd;
          failwith msg
      in
      Command tokens
      (* TODO: include prefix *)
  | t -> raise_unexpected_token "command" t

let parse_prereq_exn = function
  | MakefileLexer.TARGET t -> Prerequisite t
  | t -> raise_unexpected_token "prerequisite" t

let parse_targets_exn = parse_list_exn MakefileLexer.COLON parse_target_exn
let parse_prerequisites_exn = parse_list_exn MakefileLexer.EOL parse_prereq_exn

(* Another rule could follow immediately, but we don't support that *)
let parse_commands_exn = parse_list_exn MakefileLexer.EOL parse_command_exn

let parse_exn lexbuf =
  let targets = parse_targets_exn lexbuf in
  let prerequisites = parse_prerequisites_exn lexbuf in
  let commands = parse_commands_exn lexbuf in
  match targets with
  | [] -> None (* assume eof *)
  | _ -> Some { targets; prerequisites; commands }

let () =
  let pos_fname, ch =
    match Sys.argv with
    | [| _ |] | [| _; "-" |] -> ("<stdin>", stdin)
    | [| _; fname |] -> (fname, open_in fname)
    | _ -> failwith (Printf.sprintf "Too many arguments: at most one expected")
  in

  let lexbuf = Lexing.from_channel ch in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with pos_fname };

  let rules = Queue.create () in
  let rec loop () =
    let rule =
      try parse_exn lexbuf
      with Failure msg ->
        fprintf stderr "%a: %s" print_position lexbuf msg;
        exit 1
    in
    match rule with
    | Some r ->
        Queue.add r rules;
        loop ()
    | None -> ()
  in
  loop ();
  process_rules (Queue.to_seq rules |> List.of_seq);
  close_in ch
