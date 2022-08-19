(* A lexer to parse the makefile output by 'dune rules --makefile'

  POSIX makefile format: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/make.html#tag_20_76_13_01
  Extensions to parse Dune's output:
  * Allow characters /@- in target names

  We do not implement the full syntax, just enough to parse Dune's output:
  * comments other than blank/empty lines are not implemented
  * include lines are not implemented
  * macros are not implemented
  * ; is not valid for starting commands on target lines

  All non-implemented features print an error message
*)

{
  open Lexing
  
  type target =
  [ `TARGET of string
  | `COLON
  | `EOL
  | `EOF ]

  type command =
  [ `COMMAND of char option * string
  | `EOL
  | `EOF
  ]

  let raise_invalid_char msg c =
    failwith (Printf.sprintf "Invalid character while parsing %s: %C" msg c)

  let raise_notsup msg =
    failwith ("Not implemented: " ^ msg)
}

(*
  POSIX character classes in the POSIX locale according to
  https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap07.html#tag_07_03_01
*)

let blank = [' ' '\t']
let tab = '\t'
let newline = '\n'

(* newline needs special processing, don't define rules directly with space *)
let space_nonl = [' ' '\t' '\n'-'\r'] # '\n'
let escape = '\\'

(* /@- are extensions *)
let target_chars = ['.' '_' '0'-'9' 'a'-'z' 'A'-'Z' '/' '@' '-']

let command_prefix = ['-' '@' '+']

let space_followed_by_blanks = ' ' blank*

(* The Makefile has multiple syntaxes in one:
  - target lines
  - include lines
  - command lines
  - comments

  This results in a context dependent grammar, so we don't use ocamlyacc/menhir
  to invoke the lexer, but just call the appropriate lexer in each context
  directly
*)

(* bol = beginning of line *)
rule target_line = parse
| blank* newline
  (*
     Call Lexing.new_line to update position information,
     this is needed so that syntax errors have accurate position reported
     The caller will decide whether this is a comment (a blank/empty line),
     or not (e.g. if we're not at the beginning of the line)
  *)
  { new_line lexbuf;
    `EOL
  }

| space_nonl* escape newline
  (* GNU make would also condense blank before backslash, follow GNU make here.
    This would produce a single space (blank) and would get ignored,
    so continue parsing the line
    For positions to be accurate the regex above needs to end with newline
  *)
  { new_line lexbuf; condense_whitespace lexbuf; target_line lexbuf }

| space_followed_by_blanks* (target_chars+ as target) blank*
  (* blank separated targets/prerequisites:
     although we could begin with a tab here that would make distinguishing
     targets and commands more difficult, so we don't support it
     (it'd also make the lexer here context-dependent)
  *)
  { `TARGET target }

| ':' { `COLON }

| '\t' { raise_notsup "leading tab for targets/prerequisites"}
| '#' { raise_notsup "# comment" }
| ';' { raise_notsup "starting a command with ; on target lines" }
| "include" blank+ { raise_notsup "include line" }
| blank* (target_chars+) blank* '=' { raise_notsup "macro definition" }
| '$' { raise_notsup "macro expansion" }
| _ as c { raise_invalid_char "line" c }

| eof { `EOF }

and command_line = parse
| '\t' (command_prefix as p)?
  { `COMMAND (p, command (Buffer.create 128) lexbuf) }

| blank* '\n'
  { `EOL }

| _ as c { raise_invalid_char "command_line" c }

| eof { `EOF }

and command buf = parse
| newline
  (* done parsing command *)
  { new_line lexbuf; Buffer.contents buf }

| eof
  (* done parsing command *)
  { Buffer.contents buf }

| (escape newline) as s
  (* escaped newline removes exactly one tab if present,
     and continues parsing
  *)
  { new_line lexbuf
  ; command_remove_tab lexbuf
  (* at the makefile level this is preserved and passed to the shell,
     which may remove it *)
  ; Buffer.add_string buf s
  ; command buf lexbuf
  }

| ((escape [^ '\n']) | [^'\\' '\n'])+ as s
  (* negation of above rules: continue parsing *)
  { Buffer.add_string buf s; command buf lexbuf }

and command_remove_tab = parse
| '\t'? { () }

and condense_whitespace = parse
| '\n' { new_line lexbuf; condense_whitespace lexbuf }
| space_nonl* { () }
| "include" blank+ { failwith "Include line following escaped newline" }
