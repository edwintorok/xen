(* Parse shell commands created by dune rules --makefile

  Shell tokenization is described in
  https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_03
  and
  https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_10_02

  We only implement the tokenization rules needed to recognize Dune's output.
  The more complicated / corner case situations are explicitly rejected.
  (If you need parsing those use an opam library like morbig/morsmall instead)
*)

{
  type t =
  | TOKEN of string
  | OPERATOR of string
}

(*
  POSIX character classes in the POSIX locale according to
  https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap07.html#tag_07_03_01
*)
let blank = [' ' '\t']

let multi_operator = "&&" | "||" | ";;" | ">>" | "<&" | ">&" | "<>" | ">|"
let single_operator = ['|' '(' ')' '<' '>' '&' ';']
let iohere_operator = "<<" | "<<-"

(* We require all tokens to be blank delimited,
   and we don't allow escape newline in the middle of a token.
   So we don't need to keep track of the previous character
*)
rule token = parse
| iohere_operator
  (* not used in dune output currently *)
  { failwith "Here documents are not supported" }

| eof
  (* rule 1, there is no current token *)
  { None }

| multi_operator as o
  (* rule 2 *)
  { Some (OPERATOR o) }

| single_operator as o
  (* rule 3 in limited form: previous token must've been blank delimited *)
  { Some (OPERATOR (String.make 1 o)) }

| '\\' '\n'
  (* rule 4 and section 2.2.1: not replaced by whitespace, just removed *)
  { token lexbuf }

| '\\'
  (* rule 4 blackslash *)
  { failwith "generic escaping not implemented" }

| '\'' ([^ '\'' '\n']* as s) '\''
  (* rule 4 single-quote,
     but we don't support mixing quoted and unquoted in a single token *)
  { Some (TOKEN s) }

| '"'
  (* rule 4 double-quote *)
  { failwith "double quotes not implemented"}

| '$' | '`'
  (* rule 5 *)
  { failwith "shell expansion not implemented" }

  (* rule 6 not supported: require tokens to be blank separated *)

| blank+
  (* rule 7 *)
  { token lexbuf }

| '#'
  (* rule  9 *)
  { failwith "comments not supported" }

| ([^ '\n' '\\' '\'' '"' '$' '`' '#' ] # blank # single_operator)+ as t
  (* above rules start chars negated, ocamllex uses longest match, so we can't use _ here *)
  (* rule 8 and 10 *)
  { Some (TOKEN t) }

| ([^ '\n'] # blank # single_operator)+ as fail
  (* tokens are delimited by:
     rule 7: blanks
     rule 6: operators
  *)
  { failwith ("Unsupported syntax: " ^ fail) }
