open Makefile

let pp_escaped_newline_break use_tabs count ppf () =
  let fits = ("", 1, "") (* if it fits just a single space *)
  and breaks =
    (* escape the line break and then print [spaces] or [tabs] as needed *)
    if use_tabs then ("\\", 0, String.make count '\t') else ("\\", count, "")
  in
  Format.pp_print_custom_break ppf ~fits ~breaks

(* use these only to break long lines, not for breaking lines where the syntax
   demands it - such as after targets or a command *)
let sep_space = pp_escaped_newline_break false 2
let sep_tab = pp_escaped_newline_break true 1
let pp_target ppf (Target t) = Format.pp_print_string ppf t
let pp_prerequisite ppf (Prerequisite p) = Format.pp_print_string ppf p
let pp_targets = Format.pp_print_list ~pp_sep:sep_space pp_target
let pp_prerequisites = Format.pp_print_list ~pp_sep:sep_space pp_prerequisite

let rec should_quote s i =
  if i = String.length s then false
  else
    (* See https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_02 *)
    match s.[i] with
    | '|' | '&' | ';' | '<' | '>' | '(' | ')' | '$' | '`' | '\\' | '"' | ' '
    | '\t' | '\n' | '*' | '?' | '[' | '#' | '~' | '=' | '%' | ']' ->
        true (* ] is not required to be quoted, but for symmetry with [ *)
    | _ -> should_quote s (i + 1)

let pp_token ppf = function
  | ShellLexer.OPERATOR ";" ->
      (* Output operator, split the line
         by closing the inner box and printing a break hint
         (the outer box is a vertical box, so will always break)
         Then open another box for the next single command
         tab needs to be inside the box to avoid Format from inserting space
         indents before the tab
      *)
      Format.fprintf ppf ";@]\\@,@[<hov>\t"
  | OPERATOR op -> Format.pp_print_string ppf op
  | TOKEN t ->
      assert (not (String.contains t '\n'));
      let s = if should_quote t 0 then Filename.quote t else t in
      Format.pp_print_string ppf s

let pp_command ppf (Command tokens) =
  (* all printing boxes use 0 additional indent:
     we specify the desired amount of indent in the custom break printers above
     otherwise spaces and tabs would be in wrong order
     (format would indent with spaces from enclosing box first)

     '@[<hov>\t' here must match the one used in [pp_token]
  *)
  Format.fprintf ppf "@[<v>@[<hov>\t%a@]@]"
    (Format.pp_print_list ~pp_sep:sep_tab pp_token)
    tokens

let pp_vertical pp_el ppf =
  (* Printing each command will force a line break, it is a vertical box.
     This is an unescaped newline, do not use sep_tab here,
     the tab is printed by [pp_command].
  *)
  Format.fprintf ppf "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_el)

let pp_rule ppf rule =
  Format.fprintf ppf "@[<hov>%a:%a%a@]@,%a@," pp_targets rule.targets sep_space
    () pp_prerequisites rule.prerequisites (pp_vertical pp_command)
    rule.commands

let printf ch t =
  let fmt = Format.formatter_of_out_channel ch in
  pp_vertical pp_rule fmt t;
  Format.pp_print_newline fmt ()
