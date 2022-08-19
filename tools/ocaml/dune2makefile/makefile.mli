type target = Target of string
type prerequisite = Prerequisite of string
type command = Command of ShellLexer.t list

type rule = {
  targets : target list;
  prerequisites : prerequisite list;
  commands : command list;
}

type t = rule list
