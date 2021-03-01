(**
 * This module should include utilities for parsing and printing template
 * strings, but should not have any dependencies on any printing framework
 * (like Easy_format or Reason_layout). For that, make another module.  This
 * file should be shared between the printer and the parser, so avoiding
 * dependencies on printing frameworks, makes it easy to bundle just the parser
 * if necessary.
 *)

module Parse : sig
  val normalize_or_remove_last_line: string list -> int * string list

  val strip_leading_for_non_last: indent:int -> string -> string list -> string
end

module Print : sig
  val escape_string_template : string -> string
  val is_template_style : Reason_migrate_parsetree.Ast_408.Parsetree.attribute list -> bool
end
