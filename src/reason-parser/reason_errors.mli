(* There are three main categories of error:
   - _lexer errors_, thrown by Reason_lexer when the source **text is malformed**
     and no token can be produced
   - _concrete parsing errors_, thrown by the menhir parser / parsing loop
     when a **token is unexpected**
   - _abstract parsing errors_, thrown by hand-written semantic actions or
     further AST checks, when the source text was incorrect but this restriction
     was too fine to be captured by the grammar rules
*)

open Reason_migrate_parsetree.Ast_408

type lexing_error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Keyword_as_tag of string
  | Literal_overflow of string
  | Invalid_literal of string

type ast_error =
  | Not_expecting of Location.t * string
  | Other_syntax_error of string
  | Variable_in_scope of Location.t * string
  | Applicative_path of Location.t

type parsing_error = string

type reason_error =
  | Lexing_error of lexing_error
  | Parsing_error of parsing_error
  | Ast_error of ast_error

exception Reason_error of reason_error * Location.t

val raise_error : reason_error -> Location.t -> unit
val raise_fatal_error : reason_error -> Location.t -> 'a

val recover_non_fatal_errors : (unit -> 'a) ->
  ('a, exn) Result.result * (reason_error * Location.t) list

val recover_parser_error :
  (Location.t -> string -> 'a) -> Location.t -> string -> 'a

val report_error : Format.formatter -> loc:Location.t -> reason_error -> unit

val error_extension_node_from_recovery :
  Location.t -> string -> string Location.loc * Parsetree.payload

val error_extension_node :
  Location.t -> string -> string Location.loc * Parsetree.payload

