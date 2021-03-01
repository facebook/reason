(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 *  Forked from OCaml, which is provided under the license below:
 *
 *  Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *
 *  Copyright © 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006 Inria
 *
 *  Permission is hereby granted, free of charge, to the Licensee obtaining a
 *  copy of this software and associated documentation files (the "Software"),
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense
 *  under any license of the Licensee's choice, and/or sell copies of the
 *  Software, subject to the following conditions:
 *
 *  1.	Redistributions of source code must retain the above copyright notice
 *  and the following disclaimer.
 *  2.	Redistributions in binary form must reproduce the above copyright
 *  notice, the following disclaimer in the documentation and/or other
 *  materials provided with the distribution.
 *  3.	All advertising materials mentioning features or use of the Software
 *  must display the following acknowledgement: This product includes all or
 *  parts of the Caml system developed by Inria and its contributors.
 *  4.	Other than specified in clause 3, neither the name of Inria nor the
 *  names of its contributors may be used to endorse or promote products
 *  derived from the Software without specific prior written permission.
 *
 *  Disclaimer
 *
 *  This software is provided by Inria and contributors “as is” and any express
 *  or implied warranties, including, but not limited to, the implied
 *  warranties of merchantability and fitness for a particular purpose are
 *  disclaimed. in no event shall Inria or its contributors be liable for any
 *  direct, indirect, incidental, special, exemplary, or consequential damages
 *  (including, but not limited to, procurement of substitute goods or
 *  services; loss of use, data, or profits; or business interruption) however
 *  caused and on any theory of liability, whether in contract, strict
 *  liability, or tort (including negligence or otherwise) arising in any way
 *  out of the use of this software, even if advised of the possibility of such
 *  damage.
 *
 *)

(* This is the Reason lexer. As stated in docs/GETTING_STARTED_CONTRIBUTING.md
 * there's a good section in Real World OCaml that describes what a lexer is:

  https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html

  Basically, it uses regular expressions to first cut the big code string into
  more meaningful chunks, called tokens. For example, it cuts the string

    let foo = 1

  into `let`, `foo`, `=` and `1`, massage them a bit into nice variants, then
  send the data structures into the parser `reason_parser.mly`, which takes
  care of the next step (turning the stream of tokens into an AST, abstract
  syntax tree).

  The file's syntax's a bit special. It's not the conventional OCaml syntax. An
  ordinary language syntax isn't expressive/convenient enough for a lexer. This
  mll ("ml lexer") syntax is a fine-tuned variation of the usual OCaml syntax.
 *)

{
open Lexing
open Reason_parser
open Reason_errors

(* The table of keywords *)

let keyword_table, reverse_keyword_table =
  let create_hashtable n l =
    let t = Hashtbl.create n in
    let rev_t = Hashtbl.create n in
    List.iter (fun (k, v) ->
      Hashtbl.add t k v;
      Hashtbl.add rev_t v k;
    ) l;
    t, rev_t
  in
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "esfun", ES6_FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "switch", SWITCH;
    "module", MODULE;
    "pub", PUB;
    "mutable", MUTABLE;
    "new", NEW;
    "nonrec", NONREC;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
    "pri", PRI;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor", INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
]

(* The only internal state of the lexer is two scratch buffers.
   They could be allocated everytime they are needed, but
   for better performance (FIXME: does this really matter?)
   they are preallocated.*)

type state = {
  raw_buffer : Buffer.t;
  txt_buffer : Buffer.t;
}

type string_template_parse_result =
  | TemplateTerminated
  | TemplateNotTerminated
  | TemplateInterpolationMarker

let get_scratch_buffers { raw_buffer; txt_buffer } =
  Buffer.reset raw_buffer;
  Buffer.reset txt_buffer;
  ( raw_buffer, txt_buffer )

let flush_buffer buffer =
  let result = Buffer.contents buffer in
  Buffer.reset buffer;
  result

let make () = {
  raw_buffer = Buffer.create 255;
  txt_buffer = Buffer.create 255;
}

(* Specialize raise_error for lexing errors *)

let raise_error loc error = raise_error (Lexing_error error) loc

let store_lexeme buffer lexbuf =
  Buffer.add_string buffer (Lexing.lexeme lexbuf)

(* To "unlex" a few characters *)
let set_lexeme_length buf n = (
  let open Lexing in
  if n < 0 then
    invalid_arg "set_lexeme_length: offset should be positive";
  if n > buf.lex_curr_pos - buf.lex_start_pos then
    invalid_arg "set_lexeme_length: offset larger than lexeme";
  buf.lex_curr_pos <- buf.lex_start_pos + n;
  buf.lex_curr_p <- {buf.lex_start_p
                     with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
)

(* This cut comment characters of the current buffer.
 * Operators (including "/*" and "//") are lexed with the same rule, and this
 * function cuts the lexeme at the beginning of an operator. *)
let lexeme_without_comment buf = (
  let lexeme = Lexing.lexeme buf in
  let i = ref 0 and len = String.length lexeme - 1 in
  let found = ref (-1) in
  while !i < len && !found = -1 do
    begin match lexeme.[!i], lexeme.[!i+1] with
      | ('/', '*') | ('/', '/') | ('*', '/') ->
        found := !i;
      | _ -> ()
    end;
    incr i
  done;
  match !found with
  | -1 -> lexeme
  | n ->
      set_lexeme_length buf n;
      String.sub lexeme 0 n
)

(* Operators that could conflict with comments (those containing /*, */ and //)
 * are escaped in the source. The lexer removes the escapes so that the
 * identifier looks like OCaml ones.
 * An escape in first position is kept to distinguish "verbatim" operators
 * (\=== for instance). *)
let unescape_operator str =
  if (str <> "" && String.contains_from str 1 '\\') then (
    let b = Buffer.create (String.length str) in
    Buffer.add_char b str.[0];
    for i = 1 to String.length str - 1 do
      let c = str.[i] in
      if c <> '\\' then Buffer.add_char b c
    done;
    Buffer.contents b
  ) else str

let lexeme_operator lexbuf =
  unescape_operator (lexeme_without_comment lexbuf)

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then (
    raise_error
        (Location.curr lexbuf)
        (Illegal_escape (Lexing.lexeme lexbuf));
    'x'
  ) else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0
                                                       (String.length s - 1)))

(* Remove underscores from float literals *)

let remove_underscores s =
  let l = String.length s in
  let b = Bytes.create l in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else Bytes.sub_string b 0 dst
    else
      match s.[src] with
        '_' -> remove (src + 1) dst
      |  c  -> Bytes.set b dst c; remove (src + 1) (dst + 1)
  in remove 0 0

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

}


let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let lowercase_no_under = ['a'-'z']
let uppercase = ['A'-'Z']
let uppercase_or_lowercase = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let lowercase_latin1 = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase_latin1 = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar_latin1 =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let operator_chars =
  ['!' '$' '%' '&' '+' '-' ':' '<' '=' '>' '?' '@' '^' '|' '~' '#' '.'] |
  ( '\\'? ['/' '*'] )
let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|' '\\' 'a'-'z' 'A'-'Z' '_' '0'-'9']
let kwdopchar = ['$' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|' '.' '!']

let decimal_literal = ['0'-'9'] ['0'-'9' '_']*

let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*

let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?

let literal_modifier = ['G'-'Z' 'g'-'z']

rule token state = parse
  | "\\" newline {
      raise_error
        (Location.curr lexbuf)
        (Illegal_character (Lexing.lexeme_char lexbuf 0));
      update_loc lexbuf None 1 false 0;
      token state lexbuf
    }
  | newline
    { update_loc lexbuf None 1 false 0;
      token state lexbuf
    }
  | blank +
    { token state lexbuf }
  | "_"
    { UNDERSCORE }
  | "~"
    { TILDE }
  | "?"
    { QUESTION }
  | "=?"
    { set_lexeme_length lexbuf 1; EQUAL }
  | lowercase identchar *
    { let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_table s
      with Not_found -> LIDENT s
    }
  | "`" (lowercase | uppercase) identchar *
    { let s = Lexing.lexeme lexbuf in
      let word = String.sub s 1 (String.length s - 1) in
      match Hashtbl.find keyword_table word with
      | exception Not_found -> NAMETAG word
      | _ ->
        raise_error (Location.curr lexbuf) (Keyword_as_tag word);
        LIDENT "thisIsABugReportThis"
    }
  | lowercase_latin1 identchar_latin1 *
    { Ocaml_util.warn_latin1 lexbuf; LIDENT (Lexing.lexeme lexbuf) }
  | uppercase identchar *
    { UIDENT(Lexing.lexeme lexbuf) }       (* No capitalized keywords *)
  | uppercase_latin1 identchar_latin1 *
    { Ocaml_util.warn_latin1 lexbuf; UIDENT(Lexing.lexeme lexbuf) }
  | int_literal
    { INT (Lexing.lexeme lexbuf, None) }
  | (int_literal as lit) (literal_modifier as modif)
    { INT (lit, Some modif) }
  | float_literal | hex_float_literal
    { FLOAT (Lexing.lexeme lexbuf, None) }
  | ((float_literal | hex_float_literal) as lit) (literal_modifier as modif)
    { FLOAT (lit, Some modif) }
  | ((float_literal | hex_float_literal) as lit) identchar+
    { raise_error
        (Location.curr lexbuf)
        (Invalid_literal (Lexing.lexeme lexbuf));
      FLOAT (lit, None)
    }
  | (int_literal as lit) identchar+
    { raise_error
        (Location.curr lexbuf)
        (Invalid_literal (Lexing.lexeme lexbuf));
      INT (lit, None)
    }
  | "\""
    { let string_start = lexbuf.lex_start_p in
      let start_loc = Location.curr lexbuf in
      let raw_buffer, txt_buffer = get_scratch_buffers state in
      if not (string raw_buffer (Some txt_buffer) lexbuf) then
        raise_error start_loc Unterminated_string;
      lexbuf.lex_start_p <- string_start;
      let txt = flush_buffer txt_buffer in
      let raw = flush_buffer raw_buffer in
      STRING (txt, Some raw, None)
    }
  | "{" (lowercase* as delim) "|"
    { let string_start = lexbuf.lex_start_p in
      let start_loc = Location.curr lexbuf in
      let raw_buffer, _ = get_scratch_buffers state in
      if not (quoted_string raw_buffer delim lexbuf) then
        raise_error start_loc Unterminated_string;
      lexbuf.lex_start_p <- string_start;
      let txt = flush_buffer raw_buffer in
      STRING (txt, None, Some delim)
    }
  | "`" newline
    {
      (* Need to update the location in the case of newline so line counts are
       * correct *)
      update_loc lexbuf None 1 false 0;
      token_in_template_string_region state lexbuf
    }
  | "`" (' ' | '\t')
    {
      token_in_template_string_region state lexbuf
    }
  | "'" newline "'"
    { (* newline can span multiple characters
         (if the newline starts with \13)
         Only the first one is returned, maybe we should warn? *)
      update_loc lexbuf None 1 false 1;
      CHAR (Lexing.lexeme_char lexbuf 1)
    }
  | "'" ([^ '\\' '\'' '\010' '\013'] as c) "'"
    { CHAR c }
  | "'\\" (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c) "'"
    { CHAR (char_for_backslash c) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { CHAR (char_for_decimal_code lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { CHAR (char_for_hexadecimal_code lexbuf 3) }
  | "'" (("\\" _) as esc)
    { raise_error (Location.curr lexbuf) (Illegal_escape esc);
      token state lexbuf
    }
  | "#=<"
    { (* Allow parsing of foo#=<bar /> *)
      set_lexeme_length lexbuf 2;
      SHARPEQUAL
    }
  | "#="
    { SHARPEQUAL }
  | "#" operator_chars+
    { SHARPOP (lexeme_operator lexbuf) }
  (* File name / line number source mapping # n string\n *)
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
    { update_loc lexbuf name (int_of_string num) true 0;
      token state lexbuf
    }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
  | "->" { MINUSGREATER }
  | "=>" { EQUALGREATER }
  (* allow lexing of | `Variant =><Component /> *)
  | "=><" uppercase_or_lowercase (identchar | '.') * {
    set_lexeme_length lexbuf 2;
    EQUALGREATER
  }
  | "#"  { SHARP }
  | "."  { DOT }
  | ".." { DOTDOT }
  | "..."{ DOTDOTDOT }
  | ":"  { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ":>" { COLONGREATER }
  | ";"  { SEMI }
  | ";;" { SEMISEMI }
  | "<"  { LESS }
  | "="  { EQUAL }
  | "["  { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "[>" { LBRACKETGREATER }
  | "<" (((uppercase identchar* '.')*
         (lowercase_no_under | lowercase identchar identchar*)) as tag)
    (* Parsing <_ helps resolve no conflicts in the parser and creates other
     * challenges with splitting up INFIXOP0 tokens (in Reason_parser_single)
     * so we don't do it. *)
    { LESSIDENT tag }
  | "<" ((uppercase identchar*) as tag)
    { LESSUIDENT tag }
  | ">..." { GREATERDOTDOTDOT }
  (* Allow parsing of Pexp_override:
   * let z = {<state: 0, x: y>};
   *
   * Make sure {<state is emitted as LBRACELESS.
   * This contrasts with jsx:
   * in a jsx context {<div needs to be LBRACE LESS (two tokens)
   * for a valid parse.
   *)
  | "{<" uppercase_or_lowercase identchar* blank*  ":"
    { set_lexeme_length lexbuf 2;
      LBRACELESS
    }
  | "{<" uppercase_or_lowercase (identchar | '.') *
    { (* allows parsing of `{<Text` in <Description term={<Text text="Age" />}>
         as correct jsx
       *)
      set_lexeme_length lexbuf 1;
      LBRACE
    }
  | "{<>" {
    set_lexeme_length lexbuf 1;
    LBRACE
  }
  | "{<>}" {
    set_lexeme_length lexbuf 2;
    LBRACELESS
  }
  | "</" blank* ((uppercase_or_lowercase (identchar|'.')* ) as tag) blank* ">"
    { LESSSLASHIDENTGREATER tag }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "{<" { LBRACELESS }
  | "|"  { BAR }
  | "||" { BARBAR }
  | "|]" { BARRBRACKET }
  | ">"  { GREATER }
  (* Having a GREATERRBRACKET makes it difficult to parse patterns such
     as > ]. The space in between then becomes significant and must be
     maintained when printing etc. >] isn't even needed!
  | ">]" { GREATERRBRACKET }
  *)
  | "}"  { RBRACE }
  | ">}" { GREATERRBRACE }
  | "=<" uppercase_or_lowercase+
    { (* allow `let x=<div />;` *)
      set_lexeme_length lexbuf 1;
      EQUAL
    }
  | "/>|]"
    { (* jsx in arrays: [|<div />|]*)
      set_lexeme_length lexbuf 2;
      SLASHGREATER
    }
  | "[|<"
    { set_lexeme_length lexbuf 2;
      (* TODO: See if decompose_token in Reason_single_parser.ml would work better for this *)
      LBRACKETBAR
    }
    (* allow parsing of <div /></Component> *)
  | "/></" uppercase_or_lowercase+
    { (* allow parsing of <div asd=1></div> *)
      (* TODO: See if decompose_token in Reason_single_parser.ml would work better for this *)
      set_lexeme_length lexbuf 2;
      SLASHGREATER
    }
  | "></" uppercase_or_lowercase+
    { (* allow parsing of <div asd=1></div> *)
      (* TODO: See if decompose_token in Reason_single_parser.ml would work better for this *)
      set_lexeme_length lexbuf 1;
      GREATER
    }
  | "><" uppercase_or_lowercase+
    { (* allow parsing of <div><span> *)
      (* TODO: See if decompose_token in Reason_single_parser.ml would work better for this *)
      set_lexeme_length lexbuf 1;
      GREATER
    }
  | "[@reason.version " (['0'-'9']+ as major) '.' (['0'-'9']+ as minor) (('.' ['0'-'9']+)? as _patch) ']' {
    (* Special case parsing of attribute so that we can special case its
     * parsing. Parses x.y.z even though it is not valid syntax otherwise -
     * just gracefully remove the last number. The parser will ignore this
     * attribute when parsed, and instead record its presence, and then inject
     * the attribute into the footer of the file.  Then the printer will ensure
     * it is formatted at the top of the file, ideally after the first file
     * floating doc comment. *)
    VERSION_ATTRIBUTE (int_of_string major, int_of_string minor)
  }
  | "[@" { LBRACKETAT }
  | "[%" { LBRACKETPERCENT }
  | "[%%" { LBRACKETPERCENTPERCENT }
  | "!"  { BANG }
  | "!=" { INFIXOP0 "!=" }
  | "!==" { INFIXOP0 "!==" }
  | "\\!=" { INFIXOP0 "!=" }
  | "\\!==" { INFIXOP0 "!==" }
  | "+"  { PLUS }
  | "+." { PLUSDOT }
  | "+=" { PLUSEQ }
  | "-"  { MINUS }
  | "-." { MINUSDOT }
  | "<>" { LESSGREATER }
  | "</>" { LESSSLASHGREATER }
  | "<..>" { LESSDOTDOTGREATER }
  | '\\'? ['~' '?' '!'] operator_chars+
    { PREFIXOP (lexeme_operator lexbuf) }
  | '\\'? ['=' '<' '>' '|' '&' '$'] operator_chars*
    {
      (* See decompose_token in Reason_single_parser.ml for how let `x=-1` is lexed
       * and broken up into multiple tokens when necessary. *)
      INFIXOP0 (lexeme_operator lexbuf)
    }
  | '\\'? '@' operator_chars*
    { INFIXOP1 (lexeme_operator lexbuf) }
  | '\\'? '^' ('\\' '.')? operator_chars*
    { match lexeme_without_comment lexbuf with
      | "^." | "^|" ->
        (* ^| is not an infix op in [|a^|] *)
        set_lexeme_length lexbuf
          (if Lexing.lexeme_char lexbuf 0 = '\\' then 2 else 1);
        POSTFIXOP "^"
      | "^" -> POSTFIXOP "^"
      | op -> INFIXOP1 (unescape_operator op)
    }
  | "++" operator_chars*
    { INFIXOP1 (lexeme_operator lexbuf) }
  | '\\'? ['+' '-'] operator_chars*
    { INFIXOP2 (lexeme_operator lexbuf) }
  (* SLASHGREATER is an INFIXOP3 that is handled specially *)
  | "/>" { SLASHGREATER }
  (* The second star must be escaped so that the precedence assumptions for
   * printing match those of parsing. (Imagine what could happen if the other
   * rule beginning with * picked up */*, and we internally escaped it to **.
   * Whe printing, we have an understanding of the precedence of "**", which
   * enables us to safely print/group it, but that understanding would not
   * match the *actual* precedence that it was parsed at thanks to the *other*
   * rule beginning with *, picking it up instead of the special double ** rule
   * below.
   *)
  | '\\'? '*' '\\'? '*' operator_chars*
    { INFIXOP4 (lexeme_operator lexbuf) }
  | '%' { PERCENT }
  | '\\'? ['/' '*'] operator_chars*
    { match lexeme_operator lexbuf with
      | "" ->
          (* If the operator is empty, it means the lexeme is beginning
           * by a comment sequence: we let the comment lexer handle
           * the case. *)
          enter_comment state lexbuf
      | op -> INFIXOP3 op }
  | '%' operator_chars*
    { INFIXOP3 (lexeme_operator lexbuf) }
  | "let" kwdopchar dotsymbolchar *
    { LETOP (Reason_syntax_util.expand_letop_identifier (lexeme_operator lexbuf)) }
  | "and" kwdopchar dotsymbolchar *
    { ANDOP (Reason_syntax_util.expand_letop_identifier (lexeme_operator lexbuf)) }
  | eof { EOF }
  | _
    { raise_error
        (Location.curr lexbuf)
        (Illegal_character (Lexing.lexeme_char lexbuf 0));
      token state lexbuf
    }

and enter_comment state = parse
  | "//" ([^'\010']* newline as line)
    { update_loc lexbuf None 1 false 0;
      let physical_loc = Location.curr lexbuf in
      let location = { physical_loc with
        loc_end = { physical_loc.loc_end with
          (* Don't track trailing `\n` in the location
           * 1| // comment
           * 2| let x = 1;
           * By omitting the `\n` at the end of line 1, the location of the
           * comment spans line 1. Otherwise the comment on line 1 would end
           * on the second line. The printer looks at the closing pos_lnum
           * location to interleave whitespace correct. It needs to align
           * with what we visually see (i.e. it ends on line 1) *)
          pos_lnum = physical_loc.loc_end.pos_lnum - 1;
          pos_cnum = physical_loc.loc_end.pos_cnum + 1;
      }} in
      COMMENT (line, location)
    }
  | "//" ([^'\010']* eof as line)
    { update_loc lexbuf None 1 false 0;
      let physical_loc = Location.curr lexbuf in
      let location = { physical_loc with
        loc_end = { physical_loc.loc_end with
          pos_lnum = physical_loc.loc_end.pos_lnum - 1;
          pos_cnum = physical_loc.loc_end.pos_cnum + 1;
      }} in
      COMMENT (line, location)
    }
  | "/*" ("*" "*"+)?
    { set_lexeme_length lexbuf 2;
      let loc = Location.curr lexbuf in
      let raw_buffer, _ = get_scratch_buffers state in
      ignore (comment raw_buffer loc loc lexbuf : bool);
      lexbuf.Lexing.lex_start_p <- loc.Location.loc_start;
      let loc_end = lexbuf.Lexing.lex_curr_p in
      COMMENT (flush_buffer raw_buffer,
               {loc with Location.loc_end})
    }
  | "/**"
    { let loc = Location.curr lexbuf in
      let raw_buffer, _ = get_scratch_buffers state in
      ignore (comment raw_buffer loc loc lexbuf : bool);
      lexbuf.Lexing.lex_start_p <- loc.Location.loc_start;
      DOCSTRING (flush_buffer raw_buffer)
    }
  | "/**/"
    { DOCSTRING "" }
  | "/*/"
    { let loc = Location.curr lexbuf in
      Location.prerr_warning loc Warnings.Comment_start;
      let raw_buffer, _ = get_scratch_buffers state in
      ignore (comment raw_buffer loc loc lexbuf : bool);
      let loc_end = lexbuf.Lexing.lex_curr_p in
      COMMENT (flush_buffer raw_buffer,
               {loc with Location.loc_end})
    }
  | "*/"
    { let loc = Location.curr lexbuf in
      Location.prerr_warning loc Warnings.Comment_not_end;
      set_lexeme_length lexbuf 1;
      STAR
    }
  | _ { assert false }

(** [comment buffer locs lexbuf] will lex a comment from [lexbuf] and
   stores its raw text in [buffer], without the /* and */ delimiters.
   [locs] is a non-empty list of locations that saves the beginning
   position of each nested comments.
 *)
and comment buffer firstloc nestedloc = parse
  | "/*"
    { store_lexeme buffer lexbuf;
      if comment buffer firstloc (Location.curr lexbuf) lexbuf then (
        store_lexeme buffer lexbuf;
        comment buffer firstloc nestedloc lexbuf
      )
      else
        false
    }
  | "*/"
    { true }
  | "\""
    { Buffer.add_char buffer '"';
      let string_start = Location.curr lexbuf in
      let terminated_string = string buffer None lexbuf in
      Buffer.add_char buffer '"';
      if terminated_string then
        comment buffer firstloc nestedloc lexbuf
      else (
        raise_error nestedloc
          (Unterminated_string_in_comment (firstloc, string_start));
        false
      )
    }
  | "{" (lowercase* as delim) "|"
    { store_lexeme buffer lexbuf;
      let stringloc = Location.curr lexbuf in
      let terminated_string = quoted_string buffer delim lexbuf in
      Buffer.add_char buffer '|';
      Buffer.add_string buffer delim;
      Buffer.add_char buffer '}';
      if terminated_string then
        comment buffer firstloc nestedloc lexbuf
      else (
        raise_error nestedloc
          (Unterminated_string_in_comment (firstloc, stringloc));
        false
      )
    }
  | "''"
    { store_lexeme buffer lexbuf;
      comment buffer firstloc nestedloc lexbuf
    }

  | "'" newline "'"
    { store_lexeme buffer lexbuf;
      update_loc lexbuf None 1 false 1;
      comment buffer firstloc nestedloc lexbuf
    }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { store_lexeme buffer lexbuf;
      comment buffer firstloc nestedloc lexbuf
    }
  | eof
    { raise_error nestedloc (Unterminated_comment firstloc);
      false
    }
  | newline
    { store_lexeme buffer lexbuf;
      update_loc lexbuf None 1 false 0;
      comment buffer firstloc nestedloc lexbuf
    }
  | _
    { store_lexeme buffer lexbuf;
      comment buffer firstloc nestedloc lexbuf
    }

(** [string rawbuf txtbuf lexbuf] parses a string from [lexbuf].
    The string contents is stored in two buffers:
    - [rawbuf] for the text as it literally appear in the source
    - [txtbuf] for the processed, unescaped contents.
    [txtbuf] is optional. If it is omitted, contents is not unescaped.
    The call returns [true] iff the string was properly terminated.
    It does not register an error if the string is unterminated, this
    is the responsibility of the caller.
 *)
and string rawbuf txtbuf = parse
  | '"'
    { true }
  | '\\' newline ([' ' '\t'] * as space)
    { store_lexeme rawbuf lexbuf;
      update_loc lexbuf None 1 false (String.length space);
      string rawbuf txtbuf lexbuf
    }
  | '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
    { store_lexeme rawbuf lexbuf;
      begin match txtbuf with
      | None -> ()
      | Some buf -> Buffer.add_char buf (char_for_backslash c);
      end;
      string rawbuf txtbuf lexbuf
    }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    { store_lexeme rawbuf lexbuf;
      begin match txtbuf with
      | None -> ()
      | Some buf -> Buffer.add_char buf (char_for_decimal_code lexbuf 1);
      end;
      string rawbuf txtbuf lexbuf
    }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
    { store_lexeme rawbuf lexbuf;
      begin match txtbuf with
      | None -> ()
      | Some buf -> Buffer.add_char buf (char_for_hexadecimal_code lexbuf 2);
      end;
      string rawbuf txtbuf lexbuf
    }
  | '\\' _
    { store_lexeme rawbuf lexbuf;
      begin match txtbuf with
      | None -> ()
      | Some buf ->
         store_lexeme buf lexbuf;
         (*  FIXME: Warnings should probably go in Reason_errors
            Should be an error, but we are very lax.
              raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
                        Location.curr lexbuf))
           FIXME Using Location relies too much on compiler internals
          *)
         Location.prerr_warning (Location.curr lexbuf)
           Warnings.Illegal_backslash;
      end;
      string rawbuf txtbuf lexbuf
    }
  | newline
    { store_lexeme rawbuf lexbuf;
      begin match txtbuf with
      | None -> ()
      | Some buf ->
        store_lexeme buf lexbuf;
        Location.prerr_warning (Location.curr lexbuf)
          Warnings.Eol_in_string
      end;
      update_loc lexbuf None 1 false 0;
      string rawbuf txtbuf lexbuf
    }
  | eof
    { false }
  | _
    { store_lexeme rawbuf lexbuf;
      begin match txtbuf with
      | None -> ()
      | Some buf -> Buffer.add_char buf (Lexing.lexeme_char lexbuf 0);
      end;
      string rawbuf txtbuf lexbuf
    }

(** [quoted_string buffer delim lexbuf] parses a quoted string
    delimited by [delim] from [lexbuf] and stores the literal text in
    [buffer].
    It returns:
    - true if the string was properly delimited and
    - false if EOF was reached before finding "|delim}".
    It does not register an error if the string is unterminated, this
    is the responsibility of the caller.
 *)
and quoted_string buffer delim = parse
  | newline
    { store_lexeme buffer lexbuf;
      update_loc lexbuf None 1 false 0;
      quoted_string buffer delim lexbuf
    }
  | eof
    { false }
  | "|" (lowercase* as edelim) "}"
    { if delim = edelim then
        true
      else (
        store_lexeme buffer lexbuf;
        quoted_string buffer delim lexbuf
      )
    }
  | _ as c
    { Buffer.add_char buffer c;
      quoted_string buffer delim lexbuf
    }

and template_string_region buffer = parse
  | newline
    { store_lexeme buffer lexbuf;
      update_loc lexbuf None 1 false 0;
      template_string_region buffer lexbuf
    }
  | eof
    { TemplateNotTerminated }
  | "`"
    {
      TemplateTerminated
    }
  | "${"
    {
      (* set_lexeme_length lexbuf 1; *)
      TemplateInterpolationMarker
    }
  | '\\' '`'
    {
      Buffer.add_char buffer '`';
      template_string_region buffer lexbuf
    }
  | '\\' '$' '{'
    {
      Buffer.add_char buffer '$';
      Buffer.add_char buffer '{';
      template_string_region buffer lexbuf
    }
  | _ as c
    {
      Buffer.add_char buffer c;
      template_string_region buffer lexbuf
    }


and token_in_template_string_region state = parse
  | _
    {
      (* Unparse it, now go run the template string parser with a buffer *)
      set_lexeme_length lexbuf 0;
      let string_start = lexbuf.lex_start_p in
      let start_loc = Location.curr lexbuf in
      let raw_buffer, _ = get_scratch_buffers state in
      match template_string_region raw_buffer lexbuf with
      | TemplateNotTerminated -> raise_error start_loc Unterminated_string;
        STRING_TEMPLATE_TERMINATED
          "This should never be happen. If you see this string anywhere, file a bug to the Reason repo."
      | TemplateTerminated ->
        lexbuf.lex_start_p <- string_start;
        let txt = flush_buffer raw_buffer in
        STRING_TEMPLATE_TERMINATED txt
      | TemplateInterpolationMarker ->
        lexbuf.lex_start_p <- string_start;
        let txt = flush_buffer raw_buffer in
        STRING_TEMPLATE_SEGMENT_LBRACE txt
    }


and skip_sharp_bang = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
       { update_loc lexbuf None 3 false 0 }
  | "#!" [^ '\n']* '\n'
       { update_loc lexbuf None 1 false 0 }
  | "" { () }
