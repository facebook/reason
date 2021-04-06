(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

(* TODO more fine-grained precedence pretty-printing *)

module Easy_format = Vendored_easy_format

open Reason_migrate_parsetree
open Ast_408
open Asttypes
open Location
open Longident
open Parsetree
open Easy_format
open Reason_syntax_util
open Reason_attributes

module Comment = Reason_comment
module Layout = Reason_layout
module WhitespaceRegion = Layout.WhitespaceRegion
module Range = Reason_location.Range

let source_map = Layout.source_map

exception NotPossible of string

let commaTrail = Layout.SepFinal (",", Reason_syntax_util.TrailingCommaMarker.string)
let commaSep = Layout.Sep (",")

type ruleInfoData = {
  reducePrecedence: precedence;
  shiftPrecedence: precedence;
}

and ruleCategory =
  (* Printing will be parsed with very high precedence, so not much need to
     worry about ensuring it will reduce correctly. In short, you can put
     `FunctionApplication` content anywhere around an infix identifier without
     wrapping in parens. For example `myFunc x y z` or `if x {y} else {z}`
     The layout is kept in list form only to allow for elegant wrapping rules
     to take into consideration the *number* of high precedence parsed items. *)
  | FunctionApplication of Layout.t list
  (* Care should be taken to ensure the rule that caused it to be parsed will
     reduce again on the printed output - context should carefully consider
     wrapping in parens according to the ruleInfoData. *)
  | SpecificInfixPrecedence of ruleInfoData * resolvedRule
  (* Not safe to include anywhere between infix operators without wrapping in
     parens. This describes expressions like `fun x => x` which doesn't fit into
     our simplistic algorithm for printing function applications separated by infix.

     It might be possible to include these in between infix, but there are
     tricky rules to determining when these must be guarded by parens (it
     depends highly on context that is hard to reason about). It's so nuanced
     that it's easier just to always wrap them in parens.  *)
  | PotentiallyLowPrecedence of Layout.t
  (* Simple means it is clearly one token (such as (anything) or [anything] or identifier *)
  | Simple of Layout.t

(* Represents a ruleCategory where the precedence has been resolved.
 * The precedence of a ruleCategory gets resolved in `ensureExpression` or
 * `ensureContainingRule`. The result is either a plain Layout.t (where
 * parens probably have been applied) or an InfixTree containing the operator and
 * a left & right resolvedRule. The latter indicates that the precedence has been resolved,
 * but the actual formatting is deferred to a later stadium.
 * Think `let x = foo |> f |> z |>`, which requires a certain formatting style when
 * things break over multiple lines. *)
and resolvedRule =
  | LayoutNode of Layout.t
  | InfixTree of string * resolvedRule * resolvedRule

and associativity =
  | Right
  | Nonassoc
  | Left

and precedenceEntryType =
  | TokenPrecedence
  | CustomPrecedence

and precedence =
  | Token of string
  | Custom of string

(* Describes the "fixity" of a token, and stores its *printed* representation
   should it be rendered as infix/prefix (This rendering may be different than
   how it is stored in the AST). *)
and tokenFixity =
  (* Such as !simple_expr and ~!simple_expr. These function applications are
     considered *almost* "simple" because they may be allowed anywhere a simple
     expression is accepted, except for when on the left hand side of a
     dot/send. *)
  | AlmostSimplePrefix of string
  | UnaryPlusPrefix of string
  | UnaryMinusPrefix of string
  | UnaryNotPrefix of string
  | UnaryPostfix of string
  | Infix of string
  | Letop of string
  | Andop of string
  | Normal

(* Type which represents a resolvedRule's InfixTree flattened *)
type infixChain =
  | InfixToken of string
  | Layout of Layout.t

(* Helpers for dealing with extension nodes (%expr) *)

let expression_extension_sugar x =
  if x.pexp_attributes != [] then None
  else match x.pexp_desc with
    | Pexp_extension (name, PStr [{pstr_desc = Pstr_eval(expr, [])}])
      when name.txt <> "bs.obj" ->
      Some (name, expr)
    | _ -> None

let expression_immediate_extension_sugar x =
  match expression_extension_sugar x with
  | None -> (None, x)
  | Some (name, expr) ->
    match expr.pexp_desc with
    | Pexp_for _ | Pexp_while _ | Pexp_ifthenelse _
    | Pexp_function _ | Pexp_newtype _
    | Pexp_try _ | Pexp_match _ ->
      (Some name, expr)
    | _ -> (None, x)

let expression_not_immediate_extension_sugar x =
  match expression_immediate_extension_sugar x with
  | (Some _, _) -> None
  | (None, _) -> expression_extension_sugar x

let add_extension_sugar keyword = function
  | None -> keyword
  | Some str -> keyword ^ "%" ^ str.txt

let string_equal : string -> string -> bool = (=)

let string_loc_equal: string Ast_408.Asttypes.loc -> string Ast_408.Asttypes.loc -> bool =
  fun l1 l2 -> l1.txt = l2.txt

let longident_same l1 l2 =
  let rec equal l1 l2 =
    match l1, l2 with
    | Lident l1, Lident l2 -> string_equal l1 l2
    | Ldot (path1, l1), Ldot (path2, l2) ->
      equal path1 path2 && string_equal l1 l2
    | Lapply (l11, l12), Lapply (l21, l22) ->
      equal l11 l21 && equal l12 l22
    | _ -> false
  in
  equal l1.txt l2.txt

(* A variant of List.for_all2 that returns false instead of failing on lists
   of different size *)
let for_all2' pred l1 l2 =
  List.length l1 = List.length l2 &&
  List.for_all2 pred l1 l2

(*
   Checks to see if two types are the same modulo the process of varification
   which turns abstract types into type variables of the same name.
   For example, [same_ast_modulo_varification] would consider (a => b) and ('a
   => 'b) to have the same ast. This is useful in recovering syntactic sugar
   for explicit polymorphic types with locally abstract types.

   Does not compare attributes, or extensions intentionally.

   TODO: This has one more issue: We need to compare only accepting t1's type
   variables, to be considered compatible with t2's type constructors - not the
   other way around.
 *)
let same_ast_modulo_varification_and_extensions t1 t2 =
  let rec loop t1 t2 = match (t1.ptyp_desc, t2.ptyp_desc) with
    (* Importantly, cover the case where type constructors (of the form [a])
       are converted to type vars of the form ['a].
     *)
    | (Ptyp_constr({txt=Lident s1}, []), Ptyp_var s2) -> string_equal s1 s2
    (* Now cover the case where type variables (of the form ['a]) are
       converted to type constructors of the form [a].
     *)
    | (Ptyp_var s1, Ptyp_constr({txt=Lident s2}, [])) -> string_equal s1 s2
    (* Now cover the typical case *)
    | (Ptyp_constr(longident1, lst1), Ptyp_constr(longident2, lst2))  ->
      longident_same longident1 longident2 &&
      for_all2' loop lst1 lst2
    | (Ptyp_any, Ptyp_any) -> true
    | (Ptyp_var x1, Ptyp_var x2) -> string_equal x1 x2
    | (Ptyp_arrow (label1, core_type1, core_type1'), Ptyp_arrow (label2, core_type2, core_type2')) ->
      begin
         match label1, label2 with
         | Nolabel, Nolabel -> true
         | Labelled s1, Labelled s2 -> string_equal s1 s2
         | Optional s1, Optional s2 -> string_equal s1 s2
         | _ -> false
      end &&
      loop core_type1 core_type2 &&
      loop core_type1' core_type2'
    | (Ptyp_tuple lst1, Ptyp_tuple lst2) -> for_all2' loop lst1 lst2
    | (Ptyp_object (lst1, o1), Ptyp_object (lst2, o2)) ->
      let tester = fun t1 t2 ->
        match t1.pof_desc, t2.pof_desc with
        | Otag (s1, t1), Otag (s2, t2) ->
          string_equal s1.txt s2.txt &&
          loop t1 t2
        | Oinherit t1, Oinherit t2 -> loop t1 t2
        | _ -> false
      in
      for_all2' tester lst1 lst2 && o1 = o2
    | (Ptyp_class (longident1, lst1), Ptyp_class (longident2, lst2)) ->
      longident_same longident1 longident2 &&
      for_all2' loop lst1 lst2
    | (Ptyp_alias(core_type1, string1), Ptyp_alias(core_type2, string2)) ->
      loop core_type1 core_type2 &&
      string_equal string1 string2
    | (Ptyp_variant(row_field_list1, flag1, lbl_lst_option1), Ptyp_variant(row_field_list2, flag2, lbl_lst_option2)) ->
      for_all2' rowFieldEqual row_field_list1 row_field_list2 &&
      flag1 = flag2 &&
      lbl_lst_option1 = lbl_lst_option2
    | (Ptyp_poly (string_lst1, core_type1), Ptyp_poly (string_lst2, core_type2))->
      for_all2' string_loc_equal string_lst1 string_lst2 &&
      loop core_type1 core_type2
    | (Ptyp_package(longident1, lst1), Ptyp_package (longident2, lst2)) ->
      longident_same longident1 longident2 &&
      for_all2' testPackageType lst1 lst2
    | (Ptyp_extension (s1, _), Ptyp_extension (s2, _)) ->
      string_equal s1.txt s2.txt
    | _ -> false
  and testPackageType (lblLongIdent1, ct1) (lblLongIdent2, ct2) =
    longident_same lblLongIdent1 lblLongIdent2 &&
    loop ct1 ct2
  and rowFieldEqual f1 f2 = match (f1.prf_desc, f2.prf_desc) with
    | ((Rtag(label1, flag1, lst1)), (Rtag (label2, flag2, lst2))) ->
      string_equal label1.txt label2.txt &&
      flag1 = flag2 &&
      for_all2' loop lst1 lst2
    | (Rinherit t1, Rinherit t2) -> loop t1 t2
    | _ -> false
  in
  loop t1 t2

let expandLocation pos ~expand:(startPos, endPos) =
  { pos with
    loc_start = {
      pos.loc_start with
        Lexing.pos_cnum = pos.loc_start.Lexing.pos_cnum + startPos
    };
    loc_end = {
      pos.loc_end with
        Lexing.pos_cnum = pos.loc_end.Lexing.pos_cnum + endPos
    }
  }

(* Computes the location of the attribute with the lowest line number
 * that isn't ghost. Useful to determine the start location of an item
 * in the parsetree that has attributes.
 * If there are no valid attributes, defaults to the passed location.
 * 1| [@attr]           --> notice how the "start" is determined
 * 2| let f = ...           by the attr on line 1, not the lnum of the `let`
 *)
let rec firstAttrLoc loc = function
  | ({ attr_name = attrLoc; _} : Ast_408.Parsetree.attribute) ::attrs ->
      if attrLoc.loc.loc_start.pos_lnum < loc.loc_start.pos_lnum
         && not attrLoc.loc.loc_ghost
      then
        firstAttrLoc attrLoc.loc attrs
      else
        firstAttrLoc loc attrs
  | [] -> loc

let extractLocationFromValBindList expr vbs =
  let rec extract loc = function
    | x::xs ->
        let {pvb_expr} = x in
        let loc = {loc with loc_end = pvb_expr.pexp_loc.loc_end} in
        extract loc xs
    | [] -> loc
  in
  let loc = match vbs with
    | x::xs ->
        let {pvb_pat; pvb_expr} = x in
        let loc = {pvb_pat.ppat_loc with loc_end = pvb_expr.pexp_loc.loc_end} in
        extract loc xs
    | [] -> expr.pexp_loc
  in
  { loc with loc_start = expr.pexp_loc.loc_start }

let extractLocValBinding {pvb_pat; pvb_expr; pvb_attributes;} =
  let estimatedLoc = firstAttrLoc pvb_pat.ppat_loc pvb_attributes in
  {estimatedLoc with loc_end = pvb_expr.pexp_loc.loc_end}

let extractLocBindingOp {pbop_pat; pbop_exp} =
  let estimatedLoc = firstAttrLoc pbop_pat.ppat_loc [] in
  {estimatedLoc with loc_end = pbop_exp.pexp_loc.loc_end}

let extractLocModuleBinding {pmb_expr; pmb_attributes} =
  let estimatedLoc = firstAttrLoc pmb_expr.pmod_loc pmb_attributes in
  {estimatedLoc with loc_end = pmb_expr.pmod_loc.loc_end}

let extractLocModDecl {pmd_type; pmd_attributes} =
  let estimatedLoc = firstAttrLoc pmd_type.pmty_loc pmd_attributes in
  {estimatedLoc with loc_end = pmd_type.pmty_loc.loc_end}

let rec sequentialIfBlocks x =
  match x with
    | Some ({pexp_desc=Pexp_ifthenelse (e1, e2, els)}) -> (
       let (nestedIfs, finalExpression) = (sequentialIfBlocks els) in
       ((e1, e2)::nestedIfs, finalExpression)
      )
    | Some e -> ([], Some e)
    | None -> ([], None)

(*
  TODO: IDE integration beginning with Vim:

  - Create recovering version of parser that creates regions of "unknown"
    content in between let sequence bindings (anything between semicolons,
    really).
  - Use Easy_format's "style" features to tag each known node.
  - Turn those style annotations into editor highlight commands.
  - Editors have a set of keys that retrigger the parsing/rehighlighting
    process (typically newline/semi/close-brace).
  - On every parsing/rehighlighting, this pretty printer can be used to
    determine the highlighting of recovered regions, and the editor plugin can
    relegate highlighting of malformed regions to the editor which mostly does
    so based on token patterns.

*)

(*
     @avoidSingleTokenWrapping

  +-----------------------------+
  |+------+                     |     Another label
  || let ( \                    |
  ||    a  | Label              |
  ||    o  |                    |     The thing to the right of any label must be a
  ||    p _+ label RHS          |     list in order for it to wrap correctly. Lists
  ||  ): /   v                  |     will wrap if they need to/can. NON-lists will
  |+--+ sixteenTuple = echoTuple|(    wrap (indented) even though they're no lists!
  +---/ 0,\---------------------+     To prevent a single item from wrapping, make
        0,                            an unbreakable list via ensureSingleTokenSticksToLabel.
        0
      );                              In general, the best approach for indenting
                                      let bindings is to keep building up labels from
                                      the "let", always ensuring things that you want
                                      to wrap will either be lists or guarded in
                                      [ensureSingleTokenSticksToLabel].
                                      If you must join several lists together (via =)
                                      (or colon), ensure that joining is done via
                                      [makeList] (which won't break), and that new
                                      list is always appended to the left
                                      hand side of the label. (So that the right hand
                                      side may always be the untouched list that you want
                                      to wrap with aligned closing).
                                      Always make sure rhs of the label are the

                                      Creating nested labels will preserve the original
                                      indent location ("let" in this
                                      case) as long as that nesting is
                                      done on the left hand side of the labels.

*)

(*
    Table 2.1. Precedence and associativity.
    Precedence from highest to lowest: From RWOC, modified to include !=
    ---------------------------------------

    Operator prefix	Associativity
    !..., ?..., ~...	                              Prefix
    ., .(, .[	-
    function application, constructor, assert, lazy	Left associative
    -, -.                                           Prefix
    **..., lsl, lsr, asr                            Right associative
    *..., /..., %..., mod, land, lor, lxor          Left associative
    +..., -...                                      Left associative
    ::                                              Right associative
    @..., ^...                                      Right associative
---
    !=                                              Left associative (INFIXOP0 listed first in lexer)
    =..., <..., >..., |..., &..., $...              Left associative (INFIXOP0)
    =, <, >                                         Left associative (IN SAME row as INFIXOP0 listed after)
---
    &, &&                                           Right associative
    or, ||                                          Right associative
    ,                                               -
    :=, =                                         	Right associative
    if                                              -
    ;                                               Right associative


   Note: It would be much better if &... and |... were in separate precedence
   groups just as & and | are. This way, we could encourage custom infix
   operators to use one of the two precedences and no one would be confused as
   to precedence (leading &, | are intuitive). Two precedence classes for the
   majority of infix operators is totally sufficient.

   TODO: Free up the (&) operator from pervasives so it can be reused for
   something very common such as string concatenation or list appending.

   let x = tail & head;
 *)

(* "Almost Simple Prefix" function applications parse with the rule:

   `PREFIXOP simple_expr %prec below_DOT_AND_SHARP`, which in turn is almost
   considered a "simple expression" (it's acceptable anywhere a simple
   expression is except in a couple of edge cases.

   "Unary Prefix" function applications parse with the rule:

   `MINUS epxr %prec prec_unary_minus`, which in turn is considered an
   "expression" (not simple). All unary operators are mapped into an identifier
   beginning with "~".

   TODO: Migrate all "almost simple prefix" to "unsary prefix". When `!`
   becomes "not", then it will make more sense that !myFunc (arg) is parsed as
   !(myFunc arg) instead of (!myFunc) arg.

 *)
let almost_simple_prefix_symbols  = [ '!'; '?'; '~'] ;;
(* Subset of prefix symbols that have special "unary precedence" *)
let unary_minus_prefix_symbols  = [ "~-"; "~-."] ;;
let unary_plus_prefix_symbols  = ["~+"; "~+." ] ;;
let infix_symbols = [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/';
                      '$'; '%'; '\\'; '#' ]
(* this should match "kwdopchar" from reason_declarative_lexer.mll *)
let special_infix_strings =
  ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "!=="]

let updateToken = "="
let sharpOpEqualToken = "#="
let pipeFirstToken = "->"
let requireIndentFor = [updateToken; ":="]

let namedArgSym = "~"

let requireNoSpaceFor tok =
  tok = pipeFirstToken || (tok.[0] = '#' && tok <> "#=")

let funToken = "fun"

let getPrintableUnaryIdent s =
  if List.mem s unary_minus_prefix_symbols ||
     List.mem s unary_plus_prefix_symbols
  then String.sub s 1 (String.length s -1)
  else s

(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let printedStringAndFixity = function
  | s when List.mem s special_infix_strings -> Infix s
  | "^" -> UnaryPostfix "^"
  | s when List.mem s.[0] infix_symbols -> Infix s
  (* Correctness under assumption that unary operators are stored in AST with
     leading "~" *)
  | s when List.mem s.[0] almost_simple_prefix_symbols &&
           not (List.mem s special_infix_strings) &&
           not (s = "?") -> (
      (* What *kind* of prefix fixity? *)
      if List.mem s unary_plus_prefix_symbols then
        UnaryPlusPrefix (getPrintableUnaryIdent s)
      else if List.mem s unary_minus_prefix_symbols then
        UnaryMinusPrefix (getPrintableUnaryIdent s)
      else if s = "!" then
        UnaryNotPrefix s
      else
        AlmostSimplePrefix s
  )
  | s when is_letop s -> Letop s
  | s when is_andop s -> Andop s
  | _ -> Normal


(* Also, this doesn't account for != and !== being infixop!!! *)
let isSimplePrefixToken s = match printedStringAndFixity s with
  | AlmostSimplePrefix _ | UnaryPostfix "^" -> true
  | _ -> false


(* Convenient bank of information that represents the parser's precedence
   rankings.  Each instance describes a precedence table entry. The function
   tests either a token string encountered by the parser, or (in the case of
   `CustomPrecedence`) the string name of a custom rule precedence declared
   using %prec *)
let rules = [
  [
    (TokenPrecedence, (fun s -> (Left, s = pipeFirstToken)));
    (TokenPrecedence, (fun s -> (Left, s.[0] = '#' &&
                                       s <> sharpOpEqualToken &&
                                       s <> "#")));
    (TokenPrecedence, (fun s -> (Left, s = ".")));
    (CustomPrecedence, (fun s -> (Left, s = "prec_lbracket")));
  ];
  [
    (CustomPrecedence, (fun s -> (Nonassoc, s = "prec_functionAppl")));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, isSimplePrefixToken s)));
  ];
  [
    (TokenPrecedence, (fun s -> (Left, s = sharpOpEqualToken)));
  ];
  [
    (CustomPrecedence, (fun s -> (Nonassoc, s = "prec_unary")));
  ];
  (* Note the special case for "*\*", BARBAR, and LESSMINUS, AMPERSAND(s) *)
  [
    (TokenPrecedence, (fun s -> (Right, s = "**")));
    (TokenPrecedence, (fun s -> (Right, String.length s > 1 && s.[0] == '*' && s.[1] == '\\' && s.[2] == '*')));
    (TokenPrecedence, (fun s -> (Right, s = "lsl")));
    (TokenPrecedence, (fun s -> (Right, s = "lsr")));
    (TokenPrecedence, (fun s -> (Right, s = "asr")));
  ];
  [
    (TokenPrecedence, (fun s -> (Left, s.[0] == '*' && (String.length s == 1 || s != "*\\*"))));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '/')));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '%' )));
    (TokenPrecedence, (fun s -> (Left, s = "mod" )));
    (TokenPrecedence, (fun s -> (Left, s = "land" )));
    (TokenPrecedence, (fun s -> (Left, s = "lor" )));
    (TokenPrecedence, (fun s -> (Left, s = "lxor" )));
  ];
  [
    (* Even though these use the same *tokens* as unary plus/minus at parse
       time, when unparsing infix -/+, the CustomPrecedence rule would be
       incorrect to use, and instead we need a rule that models what infix
       parsing would use - just the regular token precedence without a custom
       precedence. *)
    (TokenPrecedence,
    (fun s -> (
      Left,
      if String.length s > 1 && s.[0] == '+' && s.[1] == '+' then
        (*
          Explicitly call this out as false because the other ++ case below
          should have higher *lexing* priority. ++operator_chars* is considered an
          entirely different token than +(non_plus_operator_chars)*
        *)
        false
      else
        s.[0] == '+'
    )));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '-' && s <> pipeFirstToken)));
    (TokenPrecedence, (fun s -> (Left, s = "!" )));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s = "::")));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s.[0] == '@')));
    (TokenPrecedence, (fun s -> (Right, s.[0] == '^')));
    (TokenPrecedence, (fun s -> (Right, String.length s > 1 && s.[0] == '+' && s.[1] == '+')));
  ];
  [
    (TokenPrecedence, (fun s -> (Left, s.[0] == '=' && not (s = "=") && not (s = "=>"))));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '<' && not (s = "<"))));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '>' && not (s = ">"))));
    (TokenPrecedence, (fun s -> (Left, s = "!=")));  (* Not preset in the RWO table! *)
    (TokenPrecedence, (fun s -> (Left, s = "!==")));  (* Not preset in the RWO table! *)
    (TokenPrecedence, (fun s -> (Left, s = "==")));
    (TokenPrecedence, (fun s -> (Left, s = "===")));
    (TokenPrecedence, (fun s -> (Left, s = "<")));
    (TokenPrecedence, (fun s -> (Left, s = ">")));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '|' && not (s = "||"))));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '&' && not (s = "&") && not (s = "&&"))));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '$')));
  ];
  [
    (CustomPrecedence, (fun s -> (Left, s = funToken)));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s = "&")));
    (TokenPrecedence, (fun s -> (Right, s = "&&")));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s = "or")));
    (TokenPrecedence, (fun s -> (Right, s = "||")));
  ];
  [
    (* The Left shouldn't ever matter in practice. Should never get in a
       situation with two consecutive infix ? - the colon saves us. *)
    (TokenPrecedence, (fun s -> (Left, s = "?")));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s = ":=")));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s = updateToken)));
  ];
  (* It's important to account for ternary ":" being lower precedence than "?" *)
  [
    (TokenPrecedence, (fun s -> (Right, s = ":")))
  ];
  [
    (TokenPrecedence, (fun s -> (Nonassoc, s = "=>")));
  ];
]

(* remove all prefixing backslashes, e.g. \=== becomes === *)
let without_prefixed_backslashes str =
  if str = "" then str
  else if String.get str 0 = '\\' then String.sub str 1 (String.length str - 1)
  else str

let indexOfFirstMatch ~prec lst =
  let rec aux n = function
    | [] -> None
    | [] :: tl -> aux (n + 1) tl
    | ((kind, tester) :: hdTl) :: tl ->
      match prec, kind with
      | Token str, TokenPrecedence | Custom str, CustomPrecedence ->
        let associativity, foundMatch = tester str in
        if foundMatch
        then Some (associativity, n)
        else aux n (hdTl::tl)
      | _ -> aux n (hdTl::tl)
  in
  aux 0 lst

(* Assuming it's an infix function application. *)
let precedenceInfo ~prec =
  (* Removes prefixed backslashes in order to do proper conversion *)
  let prec = match prec with
    | Token str -> Token (without_prefixed_backslashes str)
    | Custom _ -> prec
  in
  indexOfFirstMatch ~prec rules

let isLeftAssociative ~prec = match precedenceInfo ~prec with
  | None -> false
  | Some (Left, _) -> true
  | Some (Right, _) -> false
  | Some (Nonassoc, _) -> false

let isRightAssociative ~prec = match precedenceInfo ~prec with
  | None -> false
  | Some (Right, _) -> true
  | Some (Left, _) -> false
  | Some (Nonassoc, _) -> false

let higherPrecedenceThan c1 c2 =
  match ((precedenceInfo ~prec:c1), (precedenceInfo ~prec:c2)) with
  | (_, None)
  | (None, _) ->
    let (str1, str2) = match (c1, c2) with
      | (Token s1, Token s2) -> ("Token " ^ s1, "Token " ^ s2)
      | (Token s1, Custom s2) -> ("Token " ^ s1, "Custom " ^ s2)
      | (Custom s1, Token s2) -> ("Custom " ^ s1, "Token " ^ s2)
      | (Custom s1, Custom s2) -> ("Custom " ^ s1, "Custom " ^ s2)
    in
    raise (NotPossible ("Cannot determine precedence of two checks " ^ str1 ^ " vs. " ^ str2))
  | (Some (_, p1), Some (_, p2)) -> p1 < p2

let printedStringAndFixityExpr = function
  | {pexp_desc = Pexp_ident {txt=Lident l}} -> printedStringAndFixity l
  | _ -> Normal

(* which identifiers are in fact operators needing parentheses *)
let needs_parens txt =
  match printedStringAndFixity txt with
  | Infix _ -> true
  | UnaryPostfix _ -> true
  | UnaryPlusPrefix _ -> true
  | UnaryMinusPrefix _ -> true
  | UnaryNotPrefix _ -> true
  | AlmostSimplePrefix _ -> true
  | Letop _ -> true
  | Andop _ -> true
  | Normal -> false

(* some infixes need spaces around parens to avoid clashes with comment
   syntax. This isn't needed for comment syntax /* */ *)
let needs_spaces txt =
  txt.[0]='*' || txt.[String.length txt - 1] = '*'

let rec orList = function (* only consider ((A|B)|C)*)
  | {ppat_desc = Ppat_or (p1, p2)} -> (orList p1) @ (orList p2)
  | x -> [x]

let override = function
  | Override -> "!"
  | Fresh -> ""

(* variance encoding: need to sync up with the [parser.mly] *)
let type_variance = function
  | Invariant -> ""
  | Covariant -> "+"
  | Contravariant -> "-"

type construct =
  [ `cons of expression list
  | `list of expression list
  | `nil
  | `normal
  | `simple of Longident.t
  | `tuple ]

let view_expr x =
  match x.pexp_desc with
  | Pexp_construct ( {txt= Lident "()"},_) -> `tuple
  | Pexp_construct ( {txt= Lident "[]"},_) -> `nil
  | Pexp_construct ( {txt= Lident"::"},Some _) ->
    let rec loop exp acc = match exp with
      | {pexp_desc=Pexp_construct ({txt=Lident "[]"},_)} ->
        (List.rev acc,true)
      | {pexp_desc=
          Pexp_construct ({txt=Lident "::"},
                           Some ({pexp_desc= Pexp_tuple([e1;e2])}))} ->
        loop e2 (e1::acc)
      | e -> (List.rev (e::acc),false) in
    let (ls,b) = loop x []  in
    if b
    then `list ls
    else `cons ls
  | Pexp_construct (x,None) -> `simple x.txt
  | _ -> `normal

let is_simple_list_expr x =
  match view_expr x with
  | `list _ | `cons _ -> true
  | _ -> false

let is_simple_construct : construct -> bool = function
  | `nil | `tuple | `list _ | `simple _ | `cons _  -> true
  | `normal -> false

let uncurriedTable = Hashtbl.create 42

(* Determines if a list of expressions contains a single unit construct
 * e.g. used to check: MyConstructor() -> exprList == [()]
 * useful to determine if MyConstructor(()) should be printed as MyConstructor()
 * *)
let is_single_unit_construct exprList =
  match exprList with
  | x::[] ->
    let view = view_expr x in
    (match view with
    | `tuple -> true
    | _ -> false)
  | _ -> false

let detectTernary l = match l with
  | [{
      pc_lhs={ppat_desc=Ppat_construct ({txt=Lident "true"}, _)};
      pc_guard=None;
      pc_rhs=ifTrue
    };
    {
      pc_lhs={ppat_desc=Ppat_construct ({txt=Lident "false"}, _)};
      pc_guard=None;
      pc_rhs=ifFalse
    }] -> Some (ifTrue, ifFalse)
  | _ -> None
type funcApplicationLabelStyle =
  (* No attaching to the label, but if the entire application fits on one line,
     the entire application will appear next to the label as you 'd expect. *)
  | NeverWrapFinalItem
  (* Attach the first term if there are exactly two terms involved in the
     application.

     let x = firstTerm (secondTerm_1 secondTerm_2) thirdTerm;

     Ideally, we'd be able to attach all but the last argument into the label any
     time all but the last term will fit - and *not* when (attaching all but
     the last term isn't enough to prevent a wrap) - But there's no way to tell
     ahead of time if it would prevent a wrap.

     However, the number two is somewhat convenient. This models the
     indentation that you'd prefer in non-curried syntax languages like
     JavaScript, where application only ever has two terms.
  *)
  | WrapFinalListyItemIfFewerThan of int

type formatSettings = {
  (* Whether or not to expect that the original parser that generated the AST
     would have annotated constructor argument tuples with explicit arity to
     indicate that they are multiple arguments. (True if parsed in original
     OCaml AST, false if using Reason parser).
  *)
  constructorTupleImplicitArity: bool;
  space: int;

  (* For curried arguments in function *definitions* only: Number of [space]s
     to offset beyond the [let] keyword. Default 1.
  *)
  listsRecordsIndent: int;

  indentWrappedPatternArgs: int;

  indentMatchCases: int;

  (* Amount to indent in label-like constructs such as wrapped function
     applications, etc - or even record fields. This is not the same concept as an
     indented curried argument list. *)
  indentAfterLabels: int;

  (* Amount to indent after the opening brace of switch/try.
     Here's an example of what it would look like w/ [trySwitchIndent = 2]:
     Sticks the expression to the last item in a sequence in several [X | Y | Z
     => expr], and forces X, Y, Z to be split onto several lines. (Otherwise,
     sticking to Z would result in hanging expressions).  TODO: In the first case,
     it's clear that we want patterns to have an "extra" indentation with matching
     in a "match". Create extra config param to pass to [self#pattern] for extra
     indentation in this one case.

      switch x {
      | TwoCombos
          (HeresTwoConstructorArguments x y)
          (HeresTwoConstructorArguments a b) =>
          ((a + b) + x) + y;
      | Short
      | AlsoHasARecord a b {x, y} => (
          retOne,
          retTwo
        )
      | AlsoHasARecord a b {x, y} =>
        callMyFunction
          withArg
          withArg
          withArg
          withArg;
      }
  *)
  trySwitchIndent: int;


  (* In the case of two term function application (when flattened), the first
     term should become part of the label, and the second term should be able to wrap
     This doesn't effect n != 2.

       [true]
       let x = reallyShort allFitsOnOneLine;
       let x = someFunction {
         reallyLongObject: true,
         thatWouldntFitOnThe: true,
         firstLine: true
       };

       [false]
       let x = reallyShort allFitsOnOneLine;
       let x =
        someFunction
          {
            reallyLongObject: true,
            thatWouldntFitOnThe: true,
            firstLine: true
          };
  *)
  funcApplicationLabelStyle: funcApplicationLabelStyle;

  funcCurriedPatternStyle: funcApplicationLabelStyle;

  width: int;

  assumeExplicitArity: bool;

  constructorLists: string list;
}

let defaultSettings = {
  constructorTupleImplicitArity = false;
  space = 1;
  listsRecordsIndent = 2;
  indentWrappedPatternArgs = 2;
  indentMatchCases = 2;
  indentAfterLabels = 2;
  trySwitchIndent = 0;
  funcApplicationLabelStyle = WrapFinalListyItemIfFewerThan 3;
  (* WrapFinalListyItemIfFewerThan is currently a bad idea for curried
     arguments: It looks great in some cases:

        let myFun (a:int) :(
          int,
          string
        ) => (a, "this is a");

     But horrible in others:

        let myFun
            {
              myField,
              yourField
            } :someReturnType => myField + yourField;

        let myFun
            {            // Curried arg wraps
              myField,
              yourField
            } : (       // But the last is "listy" so it docks
          int,          // To the [let].
          int,
          int
        ) => myField + yourField;

     We probably want some special listy label docking/wrapping mode for
     curried function bindings.

  *)
  funcCurriedPatternStyle = NeverWrapFinalItem;
  width = 80;
  assumeExplicitArity = false;
  constructorLists = [];
}
let configuredSettings = ref defaultSettings

let configure ~width ~assumeExplicitArity ~constructorLists = (
  configuredSettings := {defaultSettings with width; assumeExplicitArity; constructorLists}
)

let createFormatter () =
let module Formatter = struct

let settings = !configuredSettings


(* How do we make
   this a label?

   /---------------------\
   let myVal = (oneThing, {
   field: [],
   anotherField: blah
   });

   But in this case, this wider region a label?
   /------------------------------------------------------\
   let myVal = callSomeFunc (oneThing, {field: [], anotherField: blah}, {
   boo: 'hi'
   });

   This is difficult. You must form a label from the preorder traversal of every
   node - except the last encountered in the traversal. An easier heuristic is:

   - The last argument to a functor application is expanded.

   React.CreateClass SomeThing {
   let render {props} => {
   };
   }

   - The last argument to a function application is expanded on the same line.
   - Only if it's not curried with another invocation.
   -- Optionally: "only if everything else is an atom"
   -- Optionally: "only if there are no other args"

   React.createClass someThing {
   render: fn x => y,
   }

   !!! NOT THIS
   React.createClass someThing {
   render: fn x => y,
   }
   somethingElse
*)

let isArityClear attrs =
  (!configuredSettings).assumeExplicitArity ||
  List.exists
    (function
      | { attr_name = {txt="explicit_arity"}; _} -> true
      | _ -> false
    )
    attrs

let default_indent_body =
  settings.listsRecordsIndent * settings.space

let makeList
    ?listConfigIfCommentsInterleaved
    ?listConfigIfEolCommentsInterleaved
    ?(break=Layout.Never)
    ?(wrap=("", ""))
    ?(inline=(true, false))
    ?(sep=Layout.NoSep)
    ?(indent=default_indent_body)
    ?(sepLeft=true)
    ?(preSpace=false)
    ?(postSpace=false)
    ?(pad=(false,false))
    lst =
  let config =
    { Layout.
      listConfigIfCommentsInterleaved; listConfigIfEolCommentsInterleaved;
      break; wrap; inline; sep; indent; sepLeft; preSpace; postSpace; pad;
    }
  in
  Layout.Sequence (config, lst)

let makeAppList = function
  | [hd] -> hd
  | l -> makeList ~inline:(true, true) ~postSpace:true ~break:IfNeed l

let makeTup ?(wrap=("", ""))?(trailComma=true) ?(uncurried = false) l =
  let (lwrap, rwrap) = wrap in
  let lparen = lwrap ^ (if uncurried then "(. " else "(") in
  makeList
    ~wrap:(lparen, ")" ^ rwrap)
    ~sep:(if trailComma then commaTrail else commaSep)
    ~postSpace:true
    ~break:IfNeed l

let ensureSingleTokenSticksToLabel x =
  let listConfigIfCommentsInterleaved cfg =
    let inline = (true, true) and postSpace = true and indent = 0 in
    {cfg with Layout.break=Always_rec; postSpace; indent; inline}
  in
  makeList ~listConfigIfCommentsInterleaved [x]

let unbreakLabelFormatter formatter =
  let newFormatter labelTerm term =
    match formatter labelTerm term with
    | Easy_format.Label ((labelTerm, settings), term) ->
       Easy_format.Label ((labelTerm,
                           {settings with label_break = `Never}),
                          term)
    | _ -> failwith "not a label"
  in newFormatter

let inlineLabel labelTerm term =
  let settings = {
    label_break = `Never;
    space_after_label = true;
    indent_after_label = 0;
    label_style = Some "inlineLabel";
  } in
  Easy_format.Label ((labelTerm, settings), term)


(* Just for debugging: Set debugWithHtml = true *)
let debugWithHtml = ref false

let html_escape_string s =
  let buf = Buffer.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let html_escape = `Escape_string html_escape_string

let html_style = [
  "atom", { Easy_format.tag_open = "<a>"; tag_close = "</a>" };
  "body", { tag_open = "<lb>"; tag_close = "</lb>" };
  "list", { tag_open = "<l>"; tag_close = "</l>" };
  "op", { tag_open = "<op>"; tag_close = "</op>" };
  "cl", { tag_open = "<cl>"; tag_close = "</cl>" };
  "sep", { tag_open = "<sep>"; tag_close = "</sep>" };
  "label", { tag_open = "<la>"; tag_close = "</la>" };
]

let easyLabel
    ?(break=`Auto) ?(space=false) ?(indent=settings.indentAfterLabels)
    labelTerm term =
  let settings = {
    label_break = break;
    space_after_label = space;
    indent_after_label = indent;
    label_style = Some "label";
  } in
  Easy_format.Label ((labelTerm, settings), term)

let label ?break ?space ?indent (labelTerm:Layout.t) (term:Layout.t) =
  Layout.Label (easyLabel ?break ?indent ?space, labelTerm, term)

let atom ?loc str =
  let style = { Easy_format.atom_style = Some "atomClss" } in
  source_map ?loc (Layout.Easy (Easy_format.Atom(str, style)))

(** Take x,y,z and n and generate [x, y, z, ...n] *)
let makeES6List ?wrap:((lwrap,rwrap)=("", "")) lst last =
  makeList
    ~wrap:(lwrap ^ "[", "]" ^ rwrap)
    ~break:IfNeed ~postSpace:true ~sep:commaTrail
    (lst @ [makeList [atom "..."; last]])

let makeNonIndentedBreakingList lst =
    (* No align closing: So that semis stick to the ends of every break *)
  makeList ~break:Always_rec ~indent:0 ~inline:(true, true) lst

(* Like a <span> could place with other breakableInline lists without upsetting final semicolons *)
let makeSpacedBreakableInlineList lst =
  makeList ~break:IfNeed ~inline:(true, true) ~postSpace:true lst

let makeCommaBreakableListSurround opn cls lst =
  makeList ~break:IfNeed ~postSpace:true ~sep:(Sep ",") ~wrap:(opn, cls) lst

(* TODO: Allow configuration of spacing around colon symbol *)

let formatPrecedence ?(inline=false) ?(wrap=("(", ")")) ?loc formattedTerm =
  source_map ?loc (makeList ~inline:(true, inline) ~wrap ~break:IfNeed [formattedTerm])

let wrap fn term =
  ignore (Format.flush_str_formatter ());
  fn Format.str_formatter term;
  atom (Format.flush_str_formatter ())

(* Don't use `trim` since it kills line return too? *)
let rec beginsWithStar_ line length idx =
  if idx = length then false else
    match String.get line idx with
    | '*' -> true
    | '\t' | ' ' -> beginsWithStar_ line length (idx + 1)
    | _ -> false

let beginsWithStar line = beginsWithStar_ line (String.length line) 0

let rec numLeadingSpace_ line length idx accum =
  if idx = length then accum else
    match String.get line idx with
    | '\t' | ' ' -> numLeadingSpace_ line length (idx + 1) (accum + 1)
    | _ -> accum

let numLeadingSpace line = numLeadingSpace_ line (String.length line) 0 0

(* Computes the smallest leading spaces for non-empty lines *)
let smallestLeadingSpaces strs =
  let rec smallestLeadingSpaces curMin strs = match strs with
    | [] -> curMin
    | ""::tl -> smallestLeadingSpaces curMin tl
    | hd::tl ->
      let leadingSpace = numLeadingSpace hd in
      let nextMin = min curMin leadingSpace in
      smallestLeadingSpaces nextMin tl
  in
  smallestLeadingSpaces 99999 strs

let rec isSequencey = function
  | Layout.SourceMap (_, sub) -> isSequencey sub
  | Layout.Sequence _ -> true
  | Layout.Label (_, _, _) -> false
  | Layout.Easy (Easy_format.List _) -> true
  | Layout.Easy _ -> false
  | Layout.Whitespace (_, sub) -> isSequencey sub

let inline ?(preSpace=false) ?(postSpace=false) labelTerm term =
  makeList [labelTerm; term]
    ~inline:(true, true) ~postSpace ~preSpace ~indent:0 ~break:Layout.Never

let breakline labelTerm term =
  makeList [labelTerm; term]
    ~inline:(true, true) ~indent:0 ~break:Always_rec

let insertBlankLines n term =
  if n = 0
  then term
  else makeList ~inline:(true, true) ~indent:0 ~break:Always_rec
      (Array.to_list (Array.make n (atom "")) @ [term])

let string_after s n = String.sub s n (String.length s - n)

(* This is a special-purpose functions only used by `formatComment_`. Notice we
skip a char below during usage because we know the comment starts with `/*` *)
let rec lineZeroMeaningfulContent_ line length idx accum =
  if idx = length then None
  else
    let ch = String.get line idx in
    if ch = '\t' || ch = ' ' || ch = '*' then
      lineZeroMeaningfulContent_ line length (idx + 1) (accum + 1)
    else Some accum

let lineZeroMeaningfulContent line =
  lineZeroMeaningfulContent_ line (String.length line) 1 0

let formatComment_ txt =
  let commLines =
    Reason_syntax_util.split_by ~keep_empty:true (fun x -> x = '\n')
      (Comment.wrap txt)
  in
  match commLines with
  | [] -> atom ""
  | [hd] ->
    atom hd
  | zero::one::tl ->
    let attemptRemoveCount = (smallestLeadingSpaces (one::tl)) in
    let leftPad =
      if beginsWithStar one then 1
      else match lineZeroMeaningfulContent zero with
        | None -> 1
        | Some num -> num + 1
    in
    let padNonOpeningLine s =
      let numLeadingSpaceForThisLine = numLeadingSpace s in
      if String.length s == 0 then ""
      else (String.make leftPad ' ') ^
           (string_after s (min attemptRemoveCount numLeadingSpaceForThisLine)) in
    let lines = zero :: List.map padNonOpeningLine (one::tl) in
    makeList ~inline:(true, true) ~indent:0 ~break:Always_rec (List.map atom lines)

let formatComment comment =
  source_map ~loc:(Comment.location comment) (formatComment_ comment)

let rec append ?(space=false) txt = function
  | Layout.SourceMap (loc, sub) ->
    Layout.SourceMap (loc, append ~space txt sub)
  | Sequence (config, l) when snd config.wrap <> "" ->
    let sep = if space then " " else "" in
    Sequence ({config with wrap=(fst config.wrap, snd config.wrap ^ sep ^ txt)}, l)
  | Sequence (config, []) ->
    Sequence (config, [atom txt])
  | Sequence ({sep=NoSep} as config, l)
  | Sequence ({sep=Sep("")} as config, l) ->
    let len = List.length l in
    let sub = List.mapi (fun i layout ->
        (* append to the end of the list *)
        if i + 1 = len then
          append ~space txt layout
        else
          layout
      ) l in
    Sequence (config, sub)
  | Label (formatter, left, right) ->
    Label (formatter, left, append ~space txt right)
  | Whitespace(info, sub) ->
      Whitespace(info, append ~space txt sub)
  | layout ->
    inline ~postSpace:space layout (atom txt)

let appendSep spaceBeforeSep sep layout =
  append (if spaceBeforeSep then " " ^ sep else sep) layout

let rec flattenCommentAndSep ?spaceBeforeSep:(spaceBeforeSep=false) ?sepStr = function
  | Layout.SourceMap (loc, sub) ->
     Layout.SourceMap (loc, flattenCommentAndSep ~spaceBeforeSep ?sepStr sub)
  | Layout.Whitespace(info, sub) ->
     Layout.Whitespace(info, flattenCommentAndSep ~spaceBeforeSep ?sepStr sub)
  | layout ->
     begin
       match sepStr with
       | None -> layout
       | Some sep -> appendSep spaceBeforeSep sep layout
     end

let rec preOrderWalk f layout =
  match f layout with
  | Layout.Sequence (listConfig, sublayouts) ->
    let newSublayouts = List.map (preOrderWalk f) sublayouts in
    Layout.Sequence (listConfig, newSublayouts)
  | Layout.Label (formatter, left, right) ->
    let newLeftLayout = preOrderWalk f left in
    let newRightLayout = preOrderWalk f right in
    Layout.Label (formatter, newLeftLayout, newRightLayout)
  | Layout.SourceMap (loc, sub) ->
    Layout.SourceMap (loc, preOrderWalk f sub)
  | Layout.Easy _ as layout -> layout
  | Layout.Whitespace (info, sub) ->
      Layout.Whitespace(info, preOrderWalk f sub)

(** Recursively unbreaks a layout to make sure they stay within the same line *)
let unbreaklayout = preOrderWalk (function
  | Layout.Sequence (listConfig, sublayouts) ->
    Layout.Sequence ({listConfig with break=Layout.Never}, sublayouts)
  | Layout.Label (formatter, left, right) ->
    Layout.Label (unbreakLabelFormatter formatter, left, right)
  | layout -> layout
)

(** [consolidateSeparator layout] walks the [layout], extract separators out of each
 *  list and insert them into PrintTree as separated items
 *)
let consolidateSeparator l = preOrderWalk (function
  | Sequence (listConfig, sublayouts) when listConfig.sep != NoSep && listConfig.sepLeft ->
     (* TODO: Support !sepLeft, and this should apply to the *first* separator if !sepLeft.  *)
     let sublayoutsLen = List.length sublayouts in
     let mapSublayout i layout =
        match (listConfig.sep, (i + 1 = sublayoutsLen)) with
        | (NoSep, _) -> raise (NotPossible "We already covered this case. This shouldn't happen.")
        | (Sep _, true) -> layout
        | (SepFinal (sepStr, _), false)
        | (Sep sepStr, false) ->
          flattenCommentAndSep ~spaceBeforeSep:listConfig.preSpace ~sepStr:sepStr layout
        | (SepFinal (_, finalSepStr), true) ->
          flattenCommentAndSep ~spaceBeforeSep:listConfig.preSpace ~sepStr:finalSepStr layout
     in
     let layoutsWithSepAndComment = List.mapi mapSublayout sublayouts in
     let sep = Layout.NoSep in
     let preSpace = false in
     Sequence ({listConfig with sep; preSpace}, layoutsWithSepAndComment)
  | layout -> layout
) l


(** [insertLinesAboveItems layout] walks the [layout] and insert empty lines *)
let insertLinesAboveItems items = preOrderWalk (function
  | Whitespace(region, sub) ->
      insertBlankLines (WhitespaceRegion.newlines region) sub
  | layout -> layout
) items

let insertCommentIntoWhitespaceRegion comment region subLayout =
  let cl = Comment.location comment in
  let range = WhitespaceRegion.range region in
  (* append the comment to the list of inserted comments in the whitespace region *)
  let nextRegion = WhitespaceRegion.addComment region comment in
  let formattedComment = formatComment comment in
  match WhitespaceRegion.comments region with
  (* the comment inserted into the whitespace region is the first in the region *)
  | [] ->
      (*
       * 1| let a = 1;
       * 2|
       * 3| /* comment at end of whitespace region */
       * 4| let b = 2;
       *)
      if range.lnum_end = cl.loc_end.pos_lnum then
        let subLayout = breakline formattedComment subLayout in
        Layout.Whitespace(nextRegion, subLayout)

      (*
       * 1| let a = 1;
       * 2| /* comment at start of whitespace region */
       * 3|
       * 4| let b = 2;
       *)
      else if range.lnum_start = cl.loc_start.pos_lnum then
        let subLayout = breakline formattedComment (insertBlankLines 1 subLayout) in
        let nextRegion = WhitespaceRegion.modifyNewlines nextRegion 0 in
        Whitespace(nextRegion, subLayout)

      (*
       * 1| let a = 1;
       * 2|
       * 3| /* comment floats in whitespace region */
       * 4|
       * 5| let b = 2;
       *)
      else
        let subLayout = breakline formattedComment (insertBlankLines 1 subLayout) in
        Whitespace(nextRegion, subLayout)

  (* The whitespace region contains already inserted comments *)
  | prevComment::_cs ->
      let pcl = Comment.location prevComment in
      (* check if the comment is attached to the start of the region *)
      let attachedToStartRegion = cl.loc_start.pos_lnum = range.lnum_start in
      let nextRegion =
        (*
         * 1| let a = 1;
         * 2| /* comment sits on the beginning of the region */
         * 3| /* previous comment */
         * 4|
         * 5| let b = 2;
         *)
        if attachedToStartRegion then
          (* we don't want a newline between `let a = 1` and the `comment sits
           * on the beginning of the region` comment*)
          WhitespaceRegion.modifyNewlines nextRegion 0
        (*
         * 1| let a = 1;
         * 2|
         * 3| /* comment isn't located at the beginnin of a region*/
         * 4| /* previous comment */
         * 5|
         * 6| let b = 2;
         *)
        else
          nextRegion
      in
      (*
       * 1| let a = 1;
       * 2| /* comment */
       * 3|                      --> whitespace between
       * 4| /* previous comment */
       * 5| let b = 1;
       *)
      if Reason_location.hasSpaceBetween pcl cl then
      (* pcl.loc_start.pos_lnum - cl.loc_end.pos_lnum > 1 then *)
        let subLayout = breakline formattedComment (insertBlankLines 1 subLayout) in
        let withComment = Layout.Whitespace(nextRegion, subLayout) in
        withComment

      (*
       * 1| let a = 1;
       * 2|
       * 3| /* comment */          | no whitespace between `comment`
       * 4| /* previous comment */ | and `previous comment`
       * 5| let b = 1;
       *)
      else
        let subLayout = breakline formattedComment subLayout in
        let withComment =  Layout.Whitespace(nextRegion, subLayout) in
        withComment

(**
 * prependSingleLineComment inserts a single line comment right above layout
 *)
let rec prependSingleLineComment comment layout =
  match layout with
  | Layout.SourceMap (loc, sub) ->
     Layout.SourceMap (loc, prependSingleLineComment comment sub)
  | Sequence (config, hd::tl) when config.break = Always_rec->
     Sequence(config, (prependSingleLineComment comment hd)::tl)
  | Whitespace(info, sub) ->
      insertCommentIntoWhitespaceRegion comment info sub
  | layout ->
      breakline (formatComment comment) layout

(* breakAncestors break ancestors above node, but not comment attachment itself.*)
let appendComment ~breakAncestors layout comment =
  let text = Comment.wrap comment in
  let layout = match layout with
  | Layout.Whitespace(info, sublayout) ->
    Layout.Whitespace(info, makeList ~break:Layout.Never ~postSpace:true [sublayout; atom text])
  | layout ->
      makeList ~break:Layout.Never ~postSpace:true [layout; atom text]
  in
  if breakAncestors then
    makeList ~inline:(true, true) ~postSpace:false ~preSpace:true ~indent:0
      ~break:Always_rec [layout]
  else
    layout

(**
 * [looselyAttachComment layout comment] preorderly walks the layout and
 * find a place where the comment can be loosely attached to
 *)
let rec looselyAttachComment ~breakAncestors layout comment =
  let location = Comment.location comment in
  match layout with
  | Layout.SourceMap (loc, sub) ->
     Layout.SourceMap (loc, looselyAttachComment ~breakAncestors sub comment)
  | Layout.Whitespace (info, sub) ->
     Layout.Whitespace(info, looselyAttachComment ~breakAncestors sub comment)
  | Easy _ ->
     inline ~postSpace:true layout (formatComment comment)
  | Sequence (listConfig, subLayouts)
    when List.exists (Layout.contains_location ~location) subLayouts ->
     (* If any of the subLayout strictly contains this comment, recurse into to it *)
    let recurse_sublayout layout =
      if Layout.contains_location layout ~location
      then begin
        looselyAttachComment ~breakAncestors layout comment
      end
      else layout
    in
    Sequence (listConfig, List.map recurse_sublayout subLayouts)
  | Sequence (listConfig, subLayouts) when subLayouts == [] ->
    (* If there are no subLayouts (empty body), create a Sequence of just the comment *)
    Sequence (listConfig, [formatComment comment])
  | Sequence (listConfig, subLayouts) ->
    let (beforeComment, afterComment) =
      Reason_syntax_util.pick_while (Layout.is_before ~location) subLayouts in
    let newSubLayout = match List.rev beforeComment with
      | [] ->
        Reason_syntax_util.map_first (prependSingleLineComment comment) afterComment
      | hd :: tl ->
        List.rev_append
          (appendComment ~breakAncestors hd comment :: tl) afterComment
    in
    Sequence (listConfig, newSubLayout)
  | Label (formatter, left, right) ->
    let newLeft, newRight =
      match (Layout.get_location left, Layout.get_location right) with
      | (None, None) ->
        (left, looselyAttachComment ~breakAncestors right comment)
      | (_, Some loc2) when location_contains loc2 location ->
        (left, looselyAttachComment ~breakAncestors right comment)
      | (Some loc1, _) when location_contains loc1 location ->
        (looselyAttachComment ~breakAncestors left comment, right)
      | (Some loc1, Some _) when location_is_before location loc1 ->
        (prependSingleLineComment comment left, right)
      | (Some _, Some loc2) when location_is_before location loc2 ->
        (left, prependSingleLineComment comment right)
      | _ -> (left, appendComment ~breakAncestors right comment)
    in
    Label (formatter, newLeft, newRight)

(**
 * [insertSingleLineComment layout comment] preorderly walks the layout and
 * find a place where the SingleLineComment can be fit into
 *)
let rec insertSingleLineComment layout comment =
  let location = Comment.location comment in
  match layout with
  | Layout.SourceMap (loc, sub) ->
    Layout.SourceMap (loc, insertSingleLineComment sub comment)
  | Layout.Whitespace (info, sub) ->
      let range = WhitespaceRegion.range info in
      if Range.containsLoc range location then
        insertCommentIntoWhitespaceRegion comment info sub
      else
        Layout.Whitespace(info, insertSingleLineComment sub comment)
  | Easy _ ->
    prependSingleLineComment comment layout
  | Sequence (listConfig, subLayouts) when subLayouts == [] ->
    (* If there are no subLayouts (empty body), create a Sequence of just the
     * comment. We need to be careful when the empty body contains a //-style
     * comment. Example:
     *   let make = () => {
     *     //
     *   };
     * It is clear that the sequence needs to always break here, otherwise
     * we get a parse error: let make = () => { // };
     * The closing brace and semicolon `};` would become part of the comment…
     *)
    let listConfig =
      if Reason_comment.isLineComment comment then
        {listConfig with break = Always_rec}
      else
        listConfig
    in
    Sequence (listConfig, [formatComment comment])
  | Sequence (listConfig, subLayouts) ->
    let (beforeComment, afterComment) =
      Reason_syntax_util.pick_while (Layout.is_before ~location) subLayouts in
    begin match afterComment with
      (* Nothing in the list is after comment, attach comment to the statement before the comment *)
      | [] ->
        let break sublayout = breakline sublayout (formatComment comment) in
        Sequence (listConfig, Reason_syntax_util.map_last break beforeComment)
      | hd::tl ->
        let afterComment =
          match Layout.get_location hd with
          | Some loc when location_contains loc location ->
            insertSingleLineComment hd comment :: tl
          | Some loc ->
            Layout.SourceMap (loc, (prependSingleLineComment comment hd)) :: tl
          | _ ->
            prependSingleLineComment comment hd :: tl
        in
        Sequence (listConfig, beforeComment @ afterComment)
    end
  | Label (formatter, left, right) ->
    let leftLoc = Layout.get_location left in
    let rightLoc = Layout.get_location right in
    let newLeft, newRight = match (leftLoc, rightLoc) with
      | (None, None) ->
        (left, insertSingleLineComment right comment)
      | (_, Some loc2) when location_contains loc2 location ->
        (left, insertSingleLineComment right comment)
      | (Some loc1, _) when location_contains loc1 location ->
        (insertSingleLineComment left comment, right)
      | (Some loc1, Some _) when location_is_before location loc1 ->
        (prependSingleLineComment comment left, right)
      | (Some _, Some loc2) when location_is_before location loc2 ->
        (left, prependSingleLineComment comment right)
      | _ ->
        (left, breakline right (formatComment comment))
    in
    Label (formatter, newLeft, newRight)

let rec attachCommentToNodeRight layout comment =
  match layout with
  | Layout.Sequence (config, sub) when snd config.wrap <> "" ->
    (* jwalke: This is quite the abuse of the "wrap" config *)
    let lwrap, rwrap = config.wrap in
    let rwrap = rwrap ^ " " ^ Comment.wrap comment in
    Layout.Sequence ({config with wrap=(lwrap, rwrap)}, sub)
  | Layout.SourceMap (loc, sub) ->
    Layout.SourceMap (loc, attachCommentToNodeRight sub comment)
  | layout -> inline ~postSpace:true layout (formatComment comment)

let rec attachCommentToNodeLeft comment layout =
  match layout with
  | Layout.Sequence (config, sub) when snd config.wrap <> "" ->
    let lwrap, rwrap = config.wrap in
    let lwrap = Comment.wrap comment ^ " " ^ lwrap in
    Layout.Sequence ({config with wrap = (lwrap, rwrap)}, sub)
  | Layout.SourceMap (loc, sub) ->
    Layout.SourceMap (loc, attachCommentToNodeLeft comment sub )
  | layout ->
    Layout.Label (inlineLabel, formatComment comment, layout)

(** [tryPerfectlyAttachComment layout comment] postorderly walk the [layout] and tries
 *  to perfectly attach a comment with a layout node.
 *
 *  Perfectly attach here means a comment's start location is equal to the node's end location
 *  and vice versa.
 *
 *  If the comment can be perfectly attached to any layout node, returns (newLayout, None),
 *  meaning the comment is consumed. Otherwise returns the (unchangedLayout, Some comment),
 *  meaning the comment is not consumed.
 *
 * "perfect attachment" doesn't make sense for end of line comments:
 *
 *       {
 *         x: 0,
 *         y: 0
 *       }
 *
 * One of these will be "perfectly attached" to the zero and the other won't.
 * Why should the comma have such an influence? Trailing commas and semicolons
 * may be inserted or removed, an we need end-of-line comments to never be
 * impacted by that. Therefore, never try to "perfectly" attach EOL comments.
 *)
let rec tryPerfectlyAttachComment layout = function
  | None -> (layout, None)
  | Some comment -> perfectlyAttachComment comment layout

and perfectlyAttachComment comment = function
  | Layout.Sequence (listConfig, subLayouts) ->
    let distributeCommentIntoSubLayouts (i, processed, newComment) layout =
      let (layout, newComment) =
        tryPerfectlyAttachComment layout newComment in
      (i + 1, layout::processed, newComment)
    in
    let (_, processed, consumed) =
      List.fold_left
        distributeCommentIntoSubLayouts
        (0, [], Some comment) (List.rev subLayouts)
    in
    Layout.Sequence (listConfig, processed), consumed
  | Layout.Label (labelFormatter, left, right) ->
    let (newRight, comment) = perfectlyAttachComment comment right in
    let (newLeft, comment) = tryPerfectlyAttachComment left comment in
    Layout.Label (labelFormatter, newLeft, newRight), comment
  | Layout.SourceMap (loc, subLayout) ->
    let commloc = Comment.location comment in
    if loc.loc_end.Lexing.pos_lnum = loc.loc_start.Lexing.pos_lnum &&
       commloc.loc_start.Lexing.pos_cnum = loc.loc_end.Lexing.pos_cnum
    then
      (Layout.SourceMap (loc, makeList ~inline:(true, true) ~break:Always
                    [unbreaklayout (attachCommentToNodeRight subLayout comment)]),
       None)
    else
      let (layout, comment) = perfectlyAttachComment comment subLayout in
      begin match comment with
        | None -> (Layout.SourceMap (loc, layout), None)
        | Some comment ->
          if commloc.loc_end.Lexing.pos_cnum =
             loc.loc_start.Lexing.pos_cnum  then
            (Layout.SourceMap (loc, attachCommentToNodeLeft comment layout), None)
          else if commloc.loc_start.Lexing.pos_cnum = loc.loc_end.Lexing.pos_cnum then
            (Layout.SourceMap (loc, attachCommentToNodeRight layout comment), None)
          else
            (Layout.SourceMap (loc, layout), Some comment)
      end
  | Whitespace(info, subLayout) ->
      begin match perfectlyAttachComment comment subLayout with
      | (newLayout, None) -> (Whitespace(info, newLayout), None)
      | (newLayout, Some c) -> (Whitespace(info, newLayout), Some c)
      end
  | layout -> (layout, Some comment)

let insertRegularComment layout comment =
  match perfectlyAttachComment comment layout with
  | (layout, None) -> layout
  | (layout, Some _) ->
      looselyAttachComment ~breakAncestors:false layout comment

let insertEndOfLineComment layout comment =
  looselyAttachComment ~breakAncestors:true layout comment

let rec partitionComments_ ((singleLines, endOfLines, regulars) as soFar) = function
  | [] -> soFar
  | com :: tl ->
    match Comment.category com with
    | Comment.EndOfLine ->
      partitionComments_ (singleLines, (com :: endOfLines), regulars) tl
    | Comment.SingleLine ->
      partitionComments_ ((com :: singleLines), endOfLines, regulars) tl
    | Comment.Regular ->
      partitionComments_ (singleLines, endOfLines, (com :: regulars)) tl

let partitionComments comments =
  let (singleLines, endOfLines, regulars) =
    partitionComments_ ([], [], []) comments in
  (singleLines, List.rev endOfLines, regulars)

(*
 * Partition single line comments based on a location into two lists:
 * - one contains the comments before/same height of that location
 * - the other contains the comments after the location
 *)
let partitionSingleLineComments loc singleLineComments =
  let (before, after) = List.fold_left (fun (before, after) comment ->
    let cl = Comment.location comment in
    let isAfter = loc.loc_end.pos_lnum < cl.loc_start.pos_lnum in
    if isAfter then
      (before, comment::after)
    else
      (comment::before, after)
  ) ([], []) singleLineComments
  in (List.rev before, after)

(*
 * appends all [singleLineComments] after the [layout].
 * [loc] marks the end of [layout]
 *)
let appendSingleLineCommentsToEnd loc layout singleLineComments =
  let rec aux prevLoc layout i = function
    | comment::cs ->
        let loc = Comment.location comment in
        let formattedComment = formatComment comment in
        let commentLayout = if Reason_location.hasSpaceBetween loc prevLoc then
          insertBlankLines 1 formattedComment
        else
          formattedComment
        in
        (* The initial layout breaks ugly with `breakline`,
         * an inline list (that never breaks) fixes this *)
        let newLayout = if i == 0 then
          makeList ~inline:(true, true) ~break:Never [layout; commentLayout]
        else
          breakline layout commentLayout
        in
        aux loc newLayout (i + 1) cs
    | [] -> layout
  in
  aux loc layout 0 singleLineComments

(*
 * For simplicity, the formatting of comments happens in two parts in context of a source map:
 * 1) insert the singleLineComments with the interleaving algorithm contained in
 *    `insertSingleLineComment` for all comments overlapping with the sourcemap.
 *    A `Layout.Whitespace` node signals an intent to preserve whitespace here.
 * 2) SingleLineComments after the sourcemap, e.g. at the end of .re/.rei file,
 *    get attached with `appendSingleLineCommentsToEnd`. Due to the fact there
 *    aren't any real ocaml ast nodes anymore after the sourcemap (end of a
 *    file), the printing of the comments can be done in one pass with
 *    `appendSingleLineCommentsToEnd`. This is more performant and
 *    simplifies the implementation of comment attachment.
 *)
let attachSingleLineComments singleLineComments = function
  | Layout.SourceMap(loc, subLayout) ->
      let (before, after) = partitionSingleLineComments loc singleLineComments in
      let layout = List.fold_left insertSingleLineComment subLayout before in
      appendSingleLineCommentsToEnd loc layout after
  | layout ->
    List.fold_left insertSingleLineComment layout singleLineComments

let format_layout ?comments ppf layout =
  let easy = match comments with
    | None -> Layout.to_easy_format layout
    | Some comments ->
      let (singleLines, endOfLines, regulars) = partitionComments comments in
      (* TODO: Stop generating multiple versions of the tree, and instead generate one new tree. *)
      (* Layout.dump Format.std_formatter layout; *)
      let layout = List.fold_left insertRegularComment layout regulars in
      let layout = consolidateSeparator layout in
      let layout = List.fold_left insertEndOfLineComment layout endOfLines in
      (* Layout.dump Format.std_formatter layout; *)
      let layout = attachSingleLineComments singleLines layout in
      (* Layout.dump Format.std_formatter layout; *)
      let layout = insertLinesAboveItems layout in
      let layout = Layout.to_easy_format layout in
      (* Layout.dump_easy Format.std_formatter layout; *)
      layout
  in
  let buf = Buffer.create 1000 in
  let fauxmatter = Format.formatter_of_buffer buf in
  let _ = Format.pp_set_margin fauxmatter settings.width in
  if debugWithHtml.contents then
    Easy_format.Pretty.define_styles fauxmatter html_escape html_style;
  let _ = Easy_format.Pretty.to_formatter fauxmatter easy in
  let trimmed = Reason_syntax_util.processLineEndingsAndStarts (Buffer.contents buf) in
  Format.fprintf ppf "%s\n" trimmed;
  Format.pp_print_flush ppf ()

let partitionFinalWrapping listTester wrapFinalItemSetting x =
  let rev = List.rev x in
  match (rev, wrapFinalItemSetting) with
    | ([], _) -> raise (NotPossible "shouldnt be partitioning 0 label attachments")
    | (_, NeverWrapFinalItem) -> None
    | (last::revEverythingButLast, WrapFinalListyItemIfFewerThan max) ->
        if not (listTester last) || (List.length x) >= max then
          None
        else
          Some (List.rev revEverythingButLast, last)

let semiTerminated term = makeList [term; atom ";"]

(* postSpace is so that when comments are interleaved, we still use spacing rules. *)
let makeLetSequence ?(wrap=("{", "}")) letItems =
  makeList
    ~break:Always_rec
    ~inline:(true, false)
    ~wrap
    ~postSpace:true
    ~sep:(SepFinal (";", ";"))
    letItems

let makeLetSequenceSingleLine ?(wrap=("{", "}")) letItems =
  makeList
    ~break:IfNeed
    ~inline:(true, false)
    ~wrap
    ~preSpace:true
    ~postSpace:true
    ~sep:(Sep ";")
    letItems

(* postSpace is so that when comments are interleaved, we still use spacing rules. *)
let makeUnguardedLetSequence ?(sep=(Layout.SepFinal (";", ";"))) letItems =
  makeList
    ~break:Always_rec
    ~inline:(true, true)
    ~wrap:("", "")
    ~indent:0
    ~postSpace:true
    ~sep
    letItems

let formatSimpleAttributed x y =
  makeList
    ~wrap:("(", ")")
    ~break:IfNeed
    ~indent:0
    ~postSpace:true
    (List.concat [y; [x]])

let formatAttributed ?(labelBreak=`Auto) x y =
  label
    ~break:labelBreak
    ~indent:0
    ~space:true
    (makeList ~inline:(true, true) ~postSpace:true y)
    x

(* For when the type constraint should be treated as a separate breakable line item itself
   not docked to some value/pattern label.
   fun x
       y
       : retType => blah;
 *)
let formatJustTheTypeConstraint typ =
  makeList ~postSpace:false ~sep:(Sep " ") [atom ":"; typ]

let formatTypeConstraint one two =
  label ~space:true (makeList ~postSpace:false [one; atom ":"]) two

let formatCoerce expr optType coerced =
  match optType with
    | None ->
      label ~space:true (makeList ~postSpace:true [expr; atom ":>"]) coerced
    | Some typ ->
      label ~space:true (makeList ~postSpace:true [formatTypeConstraint expr typ; atom ":>"]) coerced


(* Standard function application style indentation - no special wrapping
 * behavior.
 *
 * Formats like this:
 *
 *   let result =
 *     someFunc
 *       (10, 20);
 *
 *
 * Instead of this:
 *
 *   let result =
 *     someFunc (
 *       10,
 *       20
 *     );
 *
 * The outer list wrapping fixes #566: format should break the whole
 * application before breaking arguments.
 *)
let formatIndentedApplication headApplicationItem argApplicationItems =
  makeList ~inline:(true, true) ~postSpace:true ~break:IfNeed [
    label
      ~space:true
      headApplicationItem
      (makeAppList argApplicationItems)
  ]


(* The loc, is an optional location or the returned app terms *)
let formatAttachmentApplication finalWrapping (attachTo: (bool * Layout.t) option) (appTermItems, loc) =
  let partitioning = finalWrapping appTermItems in
  match partitioning with
    | None -> (
        match (appTermItems, attachTo) with
          | ([], _) -> raise (NotPossible "No app terms")
          | ([hd], None) -> source_map ?loc hd
          | ([hd], (Some (useSpace, toThis))) -> label ~space:useSpace toThis (source_map ?loc hd)
          | (hd::tl, None) ->
            source_map ?loc (formatIndentedApplication hd tl)
          | (hd::tl, (Some (useSpace, toThis))) ->
            label
              ~space:useSpace
              toThis
              (source_map ?loc (formatIndentedApplication hd tl))
      )
    | Some (attachedList, wrappedListy) -> (
        match (attachedList, attachTo) with
        | ([], Some (useSpace, toThis)) ->
          label ~space:useSpace toThis (source_map ?loc wrappedListy)
        | ([], None) ->
          (* Not Sure when this would happen *)
          source_map ?loc wrappedListy
        | (_::_, Some (useSpace, toThis)) ->
          (* TODO: Can't attach location to this - maybe rewrite anyways *)
          let attachedArgs = makeAppList attachedList in
          label ~space:useSpace toThis
            (label ~space:true attachedArgs wrappedListy)
        | (_::_, None) ->
          (* Args that are "attached to nothing" *)
          let appList = makeAppList attachedList in
          source_map ?loc (label ~space:true appList wrappedListy)
      )

(*
  Preprocesses an expression term for the sake of label attachments ([letx =
  expr]or record [field: expr]). Function application should have special
  treatment when placed next to a label. (The invoked function term should
  "stick" to the label in some cases). In others, the invoked function term
  should become a new label for the remaining items to be indented under.
 *)
let applicationFinalWrapping x =
  partitionFinalWrapping isSequencey settings.funcApplicationLabelStyle x

let curriedFunctionFinalWrapping x =
  partitionFinalWrapping isSequencey settings.funcCurriedPatternStyle x

let typeApplicationFinalWrapping typeApplicationItems =
  partitionFinalWrapping isSequencey settings.funcApplicationLabelStyle typeApplicationItems


(* add parentheses to binders when they are in fact infix or prefix operators *)
let protectIdentifier txt =
  let needs_parens = needs_parens txt in
  let txt =
    if is_andop txt || is_letop txt then
      Reason_syntax_util.compress_letop_identifier txt
    else
      txt
  in
  if not needs_parens then atom txt
  else if needs_spaces txt then makeList ~wrap:("(", ")") ~pad:(true, true) [atom txt]
  else atom ("(" ^ txt ^ ")")

let protectLongIdentifier longPrefix txt =
  makeList [longPrefix; atom "."; protectIdentifier txt]

let paren b fu ppf x =
  if b
  then Format.fprintf ppf "(%a)" fu x
  else fu ppf x

let constant_string_for_primitive ppf s =
  let hasQuote = try String.index s '"' with Not_found -> -1 in
  let hasNewline = try String.index s '\n' with Not_found -> -1 in
  if hasQuote > -1 || hasNewline > -1 then
    Format.fprintf ppf "{|%s|}" s
  else Format.fprintf ppf "%S" s

let tyvar ppf str =
  Format.fprintf ppf "'%s" str

(* In some places parens shouldn't be printed for readability:
 * e.g. Some((-1)) should be printed as Some(-1)
 * In `1 + (-1)` -1 should be wrapped in parens for readability
 *)
let constant ?raw_literal ?(parens=true) ppf = function
  | Pconst_char i ->
    Format.fprintf ppf "%C"  i
  | Pconst_string (i, None) ->
    begin match raw_literal with
      | Some text ->
        Format.fprintf ppf "\"%s\"" text
      | None ->
        Format.fprintf ppf "\"%s\"" (Reason_syntax_util.escape_string i)
    end
  | Pconst_string (i, Some delim) ->
    Format.fprintf ppf "{%s|%s|%s}" delim i delim
  | Pconst_integer (i, None) ->
    paren (parens && i.[0] = '-')
      (fun ppf -> Format.fprintf ppf "%s") ppf i
  | Pconst_integer (i, Some m) ->
    paren (parens && i.[0] = '-')
      (fun ppf (i, m) -> Format.fprintf ppf "%s%c" i m) ppf (i,m)
  | Pconst_float (i, None) ->
    paren (parens && i.[0] = '-')
      (fun ppf -> Format.fprintf ppf "%s") ppf i
  | Pconst_float (i, Some m) ->
    paren (parens && i.[0] = '-')
      (fun ppf (i,m) -> Format.fprintf ppf "%s%c" i m) ppf (i,m)

let is_punned_labelled_pattern_no_attrs p lbl = match p.ppat_attributes, p.ppat_desc with
  | _::_, _ -> false
  | [], Ppat_constraint ({ ppat_desc = Ppat_var { txt }; ppat_attributes=[]}, _)
  | [], Ppat_var { txt } -> txt = lbl
  | _ -> false

let isLongIdentWithDot = function
  | Ldot _ -> true
  | _ -> false

(* Js.t -> useful for bucklescript sugar `Js.t({. foo: bar})` -> `{. "foo": bar}` *)
let isJsDotTLongIdent ident = match ident with
  | Ldot (Lident "Js", "t") -> true
  | _ -> false

let recordRowIsPunned pld =
      let name = pld.pld_name.txt in
      (match pld.pld_type with
        | { ptyp_desc = (
            Ptyp_constr (
              { txt },
              (* don't pun parameterized types, e.g. {tag: tag 'props} *)
              [])
            );
            (* Don't pun types that have attributes attached, e.g. { foo: [@bar] foo } *)
            ptyp_attributes = [];
          _}
            when
            (Longident.last txt = name
              (* Don't pun types from other modules, e.g. type bar = {foo: Baz.foo}; *)
              && isLongIdentWithDot txt == false) -> true
        | _ -> false)

let isPunnedJsxArg lbl ident =
  not (isLongIdentWithDot ident.txt) && (Longident.last ident.txt) = lbl

let is_unit_pattern x = match x.ppat_desc with
  | Ppat_construct ( {txt= Lident"()"}, None) -> true
  | _ -> false

let is_ident_pattern x = match x.ppat_desc with
  | Ppat_var _ -> true
  | _ -> false

let is_any_pattern x = x.ppat_desc = Ppat_any

let is_direct_pattern x = x.ppat_attributes == [] && match x.ppat_desc with
  | Ppat_construct ( {txt= Lident"()"}, None) -> true
  | _ -> false

let isJSXComponent expr =
  match expr with
  | ({pexp_desc= Pexp_apply ({pexp_desc=Pexp_ident _}, args); pexp_attributes})
  | ({pexp_desc= Pexp_apply ({pexp_desc=Pexp_letmodule(_,_,_)}, args); pexp_attributes}) ->
    let {jsxAttrs} = partitionAttributes pexp_attributes in
    let hasLabelledChildrenLiteral = List.exists (function
      | (Labelled "children", _) -> true
      | _ -> false
    ) args in
    let rec hasSingleNonLabelledUnitAndIsAtTheEnd l = match l with
    | [] -> false
    | (Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, _)}) :: [] -> true
    | (Nolabel, _) :: _ -> false
    | _ :: rest -> hasSingleNonLabelledUnitAndIsAtTheEnd rest
    in
    if jsxAttrs != []
       && hasLabelledChildrenLiteral
       && hasSingleNonLabelledUnitAndIsAtTheEnd args
    then
      true
    else
      false
  | _ -> false

(* Some cases require special formatting when there's a function application
 * with a single argument containing some kind of structure with braces/parens/brackets.
 * Example: `foo({a: 1, b: 2})` needs to be formatted as
 *  foo({
 *    a: 1,
 *    b: 2
 *  })
 *  when the line length dictates breaking. Notice how `({` and `})` 'hug'.
 *  Also applies to (poly)variants because they can be seen as a form of "function application".
 *  This function says if a list of expressions fulfills the need to be formatted like
 *  the example above. *)
let isSingleArgParenApplication = function
  | [{pexp_attributes = []; pexp_desc = Pexp_record _}]
  | [{pexp_attributes = []; pexp_desc = Pexp_tuple _}]
  | [{pexp_attributes = []; pexp_desc = Pexp_array _}]
  | [{pexp_attributes = []; pexp_desc = Pexp_object _}] -> true
  | [{pexp_attributes = []; pexp_desc = Pexp_extension (s, _)}] when s.txt = "bs.obj" -> true
  | [({pexp_attributes = []} as exp)] when (is_simple_list_expr exp) -> true
  | _ -> false

(*
 * Determines if the arguments of a constructor pattern match need
 * special printing. If there's one argument & they have some kind of wrapping,
 * they're wrapping need to 'hug' the surrounding parens.
 * Example:
 *  switch x {
 *  | Some({
 *      a,
 *      b,
 *    }) => ()
 *  }
 *
 *  Notice how ({ and }) hug.
 *  This applies for records, arrays, tuples & lists.
 *  See `singleArgParenPattern` for the acutal formatting
 *)
let isSingleArgParenPattern = function
  | [{ppat_attributes = []; ppat_desc = Ppat_record _}]
  | [{ppat_attributes = []; ppat_desc = Ppat_array _}]
  | [{ppat_attributes = []; ppat_desc = Ppat_tuple _}] -> true
  | [{ppat_attributes = []; ppat_desc = Ppat_construct (({txt=Lident "::"}), _)}] -> true
  | _ -> false

(* Flattens a resolvedRule into a list of infixChain nodes.
 * When foo |> f |> z gets parsed, we get the following tree:
 *         |>
 *        /  \
 *    foo      |>
 *            /  \
 *          f      z
 * To format this recursive tree in a way that allows nice breaking
 * & respects the print-width, we need some kind of flattened
 * version of the above tree. `computeInfixChain` transforms the tree
 * in a flattened version which allows flexible formatting.
 * E.g. we get
 *  [LayoutNode foo; InfixToken |>; LayoutNode f; InfixToken |>; LayoutNode z]
 *)
let rec computeInfixChain = function
  | LayoutNode layoutNode -> [Layout layoutNode]
  | InfixTree (op, leftResolvedRule, rightResolvedRule) ->
      (computeInfixChain leftResolvedRule) @ [InfixToken op] @ (computeInfixChain rightResolvedRule)

let equalityOperators = ["!="; "!=="; "==="; "=="; ">="; "<="; "<"; ">"]

(* Formats a flattened list of infixChain nodes into a list of layoutNodes
 * which allow smooth line-breaking
 * e.g. [LayoutNode foo; InfixToken |>; LayoutNode f; InfixToken |>; LayoutNode z]
 * becomes
 * [
 *   foo
 * ; |> f        --> label
 * ; |> z        --> label
 * ]
 * If you make a list out of this items, we get smooth line breaking
 *  foo |> f |> z
 * becomes
 *  foo
 *  |> f
 *  |> z
 *  when the print-width forces line breaks.
 *)
let formatComputedInfixChain infixChainList =
  let layout_of_group group currentToken =
    (* Represents the `foo` in
     * foo
     * |> f
     * |> z *)
    if List.length group < 2 then
      makeList ~inline:(true, true) ~sep:(Sep " ") group
    (* Basic equality operators require special formatting, we can't give it
     * 'classic' infix operator formatting, otherwise we would get
     * let example =
     *  true
     *  != false
     *  && "a"
     *  == "b"
     *  *)
    else if List.mem currentToken equalityOperators then
      let hd = List.hd group in
      let tl = makeList ~inline:(true, true) ~sep:(Sep " ") (List.tl group) in
      makeList ~inline:(true, true) ~sep:(Sep " ") ~break:IfNeed [hd; tl]
    else if currentToken.[0] = '#' then
      let isSharpEqual = currentToken = sharpOpEqualToken in
      makeList ~postSpace:isSharpEqual group
    else
      (* Represents `|> f` in foo |> f
       * We need a label here to indent possible closing parens
       * on the same height as the infix operator
       * e.g.
       * >|= (
       *   fun body =>
       *     Printf.sprintf
       *       "okokok" uri meth headers body
       * )   <-- notice how this closing paren is on the same height as >|=
       *)
      label ~break:`Never ~space:true (atom currentToken) (List.nth group 1)
  in
  let rec print acc group currentToken l =
    match l with
    | x::xs -> (match x with
      | InfixToken t ->
          (* = or := *)
          if List.mem t requireIndentFor then
            let groupNode =
              makeList ~inline:(true, true) ~sep:(Sep " ") ((print [] group currentToken []) @ [atom t])
            in
            let children =
              makeList ~inline:(true, true) ~preSpace:true ~break:IfNeed
                (print [] [] t xs)
            in
            print (acc @ [label ~space:true groupNode children]) [] t []
          (* Represents:
           * List.map @@
           * List.length
           *
           * Notice how we want the `@@` on the first line.
           * Extra indent puts pressure on the subsequent line lengths
           * *)
          else if t = "@@" then
            let groupNode =
              makeList ~inline:(true, true) ~sep:(Sep " ") (group @ [atom t])
            in
            print (acc @ [groupNode]) [] t xs
          (* != !== === == >= <= < > etc *)
          else if List.mem t equalityOperators then
            print acc ((print [] group currentToken []) @ [atom t]) t xs
          else
            begin if requireNoSpaceFor t then
              begin if (currentToken = "" || requireNoSpaceFor currentToken) then
                print acc (group@[atom t]) t xs
              else
                (* a + b + foo##bar##baz
                 * `foo` needs to be picked from the current group
                 * and inserted into a new one. This way `foo`
                 * gets the special "chained"-printing:
                 * foo##bar##baz. *)
                 begin match List.rev group with
                 |  hd::tl ->
                     let acc =
                       acc @ [layout_of_group (List.rev tl) currentToken]
                      in
                     print acc [hd; atom t] t xs
                 | [] -> print acc (group@[atom t]) t xs
                 end
              end
            else
              print (acc @ [layout_of_group group currentToken]) [(atom t)] t xs
            end
      | Layout layoutNode -> print acc (group @ [layoutNode]) currentToken xs
      )
    | [] ->
      if List.mem currentToken requireIndentFor then
        acc @ group
        else
          acc @ [layout_of_group group currentToken]
  in
  let l = print [] [] "" infixChainList in
  makeList ~inline:(true, true) ~sep:(Sep " ") ~break:IfNeed l

(**
 * [groupAndPrint] will print every item in [items] according to the function [xf].
 * [getLoc] will extract the location from an item. Based on the difference
 * between the location of two items, if there's whitespace between the two
 * (taken possible comments into account), items get grouped.
 * Every group designates a series of layout nodes "in need
 * of whitespace above". A group gets decorated with a Whitespace node
 * containing enough info to interleave whitespace at a later time during
 * printing.
 *)
let groupAndPrint ~xf ~getLoc ~comments items =
  let rec group prevLoc curr acc = function
    (* group items *)
    | x::xs ->
        let item = xf x in
        let loc = getLoc x in
        (* Get the range between the current and previous item
         * Example:
         * 1| let a = 1;
         * 2|            --> this is the range between the two
         * 3| let b = 2;
         * *)
        let range = Range.makeRangeBetween prevLoc loc in
        (* If there's whitespace interleaved, append the new layout node
         * to a new group, otherwise keep it in the current group.
         * Takes possible comments interleaved into account.
         *
         * Example:
         * 1| let a = 1;
         * 2|
         * 3| let b = 2;
         * 4| let c = 3;
         * `let b = 2` will mark the start of a new group
         * `let c = 3` will be added to the group containing `let b = 2`
         *)
        if Range.containsWhitespace ~range ~comments () then
          group loc [(range, item)] ((List.rev curr)::acc) xs
        else
          group loc ((range, item)::curr) acc xs
    (* convert groups into "Layout.Whitespace" *)
    | [] ->
        let groups = List.rev ((List.rev curr)::acc) in
        List.mapi (fun i group -> match group with
          | curr::xs ->
              let (range, x) = curr in
              (* if this is the first group of all "items", the number of
               * newlines interleaved should be 0, else we collapse all newlines
               * to 1.
               *
               * Example:
               * module Abc = {
               *   let a = 1;
               *
               *   let b = 2;
               * }
               * `let a = 1` should be wrapped in a `Layout.Whitespace` because a
               * user might put comments above the `let a = 1`.
               * e.g.
               * module Abc = {
               *   /* comment 1 */
               *
               *   /* comment 2 */
               *   let a = 1;
               *
               *  A Whitespace-node will automatically take care of the whitespace
               *  interleaving between the comments.
               *)
              let newlines = if i > 0 then 1 else 0 in
              let region = WhitespaceRegion.make ~range ~newlines () in
              let firstLayout = Layout.Whitespace(region, x) in
              (* the first layout node of every group taks care of the
               * whitespace above a group*)
              (firstLayout::(List.map snd xs))
          | [] -> []
        ) groups
  in
  match items with
  | first::rest ->
      List.concat (group (getLoc first) [] [] (first::rest))
  | [] -> []

let printer = object(self:'self)
  val pipe = false
  val semi = false

  val inline_braces = false
  val preserve_braces = true

  (* *Mutable state* in the printer to keep track of all comments
   * Used when whitespace needs to be interleaved.
   * The printing algorithm needs to take the comments into account in between
   * two items, to correctly determine if there's whitespace between two items.
   * The ast doesn't know if there are comments between two items, since
   * comments are store separately. The location diff between two items
   * might indicate whitespace between the two. While in reality there are
   * comments filling that whitespace. The printer needs access to the comments
   * for this reason.
   *
   * Example:
   * 1| let a = 1;
   * 2|
   * 3|
   * 4| let b = 2;
   *  -> here we can just diff the locations between `let a = 1` and `let b = 2`
   *
   * 1| let a = 1;
   * 2| /* a comment */
   * 3| /* another comment */
   * 4| let b = 2;
   *  -> here the location diff will result into false info if we don't include
   *  the comments in the diffing
   *)
  val mutable comments = []

  method comments = comments
  method trackComment comment = comments <- comment::comments

  (* The test and first branch of ternaries must be guarded *)
  method under_pipe = {<pipe=true>}
  method under_semi = {<semi=true>}
  method reset_semi = {<semi=false>}
  method reset_pipe = {<pipe=false>}
  method reset = {<pipe=false;semi=false>}

  method inline_braces = {<inline_braces=true>}
  method dont_preserve_braces = {<preserve_braces=false>}
  method reset_request_braces = {<inline_braces=false; preserve_braces=true>}


  method longident = function
    | Lident s -> (protectIdentifier s)
    | Ldot(longPrefix, s) ->
        (protectLongIdentifier (self#longident longPrefix) s)
    | Lapply (y,s) -> makeList [self#longident y; atom "("; self#longident s; atom ")";]

  (* This form allows applicative functors. *)
  method longident_class_or_type_loc x = self#longident x.txt
  (* TODO: Fail if observing applicative functors for this form. *)
  method longident_loc (x:Longident.t Location.loc) =
    source_map ~loc:x.loc (self#longident x.txt)

  method constant ?raw_literal ?(parens=true) =
    wrap (constant ?raw_literal ~parens)

  method constant_string_for_primitive = wrap constant_string_for_primitive
  method tyvar = wrap tyvar

  (* c ['a,'b] *)
  method class_params_def = function
    | [] -> atom ""
    | l -> makeTup (List.map self#type_param l)

  (* This will fall through to the simple version. *)
  method non_arrowed_core_type x = self#non_arrowed_non_simple_core_type x

  method core_type2 x =
    let {stdAttrs; uncurried} = partitionAttributes x.ptyp_attributes in
    let uncurried = uncurried || try Hashtbl.find uncurriedTable x.ptyp_loc with | Not_found -> false in
    if stdAttrs != [] then
      formatAttributed
        (self#non_arrowed_simple_core_type {x with ptyp_attributes = []})
        (self#attributes stdAttrs)
    else
      let x = if uncurried then { x with ptyp_attributes = [] } else x in
      match x.ptyp_desc with
        | Ptyp_arrow _ ->
          let rec allArrowSegments ?(uncurried=false) acc = function
            | { ptyp_desc = Ptyp_arrow (l, ct1, ct2); ptyp_attributes = []} ->
              allArrowSegments ~uncurried:false
                ((l,ct1, false || uncurried) :: acc) ct2
            | rhs ->
              let rhs = self#core_type2 rhs in
              let is_tuple typ = match typ.ptyp_desc with
                | Ptyp_tuple _ -> true
                | _ -> false
              in
              match acc with
              | [(Nolabel, lhs, uncurried )] when not (is_tuple lhs) ->
                  let t = self#non_arrowed_simple_core_type lhs in
                  let lhs = if uncurried then
                    makeList ~wrap:("(. ", ")") ~postSpace:true [t]
                  else t in
                (lhs, rhs)
              | acc ->
                let params = List.rev_map self#type_with_label acc in
                (makeCommaBreakableListSurround "(" ")" params, rhs)
          in
          let (lhs, rhs) = allArrowSegments ~uncurried [] x in
          let normalized = makeList
              ~preSpace:true ~postSpace:true ~inline:(true, true)
              ~break:IfNeed ~sep:(Sep "=>") [lhs; rhs]
          in source_map ~loc:x.ptyp_loc normalized
        | Ptyp_poly (sl, ct) ->
          let ct = self#core_type ct in
          let poly = match sl with
          | [] -> ct
          | sl ->
            makeList ~break:IfNeed ~postSpace:true [
              makeList [
                makeList ~postSpace:true (List.map (fun {txt} -> self#tyvar txt) sl);
                atom ".";
              ];
              ct
            ]
          in source_map ~loc:x.ptyp_loc poly
        | _ -> self#non_arrowed_core_type x

  (* Same as core_type2 but can be aliased *)
  method core_type x =
    let {stdAttrs; uncurried} = partitionAttributes x.ptyp_attributes in
    let () = if uncurried then Hashtbl.add uncurriedTable x.ptyp_loc true in
    if stdAttrs != [] then
      formatAttributed
        (self#non_arrowed_simple_core_type {x with ptyp_attributes = []})
        (self#attributes stdAttrs)
    else match x.ptyp_desc with
      | (Ptyp_alias (ct, s)) ->
        source_map ~loc:x.ptyp_loc
          (label
             ~space:true
             (self#core_type ct)
             (makeList ~postSpace:true [atom "as"; atom ("'" ^ s)]))
      | _ -> self#core_type2 x

  method type_with_label (lbl, c, uncurried) =
    let typ = self#core_type c in
    let t = match lbl with
    | Nolabel -> typ
    | Labelled lbl ->
        makeList ~sep:(Sep " ") [atom (namedArgSym ^ lbl ^ ":"); typ]
    | Optional lbl ->
        makeList ~sep:(Sep " ") [atom (namedArgSym ^ lbl ^ ":"); label typ (atom "=?")]
    in
    if uncurried then
      makeList ~postSpace:true [atom "."; t]
    else t

  method type_param (ct, a) =
    makeList [atom (type_variance a); self#core_type ct]

  (* According to the parse rule [type_declaration], the "type declaration"'s
   * physical location (as indicated by [td.ptype_loc]) begins with the
   * identifier and includes the constraints. *)
  method formatOneTypeDef prepend name assignToken ({ptype_params; ptype_kind; ptype_loc} as td) =
    let (equalInitiatedSegments, constraints) = (self#type_declaration_binding_segments td) in
    let formattedTypeParams = List.map self#type_param ptype_params in
    let binding = makeList ~postSpace:true [prepend;name] in
    (*
        /-----------everythingButConstraints--------------  | -constraints--\
       /-innerL---| ------innerR--------------------------\
      /binding\     /typeparams\ /--equalInitiatedSegments-\
      type name      'v1    'v1  =  foo = private bar        constraint a = b
    *)

    let labelWithParams = match formattedTypeParams with
      | [] -> binding
      | l -> label binding (makeTup l)
    in
    let everythingButConstraints =
      let nameParamsEquals = makeList ~postSpace:true [labelWithParams; assignToken] in
      match equalInitiatedSegments with
        | [] -> labelWithParams
        | _::_::_::_ -> raise (NotPossible "More than two type segments.")
        | hd::[] ->
            formatAttachmentApplication
              typeApplicationFinalWrapping
              (Some (true, nameParamsEquals))
              (hd, None)
        | hd::hd2::[] ->
            let first = makeList ~postSpace:true ~break:IfNeed ~inline:(true, true) (hd @ [atom "="]) in
            (*
             * Because we want a record as a label with the opening brace on the same line
             * and the closing brace indented at the beginning, we can't wrap it in a list here
             * Example:
             * type doubleEqualsRecord =
             *  myRecordWithReallyLongName = {   <- opening brace on the same line
             *    xx: int,
             *    yy: int
             *  };                               <- closing brace indentation
             *)
            let second = match ptype_kind with
              | Ptype_record _ -> List.hd hd2
              | _ -> makeList ~postSpace:true ~break:IfNeed ~inline:(true, true) hd2
            in
            label ~space:true nameParamsEquals (
              label ~space:true first second
            )
    in
    let everything =
      match constraints with
        | [] -> everythingButConstraints
        | hd::tl -> makeList ~break:IfNeed ~postSpace:true ~indent:0 ~inline:(true, true) (everythingButConstraints::hd::tl)
    in
    source_map ~loc:ptype_loc everything

  method formatOneTypeExt prepend name assignToken te =
    let privateAtom = (atom "pri") in
    let privatize scope lst = match scope with
      | Public -> lst
      | Private -> privateAtom::lst in
    let equalInitiatedSegments =
      let segments = List.map self#type_extension_binding_segments te.ptyext_constructors in
      let privatized_segments = privatize te.ptyext_private segments in
      [makeList ~break:Always_rec ~postSpace:true ~inline:(true, true) privatized_segments] in
    let formattedTypeParams = List.map self#type_param te.ptyext_params in
    let binding = makeList ~postSpace:true (prepend::name::[]) in
    let labelWithParams = match formattedTypeParams with
      | [] -> binding
      | l -> label binding (makeTup l)
    in
    let everything =
      let nameParamsEquals = makeList ~postSpace:true [labelWithParams; assignToken] in
      formatAttachmentApplication
             typeApplicationFinalWrapping
             (Some (true, nameParamsEquals))
             (equalInitiatedSegments, None)
    in
    source_map ~loc:te.ptyext_path.loc everything

  method type_extension_binding_segments {pext_kind; pext_loc; pext_attributes; pext_name} =
    let normalize lst = match lst with
        | [] -> raise (NotPossible "should not be called")
        | [hd] -> hd
        | _::_ -> makeList lst
      in
      let add_bar name attrs args =
        let lbl = begin match args with
        | None -> name
        | Some args -> label name args
        end in
        if attrs != [] then
         label ~space:true
            (makeList
              ~postSpace:true
              [
                atom "|";
                makeList
                  ~postSpace:true
                  ~break:Layout.IfNeed
                  ~inline:(true, true)
                  (self#attributes attrs)
              ]
            )
            lbl
        else
          makeList ~postSpace:true [atom "|"; lbl]
      in
    let sourceMappedName = atom ~loc:pext_name.loc pext_name.txt in
    let resolved = match pext_kind with
      | Pext_decl (ctor_args, gadt) ->
        let formattedArgs = match ctor_args with
          | Pcstr_tuple [] -> []
          | Pcstr_tuple args -> [makeTup (List.map self#non_arrowed_non_simple_core_type args)]
          | Pcstr_record r -> [self#record_declaration ~wrap:("({", "})") r]
        in
        let formattedGadt = match gadt with
        | None -> None
        | Some x -> Some (
            makeList [
              formatJustTheTypeConstraint (self#core_type x)
            ]
          )
        in
        (formattedArgs, formattedGadt)
      (* type bar += Foo = Attr.Foo *)
      | Pext_rebind rebind ->
        let r = self#longident_loc rebind in
        (* we put an empty space before the '=': we don't have access to the fact
         * that we need a space because of the Pext_rebind later *)
        let prepend = (atom " =") in
        ([makeList ~postSpace:true [prepend; r]], None)
    in
      (*
        The first element of the tuple represents constructor arguments,
        the second an optional formatted gadt.

        Case 1: No constructor arguments, neither a gadt
          type attr = ..;
          type attr += | Str

        Case 2: No constructor arguments, is a gadt
          type attr = ..;
          type attr += | Str :attr

        Case 3: Has Constructor args, not a gadt
          type attr  = ..;
          type attr += | Str(string);
          type attr += | Point(int, int);

        Case 4: Has Constructor args & is a gadt
          type attr  = ..;
          type attr += | Point(int, int) :attr;
      *)
    let everything = match resolved with
      | ([], None) -> add_bar sourceMappedName pext_attributes None
      | ([], Some gadt) -> add_bar sourceMappedName pext_attributes (Some gadt)
      | (ctorArgs, None) -> add_bar sourceMappedName pext_attributes (Some (normalize ctorArgs))
      | (ctorArgs, Some gadt) -> add_bar sourceMappedName pext_attributes (Some (normalize (ctorArgs@[gadt])))
    in
    source_map ~loc:pext_loc everything

  (* shared by [Pstr_type,Psig_type]*)
  method type_def_list ?(eq_symbol="=") (rf, l) =
    (* As oposed to used in type substitution. *)
    let formatOneTypeDefStandard prepend td =
      let itm =
        self#formatOneTypeDef
          prepend
          (atom ~loc:td.ptype_name.loc td.ptype_name.txt)
          (atom eq_symbol)
          td
      in
      let {stdAttrs; docAttrs} = partitionAttributes ~partDoc:true td.ptype_attributes in
      let layout = self#attach_std_item_attrs stdAttrs itm in
      self#attachDocAttrsToLayout
        ~stdAttrs
        ~docAttrs
        ~loc:td.ptype_loc
        ~layout
        ()
    in

    match l with
      | [] -> raise (NotPossible "asking for type list of nothing")
      | hd::tl ->
          let first =
            match rf with
            | Recursive -> formatOneTypeDefStandard (atom "type") hd
            | Nonrecursive ->
                formatOneTypeDefStandard (atom "type nonrec") hd
          in
          match tl with
            (* Exactly one type *)
            | [] -> first
            | _::_ as typeList ->
                let items = (hd.ptype_loc, first)::(List.map (fun ptyp ->
                    (ptyp.ptype_loc, formatOneTypeDefStandard (atom "and") ptyp)
                  ) typeList
                ) in
                makeList ~indent:0 ~inline:(true, true) ~break:Always_rec (
                  groupAndPrint
                    ~xf:snd
                    ~getLoc:fst
                    ~comments:self#comments
                    items
                )

  method type_variant_list lst =
    match lst with
    | [] -> [atom "|"]
    | _ -> List.map (fun x -> self#type_variant_leaf x) lst

  method type_variant_leaf ?opt_ampersand:(a=false) ?polymorphic:(p=false) = self#type_variant_leaf1 a p true
  method type_variant_leaf_nobar ?opt_ampersand:(a=false) ?polymorphic:(p=false) = self#type_variant_leaf1 a p false

  (* TODOATTRIBUTES: Attributes on the entire variant leaf are likely
   * not parsed or printed correctly. *)
  method type_variant_leaf1 opt_ampersand polymorphic print_bar x =
    let {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} = x in
    let {stdAttrs; docAttrs} = partitionAttributes ~partDoc:true pcd_attributes in
    let ampersand_helper i arg =
      let ct = self#core_type arg in
      let ct = match arg.ptyp_desc with
        | Ptyp_tuple _ -> ct
        | _ -> makeTup [ct]
      in
      if i == 0 && not opt_ampersand then
        ct
      else
        label (atom "&") ct
    in
    let args = match pcd_args with
      | Pcstr_record r ->
          [self#record_declaration ~wrap:("({", "})") r]
      | Pcstr_tuple [] -> []
      | Pcstr_tuple l when polymorphic -> List.mapi ampersand_helper l
      (* Here's why this works. With the new syntax, all the args, are already inside of
        a safely guarded place like Constructor(here, andHere). Compare that to the
        previous syntax Constructor here andHere. In the previous syntax, we needed to
        require that we print "non-arrowed" types for here, and andHere to avoid
        something like Constructor a=>b c=>d. In the new syntax, we don't care if here
        and andHere have unguarded arrow types like a=>b because they're safely
        separated by commas.
       *)
      | Pcstr_tuple l -> [makeTup (List.map self#core_type l)]
    in
    let gadtRes = match pcd_res with
      | None -> None
      | Some x -> Some (
          formatJustTheTypeConstraint (self#core_type x)
        )
    in
    let normalize lst = match lst with
      | [] -> raise (NotPossible "should not be called")
      | [hd] -> hd
      | _::_ -> makeList ~inline:(true, true) ~break:IfNeed ~postSpace:true lst
    in
    let add_bar constructor =
      makeList ~postSpace:true (if print_bar then [atom "|"; constructor] else [constructor])
    in
    (* In some cases (e.g. inline records) we want the label with bar & the gadt resolution
     * as a list.
     *   | If {
     *       pred: expr bool,
     *       true_branch: expr 'a,
     *       false_branch: expr 'a
     *     }                           ==> end of label
     *     :expr 'a;                   ==> gadt res
     * The label & the gadt res form two separate units combined into a list.
     * This is necessary to properly align the closing '}' on the same height as the 'If'.
     *)
    let add_bar_2 ?gadt name args =
      let lbl = label name args in
      let fullLbl = match gadt with
        | Some g -> makeList ~inline:(true, true) ~break:IfNeed [lbl; g]
        | None -> lbl
      in
      add_bar fullLbl
    in

    let prefix = if polymorphic then "`" else "" in
    let sourceMappedName = atom ~loc:pcd_name.loc (prefix ^ pcd_name.txt) in
    let sourceMappedNameWithAttributes =
      let layout = match stdAttrs with
      | [] -> sourceMappedName
      | stdAttrs ->
        formatAttributed sourceMappedName (self#attributes stdAttrs)
      in
      match docAttrs with
      | [] -> layout
      | docAttrs ->
        makeList ~break:Always ~inline:(true, true) [
          makeList (self#attributes docAttrs);
          layout
        ]
    in
    let constructorName = makeList ~postSpace:true [sourceMappedNameWithAttributes] in
    let everything = match (args, gadtRes) with
      | ([], None) -> add_bar sourceMappedNameWithAttributes
      | ([], Some gadt) -> add_bar_2 sourceMappedNameWithAttributes gadt
      | (_::_, None) -> add_bar_2 constructorName (normalize args)
      | (_::_, Some gadt) ->
          (match pcd_args with
            | Pcstr_record _ -> add_bar_2 ~gadt constructorName (normalize args)
            | _ -> add_bar_2 constructorName ~gadt (normalize args))
    in
    source_map ~loc:pcd_loc everything

  method record_declaration ?(wrap=("{", "}")) ?assumeRecordLoc lbls =
    let recordRow pld =
      let hasPunning = recordRowIsPunned pld in
      let name =
        if hasPunning
        then [atom pld.pld_name.txt]
        else [atom pld.pld_name.txt; atom ":"]
      in
      let name = source_map ~loc:pld.pld_name.loc (makeList name) in
      let withMutable =
        match pld.pld_mutable with
        | Immutable -> name
        | Mutable -> makeList ~postSpace:true [atom "mutable"; name]
      in
      let recordRow = if hasPunning then
          label withMutable (atom "")
        else
          label ~space:true withMutable (self#core_type pld.pld_type)
      in
      let recordRow = match pld.pld_attributes with
      | [] -> recordRow
      | attrs ->
          let {stdAttrs; docAttrs} = partitionAttributes ~partDoc:true attrs in
        let stdAttrsLayout =
          makeList ~inline:(true, true) ~postSpace:true (self#attributes stdAttrs)
        in
        let docAttrsLayout = makeList ~inline:(true, true) (self#attributes docAttrs) in
        let children = match (docAttrs, stdAttrs) with
        | [], [] -> [recordRow]
        | _, [] -> [docAttrsLayout; recordRow]
        | [], _ -> [stdAttrsLayout; recordRow]
        | _, _ ->
            [docAttrsLayout; stdAttrsLayout; recordRow]
        in
        makeList ~inline:(true, true) ~break:Always_rec children
      in
      source_map ~loc:pld.pld_loc recordRow
    in
    let rows = List.map recordRow lbls in
    (* if a record has more than 2 rows, always break *)
    let break =
      if List.length rows >= 2
      then Layout.Always_rec
      else Layout.IfNeed
    in
    source_map ?loc:assumeRecordLoc
      (makeList ~wrap ~sep:commaTrail ~postSpace:true ~break rows)

  (* Returns the type declaration partitioned into three segments - one
     suitable for appending to a label, the actual type manifest
     and the list of constraints. *)
  method type_declaration_binding_segments x =
    (* Segments of the type binding (occuring after the type keyword) that
       should begin with "=". Zero to two total sections.
       This is just a straightforward reverse mapping from the original parser:
        type_kind:
            /*empty*/
              { (Ptype_abstract, Public, None) }
          | EQUAL core_type
              { (Ptype_abstract, Public, Some $2) }
          | EQUAL PRIVATE core_type
              { (Ptype_abstract, Private, Some $3) }
          | EQUAL constructor_declarations
              { (Ptype_variant(List.rev $2), Public, None) }
          | EQUAL PRIVATE constructor_declarations
              { (Ptype_variant(List.rev $3), Private, None) }
          | EQUAL private_flag BAR constructor_declarations
              { (Ptype_variant(List.rev $4), $2, None) }
          | EQUAL DOTDOT
              { (Ptype_open, Public, None) }
          | EQUAL private_flag LBRACE label_declarations opt_comma RBRACE
              { (Ptype_record(List.rev $4), $2, None) }
          | EQUAL core_type EQUAL private_flag opt_bar constructor_declarations
              { (Ptype_variant(List.rev $6), $4, Some $2) }
          | EQUAL core_type EQUAL DOTDOT
              { (Ptype_open, Public, Some $2) }
          | EQUAL core_type EQUAL private_flag LBRACE label_declarations opt_comma RBRACE
              { (Ptype_record(List.rev $6), $4, Some $2) }
    *)
    let privateAtom = (atom "pri") in
    let privatize scope lst = match scope with
      | Public -> lst
      | Private -> privateAtom::lst in

    let estimateRecordOpenBracePoint () =
      match x.ptype_params with
        | [] -> x.ptype_name.loc.loc_end
        | _ ->
          (fst (List.nth x.ptype_params (List.length x.ptype_params - 1))).ptyp_loc.loc_end
    in

    let equalInitiatedSegments = match (x.ptype_kind, x.ptype_private, x.ptype_manifest) with
      (* /*empty*/ {(Ptype_abstract, Public, None)} *)
      | (Ptype_abstract, Public, None) -> [

        ]
      (* EQUAL core_type {(Ptype_abstract, Public, Some _)} *)
      | (Ptype_abstract, Public, Some y) -> [
          [self#core_type y]
        ]
      (* EQUAL PRIVATE core_type {(Ptype_abstract, Private, Some $3)} *)
      | (Ptype_abstract, Private, Some y) -> [
          [privateAtom; self#core_type y]
        ]
      (* EQUAL constructor_declarations {(Ptype_variant _., Public, None)} *)
      (* This case is redundant *)
      (* | (Ptype_variant lst, Public, None) -> [ *)
      (*     [makeSpacedBreakableInlineList (List.map type_variant_leaf lst)] *)
      (*   ] *)
      (* EQUAL PRIVATE constructor_declarations {(Ptype_variant _, Private, None)} *)
      | (Ptype_variant lst, Private, None) -> [
          [privateAtom; makeList ~break:IfNeed ~postSpace:true ~inline:(true, true) (self#type_variant_list lst)]
        ]
      (* EQUAL private_flag BAR constructor_declarations {(Ptype_variant _, $2, None)} *)
      | (Ptype_variant lst, scope, None) -> [
          privatize scope
            [makeList
              ~break:(if lst == [] then IfNeed else Always_rec)
              ~postSpace:true
              ~inline:(true, true)
              (self#type_variant_list lst)]
        ]
      (* EQUAL DOTDOT {(Ptype_open, Public, None)} *)
      | (Ptype_open, Public, None) -> [
          [atom ".."]
        ]
      | (Ptype_open, Private, None) -> [
          [privateAtom; atom ".."]
        ]
      (* Super confusing how record/variants' manifest is not actually the
         description of the structure. What's in the manifest in that case is
         the *second* EQUALS asignment. *)

      (* EQUAL private_flag LBRACE label_declarations opt_comma RBRACE {(Ptype_record _, $2, None)} *)
      | (Ptype_record lst, scope, None) ->
          let assumeRecordLoc = {loc_start = estimateRecordOpenBracePoint(); loc_end = x.ptype_loc.loc_end; loc_ghost = false} in
          [privatize scope [self#record_declaration ~assumeRecordLoc lst]]
      (* And now all of the forms involving *TWO* equals *)
      (* Again, super confusing how manifests of variants/records represent the
         structure after the second equals. *)
      (* ================================================*)


      (* EQUAL core_type EQUAL private_flag opt_bar constructor_declarations {
         (Ptype_variant _, _, Some _)} *)
      | (Ptype_variant lst, scope, Some mani) -> [
          [self#core_type mani];
          let variant = makeList ~break:IfNeed ~postSpace:true ~inline:(true, true) (self#type_variant_list lst) in
          privatize scope [variant];
        ]

      (* EQUAL core_type EQUAL DOTDOT {(Ptype_open, Public, Some $2)} *)
      | (Ptype_open, Public, Some mani) -> [
          [self#core_type mani];
          [atom ".."];
        ]
      (* EQUAL core_type EQUAL private_flag LBRACE label_declarations opt_comma RBRACE
           {(Ptype_record _, $4, Some $2)} *)
      | (Ptype_record lst, scope, Some mani) ->
          let declaration = self#record_declaration lst in
          let record = match scope with
            | Public -> [declaration]
            | Private -> [label ~space:true privateAtom declaration]
          in
          [ [self#core_type mani]; record ]

      (* Everything else is impossible *)
      (* ================================================*)

      | (_, _, _ ) ->  raise (NotPossible "Encountered impossible type specification")
    in

    let makeConstraint (ct1, ct2, _) =
      let constraintEq = makeList ~postSpace:true [
        atom "constraint";
        self#core_type ct1;
        atom "=";
      ] in
      label ~space:true constraintEq (self#core_type ct2) in
    let constraints = List.map makeConstraint x.ptype_cstrs in
    (equalInitiatedSegments, constraints)

  (* "non-arrowed" means "a type where all arrows are inside at least one level of parens"

    z => z: not a "non-arrowed" type.
    (a, b): a "non-arrowed" type.
    (z=>z): a "non-arrowed" type because the arrows are guarded by parens.

    A "non arrowed, non simple" type would be one that is not-arrowed, and also
    not "simple". Simple means it is "clearly one unit" like (a, b), identifier,
    "hello", None.
  *)
  method non_arrowed_non_simple_core_type x =
    let {stdAttrs} = partitionAttributes x.ptyp_attributes in
    if stdAttrs != [] then
      formatAttributed
        (self#non_arrowed_simple_core_type {x with ptyp_attributes=[]})
        (self#attributes stdAttrs)
    else
      match x.ptyp_desc with
      (* This significantly differs from the standard OCaml printer/parser:
         Type constructors are no longer simple *)
      | _ -> self#non_arrowed_simple_core_type x

  method type_param_list_element = function
    | {ptyp_attributes = []; ptyp_desc = Ptyp_package(lid,cstrs)} ->
        self#typ_package ~mod_prefix:true lid cstrs
    | t -> self#core_type t

  method non_arrowed_simple_core_type x =
    let {stdAttrs} = partitionAttributes x.ptyp_attributes in
    if stdAttrs != [] then
      formatSimpleAttributed
        (self#non_arrowed_simple_core_type {x with ptyp_attributes=[]})
        (self#attributes stdAttrs)
    else
      let result =
        match x.ptyp_desc with
        (*   LPAREN core_type_comma_list RPAREN %prec below_NEWDOT *)
        (*       { match $2 with *)
        (*         | [] -> raise Parse_error *)
        (*         | one::[] -> one *)
        (*         | moreThanOne -> mktyp(Ptyp_tuple(List.rev moreThanOne)) } *)
        | Ptyp_tuple l -> makeTup (List.map self#type_param_list_element l)
        | Ptyp_object (l, o) -> self#unparseObject l o
        | Ptyp_package (lid, cstrs) ->
            self#typ_package ~protect:true ~mod_prefix:true lid cstrs
        (*   | QUOTE ident *)
        (*       { mktyp(Ptyp_var $2) } *)
        | Ptyp_var s -> ensureSingleTokenSticksToLabel (self#tyvar s)
        (*   | UNDERSCORE *)
        (*       { mktyp(Ptyp_any) } *)
        | Ptyp_any -> ensureSingleTokenSticksToLabel (atom "_")
        (*   | type_longident *)
        (*       { mktyp(Ptyp_constr(mkrhs $1 1, [])) } *)
        | Ptyp_constr (li, []) ->
          (* [ensureSingleTokenSticksToLabel] loses location information which is important
               when you are embedded inside a list and comments are to be interleaved around you.
               Therefore, we wrap the result in the correct [SourceMap].  *)
          source_map ~loc:li.loc
            (ensureSingleTokenSticksToLabel (self#longident_loc li))
        | Ptyp_constr (li, l) ->
            (match l with
            | [{ptyp_desc = Ptyp_object (_::_ as l, o) }] when isJsDotTLongIdent li.txt ->
                (* should have one or more rows, Js.t({..}) should print as Js.t({..})
                 * {..} has a totally different meaning than Js.t({..}) *)
                self#unparseObject ~withStringKeys:true l o
            | [{ptyp_desc = Ptyp_object (l, o) }] when not (isJsDotTLongIdent li.txt) ->
                label (self#longident_loc li)
                  (self#unparseObject ~wrap:("(",")") l o)
            | [{ptyp_desc = Ptyp_constr(lii, [{ ptyp_desc = Ptyp_object (_::_ as ll, o)}])}]
              when isJsDotTLongIdent lii.txt ->
              label (self#longident_loc li)
                (self#unparseObject ~withStringKeys:true ~wrap:("(",")") ll o)
            | _ ->
              (* small guidance: in `type foo = bar`, we're now at the `bar` part *)

              (* The single identifier has to be wrapped in a [ensureSingleTokenSticksToLabel] to
                 avoid (@see @avoidSingleTokenWrapping): *)
              label
                (self#longident_loc li)
                (makeTup (
                  List.map self#type_param_list_element l
                ))
            )
        | Ptyp_variant (l, closed, low) ->
          let pcd_attributes = x.ptyp_attributes in
          let pcd_res = None in
          let variant_helper i rf =
            match rf.prf_desc with
              | Rtag (label, opt_ampersand, ctl) ->
                let pcd_args = Pcstr_tuple ctl in
                let all_attrs = List.concat [pcd_attributes; rf.prf_attributes] in
                self#type_variant_leaf
                  ~opt_ampersand
                  ~polymorphic:true
                  {pcd_name = label; pcd_args; pcd_res; pcd_loc = label.loc; pcd_attributes = all_attrs}
              | Rinherit ct ->
                (* '| type' is required if the Rinherit is not the first
                  row_field in the list
                 *)
                if i = 0 then
                  self#core_type ct
                else
                  makeList ~postSpace:true [atom "|"; self#core_type ct] in
          let (designator, tl) =
            match (closed,low) with
              | (Closed,None) -> ("", [])
              | (Closed,Some tl) -> ("<", tl)
              | (Open,_) -> (">", []) in
          let node_list = List.mapi variant_helper l in
          let ll = (List.map (fun t -> atom ("`" ^ t)) tl) in
          let tag_list = makeList ~postSpace:true ~break:IfNeed ((atom ">")::ll) in
          let type_list = if tl != [] then node_list@[tag_list] else node_list in
          makeList ~wrap:("[" ^ designator,"]") ~pad:(true, false) ~postSpace:true ~break:IfNeed type_list
        | Ptyp_class (li, []) -> makeList [atom "#"; self#longident_loc li]
        | Ptyp_class (li, l) ->
          label
            (makeList [atom "#"; self#longident_loc li])
            (makeTup (List.map self#core_type l))
        | Ptyp_extension e -> self#extension e
        | Ptyp_arrow (_, _, _)
        | Ptyp_alias (_, _)
        | Ptyp_poly (_, _) ->
            makeList ~wrap:("(",")") ~break:IfNeed [self#core_type x]
      in
      source_map ~loc:x.ptyp_loc result
  (* TODO: ensure that we have a form of desugaring that protects *)
  (* when final argument of curried pattern is a type constraint: *)
  (* | COLON non_arrowed_core_type EQUALGREATER expr
      { mkexp_constraint $4 (Some $2, None) }         *)
  (*                         \----/   \--/
                             constraint coerce

                             Creates a ghost expression:
                             mkexp_constraint | Some t, None -> ghexp(Pexp_constraint(e, t))
  *)

  method pattern_list_split_cons acc = function
    | {
      ppat_desc = Ppat_construct (
        { txt = Lident("::")},
        Some {ppat_desc = Ppat_tuple ([pat1; pat2])}
      ) } ->
        self#pattern_list_split_cons (pat1::acc) pat2
    | p -> (List.rev acc), p

  (*
   * Adds parens to the right sub-tree when it is not a single node:
   *
   * A | B                   is formatted as    A | B
   * A | (B | C)             is formatted as    A | (B | C)
   *
   * Also, adds parens to both sub-trees when both of them
   * are not a single node:
   * (A | B) | (C | D)       is formatted as    A | B | (C | D)
   * A | B | (C | D)         is formatted as    A | B | (C | D)
   * (A | B) | C             is formatted as    A | B | C
   * A | B | C               is formatted as    A | B | C
   *
   *)
  method or_pattern p1 p2 =
    let (p1_raw, p2_raw) = (self#pattern p1, self#pattern p2) in
    let (left, right) =
      match p2.ppat_desc with
        | Ppat_or _ -> (p1_raw, formatPrecedence p2_raw)
        | _ -> (p1_raw, p2_raw)
    in
    makeList
      ~break:IfNeed
      ~inline:(true, true)
      ~sep:(Sep "|")
      ~postSpace:true
      ~preSpace:true
      [left; right]

  (*
   * Renders level 3 or simpler patterns:
   *
   * Simpler
   * ^   -----------
   * |   1.     [ ], { }, X.{  }, ident, (any-other-pattern-with-parens-around)
   * |   2.     F(args), lazy(foo), [@attr] 1-2
   * |   3.     pat as alias, pat | pat
   * |   4.     1-3 : typ
   * v   ------------
   * Complex
   *
   * Assumes visually rendered attributes have already been rendered.
   *)
  method pattern_at_least_as_simple_as_alias_or_or x =
    let {arityAttrs=_; stdAttrs} = partitionAttributes x.ppat_attributes in
    match stdAttrs, x.ppat_desc with
      | [], Ppat_or (p1, p2) ->
        self#or_pattern p1 p2
      | [], Ppat_alias (p, s) ->
          let raw_pattern = (self#pattern p) in
          let pattern_with_precedence = match p.ppat_desc with
            | Ppat_or (p1, p2) -> formatPrecedence (self#or_pattern p1 p2)
            | _ -> raw_pattern
          in
          label ~space:true
            (source_map ~loc:p.ppat_loc pattern_with_precedence)
            (makeList ~postSpace:true [
              atom "as";
              (source_map ~loc:s.loc (protectIdentifier s.txt))
            ]) (* RA*)
      | _ -> self#pattern_at_least_as_simple_as_application x

  (* Formats a pattern that is a least as "simple" as function application
   * style syntax. Produces formatting that is as simple as either 1 or 2.
   *
   * Simpler
   * ^   -----------
   * |   1.     [ ], { }, X.{  }, ident, (any-other-pattern-with-parens-around)
   * |   2.     F(args), lazy(foo), [@attr] 1-2
   * |   3.     pat as alias, pat | pat
   * |   4.     1-3 : typ
   * v   ------------
   * Complex
   *
   *
   * 1. and 2. do not need parens around them in order to apply attributes to
   * them. 3. does need parens around it to apply attributes to the whole
   * pattern.
   *
   * Assumes visually rendered attributes have already been rendered.
   *)
  method pattern_at_least_as_simple_as_application x =
    (* TODOATTRIBUTES: Handle the stdAttrs here *)
    let {stdAttrs; arityAttrs} = partitionAttributes x.ppat_attributes in
    let formattedPattern =
      match x.ppat_desc with
        | Ppat_variant (l, Some p) ->
            if arityAttrs != [] then
              raise (NotPossible "Should never see embedded attributes on poly variant")
            else
              source_map ~loc:x.ppat_loc
                (self#constructor_pattern (atom ("`" ^ l)) p
                   ~polyVariant:true ~arityIsClear:true)
        | Ppat_lazy p -> label ~space:true (atom "lazy") (self#simple_pattern p)
        | Ppat_construct (({txt} as li), po) when not (txt = Lident "::")-> (* FIXME The third field always false *)
            let formattedConstruction = match po with
              (* TODO: Check the explicit_arity field on the pattern/constructor
                 attributes to determine if should desugar to an *actual* tuple. *)
              (* | Some ({ *)
              (*   ppat_desc=Ppat_tuple l; *)
              (*   ppat_attributes=[{txt="explicit_arity"; loc}] *)
              (* }) -> *)
              (*   label ~space:true (self#longident_loc li) (makeSpacedBreakableInlineList (List.map self#simple_pattern l)) *)
              | Some pattern ->
                  let arityIsClear = isArityClear arityAttrs in
                  self#constructor_pattern ~arityIsClear (self#longident_loc li) pattern
              | None ->
                  self#longident_loc li
            in
            source_map ~loc:x.ppat_loc formattedConstruction
        | _ -> self#simple_pattern {x with ppat_attributes=arityAttrs}
    in
    if stdAttrs != [] then
      formatAttributed formattedPattern (self#attributes stdAttrs)
    else formattedPattern

  (* Format a pattern with no particular requirements of simplicity. For example when
   * formatting a pattern *inside* one tuple position:
   *        |
   *        v
   * let (x : int, foo) = ..
   *
   *
   * Renders level 3 or simpler patterns:
   *
   * Simpler
   * ^   -----------
   * |   1.     [ ], { }, X.{  }, ident, (any-other-pattern-with-parens-around)
   * |   2.     F(args), lazy(foo), [@attr] 1-2
   * |   3.     pat as alias, pat | pat
   * |   4.     1-3 : typ
   * v   ------------
   * Complex
   *
   * Assumes visually rendered attributes have already been rendered.
   *)
  method pattern x =
    let {arityAttrs=_; stdAttrs} = partitionAttributes x.ppat_attributes in
    match stdAttrs, x.ppat_desc with
      | [], Ppat_constraint (p, ct) ->
          let (pat, typ) = begin match (p, ct) with
          | (
              {ppat_desc = Ppat_unpack(unpack)},
              {ptyp_desc = Ptyp_package (lid, cstrs)}
            ) ->
              (makeList ~postSpace:true [atom "module"; atom unpack.txt],
               self#typ_package ~mod_prefix:false lid cstrs)
          | _ ->
            (* Have to call pattern_at_least_as_simple_as_alias_or_or because
             * we don't want to allow *another* nested type annotation without
             * first adding parens *)
            (self#pattern_at_least_as_simple_as_alias_or_or p, self#core_type ct)
          end in
          formatTypeConstraint pat typ
      | _ -> self#pattern_at_least_as_simple_as_alias_or_or x

  method patternList ?(wrap=("","")) pat =
    let pat_list, pat_last = self#pattern_list_split_cons [] pat in
    let pat_list = List.map self#pattern pat_list in
    match pat_last with
    | {ppat_desc = Ppat_construct ({txt=Lident "[]"},_)} -> (* [x,y,z] *)
      let (lwrap, rwrap) = wrap in
      makeList pat_list
        ~break:Layout.IfNeed ~sep:commaTrail ~postSpace:true
        ~wrap:(lwrap ^ "[", "]" ^ rwrap)
    | _ -> (* x::y *)
      makeES6List pat_list (self#pattern pat_last) ~wrap

  (* In some contexts the Ptyp_package needs to be protected by parens, or
   * the `module` keyword needs to be added.
   * Example: let f = (module Add: S.Z, x) => Add.add(x);
   *  It's clear that `S.Z` is a module because it constraints the
   *  `module Add` pattern. No need to add "module" before `S.Z`.
   *
   * Example2:
   *   type t = (module Console);
   *   In this case the "module" keyword needs to be printed to indicate
   *   usage of a first-class-module.
   *)
  method typ_package ?(protect=false) ?(mod_prefix=true) lid cstrs =
    let packageIdent =
      let packageIdent = self#longident_loc lid in
      if mod_prefix then
        makeList ~postSpace:true [atom "module"; packageIdent]
      else packageIdent
    in
    let unwrapped_layout = match cstrs with
    | [] -> packageIdent
    | cstrs ->
        label ~space:true
          (makeList ~postSpace:true [packageIdent; atom "with"])
          (makeList
            ~inline:(true, true)
            ~break:IfNeed
            ~sep:(Sep " and ")
            (List.map (fun (s, ct) ->
              label ~space:true
                (makeList
                  ~break:IfNeed ~postSpace:true
                  [atom "type"; self#longident_loc s; atom "="])
                (self#core_type ct)
            ) cstrs))
    in
    if protect then
      makeList ~postSpace:true ~wrap:("(", ")") [unwrapped_layout ]
    else unwrapped_layout

  method simple_pattern x =
    let {arityAttrs; stdAttrs} = partitionAttributes x.ppat_attributes in
    if stdAttrs != [] then
      formatSimpleAttributed
        (self#simple_pattern {x with ppat_attributes=arityAttrs})
        (self#attributes stdAttrs)
    else
      let itm =
        match x.ppat_desc with
          | Ppat_construct (({loc; txt=Lident ("()"|"[]" as x)}), _) ->
              (* Patterns' locations might include a leading bar depending on the
               * context it was parsed in. Therefore, we need to include further
               * information about the contents of the pattern such as tokens etc,
               * in order to get comments to be distributed correctly.*)
            atom ~loc x
          | Ppat_construct (({txt=Lident "::"}), _) ->
            self#patternList x (* LIST PATTERN *)
          | Ppat_construct (li, None) ->
            source_map ~loc:x.ppat_loc (self#longident_loc li)
          | Ppat_any -> atom "_"
          | Ppat_var ({loc; txt = txt}) ->
            (*
               To prevent this:

                 let oneArgShouldWrapToAlignWith
                   theFunctionNameBinding => theFunctionNameBinding;

               And instead do:

                 let oneArgShouldWrapToAlignWith
                     theFunctionNameBinding => theFunctionNameBinding;

               We have to do something to the non "listy" patterns. Non listy
               patterns don't indent the same amount as listy patterns when docked
               to a label.

               If wrapping the non-listy pattern in [ensureSingleTokenSticksToLabel]
               you'll get the following (even though it should wrap)

                 let oneArgShouldWrapToAlignWith theFunctionNameBinding => theFunctionNameBinding;

             *)
            source_map ~loc (protectIdentifier txt)
          | Ppat_array l ->
              self#patternArray l
          | Ppat_unpack s ->
              makeList ~wrap:("(", ")") ~break:IfNeed ~postSpace:true [atom "module"; atom s.txt]
          | Ppat_open (lid, pat) ->
            (* let someFn Qualified.{ record } = ... *)
            let needsParens = match pat.ppat_desc with
              | Ppat_exception _ -> true
              | _ -> false
            in
            let pat = self#simple_pattern pat in
            label
              (label (self#longident_loc lid) (atom (".")))
              (if needsParens then formatPrecedence pat else pat)
          | Ppat_type li ->
              makeList [atom "#"; self#longident_loc li]
          | Ppat_record (l, closed) ->
             self#patternRecord l closed
          | Ppat_tuple l ->
             self#patternTuple l
          | Ppat_constant c ->
            let raw_literal, _ = extract_raw_literal x.ppat_attributes in
            (self#constant ?raw_literal c)
          | Ppat_interval (c1, c2) ->
            makeList ~postSpace:true [self#constant c1; atom ".."; self#constant c2]
          | Ppat_variant (l, None) -> makeList[atom "`"; atom l]
          | Ppat_constraint (_p, _ct) ->
              formatPrecedence (self#pattern x)
          | Ppat_lazy p ->formatPrecedence (label ~space:true (atom "lazy") (self#simple_pattern p))
          | Ppat_extension e -> self#extension e
          | Ppat_exception p ->
              (*
                An exception pattern with an alias should be wrapped in (...)
                The rules for what goes to the right of the exception are a little (too) nuanced.
                It accepts "non simple" parameters, except in the case of `as`.
                Here we consistently apply "simplification" to the exception argument.
                Example:
                  | exception (Sys_error _ as exc) => raise exc
                 parses correctly while
                  | Sys_error _ as exc => raise exc
                 results in incorrect parsing with type error otherwise.
              *)
               (makeList ~postSpace:true [atom "exception"; self#simple_pattern p])
          | _ -> formatPrecedence (self#pattern x) (* May have a redundant sourcemap *)
        in
        source_map ~loc:x.ppat_loc itm

  method label_exp lbl opt pat =
    let term = self#pattern pat in
    let param = match lbl with
      | Nolabel -> term
      | Labelled lbl | Optional lbl when is_punned_labelled_pattern_no_attrs pat lbl ->
        makeList [atom namedArgSym; term]
      | Labelled lbl | Optional lbl ->
        let lblLayout=
          makeList ~sep:(Sep " ") ~break:Layout.Never
            [atom (namedArgSym ^ lbl); atom "as"]
        in
        label lblLayout ~space:true term
    in
    match opt, lbl with
    | None, Optional _ -> makeList [param; atom "=?"]
    | None, _ -> param
    | Some o, _ -> makeList [param; atom "="; (self#unparseProtectedExpr ~forceParens:true o)]

  method access op cls e1 e2 = makeList [
    (* Important that this be not breaking - at least to preserve same
       behavior as stock desugarer. It might even be required (double check
       in parser.mly) *)
    e1;
    atom op;
    e2;
    atom cls;
  ]


  method simple_get_application x =
    let {stdAttrs; jsxAttrs} = partitionAttributes x.pexp_attributes in
    match (x.pexp_desc, stdAttrs, jsxAttrs) with
    | (_, _::_, []) -> None (* Has some printed attributes - not simple *)
    | (Pexp_apply ({pexp_desc=Pexp_ident loc}, l), [], _jsx::_) -> (
      (* TODO: Soon, we will allow the final argument to be an identifier which
         represents the entire list. This would be written as
         `<tag>...list</tag>`. If you imagine there being an implicit [] inside
         the tag, then it would be consistent with array spread:
         [...list] evaluates to the thing as list.
      *)
      let hasLabelledChildrenLiteral = List.exists (function
        | (Labelled "children", _) -> true
        | _ -> false
      ) l in
      let rec hasSingleNonLabelledUnitAndIsAtTheEnd l = match l with
      | [] -> false
      | (Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, _)}) :: [] -> true
      | (Nolabel, _) :: _ -> false
      | _ :: rest -> hasSingleNonLabelledUnitAndIsAtTheEnd rest
      in
      if hasLabelledChildrenLiteral && hasSingleNonLabelledUnitAndIsAtTheEnd l then
        match loc.txt with
        | Ldot (moduleLid, "createElement") ->
          Some (self#formatJSXComponent
                  (String.concat "." (Longident.flatten moduleLid)) l)
        | lid ->
          Some (self#formatJSXComponent
                  (String.concat "." (Longident.flatten lid)) l)
      else None
    )
    | (Pexp_apply (
        {pexp_desc=
          Pexp_letmodule(_,
            ({pmod_desc=Pmod_apply _} as app),
            {pexp_desc=Pexp_ident loc}
          )}, l), [], _jsx::_) -> (
      (* TODO: Soon, we will allow the final argument to be an identifier which
         represents the entire list. This would be written as
         `<tag>...list</tag>`. If you imagine there being an implicit [] inside
         the tag, then it would be consistent with array spread:
         [...list] evaluates to the thing as list.
      *)
      let rec extract_apps args = function
        | { pmod_desc = Pmod_apply (m1, {pmod_desc=Pmod_ident loc}) } ->
          let arg = String.concat "." (Longident.flatten loc.txt) in
          extract_apps (arg :: args) m1
        | { pmod_desc=Pmod_ident loc } -> (String.concat "." (Longident.flatten loc.txt))::args
        | _ -> failwith "Functors in JSX tags support only module names as parameters" in
      let hasLabelledChildrenLiteral = List.exists (function
        | (Labelled "children", _) -> true
        | _ -> false
      ) l in
      let rec hasSingleNonLabelledUnitAndIsAtTheEnd l = match l with
      | [] -> false
      | (Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, _)}) :: [] -> true
      | (Nolabel, _) :: _ -> false
      | _ :: rest -> hasSingleNonLabelledUnitAndIsAtTheEnd rest
      in
      if hasLabelledChildrenLiteral && hasSingleNonLabelledUnitAndIsAtTheEnd l then
        if List.length (Longident.flatten loc.txt) > 1 then
          if Longident.last loc.txt = "createElement" then
            begin match extract_apps [] app with
              | ftor::args ->
                let applied = ftor ^ "(" ^ String.concat ", " args ^ ")" in
                Some (self#formatJSXComponent ~closeComponentName:ftor applied l)
              | _ -> None
            end
          else None
        else Some (self#formatJSXComponent (Longident.last loc.txt) l)
      else None
    )
    | _ -> None

  (** Detects "sugar expressions" (sugar for array/string setters) and returns their separate
      parts.  *)
  method sugar_set_expr_parts e =
    if e.pexp_attributes != [] then None
    (* should also check attributes underneath *)
    else match e.pexp_desc with
      | Pexp_apply ({pexp_desc=Pexp_ident{txt=Ldot (Lident ("Array"), "set")}}, [(_,e1);(_,e2);(_,e3)]) ->
        let prec = Custom "prec_lbracket" in
        let lhs = self#unparseResolvedRule (
            self#ensureExpression ~reducesOnToken:prec e1
          ) in
        Some (self#access "[" "]" lhs (self#unparseExpr e2), e3)
      | Pexp_apply ({pexp_desc=Pexp_ident {txt=Ldot (Lident "String", "set")}}, [(_,e1);(_,e2);(_,e3)]) ->
        let prec = Custom "prec_lbracket" in
        let lhs = self#unparseResolvedRule (
            self#ensureExpression ~reducesOnToken:prec e1
          ) in
        Some ((self#access ".[" "]" lhs (self#unparseExpr e2)), e3)
      | Pexp_apply (
        {pexp_desc=Pexp_ident {txt = Ldot (Ldot (Lident "Bigarray", array), "set")}},
        label_exprs
      ) -> (
        match array with
          | "Genarray" -> (
            match label_exprs with
            | [(_,a);(_,{pexp_desc=Pexp_array ls});(_,c)] ->
              let formattedList = List.map self#unparseExpr ls in
              let lhs = makeList [self#simple_enough_to_be_lhs_dot_send a; atom "."] in
              let rhs = makeList ~break:IfNeed ~postSpace:true ~sep:commaSep ~wrap:("{", "}") formattedList
              in
              Some (label lhs rhs, c)
            | _ -> None
          )
          | ("Array1"|"Array2"|"Array3") -> (
            match label_exprs with
            | (_,a)::rest -> (
              match List.rev rest with
              | (_,v)::rest ->
                let args = List.map snd (List.rev rest) in
                let formattedList = List.map self#unparseExpr args in
                let lhs = makeList [self#simple_enough_to_be_lhs_dot_send a; atom "."] in
                let rhs = makeList ~break:IfNeed ~postSpace:true ~sep:commaSep ~wrap:("{", "}") formattedList in
                Some (label lhs rhs, v)
              | _ -> assert false
            )
            | _ -> assert false
          )
          | _ -> None
        )
      | _ -> None

  (*

     How would we know not to print the sequence without { }; protecting the let a?

                            let a
                             |
                           sequence
                          /        \
                    let a           print a
                    alert a
     let res = {
       let a = something();
       {                     \
         alert(a);           | portion to be parsed as a sequence()
         let a = 20;         | The final ; print(a) causes the entire
         alert(a);           | portion to be parsed as a sequence()
       };                    |
       print (a);            /
     }

     ******************************************************************
     Any time the First expression of a sequence is another sequence, or (as in
     this case) a let, wrapping the first sequence expression in { } is
     required.
     ******************************************************************
  *)

  (**
     TODO: Configure the optional ability to print the *minimum* number of
     parens. It's simply a matter of changing [higherPrecedenceThan] to
     [higherOrEqualPrecedenceThan].
   *)

  (* The point of the function is to ensure that ~reducesAfterRight:rightExpr will reduce
     at the proper time when it is reparsed, possibly wrapping it
     in parenthesis if needed. It ensures a rule doesn't reduce
     until *after* `reducesAfterRight` gets a chance to reduce.
     Example: The addition rule which has precedence of rightmost
     token "+", in `x + a * b` should not reduce until after the a * b gets
     a chance to reduce. This function would determine the minimum parens to
     ensure that. *)
  method ensureContainingRule ~withPrecedence ~reducesAfterRight () =
    match self#unparseExprRecurse reducesAfterRight with
    | SpecificInfixPrecedence ({shiftPrecedence}, rightRecurse) ->
      if higherPrecedenceThan shiftPrecedence withPrecedence then rightRecurse
      else if (higherPrecedenceThan withPrecedence shiftPrecedence) then
        LayoutNode (formatPrecedence ~loc:reducesAfterRight.pexp_loc (self#unparseResolvedRule rightRecurse))
      else (
        if isRightAssociative ~prec:withPrecedence then
         rightRecurse
        else
          LayoutNode (formatPrecedence ~loc:reducesAfterRight.pexp_loc (self#unparseResolvedRule rightRecurse))
      )
    | FunctionApplication itms ->
      let funApplExpr = formatAttachmentApplication applicationFinalWrapping None (itms, Some reducesAfterRight.pexp_loc)
      in
      (* Little hack: need to print parens for the `bar` application in e.g.
         `foo->other##(bar(baz))` or `foo->other->(bar(baz))`. *)
      if higherPrecedenceThan withPrecedence (Custom "prec_functionAppl")
      then LayoutNode (formatPrecedence ~loc:reducesAfterRight.pexp_loc funApplExpr)
      else LayoutNode funApplExpr
    | PotentiallyLowPrecedence itm -> LayoutNode (formatPrecedence ~loc:reducesAfterRight.pexp_loc itm)
    | Simple itm -> LayoutNode itm

  method ensureExpression ~reducesOnToken expr =
    match self#unparseExprRecurse expr with
    | SpecificInfixPrecedence ({reducePrecedence}, leftRecurse) ->
      if higherPrecedenceThan reducePrecedence reducesOnToken then leftRecurse
      else if higherPrecedenceThan reducesOnToken reducePrecedence then
        LayoutNode (formatPrecedence ~loc:expr.pexp_loc (self#unparseResolvedRule leftRecurse))
      else (
        if isLeftAssociative ~prec:reducesOnToken then
          leftRecurse
        else
          LayoutNode (formatPrecedence ~loc:expr.pexp_loc (self#unparseResolvedRule leftRecurse))
      )
    | FunctionApplication itms -> LayoutNode (formatAttachmentApplication applicationFinalWrapping None (itms, Some expr.pexp_loc))
    | PotentiallyLowPrecedence itm -> LayoutNode (formatPrecedence ~loc:expr.pexp_loc itm)
    | Simple itm -> LayoutNode itm

  (** Attempts to unparse: The beginning of a more general printing algorithm,
      that determines how to print based on precedence of tokens and rules.
      The end goal is that this should be completely auto-generated from the
      Menhir parsing tables. We could move more and more into this function.

      You could always just call self#expression, but `unparseExpr` will render
      infix/prefix/unary/terary fixities in their beautiful forms while
      minimizing parenthesis.
  *)
  method unparseExpr x =
    match self#unparseExprRecurse x with
    | SpecificInfixPrecedence (_, resolvedRule) ->
        self#unparseResolvedRule resolvedRule
    | FunctionApplication itms ->
        formatAttachmentApplication applicationFinalWrapping None (itms, Some x.pexp_loc)
    | PotentiallyLowPrecedence itm -> itm
    | Simple itm -> itm

  (* This method may not even be needed *)
  method unparseUnattributedExpr x =
    match partitionAttributes x.pexp_attributes with
    | {docAttrs = []; stdAttrs = []} -> self#unparseExpr x
    | _ -> makeList ~wrap:("(",")") [self#unparseExpr x]

  (* ensureExpr ensures that the expression is wrapped in parens
   * e.g. is necessary in cases like:
   * let display = (:message=("hello": string)) => 1;
   * but not in cases like:
   * let f = (a: bool) => 1;
   * TODO: in the future we should probably use the type ruleCategory
   * to 'automatically' ensure the validity of a constraint expr with parens...
   *)
  method unparseProtectedExpr ?(forceParens=false) e =
    let itm =
      match e with
      | { pexp_attributes = []; pexp_desc = Pexp_constraint (x, ct)} ->
        let x = self#unparseExpr x in
        let children = [x; label ~space:true (atom ":") (self#core_type ct)] in
        if forceParens then
          makeList ~wrap:("(", ")") children
        else makeList children
      | { pexp_attributes; pexp_desc = Pexp_constant c } ->
        (* When we have Some(-1) or someFunction(-1, -2), the arguments -1 and -2
         * pass through this case. In this context they don't need to be wrapped in extra parens
         * Some((-1)) should be printed as Some(-1). This is in contrast with
         * 1 + (-1) where we print the parens for readability. *)
          let raw_literal, pexp_attributes =
            extract_raw_literal pexp_attributes
          in
          let constant = self#constant ?raw_literal ~parens:forceParens c in
          begin match pexp_attributes with
          | [] -> constant
          | attrs ->
              let formattedAttrs = makeSpacedBreakableInlineList (List.map self#item_attribute attrs) in
               makeSpacedBreakableInlineList [formattedAttrs; constant]
          end
      | {pexp_desc = Pexp_fun _ } -> self#formatPexpFun e
      | x -> self#unparseExpr x
    in
    source_map ~loc:e.pexp_loc itm

  method simplifyUnparseExpr ?(inline=false) ?(even_wrap_simple=false) ?(wrap=("(", ")")) x =
    match self#unparseExprRecurse x, even_wrap_simple with
    | SpecificInfixPrecedence (_, itm), _ ->
        formatPrecedence
          ~inline
          ~wrap
          ~loc:x.pexp_loc
          (self#unparseResolvedRule itm)
    | FunctionApplication itms, _ ->
      formatPrecedence
        ~inline
        ~wrap
        ~loc:x.pexp_loc
        (formatAttachmentApplication applicationFinalWrapping None (itms, Some x.pexp_loc))
    | PotentiallyLowPrecedence itm, _
    | Simple itm, true  ->
        formatPrecedence
          ~inline
          ~wrap
          ~loc:x.pexp_loc
          itm
    | Simple itm, false -> itm


  method unparseResolvedRule  = function
    | LayoutNode layoutNode -> layoutNode
    | InfixTree _ as infixTree ->
      formatComputedInfixChain (computeInfixChain infixTree)

  method unparseExprApplicationItems x =
    match self#unparseExprRecurse x with
    | SpecificInfixPrecedence (_, wrappedRule) ->
        let itm = self#unparseResolvedRule wrappedRule in
        ([itm], Some x.pexp_loc)
    | FunctionApplication itms -> (itms, Some x.pexp_loc)
    | PotentiallyLowPrecedence itm -> ([itm], Some x.pexp_loc)
    | Simple itm -> ([itm], Some x.pexp_loc)


  (* Provides beautiful printing for pipe first sugar:
   * foo
   * ->f(a, b)
   * ->g(c, d)
   *)
  method formatPipeFirst e =
    let module PipeFirstTree = struct
      type exp = Parsetree.expression

      type flatNode =
        | Exp of exp
        | ExpU of exp (* uncurried *)
        | Args of (Asttypes.arg_label * exp) list
      type flatT = flatNode list

      type node = {
        exp: exp;
        args: (Asttypes.arg_label *exp) list;
        uncurried: bool;
      }
      type t = node list

      let formatNode ?prefix ?(first=false) {exp; args; uncurried} =
        let formatLayout expr =
          let formatted = if first then
            self#ensureExpression ~reducesOnToken:(Token pipeFirstToken) expr
          else
            match expr with
            (* a->foo(x, _) and a->(foo(x, _)) are equivalent under pipe first
             * (a->foo)(x, _) is unnatural and desugars to
             *    (__x) => (a |. foo)(x, __x)
             * Under `->`, it makes more sense to desugar into
             *  a |. (__x => foo(x, __x))
             *
             * Hence we don't need parens in this case.
             *)
            | expr when Reason_heuristics.isUnderscoreApplication expr ->
                LayoutNode (self#unparseExpr expr)
            | _ ->
              self#ensureContainingRule
                ~withPrecedence:(Token pipeFirstToken) ~reducesAfterRight:expr ()
          in
          self#unparseResolvedRule formatted
        in
        let parens = match (exp.pexp_desc) with
        | Pexp_apply (e,_) -> printedStringAndFixityExpr e = UnaryPostfix "^"
        | _ -> false
        in
        let layout = match args with
        | [] ->
          let e = formatLayout exp in
          (match prefix with
          | Some l -> makeList [l; e]
          | None -> e)
        | args ->
            let args = List.map
              (fun (label, arg) ->
                label, self#process_underscore_application arg)
              args
            in
            let fakeApplExp =
              let loc_end = match List.rev args with
                | (_, e)::_ -> e.pexp_loc.loc_end
                | _ -> exp.pexp_loc.loc_end
              in
              {exp with pexp_loc = { exp.pexp_loc with loc_end = loc_end } }
            in
            makeList (
              self#formatFunAppl
                ?prefix
                ~jsxAttrs:[]
                ~args
                ~funExpr:exp
                ~applicationExpr:fakeApplExp
                ~uncurried
                ()
            )
        in
        if parens then
          formatPrecedence layout
        else layout
    end in
    (* Imagine: foo->f(a, b)->g(c,d)
     * The corresponding parsetree looks more like:
     *   (((foo->f)(a,b))->g)(c, d)
     * The extra Pexp_apply nodes, e.g. (foo->f), result into a
     * nested/recursive ast which is pretty inconvenient in terms of printing.
     * For printing purposes we actually want something more like:
     *  foo->|f(a,b)|->|g(c, d)|
     * in order to provide to following printing:
     *  foo
     *  ->f(a, b)
     *  ->g(c, d)
     * The job of "flatten" is to turn the inconvenient, nested ast
     *  (((foo->f)(a,b))->g)(c, d)
     * into
     *  [Exp foo; Exp f; Args [a; b]; Exp g; Args [c; d]]
     * which can be processed for printing purposes.
     *)
    let rec flatten ?(uncurried=false) acc = function
      | {pexp_desc = Pexp_apply(
          {pexp_desc = Pexp_ident({txt = Longident.Lident("|.")})},
          [Nolabel, arg1; Nolabel, arg2]
         )} ->
          flatten ((PipeFirstTree.Exp arg2)::acc) arg1
      | {pexp_attributes;
         pexp_desc = Pexp_apply(
          {pexp_desc = Pexp_apply(
           {pexp_desc = Pexp_ident({txt = Longident.Lident("|.")})},
             [Nolabel, arg1; Nolabel, arg2]
           )},
           args
         )} as e ->
          let args = PipeFirstTree.Args args in
          begin match pexp_attributes with
          | [{ attr_name = {txt = "bs"}; attr_payload = PStr []}] ->
            flatten ((PipeFirstTree.ExpU arg2)::args::acc) arg1
          | [] ->
              (* the uncurried attribute might sit on the Pstr_eval
               * enclosing the Pexp_apply*)
              if uncurried then
                flatten ((PipeFirstTree.ExpU arg2)::args::acc) arg1
              else
                flatten ((PipeFirstTree.Exp arg2)::args::acc) arg1
          | _ ->
            (PipeFirstTree.Exp e)::acc
          end
      | {pexp_desc = Pexp_ident({txt = Longident.Lident("|.")})} -> acc
      | arg -> ((PipeFirstTree.Exp arg)::acc)
    in
    (* Given: foo->f(a, b)->g(c, d)
     * We get the following PipeFirstTree.flatNode list:
     *   [Exp foo; Exp f; Args [a; b]; Exp g; Args [c; d]]
     * The job of `parse` is to turn the "flat representation"
     * (a.k.a. PipeFirstTree.flastNode list) into a more convenient structure
     * that allows us to express the segments: "foo" "f(a, b)" "g(c, d)".
     * PipeFirstTree.t expresses those segments.
     *  [{exp = foo; args = []}; {exp = f; args = [a; b]}; {exp = g; args = [c; d]}]
     *)
    let rec parse acc = function
    | (PipeFirstTree.Exp e)::(PipeFirstTree.Args args)::xs ->
        parse ((PipeFirstTree.{exp = e; args; uncurried = false})::acc) xs
    | (PipeFirstTree.ExpU e)::(PipeFirstTree.Args args)::xs ->
        parse ((PipeFirstTree.{exp = e; args; uncurried = true})::acc) xs
    | (PipeFirstTree.Exp e)::xs ->
        parse ((PipeFirstTree.{exp = e; args = []; uncurried = false})::acc) xs
    | _ -> List.rev acc
    in
    (* Given: foo->f(. a,b);
     * The uncurried attribute doesn't sit on the Pexp_apply, but sits on
     * the top level Pstr_eval. We don't have access to top-level context here,
     * hence the lookup in the global uncurriedTable to correctly determine
     * if we need to print uncurried. *)
    let uncurried = try Hashtbl.find uncurriedTable e.pexp_loc with
      | Not_found -> false
    in
    (* Turn
     *  foo->f(a, b)->g(c, d)
     * into
     *  [Exp foo; Exp f; Args [a; b]; Exp g; Args [c; d]]
     *)
    let (flatNodes : PipeFirstTree.flatT) = flatten ~uncurried [] e in
    (* Turn
     *  [Exp foo; Exp f; Args [a; b]; Exp g; Args [c; d]]
     * into
     *  [{exp = foo; args = []}; {exp = f; args = [a; b]}; {exp = g; args = [c; d]}]
     *)
    let (pipetree : PipeFirstTree.t) = parse [] flatNodes in
    (* Turn
     *  [{exp = foo; args = []}; {exp = f; args = [a; b]}; {exp = g; args = [c; d]}]
     * into
     *  [foo; ->f(a, b); ->g(c, d)]
     *)
    let pipeSegments = match pipetree with
    (* Special case printing of
     * foo->bar(
     *  aa,
     *  bb,
     * )
     *
     *  We don't want
     *  foo
     *  ->bar(
     *      aa,
     *      bb
     *    )
     *
     *  Notice how `foo->bar` shouldn't break, it wastes space and is
     *  inconsistent with
     *  foo.bar(
     *    aa,
     *    bb,
     *  )
     *)
    | [({exp = {pexp_desc = Pexp_ident _ }} as hd); last] ->
        let prefix = Some (
          makeList [PipeFirstTree.formatNode ~first:true hd; atom "->"]
        ) in
        [PipeFirstTree.formatNode ?prefix last]
    | hd::tl ->
        let hd = PipeFirstTree.formatNode ~first:true hd in
        let tl = List.map (fun node ->
          makeList [atom "->"; PipeFirstTree.formatNode node]
        ) tl in
        hd::tl
    | [] -> []
    in
    (* Provide nice breaking for: [foo; ->f(a, b); ->g(c, d)]
     * foo
     * ->f(a, b)
     * ->g(c, d)
     *)
    makeList ~break:IfNeed ~inline:(true, true) pipeSegments

  (*
   * Replace (__x) => foo(__x) with foo(_)
   *)
  method process_underscore_application x =
    let process_application expr =
      let process_arg (l,e) = match e.pexp_desc with
        | Pexp_ident ({ txt = Lident "__x"} as id) ->
          let pexp_desc = Pexp_ident {id with txt = Lident "_"} in
          (l, {e with pexp_desc})
        | _ ->
          (l,e) in
      match expr.pexp_desc with
      | Pexp_apply (e_fun, args) ->
        let pexp_desc = Pexp_apply (e_fun, List.map process_arg args) in
        {expr with pexp_desc}
      | _ ->
        expr in
    match x.pexp_desc with
    | Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt="__x"}},
        ({pexp_desc = Pexp_apply _} as e)) ->
      process_application e
    | Pexp_fun (l, eo, p, e) ->
       let e_processed = self#process_underscore_application e in
       if e == e_processed then
         x
       else
         {x with pexp_desc = Pexp_fun (l, eo, p, e_processed)}
    | _ ->
      x

  method unparseExprRecurse x =
    let x = self#process_underscore_application x in
    (* If there are any attributes, render unary like `(~-) x [@ppx]`, and infix like `(+) x y [@attr]` *)

    let {arityAttrs; stdAttrs; jsxAttrs; stylisticAttrs; uncurried} =
      partitionAttributes ~allowUncurry:(Reason_heuristics.bsExprCanBeUncurried x) x.pexp_attributes
    in
    let stylisticAttrs = Reason_attributes.maybe_remove_stylistic_attrs stylisticAttrs preserve_braces in
    let () = if uncurried then Hashtbl.add uncurriedTable x.pexp_loc true in
    let x = {x with pexp_attributes = (stylisticAttrs @ arityAttrs @ stdAttrs @ jsxAttrs) } in
    (* If there's any attributes, recurse without them, then apply them to
       the ends of functions, or simplify infix printings then append. *)
    if stdAttrs != [] then
      let withoutVisibleAttrs = {x with pexp_attributes=(stylisticAttrs @ arityAttrs @ jsxAttrs)} in
      let attributesAsList = (List.map self#attribute stdAttrs) in
      let itms = match self#unparseExprRecurse withoutVisibleAttrs with
        | SpecificInfixPrecedence ({reducePrecedence}, wrappedRule) ->
            let itm = self#unparseResolvedRule wrappedRule in
            (match reducePrecedence with
             (* doesn't need wrapping; we know how to parse *)
             | Custom "prec_lbracket" | Token "." -> [itm]
             | _ -> [formatPrecedence ~loc:x.pexp_loc itm])
        | FunctionApplication itms -> itms
        | PotentiallyLowPrecedence itm -> [formatPrecedence ~loc:x.pexp_loc itm]
        | Simple itm -> [itm]
      in
      FunctionApplication [
        makeList
          ~break:IfNeed
          ~inline:(true, true)
          ~indent:0
          ~postSpace:true
          (List.concat [attributesAsList; itms])
      ]
    else
    match self#simplest_expression x with
    | Some se -> Simple se
    | None ->
    let self = self#reset_request_braces in
    match x.pexp_desc with
    | Pexp_apply (e, ls) -> (
      let ls = List.map (fun (l,expr) -> (l, self#process_underscore_application expr)) ls in
      match (e, ls) with
      | (e, _) when Reason_heuristics.isPipeFirst e ->
        let prec = Token pipeFirstToken in
          SpecificInfixPrecedence
            ({reducePrecedence=prec; shiftPrecedence=prec}, LayoutNode (self#formatPipeFirst x))
      | ({pexp_desc = Pexp_ident {txt = Ldot (Lident ("Array"),"get")}}, [(_,e1);(_,e2)]) ->
        begin match e1.pexp_desc with
          | Pexp_ident ({txt = Lident "_"}) ->
            let k = atom "Array.get" in
            let v = makeList ~postSpace:true ~sep:(Layout.Sep ",") ~wrap:("(", ")")
                [atom "_"; self#unparseExpr e2]
            in
            Simple (label k v)
          | _ ->
            let prec = Custom "prec_lbracket" in
            let lhs = self#unparseResolvedRule (
              self#ensureExpression ~reducesOnToken:prec e1
            ) in
            let rhs = self#unparseExpr e2 in
            SpecificInfixPrecedence
              ({reducePrecedence=prec; shiftPrecedence=prec}, LayoutNode (self#access "[" "]" lhs rhs))
        end
      | ({pexp_desc = Pexp_ident {txt = Ldot (Lident ("String"),"get")}}, [(_,e1);(_,e2)]) ->
        if Reason_heuristics.isUnderscoreIdent e1 then
          let k = atom "String.get" in
          let v = makeList ~postSpace:true ~sep:(Layout.Sep ",") ~wrap:("(", ")")
              [atom "_"; self#unparseExpr e2]
          in
          Simple (label k v)
        else
          let prec = Custom "prec_lbracket" in
            let lhs = self#unparseResolvedRule (
              self#ensureExpression ~reducesOnToken:prec e1
            ) in
            let rhs = self#unparseExpr e2 in
          SpecificInfixPrecedence
            ({reducePrecedence=prec; shiftPrecedence=prec}, LayoutNode (self#access ".[" "]" lhs rhs))
      | (
        {pexp_desc= Pexp_ident {txt=Ldot (Ldot (Lident "Bigarray", "Genarray" ), "get")}},
        [(_,e1); (_,({pexp_desc=Pexp_array ls} as e2))]
      ) ->
        if (Reason_heuristics.isUnderscoreIdent e1) then
          let k = atom "Bigarray.Genarray.get" in
          let v = makeList ~postSpace:true ~sep:(Layout.Sep ",") ~wrap:("(", ")")
              [atom "_"; self#unparseExpr e2]
          in
          Simple (label k v)
        else
          let formattedList = List.map self#unparseExpr ls in
          let lhs = makeList [(self#simple_enough_to_be_lhs_dot_send e1); atom "."] in
          let rhs = makeList ~break:IfNeed ~postSpace:true ~sep:commaSep ~wrap:("{", "}") formattedList in
          let prec = Custom "prec_lbracket" in
          SpecificInfixPrecedence ({reducePrecedence=prec; shiftPrecedence=prec}, LayoutNode (label lhs rhs))
      | (
        {pexp_desc= Pexp_ident {txt=
                                  Ldot (Ldot (Lident "Bigarray", (("Array1"|"Array2"|"Array3") as arrayIdent)), "get")}
        },
        (_,e1)::rest
      ) ->
        if Reason_heuristics.isUnderscoreIdent e1 then
          let k = atom("Bigarray." ^ arrayIdent ^ ".get") in
          let v = makeList ~postSpace:true ~sep:(Layout.Sep ",") ~wrap:("(", ")")
              ((atom "_")::(List.map (fun (_, e) -> self#unparseExpr e) rest))
          in
          Simple (label k v)
        else
          let formattedList = List.map self#unparseExpr (List.map snd rest) in
          let lhs = makeList [(self#simple_enough_to_be_lhs_dot_send e1); atom "."] in
          let rhs = makeList ~break:IfNeed ~postSpace:true ~sep:commaSep ~wrap:("{", "}") formattedList in
          let prec = Custom "prec_lbracket" in
          SpecificInfixPrecedence ({reducePrecedence=prec; shiftPrecedence=prec}, LayoutNode (label lhs rhs))
      | _ -> (

          match (self#sugar_set_expr_parts x) with
      (* Returns None if there's attributes - would render as regular function *)
      (* Format as if it were an infix function application with identifier "=" *)
      | Some (simplyFormatedLeftItm, rightExpr) -> (
        let tokenPrec = Token updateToken in
        let rightItm = self#ensureContainingRule ~withPrecedence:tokenPrec ~reducesAfterRight:rightExpr () in
        let leftWithOp = makeList ~postSpace:true [simplyFormatedLeftItm; atom updateToken] in
        let expr = label ~space:true leftWithOp (self#unparseResolvedRule rightItm) in
        SpecificInfixPrecedence ({reducePrecedence=tokenPrec; shiftPrecedence=tokenPrec}, LayoutNode expr)
      )
      | None -> (
        match (printedStringAndFixityExpr e, ls) with
       (* We must take care not to print two subsequent prefix operators without
         spaces between them (`! !` could become `!!` which is totally
         different).  *)
        | (AlmostSimplePrefix prefixStr, [(Nolabel, rightExpr)]) ->
        let forceSpace = match rightExpr.pexp_desc with
          | Pexp_apply (ee, _) ->
            (match printedStringAndFixityExpr ee with | AlmostSimplePrefix _ -> true | _ -> false)
          | _ -> false
        in
        let prec = Token prefixStr in
        let rightItm = self#unparseResolvedRule (
          self#ensureContainingRule ~withPrecedence:prec ~reducesAfterRight:rightExpr ()
        ) in
        SpecificInfixPrecedence
          ({reducePrecedence=prec; shiftPrecedence = prec}, LayoutNode (label ~space:forceSpace (atom prefixStr) rightItm))
        | (UnaryPostfix postfixStr, [(Nolabel, leftExpr)]) ->
          let forceSpace = match leftExpr.pexp_desc with
            | Pexp_apply (ee, _) ->
              (match printedStringAndFixityExpr ee with
               | UnaryPostfix "^" | AlmostSimplePrefix _ -> true
               | _ -> false)
            | _ -> false
          in
          let leftItm = (match leftExpr.pexp_desc with
            | Pexp_apply (e,_) ->
              (match printedStringAndFixityExpr e with
               | Infix printedIdent
                 when requireNoSpaceFor printedIdent ||
                      Reason_heuristics.isPipeFirst e ->
                 self#unparseExpr leftExpr
               | _ -> self#simplifyUnparseExpr leftExpr)
            | Pexp_field _ -> self#unparseExpr leftExpr
            | _ -> self#simplifyUnparseExpr leftExpr
          )
          in
          Simple (label ~space:forceSpace leftItm (atom postfixStr))
        | (Infix printedIdent, [(Nolabel, leftExpr); (Nolabel, rightExpr)]) ->
          let infixToken = Token printedIdent in
          let rightItm = self#ensureContainingRule ~withPrecedence:infixToken ~reducesAfterRight:rightExpr () in
          let leftItm = self#ensureExpression ~reducesOnToken:infixToken leftExpr in
          (* Left exprs of infix tokens which we don't print spaces for (e.g. `##`)
             need to be wrapped in parens in the case of postfix `^`. Otherwise,
             printing will be ambiguous as `^` is also a valid start of an infix
             operator. *)
          let formattedLeftItm = (match leftItm with
            | LayoutNode x -> begin match leftExpr.pexp_desc with
                | Pexp_apply (e,_) ->
                  (match printedStringAndFixityExpr e with
                   | UnaryPostfix "^" when requireNoSpaceFor printedIdent ->
                     LayoutNode (formatPrecedence ~loc:leftExpr.pexp_loc x)
                   | _ -> leftItm)
                | _ -> leftItm
              end
            | InfixTree _ -> leftItm
          ) in
          let infixTree = InfixTree (printedIdent, formattedLeftItm, rightItm) in
          SpecificInfixPrecedence ({reducePrecedence=infixToken; shiftPrecedence=infixToken}, infixTree)
        (* Will be rendered as `(+) a b c` which is parsed with higher precedence than all
           the other forms unparsed here.*)
        | (UnaryPlusPrefix printedIdent, [(Nolabel, rightExpr)]) ->
          let prec = Custom "prec_unary" in
          let rightItm = self#unparseResolvedRule (
            self#ensureContainingRule ~withPrecedence:prec ~reducesAfterRight:rightExpr ()
          ) in
          let expr = label ~space:true (atom printedIdent) rightItm in
          SpecificInfixPrecedence ({reducePrecedence=prec; shiftPrecedence=Token printedIdent}, LayoutNode expr)
        | (UnaryMinusPrefix printedIdent as x, [(Nolabel, rightExpr)])
        | (UnaryNotPrefix printedIdent as x, [(Nolabel, rightExpr)]) ->
          let forceSpace = (match x with
              | UnaryMinusPrefix _ -> true
              | _ -> begin match rightExpr.pexp_desc with
                  | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident s}}, _) ->
                    isSimplePrefixToken s
                  | _ -> false
                end) in
          let prec = Custom "prec_unary" in
          let rightItm = self#unparseResolvedRule (
            self#ensureContainingRule ~withPrecedence:prec ~reducesAfterRight:rightExpr ()
          ) in
          let expr = label ~space:forceSpace (atom printedIdent) rightItm in
          SpecificInfixPrecedence ({reducePrecedence=prec; shiftPrecedence=Token printedIdent}, LayoutNode expr)
        (* Will need to be rendered in self#expression as (~-) x y z. *)
        | (_, _) ->
        (* This case will happen when there is something like

             Bar.createElement a::1 b::2 [] [@bla] [@JSX]

           At this point the bla will be stripped (because it's a visible
           attribute) but the JSX will still be there.
         *)

        (* this case also happens when we have something like:
         * List.map((a) => a + 1, numbers);
         * We got two "List.map" as Pexp_ident & a list of arguments:
         * [`(a) => a + 1`; `numbers`]
         *
         * Another possible case is:
         * describe("App", () =>
         *   test("math", () =>
         *     Expect.expect(1 + 2) |> toBe(3)));
         *)
        let uncurried = try Hashtbl.find uncurriedTable x.pexp_loc with | Not_found -> false in
        FunctionApplication (
          self#formatFunAppl
            ~uncurried
            ~jsxAttrs
            ~args:ls
            ~applicationExpr:x
            ~funExpr:e
            ()
          )
        )
      )
    )
    | Pexp_field (e, li) ->
        let prec = Token "." in
        let leftItm = self#unparseResolvedRule (
          self#ensureExpression ~reducesOnToken:prec e
        ) in
        let {stdAttrs} = partitionAttributes e.pexp_attributes in
        let formattedLeftItm = if stdAttrs == [] then
            leftItm
          else
            formatPrecedence ~loc:e.pexp_loc leftItm
        in
        let layout = label (makeList [formattedLeftItm; atom "."]) (self#longident_loc li) in
        SpecificInfixPrecedence ({reducePrecedence=prec; shiftPrecedence=prec}, LayoutNode layout)
    | Pexp_construct (li, Some eo) when not (is_simple_construct (view_expr x)) -> (
        match view_expr x with
        (* TODO: Explicit arity *)
        | `normal ->
            let arityIsClear = isArityClear arityAttrs in
            FunctionApplication [self#constructor_expression ~arityIsClear stdAttrs (self#longident_loc li) eo]
        | _ -> assert false
      )
    | Pexp_variant (l, Some eo) ->
        if arityAttrs != [] then
          raise (NotPossible "Should never see embedded attributes on poly variant")
        else
          FunctionApplication [self#constructor_expression ~polyVariant:true ~arityIsClear:true stdAttrs (atom ("`" ^ l)) eo]
    (* TODO: Should protect this identifier *)
    | Pexp_setinstvar (s, rightExpr) ->
      let rightItm = self#unparseResolvedRule (
        self#ensureContainingRule ~withPrecedence:(Token updateToken) ~reducesAfterRight:rightExpr ()
      ) in
      let expr = label ~space:true (makeList ~postSpace:true [(protectIdentifier s.txt); atom updateToken]) rightItm in
      SpecificInfixPrecedence ({reducePrecedence=(Token updateToken); shiftPrecedence=(Token updateToken)}, LayoutNode expr)
    | Pexp_setfield (leftExpr, li, rightExpr) ->
      let rightItm = self#unparseResolvedRule (
        self#ensureContainingRule ~withPrecedence:(Token updateToken) ~reducesAfterRight:rightExpr ()
      ) in
      let leftItm = self#unparseResolvedRule (
        self#ensureExpression ~reducesOnToken:(Token ".") leftExpr
      ) in
      let leftLbl =
        label
          (makeList [leftItm; atom "."])
          (self#longident_loc li) in
      let expr = label ~space:true (makeList ~postSpace:true [leftLbl; atom updateToken]) rightItm in
      SpecificInfixPrecedence ({reducePrecedence=(Token updateToken); shiftPrecedence=(Token updateToken)}, LayoutNode expr)
    | Pexp_match (e, l) when detectTernary l != None -> (
      match detectTernary l with
      | None -> raise (Invalid_argument "Impossible")
      | Some (tt, ff) ->
        let ifTrue = self#reset_request_braces#unparseExpr tt in
        let testItem = self#unparseResolvedRule (
          self#reset_request_braces#ensureExpression e ~reducesOnToken:(Token "?")
        ) in
        let ifFalse = self#unparseResolvedRule (
          self#reset_request_braces#ensureContainingRule ~withPrecedence:(Token ":") ~reducesAfterRight:ff ()
        ) in
        let trueBranch = label ~space:true ~break:`Never (atom "?") ifTrue
        in
        let falseBranch = label ~space:true ~break:`Never (atom ":") ifFalse
        in
        let expr = label ~space:true testItem (makeList ~break:IfNeed ~sep:(Sep " ") ~inline:(true, true) [trueBranch; falseBranch])
        in
        SpecificInfixPrecedence ({reducePrecedence=Token ":"; shiftPrecedence=Token "?"}, LayoutNode expr)
      )
    | _ -> (
      match self#expression_requiring_parens_in_infix x with
      | Some e -> e
      | None -> raise (Invalid_argument "No match for unparsing expression")
    )

  method formatNonSequencyExpression e =
    (*
     * Instead of printing:
     *   let result =  { open Fmt; strf(foo);}
     *
     * We format as:
     *   let result = Fmt.(strf(foo))
     *
     * (Also see https://github.com/facebook/Reason/issues/114)
     *)
    match e.pexp_attributes, e.pexp_desc with
    | [], Pexp_record _ (* syntax sugar for M.{x:1} *)
    | [], Pexp_tuple _ (* syntax sugar for M.(a, b) *)
    | [], Pexp_object {pcstr_fields = []} (* syntax sugar for M.{} *)
    | [], Pexp_construct ( {txt= Lident"::"},Some _)
    | [], Pexp_construct ( {txt= Lident"[]"},_)
    | [], Pexp_extension ( {txt = "bs.obj"}, _ ) ->
      self#simplifyUnparseExpr e (* syntax sugar for M.[x,y] *)
    (* syntax sugar for the rest, wrap with parens to avoid ambiguity.
     * E.g., avoid M.(M2.v) being printed as M.M2.v
     * Or ReasonReact.(<> {string("Test")} </>);
     *)
    | _ -> makeList ~wrap:("(",")") ~break:IfNeed [self#unparseExpr e]

  (*
     It's not enough to only check if precedence of an infix left/right is
     greater than the infix itself. We also should likely pay attention to
     left/right associativity. So how do we render the minimum number of
     parenthesis?

     The intuition is that sequential right associative operators will
     naturally build up deep trees on the right side (left builds up left-deep
     trees). So by default, we add parens to model the tree structure that
     we're rendering except when the parser will *naturally* parse the tree
     structure that the parens assert.

     Sequential identical infix operators:
     ------------------------------------
     So if we see a nested infix operator of precedence Y, as one side of
     another infix operator that has the same precedence (Y), that is S
     associative on the S side of the function application, we don't need to
     wrap in parens. In more detail:

     -Add parens around infix binary function application
       Exception 1: Unless we are a left-assoc operator of precedence X in the left branch of an operator w/ precedence X.
       Exception 2: Unless we are a right-assoc operator of precedence X in the right branch of an operator w/ precedence X.
       Exception 3: Unless we are a _any_-assoc X operator in the _any_ branch of an Y operator where X has greater precedence than Y.

     Note that the exceptions do not specify any special cases for mixing
     left/right associativity. Precedence is what determines necessity of
     parens for operators with non-identical precedences. Associativity
     only determines necessity of parens for identically precedented operators.

     PLUS is left assoc:
     - So this one *shouldn't* expand into two consecutive infix +:


            [Pexp_apply]
              /      \
         first +   [Pexp_apply]
                      /   \
                  second + third


     - This one *should*:

                    [Pexp_apply]
                      /      \
           [  Pexp_apply  ] + third
              /     \
           first +  second



     COLONCOLON is right assoc, so
     - This one *should* expand into two consecutive infix ::  :

            [Pexp_apply]
              /      \
         first ::   [Pexp_apply]
                      /   \
                  second :: third


     - This one *shouldn't*:

                    [Pexp_apply]
                      /      \
           [  Pexp_apply  ] :: third
              /     \
           first ::  second




     Sequential differing infix operators:
     ------------------------------------

     Neither of the following require paren grouping because of rule 3.


            [Pexp_apply]
              /      \
         first  +  [Pexp_apply]
                      /   \
                  second * third


                    [Pexp_apply]
                      /      \
            [Pexp_apply  +  third
              /     \
           first *  second

      The previous has nothing to do with the fact that + and * have the same
      associativity. Exception 3 applies to the following where :: is right assoc
      and + is left. + has higher precedence than ::

      - so parens aren't required to group + when it is in a branch of a
        lower precedence ::

            [Pexp_apply]
              /      \
         first ::   [Pexp_apply]
                      /   \
                  second + third


      - Whereas there is no Exception that applies in this case (Exception 3
        doesn't apply) so parens are required around the :: in this case.

                    [Pexp_apply]
                      /      \
           [  Pexp_apply  ] + third
              /     \
           first ::  second

  *)

  method classExpressionToFormattedApplicationItems = function
    | { pcl_desc = Pcl_apply (ce, l) } ->
      [label (self#simple_class_expr ce) (self#label_x_expression_params l)]
    | x -> [self#class_expr x]


  method dotdotdotChild expr =
    let self = self#inline_braces in
    match expr with
    | {pexp_desc = Pexp_apply (funExpr, args)}
      when printedStringAndFixityExpr funExpr == Normal &&
           Reason_attributes.without_stylistic_attrs expr.pexp_attributes == [] ->
      begin match (self#formatFunAppl ~prefix:(atom "...") ~wrap:("{", "}") ~jsxAttrs:[] ~args ~funExpr ~applicationExpr:expr ()) with
      | [x] -> x
      | xs -> makeList xs
      end
    | {pexp_desc = Pexp_fun _ } ->
        self#formatPexpFun ~prefix:(atom "...") ~wrap:("{", "}") expr
    | _ ->
      (* Currently spreading a list must be wrapped in { }.
       * You can remove the entire even_wrap_simple arg when that is fixed. *)
      let even_wrap_simple = match expr with
      | {pexp_desc = Pexp_construct ({txt = Lident"::"}, Some {pexp_desc = Pexp_tuple _})} ->
          not (Reason_attributes.has_jsx_attributes expr.pexp_attributes)
      | _ -> false
      in
      let childLayout =
        self#dont_preserve_braces#simplifyUnparseExpr
          ~even_wrap_simple
          ~wrap:("{", "}")
          expr
      in
      makeList ~break:Never [atom "..."; childLayout]
  (*
        How JSX is formatted/wrapped. We want the attributes to wrap independently
        of children.

        <xxx
          attr1=blah
          attr2=foo>
          child
          child
          child
        </x>

      +-------------------------------+
      |  left   right (list of attrs) |
      |   / \   /   \                 |
      |   <tag                        |
      |     attr1=blah                |
      |     attr2=foo                 |
      +-------------------------------+
       |
       |
       |
       |      left       right  list of children with
       |   /       \    /  \     open,close = > </tag>
       |  +---------+
       +--|         |    >
          +---------+

          </tag>           *)
  method formatJSXComponent componentName ?closeComponentName args =
    let self = self#inline_braces in
    let rec processArguments arguments processedAttrs children =
      match arguments with

      | (Labelled "children", {pexp_desc = Pexp_construct (_, None)}) :: tail ->
        processArguments tail processedAttrs None
      | (Labelled "children", (
          {pexp_desc = Pexp_construct ({txt = Lident"::"}, Some {pexp_desc = Pexp_tuple _})} as arg
          )
        ) :: tail ->
        (match self#formatJsxChildrenNonSpread arg [] with
        (* Back out of the standard jsx child formatting *)
        | None -> processArguments tail processedAttrs (Some [self#dotdotdotChild arg])
        | Some chldn -> processArguments tail processedAttrs (Some chldn))
      | (Labelled "children", expr) :: tail ->
          processArguments tail processedAttrs (Some [self#dotdotdotChild expr])
      | (Optional lbl, expression) :: tail ->
        let {jsxAttrs; _} = partitionAttributes expression.pexp_attributes in
        let value_has_jsx = jsxAttrs != [] in
        let nextAttr =
          match expression.pexp_desc with
          | Pexp_ident ident when isPunnedJsxArg lbl ident ->
              makeList ~break:Layout.Never [atom "?"; atom lbl]
           | Pexp_construct _ when value_has_jsx ->
               label
                (makeList ~break:Layout.Never [atom lbl; atom "=?"])
                (self#simplifyUnparseExpr ~wrap:("{","}") expression)
          | _ ->
              label
                (makeList ~break:Layout.Never [atom lbl; atom "=?"])
                (self#dont_preserve_braces#simplifyUnparseExpr ~wrap:("{","}") expression) in
        processArguments tail (nextAttr :: processedAttrs) children
      | (Labelled lbl, expression) :: tail ->
         let {jsxAttrs; _} = partitionAttributes expression.pexp_attributes in
         let value_has_jsx = jsxAttrs != [] in
         let nextAttr =
           match expression.pexp_desc with
           | Pexp_ident ident when isPunnedJsxArg lbl ident -> atom lbl
           | _ when isJSXComponent expression  ->
               label (atom (lbl ^ "="))
                     (makeList ~break:IfNeed ~wrap:("{", "}")
                       [self#dont_preserve_braces#simplifyUnparseExpr expression])
           | Pexp_open (me, e)
             when self#isSeriesOfOpensFollowedByNonSequencyExpression expression ->
             label (makeList [atom lbl;
                              atom "=";
                              (label (self#moduleExpressionToFormattedApplicationItems me.popen_expr) (atom "."))])
                   (self#formatNonSequencyExpression e)
           | Pexp_apply ({pexp_desc = Pexp_ident _} as funExpr, args)
                when printedStringAndFixityExpr funExpr == Normal &&
                     Reason_attributes.without_stylistic_attrs expression.pexp_attributes == [] ->
             let lhs = makeList [atom lbl; atom "="] in
             begin match (
               self#formatFunAppl
                ~prefix:lhs
                ~wrap:("{", "}")
                ~jsxAttrs:[]
                ~args
                ~funExpr
                ~applicationExpr:expression
                ())
             with
             | [x] -> x
             | xs -> makeList xs
             end
           | Pexp_apply (eFun, _) ->
             let lhs = makeList [atom lbl; atom "="] in
             let rhs = (match printedStringAndFixityExpr eFun with
                 | Infix str when requireNoSpaceFor str -> self#unparseExpr expression
                 | _ -> self#dont_preserve_braces#simplifyUnparseExpr ~wrap:("{","}") expression)
             in label lhs rhs
           | Pexp_construct _ when value_has_jsx ->
               label
                (makeList [atom lbl; atom "="])
                (self#simplifyUnparseExpr ~wrap:("{","}") expression)
           | Pexp_record _
           | Pexp_construct _
           | Pexp_array _
           | Pexp_tuple _
           | Pexp_match _
           | Pexp_extension _
           | Pexp_function _ ->
               label
                (makeList [atom lbl; atom "="])
                (self#dont_preserve_braces#simplifyUnparseExpr ~wrap:("{","}") expression)
           | Pexp_fun _ ->
               let propName = makeList [atom lbl; atom "="] in
               self#formatPexpFun ~wrap:("{", "}") ~prefix:propName expression
           | _ -> makeList [
                    atom lbl;
                    atom "=";
                    self#dont_preserve_braces#simplifyUnparseExpr ~wrap:("{","}") expression
                  ]
         in
         processArguments tail (nextAttr :: processedAttrs) children
      | [] -> (processedAttrs, children)
      | _ :: tail -> processArguments tail processedAttrs children
    in
    let (reversedAttributes, children) = processArguments args [] None in
    match children with
    | None ->
      makeList
        ~break:IfNeed
        ~wrap:("<" ^ componentName, "/>")
        ~pad:(true, true)
        ~inline:(false, false)
        ~postSpace:true
        (List.rev reversedAttributes)
    | Some renderedChildren ->
      let openTagAndAttrs =
        match reversedAttributes with
        | [] -> (atom ("<" ^ componentName ^ ">"))
        | revAttrHd::revAttrTl ->
          let finalAttrList = (List.rev (makeList ~break:Layout.Never [revAttrHd; atom ">"] :: revAttrTl)) in
          let renderedAttrList = (makeList ~inline:(true, true) ~break:IfNeed ~pad:(false, false) ~preSpace:true finalAttrList) in
          label
            ~space:true
            (atom ("<" ^ componentName))
            renderedAttrList
      in
      label
        openTagAndAttrs
        (makeList
          ~wrap:("", "</" ^ (match closeComponentName with None -> componentName | Some close -> close) ^ ">")
          ~inline:(true, false)
          ~break:IfNeed
          ~pad:(true, true)
          ~postSpace:true
          renderedChildren)

  (*
   * Format Pexp_fun expression: (a, b) => a + b;
   * Example: the `onClick` prop with Pexp_fun in
   *  <div
   *    onClick={(event) => {
   *      Js.log(event);
   *      handleChange(event);
   *    }}
   *  />;
   *
   *  The arguments of the callback (Pexp_fun) should be inlined as much as
   *  possible on the same line as `onClick={`.
   *  Also notice the brace-hugging `}}` at the end.
   *
   *  ~prefix -> prefixes the Pexp_fun layout, example `onClick=`
   *  ~wrap -> wraps the `Pexp_fun` in the tuple passed to wrap, e.g. `{` and
   *  `}` for jsx
   *)
  method formatPexpFun ?(prefix=(atom "")) ?(wrap=("","")) expression =
    let (lwrap, rwrap) = wrap in
    let {stdAttrs; uncurried} = partitionAttributes expression.pexp_attributes in
    if uncurried then Hashtbl.add uncurriedTable expression.pexp_loc true;

    let (args, ret) =
      (* omit attributes here, we're formatting them manually *)
      self#curriedPatternsAndReturnVal {expression with pexp_attributes = [] }
    in
    (* Format `onClick={` *)
    let propName = makeList ~wrap:("", lwrap) [prefix] in
    let argsList =
      let args = match args with
      | [argsList] -> argsList
      | args -> makeList args
      in
      match stdAttrs with
      | [] -> args
      | attrs ->
          (* attach attributes to the args of the Pexp_fun: `[@attr] (event)` *)
          let attrList =
            makeList ~inline:(true, true) ~break:IfNeed ~postSpace:true
              (List.map self#attribute attrs)
          in
          let all = [attrList; args] in
          makeList ~break:IfNeed ~inline:(true, true) ~postSpace:true all
    in
    (* Format `onClick={(event)` *)
    let propNameWithArgs = label propName argsList in
    (* Pick constraints: (a, b) :string => ...
     * :string is the constraint here *)
    let (return, optConstr) = match ret.pexp_desc with
      | Pexp_constraint (e, ct) -> (e, Some (self#non_arrowed_core_type ct))
      | _ -> (ret, None)
    in
    let returnExpr, leftWrap = match (self#letList return) with
    | [x] ->
      (* Format `handleChange(event)}` or
       *  handleChange(event)
       * }
       *
       * If the closing rwrap is empty, we need it to be inline, otherwise
       * we get a empty newline when the layout breaks:
       * ```
       *  handleChange(event)
       *
       * ```
       * (Notice to nonsense newline)
       *)
       let shouldPreserveBraces = self#should_preserve_requested_braces return in
       let rwrap = if shouldPreserveBraces then
         "}" ^ rwrap
       else
         rwrap
       in
       let inlineClosing = rwrap = "" in
       let layout =
         makeList ~break:IfNeed ~inline:(true, inlineClosing) ~wrap:("", rwrap) [x]
       in
       layout, if shouldPreserveBraces then "{" else ""
    | xs ->
      (* Format `Js.log(event)` and `handleChange(event)` as
       * {
       *   Js.log(event);
       *   handleChange(event);
       * }}
       *)
      let layout = makeList
          ~break:Always_rec ~sep:(SepFinal (";", ";")) ~wrap:("{", "}" ^ rwrap)
          xs
      in
      layout, ""
    in
    match optConstr with
    | Some typeConstraint ->
      let upToConstraint =
        label ~space:true
          (makeList ~wrap:("", ":") [propNameWithArgs])
          typeConstraint
      in
      label
        (makeList ~wrap:("", " => " ^ leftWrap) [upToConstraint])
        returnExpr
    | None ->
      label
        (makeList ~wrap:("", " => " ^ leftWrap) [propNameWithArgs])
        returnExpr

  (* Creates a list of simple module expressions corresponding to module
     expression or functor application. *)
  method moduleExpressionToFormattedApplicationItems ?(prefix="") x =
    match x with
    (* are we formatting a functor application with a module structure as arg?
     * YourLib.Make({
     *   type t = int;
     *   type s = string;
     * });
     *
     * We should "hug" the parens here: ({ & }) should stick together.
     *)
    | { pmod_desc = Pmod_apply (
            ({pmod_desc = Pmod_ident _} as m1),
            ({pmod_desc = Pmod_structure _} as m2)
         )
      } ->
        let modIdent = source_map ~loc:m1.pmod_loc (self#simple_module_expr m1) in
        let name = if prefix <> "" then
          makeList ~postSpace:true[atom prefix; modIdent]
          else modIdent
        in
        let arg = source_map ~loc:m2.pmod_loc (self#simple_module_expr ~hug:true m2) in
        label name arg
    | _ ->
      let rec extract_apps args = function
        | { pmod_desc = Pmod_apply (me1, me2) } ->
          let arg = source_map ~loc:me2.pmod_loc (self#simple_module_expr me2) in
          extract_apps (arg :: args) me1
        | me ->
          let head = source_map ~loc:me.pmod_loc (self#module_expr me) in
          if args == [] then head else label head (makeTup args)
      in
      let functor_application = extract_apps [] x in
      if prefix <> "" then
        makeList ~postSpace:true [atom prefix; functor_application]
      else
        functor_application

  (*

     Watch out, if you see something like below (sixteenTuple getting put on a
     newline), yet a paren-wrapped list wouldn't have had an extra newlin, you
     might need to wrap the single token (sixteenTuple) in [ensureSingleTokenSticksToLabel].
     let (
        axx,
        oxx,
        pxx
      ):
        sixteenTuple = echoTuple (
        0,
        0,
        0
      );
  *)

  method formatSimplePatternBinding labelOpener layoutPattern typeConstraint appTerms =
    let letPattern = label ~break:`Never ~space:true (atom labelOpener) layoutPattern in
    let upUntilEqual =
      match typeConstraint with
        | None -> letPattern
        | Some tc -> formatTypeConstraint letPattern tc
    in
    let includingEqual = makeList ~postSpace:true [upUntilEqual; atom "="] in
    formatAttachmentApplication applicationFinalWrapping (Some (true, includingEqual)) appTerms

  (*
     The [bindingLabel] is either the function name (if let binding) or first
     arg (if lambda).

     For defining layout of the following form:

         lbl one
             two
             constraint => {
           ...
         }

     If using "=" as the arrow, can also be used for:

         met private
             myMethod
             constraint = fun ...

   *)
  method wrapCurriedFunctionBinding
         ?attachTo
         ~arrow
         ?(sweet=false)
         ?(spaceBeforeArrow=true)
         prefixText
         bindingLabel
         patternList
         returnedAppTerms =
    let allPatterns = bindingLabel::patternList in
    let partitioning = curriedFunctionFinalWrapping allPatterns in
    let everythingButReturnVal =
      (*
         Because align_closing is set to false, you get:

         (Brackets[] inserted to show boundaries between open/close of pattern list)
         let[firstThing
             secondThing
             thirdThing]

         It only wraps to indent four by coincidence: If the "opening" token was
         longer, you'd get:

         letReallyLong[firstThing
                       secondThing
                       thirdThing]

         For curried let bindings, we stick the arrow in the *last* pattern:
         let[firstThing
             secondThing
             thirdThing =>]

         But it could have just as easily been the "closing" token corresponding to
         "let". This works because we have [align_closing = false]. The benefit of
         shoving it in the last pattern, is that we can turn [align_closing = true]
         and still have the arrow stuck to the last pattern (which is usually what we
         want) (See modeTwo below).
      *)
      match partitioning with
        | None when sweet ->
          makeList
            ~pad:(false, spaceBeforeArrow)
            ~wrap:("", arrow)
            ~indent:(settings.space * settings.indentWrappedPatternArgs)
            ~postSpace:true
            ~inline:(true, true)
            ~break:IfNeed
            allPatterns
        | None ->
            (* We want the binding label to break *with* the arguments. Again,
               there's no apparent way to add additional indenting for the
               args with this setting. *)

            (*
               Formats lambdas by treating the first pattern as the
               "bindingLabel" which is kind of strange in some cases (when
               you only have one arg that wraps)...

                  echoTheEchoer (
                    fun (
                          a,
                          p
                        ) => (
                      a,
                      b
                    )

               But it makes sense in others (where you have multiple args):

                  echoTheEchoer (
                    fun (
                          a,
                          p
                        )
                        mySecondArg
                        myThirdArg => (
                      a,
                      b
                    )

               Try any other convention for wrapping that first arg and it
               won't look as balanced when adding multiple args.

            *)
          makeList
            ~pad:(true, spaceBeforeArrow)
            ~wrap:(prefixText, arrow)
            ~indent:(settings.space * settings.indentWrappedPatternArgs)
            ~postSpace:true
            ~inline:(true, true)
            ~break:IfNeed
            allPatterns
        | Some (attachedList, wrappedListy) ->
            (* To get *only* the final argument to "break", while not
               necessarily breaking the prior arguments, we dock everything
               but the last item to a created label *)
          label
            ~space:true
            (
              makeList
                ~pad:(true, spaceBeforeArrow)
                ~wrap:(prefixText, arrow)
                ~indent:(settings.space * settings.indentWrappedPatternArgs)
                ~postSpace:true
                ~inline:(true, true)
                ~break:IfNeed
                attachedList
            )
            wrappedListy
    in

    let everythingButAppTerms = match attachTo with
      | None -> everythingButReturnVal
      | Some toThis -> label ~space:true toThis everythingButReturnVal
    in
    formatAttachmentApplication
      applicationFinalWrapping
      (Some (true, everythingButAppTerms))
      returnedAppTerms

  method leadingCurriedAbstractTypes x =
    let rec argsAndReturn xx =
      match xx.pexp_desc with
        | Pexp_newtype (str,e) ->
            let (nextArgs, return) = argsAndReturn e in
            (str::nextArgs, return)
        | _ -> ([], xx.pexp_desc)
    in argsAndReturn x

  method curriedConstructorPatternsAndReturnVal cl =
    let rec argsAndReturn args = function
      | { pcl_desc = Pcl_fun (label, eo, p, e); pcl_attributes = [] } ->
        let arg = source_map ~loc:p.ppat_loc (self#label_exp label eo p) in
        argsAndReturn (arg :: args) e
      | xx ->
        if args == [] then (None, xx) else (Some (makeTup (List.rev args)), xx)
    in
    argsAndReturn [] cl



  (*
    Returns the arguments list (if any, that occur before the =>), and the
    final expression (that is either returned from the function (after =>) or
    that is bound to the value (if there are no arguments, and this is just a
    let pattern binding)).
  *)
  method curriedPatternsAndReturnVal x =
    let uncurried = try Hashtbl.find uncurriedTable x.pexp_loc with | Not_found -> false in
    let rec extract_args xx =
      let {stdAttrs} = partitionAttributes ~allowUncurry:false xx.pexp_attributes in
      if stdAttrs != [] then
        ([], xx)
      else match xx.pexp_desc with
        (* label * expression option * pattern * expression *)
        | Pexp_fun (l, eo, p, e) ->
          let args, ret = extract_args e in
          (`Value (l,eo,p) :: args, ret)
        | Pexp_newtype (newtype,e) ->
          let args, ret = extract_args e in
          (`Type newtype :: args, ret)
        | Pexp_constraint _ -> ([], xx)
        | _ -> ([], xx)
    in
    let prepare_arg = function
      | `Value (l,eo,p) -> source_map ~loc:p.ppat_loc (self#label_exp l eo p)
      | `Type nt -> atom ("type " ^ nt.txt)
    in
    let single_argument_no_parens p ret =
      if uncurried then false
      else
        let isUnitPat = is_unit_pattern p in
        let isAnyPat = is_any_pattern p in
        begin match ret.pexp_desc with
        (* (event) :ReasonReact.event => {...}
         * The above Pexp_fun with constraint ReasonReact.event requires parens
         * surrounding the single argument `event`.*)
        | Pexp_constraint _ when not isUnitPat && not isAnyPat -> false
        | _ -> isUnitPat || isAnyPat || is_ident_pattern p
      end
    in
    match extract_args x with
    | ([], ret) -> ([], ret)
    | ([`Value (Nolabel, None, p) ], ret) when is_unit_pattern p && uncurried ->
        ( [atom "(.)"], ret)
    | ([`Value (Nolabel, None, p) as arg], ret) when single_argument_no_parens p ret ->
      ([prepare_arg arg], ret)
    | (args, ret) ->
        ([makeTup ~uncurried (List.map prepare_arg args)], ret)

  (* Returns the (curriedModule, returnStructure) for a functor *)
  method curriedFunctorPatternsAndReturnStruct = function
    (* string loc * module_type option * module_expr *)
    | { pmod_desc = Pmod_functor(s, mt, me2) } ->
        let firstOne =
          match mt with
            | None -> atom "()"
            | Some mt' -> self#module_type (makeList [atom s.txt; atom ":"]) mt'
        in
        let (functorArgsRecurse, returnStructure) = (self#curriedFunctorPatternsAndReturnStruct me2) in
        (firstOne::functorArgsRecurse, returnStructure)
    | me -> ([], me)

  method isRenderableAsPolymorphicAbstractTypes
         typeVars
         polyType
         leadingAbstractVars
         nonVarifiedType =
      same_ast_modulo_varification_and_extensions polyType nonVarifiedType &&
      for_all2' string_loc_equal typeVars leadingAbstractVars

  (* Reinterpret this as a pattern constraint since we don't currently have a
     way to disambiguate. There is currently a way to disambiguate a parsing
     from Ppat_constraint vs.  Pexp_constraint. Currently (and consistent with
     OCaml standard parser):

       let (x: typ) = blah;
         Becomes Ppat_constraint
       let x:poly . type = blah;
         Becomes Ppat_constraint
       let x:typ = blah;
         Becomes Pexp_constraint(ghost)
       let x = (blah:typ);
         Becomes Pexp_constraint(ghost)

     How are double constraints represented?
     let (x:typ) = (blah:typ);
     If currently both constraints are parsed into a single Pexp_constraint,
     then something must be lost, and how could you fail type checking on:
     let x:int = (10:string) ?? Answer: It probably parses into a nested
     Pexp_constraint.

     Proposal:

       let (x: typ) = blah;
         Becomes Ppat_constraint   (still)
       let x:poly . type = blah;
         Becomes Ppat_constraint   (still)
       let x:typ = blah;
         Becomes Ppat_constraint
       let x = blah:typ;
         Becomes Pexp_constraint


     Reasoning: Allows parsing of any of the currently valid ML forms, but
     combines the two most similar into one form. The only lossyness is the
     unnecessary parens, which there is already precedence for dropping in
     expressions. In the existing approach, preserving a paren-constrained
     expression is *impossible* because it becomes pretty printed as
     let x:t =.... In the proposal, it is not impossible - it is only
     impossible to preserve unnecessary parenthesis around the let binding.

     The one downside is that integrating with existing code that uses [let x =
     (blah:typ)] in standard OCaml will be parsed as a Pexp_constraint. There
     might be some lossiness (beyond parens) that occurs in the original OCaml
     parser.
  *)

  method locallyAbstractPolymorphicFunctionBinding prefixText layoutPattern funWithNewTypes absVars bodyType =
    let appTerms = self#unparseExprApplicationItems funWithNewTypes in
    let locallyAbstractTypes = (List.map (fun x -> atom x.txt) absVars) in
    let typeLayout =
      source_map ~loc:bodyType.ptyp_loc (self#core_type bodyType)
    in
    let polyType =
      label
        ~space:true
        (* TODO: This isn't a correct use of sep! It ruins how
         * comments are interleaved. *)
        (makeList [makeList ~sep:(Sep " ") (atom "type"::locallyAbstractTypes); atom "."])
        typeLayout
      in
    self#formatSimplePatternBinding
      prefixText
      layoutPattern
      (Some polyType)
      appTerms

  (**
      Intelligently switches between:
      Curried function binding w/ constraint on return expr:
         lbl patt
             pattAux
             arg
             :constraint => {
           ...
         }

      Constrained:
         lbl patt
             pattAux...
             :constraint = {
           ...
         }
   *)
  method wrappedBinding prefixText ~arrow pattern patternAux expr =
    let expr = self#process_underscore_application expr in
    let (argsList, return) = self#curriedPatternsAndReturnVal expr in
    let patternList = match patternAux with
      | [] -> pattern
      | _::_ -> makeList ~postSpace:true ~inline:(true, true) ~break:IfNeed (pattern::patternAux)
    in
    match (argsList, return.pexp_desc) with
      | ([], Pexp_constraint (e, ct)) ->
          let typeLayout =
            source_map ~loc:ct.ptyp_loc
              begin match ct.ptyp_desc with
              | Ptyp_package (li, cstrs) ->
                self#typ_package li cstrs
              | _ ->
                self#core_type ct
              end
          in
          let appTerms = self#unparseExprApplicationItems e in
          self#formatSimplePatternBinding prefixText patternList (Some typeLayout) appTerms
      | ([], _) ->
          (* simple let binding, e.g. `let number = 5` *)
          (* let f = (. a, b) => a + b; *)
          let appTerms = self#unparseExprApplicationItems expr in
          self#formatSimplePatternBinding prefixText patternList None appTerms
      | (_::_, _) ->
          let (argsWithConstraint, actualReturn) = self#normalizeFunctionArgsConstraint argsList return in
          let fauxArgs =
            List.concat [patternAux; argsWithConstraint] in
          let returnedAppTerms = self#unparseExprApplicationItems actualReturn in
           (* Attaches the `=` to `f` to recreate javascript function syntax in
            * let f = (a, b) => a + b; *)
          let lbl = makeList ~sep:(Sep " ") ~break:Layout.Never [pattern; atom "="] in
          self#wrapCurriedFunctionBinding prefixText ~arrow lbl fauxArgs returnedAppTerms

  (* Similar to the above method. *)
  method wrappedClassBinding prefixText pattern patternAux expr =
    let (args, return) = self#curriedConstructorPatternsAndReturnVal expr in
    let patternList =
      match patternAux with
        | [] -> pattern
        | _::_ -> makeList ~postSpace:true ~inline:(true, true) ~break:IfNeed (pattern::patternAux)
    in
    match (args, return.pcl_desc) with
      | (None, Pcl_constraint (e, ct)) ->
          let typeLayout = source_map ~loc:ct.pcty_loc (self#class_constructor_type ct) in
          self#formatSimplePatternBinding prefixText patternList (Some typeLayout)
            (self#classExpressionToFormattedApplicationItems e, None)
      | (None, _) ->
          self#formatSimplePatternBinding prefixText patternList None
            (self#classExpressionToFormattedApplicationItems expr, None)
      | (Some args, _) ->
          let (argsWithConstraint, actualReturn) =
            self#normalizeConstructorArgsConstraint [args] return in
          let fauxArgs =
            List.concat [patternAux; argsWithConstraint] in
          self#wrapCurriedFunctionBinding prefixText ~arrow:"=" pattern fauxArgs
            (self#classExpressionToFormattedApplicationItems actualReturn, None)

  (* Attaches doc comments to a layout, with whitespace preserved
   * Example:
   * /** Doc comment */
   *
   * /* another random comment */
   * let a = 1;
   *)
  method attachDocAttrsToLayout
    (* all std attributes attached on the ast node backing the layout *)
    ~stdAttrs:(stdAttrs : Ast_408.Parsetree.attributes)
    (* all doc comments attached on the ast node backing the layout *)
    ~docAttrs:(docAttrs : Ast_408.Parsetree.attributes)
    (* location of the layout *)
    ~loc
    (* layout to attach the doc comments to *)
    ~layout () =
    (*
     * compute the correct location of layout
     * Example:
     * 1| /** doc-comment */
     * 2|
     * 3| [@attribute]
     * 4| let a = 1;
     *
     * The location might indicate a start of line 4 for the ast-node
     * representing `let a = 1`. The reality is that `[@attribute]` should be
     * included (start of line 3), to represent the correct start location
     * of the whole layout.
     *)
    let loc = match stdAttrs with
    | { attr_name = astLoc; _}::_ -> astLoc.loc
    | [] -> loc
    in
    let rec aux prevLoc layout = function
      | ({ attr_name = x; _} as attr : Ast_408.Parsetree.attribute)::xs ->
        let newLayout =
          let range = Range.makeRangeBetween x.loc prevLoc in
          let layout =
            if Range.containsWhitespace ~range ~comments:self#comments () then
              let region = WhitespaceRegion.make ~range ~newlines:1 () in
              Layout.Whitespace(region, layout)
            else layout
          in
          makeList ~inline:(true, true) ~break:Always [
            self#attribute attr;
            layout
          ]
        in aux x.loc newLayout xs
      | [] -> layout
    in
    aux loc layout (List.rev docAttrs)

  method value_binding prefixText { pvb_pat; pvb_attributes; pvb_loc; pvb_expr } =
    self#binding prefixText ~attrs:pvb_attributes ~loc:pvb_loc ~pat:pvb_pat pvb_expr

  method binding_op prefixText { pbop_pat; pbop_loc; pbop_exp } =
    self#binding (escape_stars_slashes prefixText) ~loc:pbop_loc ~pat:pbop_pat pbop_exp

  method binding prefixText ?(attrs=[]) ~loc ~pat expr = (* TODO: print attributes *)
    let body = match pat.ppat_attributes, pat.ppat_desc with
      | [], (Ppat_var _) ->
        self#wrappedBinding prefixText ~arrow:"=>"
          (source_map ~loc:pat.ppat_loc (self#simple_pattern pat))
          [] expr
      (*
         Ppat_constraint is used in bindings of the form

            let (inParenVar:typ) = ...

         And in the case of let bindings for explicitly polymorphic type
         annotations (see parser for more details).

         See reason_parser.mly for explanation of how we encode the two primary
         forms of explicit polymorphic annotations in the parse tree, and how
         we must recover them here.
       *)
      | [], (Ppat_constraint(p, ty)) -> (
          (* Locally abstract forall types are *seriously* mangled by the parsing
             stage, and we have to be very smart about how to recover it.

              let df_locallyAbstractFuncAnnotated:
                type a b.
                  a =>
                  b =>
                  (inputEchoRecord a, inputEchoRecord b) =
                fun (input: a) (input2: b) => (
                  {inputIs: input},
                  {inputIs: input2}
                );

             becomes:

               let df_locallyAbstractFuncAnnotatedTwo:
                 'a 'b .
                 'a => 'b => (inputEchoRecord 'a, inputEchoRecord 'b)
                =
                 fun (type a) (type b) => (
                   fun (input: a) (input2: b) => ({inputIs: input}, {inputIs:input2}):
                     a => b => (inputEchoRecord a, inputEchoRecord b)
                 );
          *)
          let layoutPattern =
            source_map ~loc:pat.ppat_loc (self#simple_pattern p)
          in
          let leadingAbsTypesAndExpr = self#leadingCurriedAbstractTypes expr in
          match (p.ppat_desc, ty.ptyp_desc, leadingAbsTypesAndExpr) with
            | (Ppat_var _,
               Ptyp_poly (typeVars, varifiedPolyType),
               (_::_ as absVars, Pexp_constraint(funWithNewTypes, nonVarifiedExprType)))
              when self#isRenderableAsPolymorphicAbstractTypes
                  typeVars
                  (* If even artificially varified - don't know until returns*)
                  varifiedPolyType
                  absVars
                  nonVarifiedExprType ->
              (*
                 We assume was the case whenever we see this pattern in the
                 AST, it was because the parser parsed the polymorphic locally
                 abstract type sugar.

                 Ppat_var..Ptyp_poly...Pexp_constraint:

                    let x: 'a 'b . 'a => 'b => 'b =
                      fun (type a) (type b) =>
                         (fun aVal bVal => bVal : a => b => b);

                 We need to be careful not to accidentally detect similar
                 forms, that cannot be printed as sugar.

                    let x: 'a 'b . 'a => 'b => 'b =
                      fun (type a) (type b) =>
                         (fun aVal bVal => bVal : int => int => int);

                 Should *NOT* be formatted as:

                    let x: type a b. int => int => int = fun aVal bVal => bVal;

                 The helper function
                 [same_ast_modulo_varification_and_extensions] was created to
                 help compare the varified constraint pattern body, and the
                 non-varified expression constraint type.

                 The second requirement that we check before assuming that the
                 sugar form is correct, is to make sure the list of type vars
                 corresponds to a leading prefix of the Pexp_newtype variables.
              *)
              self#locallyAbstractPolymorphicFunctionBinding
                prefixText
                layoutPattern
                funWithNewTypes
                absVars
                nonVarifiedExprType
            | _ ->
              let typeLayout = source_map ~loc:ty.ptyp_loc (self#core_type ty) in
              let appTerms = self#unparseExprApplicationItems expr in
              self#formatSimplePatternBinding
                prefixText
                layoutPattern
                (Some typeLayout)
                appTerms
        )
      | _ ->
        let layoutPattern =
          source_map ~loc:pat.ppat_loc (self#pattern pat)
        in
        let appTerms = self#unparseExprApplicationItems expr in
        self#formatSimplePatternBinding prefixText layoutPattern None appTerms
    in
    let {stdAttrs; docAttrs} = partitionAttributes ~partDoc:true attrs in

    let body = makeList ~inline:(true, true) [body] in
    let layout = self#attach_std_item_attrs stdAttrs (source_map ~loc:loc body) in
    self#attachDocAttrsToLayout
      ~stdAttrs
      ~docAttrs
      ~loc:pat.ppat_loc
      ~layout
      ()

  (* Ensures that the constraint is formatted properly for sake of function
     binding (formatted without arrows)
     let x y z : no_unguarded_arrows_allowed_here => ret;
   *)
  method normalizeFunctionArgsConstraint argsList return =
    match return.pexp_desc with
      | Pexp_constraint (e, ct) ->
        let typeLayout =
          source_map ~loc:ct.ptyp_loc
            (self#non_arrowed_non_simple_core_type ct)
        in
        ([makeList
           ~break:IfNeed
           ~inline:(true, true)
           (argsList@[formatJustTheTypeConstraint typeLayout])], e)
      | _ -> (argsList, return)

  method normalizeConstructorArgsConstraint argsList return =
    match return.pcl_desc with
      | Pcl_constraint (e, ct) when return.pcl_attributes == [] ->
        let typeLayout =
          source_map ~loc:ct.pcty_loc
            (self#non_arrowed_class_constructor_type ct)
        in
        (argsList@[formatJustTheTypeConstraint typeLayout], e)
      | _ -> (argsList, return)

  method bindingsLocationRange ?extension l =
    let len = List.length l in
    let fstLoc = match extension with
    | Some ({pexp_loc = {loc_ghost = false}} as ext) -> ext.pexp_loc
    | _ -> (List.nth l 0).pvb_loc
    in
    let lstLoc = (List.nth l (len - 1)).pvb_loc in
    {
      loc_start = fstLoc.loc_start;
      loc_end = lstLoc.loc_end;
      loc_ghost = false
    }

  method bindingOpsLocationRange { let_; ands; _ } =
    let fstLoc = let_.pbop_loc in
    let lstLoc = match ands with
    | [] -> fstLoc
    | xs ->
      let len = List.length xs in
      (List.nth xs (len - 1)).pbop_loc in
    {
      loc_start = fstLoc.loc_start;
      loc_end = lstLoc.loc_end;
      loc_ghost = false
    }

  method bindings ?extension (rf, l) =
    let label = add_extension_sugar "let" extension in
    let label = match rf with
      | Nonrecursive -> label
      | Recursive -> label ^ " rec"
    in
    match l with
    | [x] -> self#value_binding label x
    | l ->
      let items = List.mapi (fun i x ->
        let loc = extractLocValBinding x in
        let layout = self#value_binding (if i == 0 then label else "and") x in
        (loc, layout)
      ) l
      in
      let itemsLayout = groupAndPrint
        ~xf:(fun (_, layout) -> layout)
        ~getLoc:(fun (loc, _) -> loc)
        ~comments:self#comments
        items
      in
      makeList
        ~postSpace:true
        ~break:Always
        ~indent:0
        ~inline:(true, true)
        itemsLayout

  method letop_bindings { let_; ands } =
    let label = compress_letop_identifier (let_.pbop_op.txt) in
    let let_item = self#binding_op label let_ in
    match ands with
    | [] -> let_item
    | l ->
      let and_items = List.map (fun x ->
        let loc = extractLocBindingOp x in
        let layout = self#binding_op (compress_letop_identifier x.pbop_op.txt) x in
        (loc, layout)
      ) l
      in
      let itemsLayout = groupAndPrint
        ~xf:(fun (_, layout) -> layout)
        ~getLoc:(fun (loc, _) -> loc)
        ~comments:self#comments
        ((extractLocBindingOp let_, let_item) :: and_items)
      in
      makeList
        ~postSpace:true
        ~break:Always
        ~indent:0
        ~inline:(true, true)
        itemsLayout

  method letList expr =
    (* Recursively transform a nested ast of "let-items", into a flat
     * list containing the location indicating start/end of the "let-item" and
     * its layout. *)
    let rec processLetList acc expr =
      let {stdAttrs; arityAttrs; jsxAttrs} =
        partitionAttributes ~allowUncurry:false expr.pexp_attributes
      in
      match (stdAttrs, expr.pexp_desc) with
        | ([], Pexp_let (rf, l, e)) ->
          (* For "letList" bindings, the start/end isn't as simple as with
           * module value bindings. For "let lists", the sequences were formed
           * within braces {}. The parser relocates the first let binding to the
           * first brace. *)
           let bindingsLayout = self#bindings (rf, l) in
           let bindingsLoc = self#bindingsLocationRange l in
           let layout = source_map ~loc:bindingsLoc bindingsLayout in
           processLetList ((bindingsLoc, layout)::acc) e
        | ([], Pexp_letop ( {body; _} as op)) ->
          (* For "letList" bindings, the start/end isn't as simple as with
           * module value bindings. For "let lists", the sequences were formed
           * within braces {}. The parser relocates the first let binding to the
           * first brace. *)
           let bindingsLayout = self#letop_bindings op in
           let bindingsLoc = self#bindingOpsLocationRange op in
           let layout = source_map ~loc:bindingsLoc bindingsLayout in
           processLetList ((bindingsLoc, layout)::acc) body
        | (attrs, Pexp_open (me, e))
            (* Add this when check to make sure these are handled as regular "simple expressions" *)
            when not (self#isSeriesOfOpensFollowedByNonSequencyExpression {expr with pexp_attributes = []}) ->
          let overrideStr = match me.popen_override with | Override -> "!" | Fresh -> "" in
          let openLayout = label ~space:true
            (atom ("open" ^ overrideStr))
            (self#moduleExpressionToFormattedApplicationItems me.popen_expr)
          in
          let attrsOnOpen =
            makeList ~inline:(true, true) ~postSpace:true ~break:Always
            ((self#attributes attrs)@[openLayout])
          in
          (* Just like the bindings, have to synthesize a location since the
           * Pexp location is parsed (potentially) beginning with the open
           * brace {} in the let sequence. *)
          let layout = source_map ~loc:me.popen_loc attrsOnOpen in
          let loc = {
            me.popen_loc with
            loc_start = {
              me.popen_loc.loc_start with
              pos_lnum = expr.pexp_loc.loc_start.pos_lnum
            }
          } in
          processLetList ((loc, layout)::acc) e
        | ([], Pexp_letmodule (s, me, e)) ->
            let prefixText = "module" in
            let bindingName = atom ~loc:s.loc s.txt in
            let moduleExpr = me in
            let letModuleLayout =
              (self#let_module_binding prefixText bindingName moduleExpr) in
            let letModuleLoc = {
              loc_start = s.loc.loc_start;
              loc_end = me.pmod_loc.loc_end;
              loc_ghost = false
            } in
            (* Just like the bindings, have to synthesize a location since the
             * Pexp location is parsed (potentially) beginning with the open
             * brace {} in the let sequence. *)
          let layout = source_map ~loc:letModuleLoc letModuleLayout in
          let (_, return) = self#curriedFunctorPatternsAndReturnStruct moduleExpr in
            let loc = {
              letModuleLoc with
              loc_end = return.pmod_loc.loc_end
            } in
           processLetList ((loc, layout)::acc) e
        | ([], Pexp_letexception (extensionConstructor, expr)) ->
            let exc = self#exception_declaration extensionConstructor in
            let layout = source_map ~loc:extensionConstructor.pext_loc exc in
            processLetList ((extensionConstructor.pext_loc, layout)::acc) expr
        | ([], Pexp_sequence (({pexp_desc=Pexp_sequence _ }) as e1, e2))
        | ([], Pexp_sequence (({pexp_desc=Pexp_let _      }) as e1, e2))
        | ([], Pexp_sequence (({pexp_desc=Pexp_open _     }) as e1, e2))
        | ([], Pexp_sequence (({pexp_desc=Pexp_letmodule _}) as e1, e2))
        | ([], Pexp_sequence (e1, e2)) ->
            let e1Layout = match expression_not_immediate_extension_sugar e1 with
              | Some (extension, e) ->
                    self#attach_std_item_attrs ~extension []
                      (self#unparseExpr e)
              | None ->
                  self#unparseExpr e1
            in
            let loc = e1.pexp_loc in
            let layout = source_map ~loc e1Layout in
            processLetList ((loc, layout)::acc) e2
        | _ ->
          let expr = { expr with pexp_attributes = (arityAttrs @ stdAttrs @ jsxAttrs) }
          in
          match expression_not_immediate_extension_sugar expr with
          | Some (extension, {pexp_attributes = []; pexp_desc = Pexp_let (rf, l, e)}) ->
            let bindingsLayout = self#bindings ~extension (rf, l) in
            let bindingsLoc = self#bindingsLocationRange ~extension:expr l in
            let layout = source_map ~loc:bindingsLoc bindingsLayout in
            processLetList ((extractLocationFromValBindList expr l, layout)::acc) e
          | Some (extension, e) ->
            let layout = self#attach_std_item_attrs ~extension [] (self#unparseExpr e) in
            (expr.pexp_loc, layout)::acc
          | None ->
            (* Should really do something to prevent infinite loops here. Never
               allowing a top level call into letList to recurse back to
               self#unparseExpr- top level calls into letList *must* be one of the
               special forms above whereas lower level recursive calls may be of
               any form. *)
            let layout = source_map ~loc:expr.pexp_loc (self#unparseExpr expr) in
            (expr.pexp_loc, layout)::acc
    in
    let es = processLetList [] expr in
    (* Interleave whitespace between the "let-items" when appropriate *)
    groupAndPrint
      ~xf:(fun (_, layout) -> layout)
      ~getLoc:(fun (loc, _) -> loc)
      ~comments:self#comments
      (List.rev es)

  method constructor_expression ?(polyVariant=false) ~arityIsClear stdAttrs ctor eo =
    let (implicit_arity, arguments) =
      match eo.pexp_desc with
      | Pexp_construct ( {txt= Lident "()"},_) ->
        (* `foo() is a polymorphic variant that contains a single unit construct as expression
         * This requires special formatting: `foo(()) -> `foo() *)
        (false, atom "()")
      (* special printing: MyConstructor(()) -> MyConstructor() *)
      | Pexp_tuple l when is_single_unit_construct l ->
          (false, atom "()")
      | Pexp_tuple l when polyVariant == true ->
          (false, self#unparseSequence ~wrap:("(", ")") ~construct:`Tuple l)
      | Pexp_tuple l ->
        (* There is no ambiguity when the number of tuple components is 1.
             We don't need put implicit_arity in that case *)
        (match l with
        | exprList when isSingleArgParenApplication exprList ->
            (false, self#singleArgParenApplication exprList)
        | _ ->
            (not arityIsClear, makeTup (List.map self#unparseProtectedExpr l)))
      | _ when isSingleArgParenApplication [eo] ->
          (false, self#singleArgParenApplication [eo])
      | _ ->
          (false, makeTup [self#unparseProtectedExpr eo])
    in
    let arguments = source_map ~loc:eo.pexp_loc arguments in
    let construction =
      label ctor (if isSequencey arguments
                  then arguments
                  else (ensureSingleTokenSticksToLabel arguments))
    in
    let attrs =
      if implicit_arity && (not polyVariant) then
        { attr_name = { txt="implicit_arity"; loc=eo.pexp_loc }
        ; attr_payload = PStr []
        ; attr_loc = eo.pexp_loc
        } :: stdAttrs
      else
        stdAttrs
    in
    match attrs with
      | [] -> construction
      | _::_ -> formatAttributed construction (self#attributes attrs)

  (* TODOATTRIBUTES: Handle stdAttrs here (merge with implicit_arity) *)
  method constructor_pattern ?(polyVariant=false) ~arityIsClear ctor po =
    let (implicit_arity, arguments) =
      match po.ppat_desc with
      (* There is no ambiguity when the number of tuple components is 1.
           We don't need put implicit_arity in that case *)
      | Ppat_tuple (([] | _::[]) as l) ->
        (false, l)
      | Ppat_tuple l ->
        (not arityIsClear, l)

      | _ -> (false, [po])
    in
    let space, arguments = match arguments with
      | [x] when is_direct_pattern x -> (true, self#simple_pattern x)
      | xs when isSingleArgParenPattern xs -> (false, self#singleArgParenPattern xs)
      (* Optimize the case when it's a variant holding a shot variable - avoid trailing*)
      | [{ppat_desc=Ppat_constant (Pconst_string (s, None))} as x]
      | [{ppat_desc=Ppat_construct (({txt=Lident s}), None)} as x]
      | [{ppat_desc=Ppat_var ({txt = s})} as x]
        when Reason_heuristics.singleTokenPatternOmmitTrail s ->
        let layout = makeTup ~trailComma:false [self#pattern x] in
        (false, source_map ~loc:po.ppat_loc layout)
      | [{ppat_desc=Ppat_any} as x]
      | [{ppat_desc=Ppat_constant (Pconst_char _)} as x]
      | [{ppat_desc=Ppat_constant (Pconst_integer _)} as x] ->
        let layout = makeTup ~trailComma:false [self#pattern x] in
        (false, source_map ~loc:po.ppat_loc layout)
      | xs ->
        let layout = makeTup (List.map self#pattern xs) in
        (false, source_map ~loc:po.ppat_loc layout)
    in
    let construction = label ~space ctor arguments in
    if implicit_arity && (not polyVariant) then
      formatAttributed construction
        (self#attributes
          [ { attr_name = { txt="implicit_arity"; loc=po.ppat_loc }
            ; attr_payload = PStr []
            ; attr_loc = po.ppat_loc }])
    else
      construction

  (*
   * Provides special printing for constructor arguments:
   * iff there's one argument & they have some kind of wrapping,
   * they're wrapping need to 'hug' the surrounding parens.
   * Example:
   *  switch x {
   *  | Some({
   *      a,
   *      b,
   *    }) => ()
   *  }
   *
   *  Notice how ({ and }) hug.
   *  This applies for records, arrays, tuples & lists.
   *  Also see `isSingleArgParenPattern` to determine if this kind of wrapping applies.
   *)
  method singleArgParenPattern = function
    | [{ppat_desc = Ppat_record (l, closed); ppat_loc = loc}] ->
      source_map ~loc (self#patternRecord ~wrap:("(", ")") l closed)
    | [{ppat_desc = Ppat_array l; ppat_loc = loc}] ->
      source_map ~loc (self#patternArray ~wrap:("(", ")") l)
    | [{ppat_desc = Ppat_tuple l; ppat_loc = loc}] ->
      source_map ~loc (self#patternTuple ~wrap:("(", ")") l)
    | [{ppat_desc = Ppat_construct (({txt=Lident "::"}), _); ppat_loc} as listPattern]  ->
      source_map ~loc:ppat_loc (self#patternList ~wrap:("(", ")")  listPattern)
    | _ -> assert false

  (* TODO: Similar to tuples, do not print parens around type constraints (same for lists) *)
  method patternArray ?(wrap=("","")) l =
    let (left, right) = wrap in
    let wrap = (left ^ "[|", "|]" ^ right) in
    makeList ~wrap ~break:IfNeed ~postSpace:true ~sep:commaTrail (List.map self#pattern l)

  method patternTuple ?(wrap=("","")) l =
    let (left, right) = wrap in
    let wrap = (left ^ "(", ")" ^ right) in
    makeList ~wrap ~sep:commaTrail ~postSpace:true ~break:IfNeed (List.map self#pattern l)

  method patternRecord ?(wrap=("","")) l closed =
    let longident_x_pattern (li, p) =
      match (li, p.ppat_desc) with
        | ({txt = ident}, Ppat_var {txt}) when Longident.last ident = txt ->
          (* record field punning when destructuring. {x: x, y: y} becomes {x, y} *)
          (* works with module prefix too: {MyModule.x: x, y: y} becomes {MyModule.x, y} *)
            self#longident_loc li
        | ({txt = ident},
           Ppat_alias ({ppat_desc = (Ppat_var {txt = ident2}) }, {txt = aliasIdent}))
           when Longident.last ident = ident2 ->
          (* record field punning when destructuring with renaming. {state: state as prevState} becomes {state as prevState *)
          (* works with module prefix too: {ReasonReact.state: state as prevState} becomes {ReasonReact.state as prevState *)
            makeList ~sep:(Sep " ") [self#longident_loc li; atom "as"; atom aliasIdent]
        | _ ->
            label ~space:true (makeList [self#longident_loc li; atom ":"]) (self#pattern p)
    in
    let rows = (List.map longident_x_pattern l)@(
      match closed with
        | Closed -> []
        | _ -> [atom "_"]
    ) in
    let (left, right) = wrap in
    let wrap = (left ^ "{", "}" ^ right) in
    makeList
      ~wrap
      ~break:IfNeed
      ~sep:commaTrail
      ~postSpace:true
      rows

  method patternFunction ?extension loc l =
    let estimatedFunLocation = {
        loc_start = loc.loc_start;
        loc_end = {loc.loc_start with pos_cnum = loc.loc_start.Lexing.pos_cnum + 3};
        loc_ghost = false;
    } in
    makeList
      ~postSpace:true
      ~break:IfNeed
      ~inline:(true, true)
      ~pad:(false, false)
      ((atom ~loc:estimatedFunLocation (add_extension_sugar funToken extension)) :: (self#case_list l))

  method parenthesized_expr ?break expr =
    let result = self#unparseExpr expr in
    match expr.pexp_attributes, expr.pexp_desc with
    | [], (Pexp_tuple _ | Pexp_construct ({txt=Lident "()"}, None)) -> result
    | _ -> makeList ~wrap:("(",")") ?break [self#unparseExpr expr]

  (* Expressions requiring parens, in most contexts such as separated by infix *)
  method expression_requiring_parens_in_infix x =
    let {stdAttrs} = partitionAttributes x.pexp_attributes in
    assert (stdAttrs == []);
    (* keep the incoming expression around, an expr with
     * immediate extension sugar might contain less than perfect location
     * info in its children (used for comment interleaving), the expression passed to
     * 'expression_requiring_parens_in_infix' contains the correct location *)
    let originalExpr = x in
    let extension, x = expression_immediate_extension_sugar x in
    match x.pexp_desc with
      (* The only reason Pexp_fun must also be wrapped in parens when under
         pipe, is that its => token will be confused with the match token.
         Simple expression will also invoke `#reset`. *)
      | Pexp_function _ when pipe || semi -> None (* Would be rendered as simplest_expression  *)
      (* Pexp_function, on the other hand, doesn't need wrapping in parens in
         most cases anymore, since `fun` is not ambiguous anymore (we print Pexp_fun
         as ES6 functions). *)
      | Pexp_function l ->
        let prec = Custom funToken in
        let expr = self#patternFunction ?extension x.pexp_loc l in
        Some (SpecificInfixPrecedence
                ({reducePrecedence=prec; shiftPrecedence=prec}, LayoutNode expr))
      | _ ->
        (* The Pexp_function cases above don't use location because comment printing
          breaks for them. *)
        let itm = match x.pexp_desc with
          | Pexp_fun _
          | Pexp_newtype _ ->
            (* let uncurried =  *)
            let (args, ret) = self#curriedPatternsAndReturnVal x in
            (match args with
              | [] -> raise (NotPossible ("no arrow args in unparse "))
              | firstArg::tl ->
                (* Suboptimal printing of parens:

                      something >>= fun x => x + 1;

                   Will be printed as:

                      something >>= (fun x => x + 1);

                   Because the arrow has lower precedence than >>=, but it wasn't
                   needed because

                      (something >>= fun x) => x + 1;

                   Is not a valid parse. Parens around the `=>` weren't needed to
                   prevent reducing instead of shifting. To optimize this part, we need
                   a much deeper encoding of the parse rules to print parens only when
                   needed, testing which rules will be reduced. It really should be
                   integrated deeply with Menhir.

                   One question is, if it's this difficult to describe when parens are
                   needed, should we even print them with the minimum amount?  We can
                   instead model everything as "infix" with ranked precedences.  *)
                let retValUnparsed = self#unparseExprApplicationItems ret in
                Some (self#wrapCurriedFunctionBinding
                        ~sweet:(extension = None)
                        (add_extension_sugar funToken extension)
                        ~arrow:"=>" firstArg tl retValUnparsed)
            )
          | Pexp_try (e, l) ->
            let estimatedBracePoint = {
              loc_start = e.pexp_loc.loc_end;
              loc_end = x.pexp_loc.loc_end;
              loc_ghost = false;
            }
            in
            let cases = (self#case_list ~allowUnguardedSequenceBodies:true l) in
            let switchWith = self#dont_preserve_braces#formatSingleArgLabelApplication
              (atom (add_extension_sugar "try" extension))
              e
            in
            Some (
              label
                ~space:true
                switchWith
                (source_map ~loc:estimatedBracePoint
                   (makeList ~indent:settings.trySwitchIndent ~wrap:("{", "}")
                      ~break:Always_rec ~postSpace:true cases))
            )
          (* These should have already been handled and we should never havgotten this far. *)
          | Pexp_setinstvar _ -> raise (Invalid_argument "Cannot handle setinstvar here - call unparseExpr")
          | Pexp_setfield (_, _, _) -> raise (Invalid_argument "Cannot handle setfield here - call unparseExpr")
          | Pexp_apply _ -> raise (Invalid_argument "Cannot handle apply here - call unparseExpr")
          | Pexp_match (e, l) ->
             let estimatedBracePoint = {
               loc_start = e.pexp_loc.loc_end;
               (* See originalExpr binding, for more info.
                * It contains the correct location under immediate extension sugar *)
               loc_end = originalExpr.pexp_loc.loc_end;
               loc_ghost = false;
             }
             in
             let cases = (self#case_list ~allowUnguardedSequenceBodies:true l) in
             let switchWith =
               label ~space:true (atom (add_extension_sugar "switch" extension))
                 (self#parenthesized_expr ~break:IfNeed e)
             in
             let lbl =
               label
                 ~space:true
                 switchWith
                 (source_map ~loc:estimatedBracePoint
                    (makeList ~indent:settings.trySwitchIndent ~wrap:("{", "}")
                       ~break:Always_rec ~postSpace:true cases))
             in
             Some lbl
          | Pexp_ifthenelse (e1, e2, eo) ->
            let (blocks, finalExpression) = sequentialIfBlocks eo in
            let rec singleExpression exp =
              match exp.pexp_desc with
              | Pexp_ident _ -> true
              | Pexp_constant _ -> true
              | Pexp_construct (_, arg) ->
                (match arg with
                | None -> true
                | Some x -> singleExpression x)
              | _ -> false
            in
            let singleLineIf =
              (singleExpression e1) &&
              (singleExpression e2) &&
              (match eo with
               | Some expr -> singleExpression expr
               | None -> true
              )
            in
            let makeLetSequence =
              if singleLineIf then
                makeLetSequenceSingleLine
              else
                makeLetSequence
            in
            let rec sequence soFar remaining = (
              match (remaining, finalExpression) with
                | ([], None) -> soFar
                | ([], Some e) ->
                  let soFarWithElseAppended = makeList ~postSpace:true [soFar; atom "else"] in
                  label ~space:true soFarWithElseAppended
                    (source_map ~loc:e.pexp_loc (makeLetSequence (self#letList e)))
                | (hd::tl, _) ->
                  let (e1, e2) = hd in
                  let soFarWithElseIfAppended =
                    label
                      ~space:true
                      (makeList ~postSpace:true [soFar; atom "else if"])
                      (makeList ~wrap:("(",")") [self#unparseExpr e1])
                  in
                  let nextSoFar =
                    label ~space:true soFarWithElseIfAppended
                      (source_map ~loc:e2.pexp_loc (makeLetSequence (self#letList e2)))
                  in
                  sequence nextSoFar tl
            ) in
            let init =
              let if_ = atom (add_extension_sugar "if" extension) in
              let cond = self#parenthesized_expr e1 in
              label ~space:true
                (source_map ~loc:e1.pexp_loc (label ~space:true if_ cond))
                (source_map ~loc:e2.pexp_loc (makeLetSequence (self#letList e2)))
            in
            Some (sequence init blocks)
          | Pexp_while (e1, e2) ->
            let lbl =
              let while_ = atom (add_extension_sugar "while" extension) in
              let cond = self#parenthesized_expr e1 in
              label ~space:true
                (label ~space:true while_ cond)
                (source_map ~loc:e2.pexp_loc (makeLetSequence (self#letList e2)))
            in
            Some lbl
          | Pexp_for (s, e1, e2, df, e3) ->
            (*
             *  for longIdentifier in
             *      (longInit expr) to
             *      (longEnd expr) {
             *    print_int longIdentifier;
             *  };
             *)
            let identifierIn = (makeList ~postSpace:true [self#pattern s; atom "in";]) in
            let dockedToFor = makeList
                ~break:IfNeed
                ~postSpace:true
                ~inline:(true, true)
                ~wrap:("(",")")
                [
                  identifierIn;
                  makeList ~postSpace:true [self#unparseExpr e1; self#direction_flag df];
                  (self#unparseExpr e2);
                ]
            in
            let upToBody = makeList ~inline:(true, true) ~postSpace:true
                [atom (add_extension_sugar "for" extension); dockedToFor]
            in
            Some (label ~space:true upToBody
                    (source_map ~loc:e3.pexp_loc (makeLetSequence (self#letList e3))))
          | Pexp_new li ->
            Some (label ~space:true (atom "new") (self#longident_class_or_type_loc li))
          | Pexp_assert e ->
            Some (
              label
                (atom "assert")
                (makeTup [(self#unparseExpr e)]);
            )
          | Pexp_lazy e ->
            Some (self#formatSingleArgLabelApplication (atom "lazy") e)
          | Pexp_poly _ ->
            failwith (
              "This version of the pretty printer assumes it is impossible to " ^
              "construct a Pexp_poly outside of a method definition - yet it sees one."
            )
          | _ -> None
        in
        match itm with
          | None -> None
          | Some i -> Some (PotentiallyLowPrecedence (source_map ~loc:x.pexp_loc i))

  method potentiallyConstrainedExpr x =
    match x.pexp_desc with
      | Pexp_constraint (e, ct) ->
          formatTypeConstraint (self#unparseExpr e) (self#core_type ct)
      | _ -> self#unparseExpr x


  (*
   * Because the rule BANG simple_expr was given %prec below_DOT_AND_SHARP,
   * !x.y.z will parse as !(x.y.z) and not (!x).y.z.
   *
   *     !x.y.z == !((x.y).z)
   *     !x#y#z == !((x#y)#z)
   *
   * So the intuition is: In general, any simple expression can exist to the
   * left of a `.`, except `BANG simple_expr`, which has special precedence,
   * and must be guarded in this one case.
   *
   * TODO: Instead of special casing this here, we should continue to extend
   * unparseExpr to also unparse simple expressions, (by encoding the
   * rules precedence below_DOT_AND_SHARP).
   *
   * TODO:
   *  Some would even have the prefix application be parsed with lower
   *  precedence function *application*. In the case of !, where ! means not,
   *  it makes a lot of sense because (!identifier)(arg) would be meaningless.
   *
   *  !callTheFunction(1, 2, 3)(andEvenCurriedArgs)
   *
   * Only problem is that it could then not appear anywhere simple expressions
   * would appear.
   *
   * We could make a special case for ! followed by one simple expression, and
   * consider the result simple.
   *
   * Alternatively, we can figure out a way to not require simple expressions
   * in the most common locations such as if/while tests. This is really hard
   * (impossible w/ grammars Menhir supports?)
   *
   * if ! myFunc argOne argTwo {
   *
   * } else {
   *
   * };
   *
   *)
  method simple_enough_to_be_lhs_dot_send x =
    match x.pexp_desc with
    | (Pexp_apply (eFun, _)) -> (
        match printedStringAndFixityExpr eFun with
        | AlmostSimplePrefix _
        | UnaryPlusPrefix _
        | UnaryMinusPrefix _
        | UnaryNotPrefix _
        | UnaryPostfix _
        | Infix _ -> self#simplifyUnparseExpr x
        | Letop _ | Andop _
        | Normal ->
          if x.pexp_attributes == [] then
            (* `let a = foo().bar` instead of `let a = (foo()).bar *)
            (* same for foo()##bar, foo()#=bar, etc. *)
            self#unparseExpr x
          else
            self#simplifyUnparseExpr x
      )
    | _ -> self#simplifyUnparseExpr x

  method unparseRecord
    ?wrap:((lwrap, rwrap)=("", ""))
    ?withStringKeys:(withStringKeys=false)
    ?allowPunning:(allowPunning=true)
    ?forceBreak:(forceBreak=false)
    l eo =
    (* forceBreak is a ref which can be set to always break the record rows.
     * Example, when we have a row which contains a nested record,
     * this ref can be set to true from inside the printing of that row,
     * which forces breaks for the outer record structure. *)
    let forceBreak = ref forceBreak in
    let quote = (atom "\"") in
    let maybeQuoteFirstElem fst rest =
        if withStringKeys then (match fst.txt with
          | Lident s -> quote::(atom s)::quote::rest
          | Ldot _  | Lapply _ -> assert false
          )
        else
          (self#longident_loc fst)::rest
    in
    let makeRow (li, e) shouldPun =
      let totalRowLoc = {
        loc_start = li.Asttypes.loc.loc_start;
        loc_end = e.pexp_loc.loc_end;
        loc_ghost = false;
      } in
      let theRow = match (e.pexp_desc, shouldPun, allowPunning) with
        (* record value punning. Turns {foo: foo, bar: 1} into {foo, bar: 1} *)
        (* also turns {Foo.bar: bar, baz: 1} into {Foo.bar, baz: 1} *)
        (* don't turn {bar: Foo.bar, baz: 1} into {bar, baz: 1}, naturally *)
        | (Pexp_ident {txt = Lident value}, true, true) when Longident.last li.txt = value ->
          makeList (maybeQuoteFirstElem li [])

          (* Force breaks for nested records or bs obj sugar
           * Example:
           *  let person = {name: {first: "Bob", last: "Zhmith"}, age: 32};
           * is a lot less readable than
           *  let person = {
           *   "name": {
           *     "first": "Bob",
           *     "last": "Zhmith"
           *   },
           *  "age": 32
           *  };
           *)
        | (Pexp_record (recordRows, optionalGadt), _, _) ->
            forceBreak := true;
            let keyWithColon = makeList (maybeQuoteFirstElem li [atom ":"]) in
            let value = self#unparseRecord ~forceBreak: true recordRows optionalGadt in
            label ~space:true keyWithColon value
        | (Pexp_extension (s, p), _, _) when s.txt = "bs.obj" ->
            forceBreak := true;
            let keyWithColon = makeList (maybeQuoteFirstElem li [atom ":"]) in
            let value = self#formatBsObjExtensionSugar ~forceBreak:true p in
            label ~space:true keyWithColon value
        | (Pexp_object classStructure, _, _) ->
            forceBreak := true;
            let keyWithColon = makeList (maybeQuoteFirstElem li [atom ":"]) in
            let value = self#classStructure ~forceBreak:true classStructure in
            label ~space:true keyWithColon value
        | _ ->
          let (argsList, return) = self#curriedPatternsAndReturnVal e in
          match argsList with
          | [] ->
            let appTerms = self#unparseExprApplicationItems e in
            let upToColon = makeList (maybeQuoteFirstElem li [atom ":"]) in
            formatAttachmentApplication applicationFinalWrapping (Some (true, upToColon)) appTerms
          | firstArg :: tl ->
            let upToColon = makeList (maybeQuoteFirstElem li [atom ":"]) in
            let returnedAppTerms = self#unparseExprApplicationItems return in
            self#wrapCurriedFunctionBinding
              ~sweet:true ~attachTo:upToColon funToken ~arrow:"=>"
              firstArg tl returnedAppTerms
      in (source_map ~loc:totalRowLoc theRow, totalRowLoc)
    in
    let rec getRows l =
      match l with
        | [] -> []
        | hd::[] -> [makeRow hd true]
        | hd::hd2::tl -> (makeRow hd true)::(getRows (hd2::tl))
    in

    let allRows = match eo with
      | None -> (
        match l with
          (* No punning (or comma) for records with only a single field. It's ambiguous with an expression in a scope *)
          (* See comment in parser.mly for lbl_expr_list_with_at_least_one_non_punned_field *)
          | [hd] -> [makeRow hd false]
          | _ -> getRows l
        )
      (* This case represents a "spread" being present -> {...x, a: 1, b: 2} *)
      | Some withRecord ->
        let firstRow =
          let row = (
            (* Unclear why "sugar_expr" was special cased hre. *)
            let appTerms = self#unparseExprApplicationItems withRecord in
            formatAttachmentApplication applicationFinalWrapping (Some (false, (atom "..."))) appTerms
          )
          in (
            source_map ~loc:withRecord.pexp_loc row,
            withRecord.pexp_loc
          )
        in
        firstRow::(getRows l)
    in
    makeList
      ~wrap:(lwrap ^ "{" ,"}" ^ rwrap)
      ~break:(if !forceBreak then Layout.Always else Layout.IfNeed)
      ~sep:commaTrail
      ~postSpace:true
      (groupAndPrint ~xf:fst ~getLoc:snd ~comments:self#comments allRows)

  method isSeriesOfOpensFollowedByNonSequencyExpression expr =
    match (expr.pexp_attributes, expr.pexp_desc) with
        | ([], Pexp_let _) -> false
        | ([], Pexp_letop _) -> false
        | ([], Pexp_sequence _) -> false
        | ([], Pexp_letmodule _) -> false
        | ([], Pexp_open (me, e)) ->
          me.popen_override == Fresh && self#isSeriesOfOpensFollowedByNonSequencyExpression e
        | ([], Pexp_letexception _) -> false
        | ([], Pexp_extension ({txt}, _)) -> txt = "bs.obj"
        | _ -> true

  method unparseObject ?wrap:((lwrap,rwrap)=("", "")) ?(withStringKeys=false) l o =
    let core_field_type = fun { pof_desc; pof_attributes; _ } -> match pof_desc with
      | Otag ({txt}, ct) ->
        let l = extractStdAttrs pof_attributes in
        let row =
          let rowKey = if withStringKeys then
              (makeList ~wrap:("\"", "\"") [atom txt])
            else (atom txt)
          in
          label ~space:true
            (makeList ~break:Layout.Never [rowKey; (atom ":")])
            (self#core_type ct)
        in
        (match l with
         | [] -> row
         | _::_ ->
           makeList
             ~postSpace:true
             ~break:IfNeed
             ~inline:(true, true)
             (List.concat [self#attributes pof_attributes; [row]]))
      | Oinherit ct -> makeList ~break:Layout.Never [atom "..."; self#core_type ct]
    in
    let rows = List.map core_field_type l in
    let openness = match o with
      | Closed -> atom "."
      | Open -> atom ".."
    in
    (* if an object has more than 2 rows, always break for readability *)
    let rows_layout = makeList
        ~inline:(true, true) ~postSpace:true ~sep:commaTrail rows
        ~break:(if List.length rows >= 2
                then Layout.Always_rec
                else Layout.IfNeed)
    in
    makeList
      ~break:Layout.IfNeed
      ~preSpace:(rows != [])
      ~wrap:(lwrap ^ "{", "}" ^ rwrap)
      (openness::[rows_layout])

  method unparseSequence ?wrap:(wrap=("", "")) ~construct l =
    match construct with
    | `ES6List ->
      let seq, ext = (match List.rev l with
        | ext :: seq_rev -> (List.rev seq_rev, ext)
        | [] -> assert false) in
      makeES6List ~wrap (List.map self#unparseExpr seq) (self#unparseExpr ext)
    | _ ->
      let (left, right) = wrap in
      let (xf, (leftDelim, rightDelim)) = (match construct with
        | `List -> (self#unparseExpr, ("[", "]"))
        | `Array -> (self#unparseExpr, ("[|", "|]"))
        | `Tuple -> (self#potentiallyConstrainedExpr, ("(", ")"))
        | `ES6List -> assert false)
      in
      let wrap = (left ^ leftDelim, rightDelim ^ right) in
      makeList
        ~wrap
        ~sep:commaTrail
        ~break:IfNeed
        ~postSpace:true
        (List.map xf l)


  method formatBsObjExtensionSugar ?wrap:(wrap=("", "")) ?(forceBreak=false) payload =
    match payload with
    | PStr [itm] -> (
      match itm with
      | {pstr_desc = Pstr_eval ({ pexp_desc = Pexp_record (l, eo) }, []) } ->
        self#unparseRecord ~forceBreak ~wrap ~withStringKeys:true ~allowPunning:false l eo
      | {pstr_desc = Pstr_eval ({ pexp_desc = Pexp_extension ({txt = "bs.obj"}, payload) }, []) } ->
        (* some folks write `[%bs.obj [%bs.obj {foo: bar}]]`. This looks improbable but
          it happens often if you use the sugared version: `[%bs.obj {"foo": bar}]`.
          We're gonna be lenient here and treat it as if they wanted to just write
          `{"foo": bar}`. BuckleScript does the same relaxation when parsing bs.obj
        *)
        self#formatBsObjExtensionSugar ~wrap ~forceBreak payload
      | _ -> raise (Invalid_argument "bs.obj only accepts a record. You've passed something else"))
    | _ -> assert false

  method should_preserve_requested_braces expr =
    let {stylisticAttrs} = partitionAttributes expr.pexp_attributes in
    match expr.pexp_desc with
    | Pexp_ifthenelse _
    | Pexp_try _ -> false
    | Pexp_sequence _ ->
      (* `let ... in` should _always_ preserve braces *)
      true
    | _ ->
        preserve_braces &&
        Reason_attributes.has_preserve_braces_attrs stylisticAttrs

  method simplest_expression x =
    let {stdAttrs; jsxAttrs; stylisticAttrs; arityAttrs} = partitionAttributes x.pexp_attributes in
    let hasJsxAttribute = jsxAttrs != [] in
    if stdAttrs != [] then
      None
    else if self#should_preserve_requested_braces x then
      let layout =
        makeList
          ~break:(if inline_braces then Always else Always_rec)
          ~inline:(true, inline_braces)
          ~wrap:("{", "}")
          ~postSpace:true
          ~sep:(if inline_braces then (Sep ";") else (SepFinal (";", ";")))
          (self#letList x)
      in
      Some layout
    else
      let item =
        match x.pexp_desc with
        (* The only reason Pexp_fun must also be wrapped in parens is that its =>
           token will be confused with the match token. *)
        | Pexp_fun _ when pipe || semi -> Some (self#reset#simplifyUnparseExpr x)
        | Pexp_function l when pipe || semi -> Some (formatPrecedence ~loc:x.pexp_loc (self#reset#patternFunction x.pexp_loc l))
        | Pexp_apply _ -> (
          match self#simple_get_application x with
          (* If it's the simple form of application. *)
          | Some simpleGet -> Some simpleGet
          | None -> None
        )
        | Pexp_object cs -> Some (self#classStructure cs)
        | Pexp_override l -> (* FIXME *)
          let string_x_expression (s, e) =
            label ~space:true (atom (s.txt ^ ":")) (self#unparseExpr e)
          in
          Some (
            makeList
              ~postSpace:true
              ~wrap:("{<", ">}")
              ~sep:(Sep ",")
              (List.map string_x_expression l)
          )
        | Pexp_construct ( {txt= Lident "[]"},_) when hasJsxAttribute -> Some (atom "<> </>")
        | Pexp_construct ( {txt= Lident"::"},Some _) when hasJsxAttribute ->
            (match self#formatJsxChildrenNonSpread x [] with
            | None ->
                (* Back out of the standard jsx child formatting *)
                (* This is actually not a useful construct to have written:
                 *   <> ... x </>
                 * Is the same as:
                 *   x
                 * There is also a bug in the parser where a space is needed
                 * between <> and ..., but no one would write the ... form of
                 * <> anyways. *)
                let withoutJsxAttributes = {x with pexp_attributes=(stylisticAttrs @ arityAttrs)} in
                self#simplest_expression withoutJsxAttributes
            | Some chldn ->
              Some (makeList
                ~break:IfNeed
                ~inline:(false, false)
                ~postSpace:true
                ~wrap:("<>", "</>")
                ~pad:(true, true)
                chldn))
        | Pexp_construct _  when is_simple_construct (view_expr x) ->
            Some (
              match view_expr x with
              | `nil -> atom "[]"
              | `tuple -> atom "()"
              | `list xs -> (* LIST EXPRESSION *)
                  self#unparseSequence ~construct:`List xs
              | `cons xs ->
                  self#unparseSequence ~construct:`ES6List xs
              | `simple x -> self#longident x
              | _ -> assert false
            )
        | Pexp_ident li ->
            (* Lone identifiers shouldn't break when to the right of a label *)
            Some (ensureSingleTokenSticksToLabel (self#longident_loc li))
        | Pexp_constant c ->
            (* Constants shouldn't break when to the right of a label *)
          let raw_literal, _ = extract_raw_literal x.pexp_attributes in
            Some (ensureSingleTokenSticksToLabel
                    (self#constant ?raw_literal c))
        | Pexp_pack me ->
          Some (
            makeList
              ~break:IfNeed
              ~postSpace:true
              ~wrap:("(", ")")
              ~inline:(true, true)
              [atom "module"; self#module_expr me;]
          )
        | Pexp_tuple l ->
            (* TODO: These may be simple, non-simple, or type constrained
               non-simple expressions *)
          Some (self#unparseSequence ~construct:`Tuple l)
        | Pexp_constraint (e, ct) ->
          Some (
            makeList
              ~break:IfNeed
              ~wrap:("(", ")")
              [formatTypeConstraint (self#unparseExpr e) (self#core_type ct)]
          )
        | Pexp_coerce (e, cto1, ct) ->
            let optFormattedType = match cto1 with
              | None -> None
              | Some typ -> Some (self#core_type typ) in
            Some (
              makeList
                ~break:IfNeed
                ~wrap:("(", ")")
                [formatCoerce (self#unparseExpr e) optFormattedType (self#core_type ct)]
            )
        | Pexp_variant (l, None) ->
            Some (ensureSingleTokenSticksToLabel (atom ("`" ^ l)))
        | Pexp_record (l, eo) -> Some (self#unparseRecord l eo)
        | Pexp_array l ->
          Some (self#unparseSequence ~construct:`Array l)
        | Pexp_let _ | Pexp_sequence _
        | Pexp_letmodule _ | Pexp_letexception _
        | Pexp_letop _ ->
          Some (makeLetSequence (self#letList x))
        | Pexp_extension e ->
          begin match expression_immediate_extension_sugar x with
            | (Some _, _) -> None
            | (None, _) ->
              match expression_extension_sugar x with
              | None -> Some (self#extension e)
              | Some (_, x') ->
                match x'.pexp_desc with
                | Pexp_let _ ->
                  Some (makeLetSequence (self#letList x))
                | _ -> Some (self#extension e)
          end
        | Pexp_open (me, e) ->
            if self#isSeriesOfOpensFollowedByNonSequencyExpression x then
              Some
                (label
                  (label
                    (self#moduleExpressionToFormattedApplicationItems me.popen_expr)
                    (atom (".")))
                  (self#formatNonSequencyExpression e))
          else
            Some (makeLetSequence (self#letList x))
        | Pexp_send (e, s) ->
          let needparens = match e.pexp_desc with
            | Pexp_apply (ee, _) ->
              (match printedStringAndFixityExpr ee with
               | UnaryPostfix "^" -> true
               | _ -> false)
            | _ -> false
          in
          let lhs = self#simple_enough_to_be_lhs_dot_send e in
          let lhs = if needparens then makeList ~wrap:("(",")") [lhs] else lhs in
          Some (label (makeList [lhs; atom "#";]) (atom s.txt))
        | _ -> None
      in
      match item with
      | None -> None
      | Some i -> Some (source_map ~loc:x.pexp_loc i)

  (* Renders jsx children. Returns None if it is not a valid JSX child
   * structure and must be rendered as spread.  You cannot render any list of
   * JSX children in Reason unless it is nil-terminated. Otherwise you must use
   * spread.  *)
  method formatJsxChildrenNonSpread expr processedRev =
    let formatJsxChild x =
      match x with
      | ({pexp_desc = Pexp_apply _} as e) ->
          (* Pipe first behaves differently according to the expression on the
           * right. In example (1) below, it's a `SpecificInfixPrecedence`; in
           * (2), however, it's `Simple` and doesn't need to be wrapped in parens.
           *
           * (1). <div> {items->Belt.Array.map(ReasonReact.string)->ReasonReact.array} </div>;
           * (2). <Foo> (title === "" ? [1, 2, 3] : blocks)->Foo.toString </Foo>; *)
          if Reason_heuristics.isPipeFirst e &&
             not (Reason_heuristics.isPipeFirstWithNonSimpleJSXChild e)
          then
            self#formatPipeFirst e
          else
            self#inline_braces#simplifyUnparseExpr ~inline:true ~wrap:("{", "}") e

      (* No braces - very simple *)
      | {pexp_desc = Pexp_ident li} -> self#longident_loc li
      | {pexp_desc = Pexp_constant constant} as x ->
          let raw_literal, _ = extract_raw_literal x.pexp_attributes in
          self#constant ?raw_literal constant
      | _ ->
          (* Currently spreading a list, or having a list as a child must be
           * wrapped in { }. You can remove the entire even_wrap_simple arg
           * when that is fixed (there is a conflict in grammar when allowing
           * a [] without {[]} as child. *)
          (* Simple child that has jsx: <hi> </hi> *)
          (* Simple child that doesn't have jsx: "hello" *)
          (* Simple child that doesn't have jsx but is a "::" and requires braces: [a, b] *)
          self#inline_braces#simplifyUnparseExpr ~inline:true ~wrap:("{", "}") x
    in
    match expr with
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} ->
       (match processedRev with
        | [] -> None
        | _::_ -> Some (List.rev processedRev))
    | {pexp_desc = Pexp_construct ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple [hd; tl]} )} ->
        self#formatJsxChildrenNonSpread tl (formatJsxChild hd :: processedRev)
    | _ -> None

  method direction_flag = function
    | Upto -> atom "to"
    | Downto -> atom "downto"

  method payload ppxToken ppxId e =
    let wrap = ("[" ^ ppxToken ^ ppxId.txt, "]") in
    let wrap_prefix str (x,y) = (x^str, y) in
    let break = Layout.IfNeed in
    let pad = (true, false) in
    let postSpace = true in
    match e with
    | PStr [] -> atom ("[" ^ ppxToken  ^ ppxId.txt  ^ "]")
    | PStr [itm] -> makeList ~break ~wrap ~pad [self#structure_item itm]
    | PStr (_::_ as items) ->
      let rows = List.map self#structure_item items in
      makeList ~wrap ~break ~pad ~postSpace ~sep:(Layout.Sep ";") rows
    | PTyp x ->
      let wrap = wrap_prefix ":" wrap in
      makeList ~wrap ~break ~pad [self#core_type x]
    (* Signatures in attributes were added recently *)
    | PSig [] -> atom ("[" ^ ppxToken ^ ppxId.txt ^":]")
    | PSig [x] ->
      let wrap = wrap_prefix ":" wrap in
      makeList ~break ~wrap ~pad [self#signature_item x]
    | PSig items ->
      let wrap = wrap_prefix ":" wrap in
      let rows = List.map self#signature_item items in
      makeList ~wrap ~break ~pad ~postSpace ~sep:(Layout.Sep ";") rows
    | PPat (x, None) ->
      let wrap = wrap_prefix "?" wrap in
      makeList ~wrap ~break ~pad [self#pattern_at_least_as_simple_as_alias_or_or x]
    | PPat (x, Some e) ->
      let wrap = wrap_prefix "?" wrap in
      makeList ~wrap ~break ~pad ~postSpace [
        self#pattern_at_least_as_simple_as_alias_or_or x;
        label ~space:true (atom "when") (self#unparseExpr e)
      ]

  method extension (s, p) =
    match s.txt with
    (* We special case "bs.obj" for now to allow for a nicer interop with
     * BuckleScript. We might be able to generalize to any kind of record
     * looking thing with struct keys. *)
    | "bs.obj" -> self#formatBsObjExtensionSugar p
    | _ -> (self#payload "%" s p)

  method item_extension (s, e) = (self#payload "%%" s e)


  (* [@ ...] Simple attributes *)
  method attribute = function
    | { attr_name = { Location. txt = ("ocaml.doc" | "ocaml.text") }
      ; attr_payload =
          PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string(text, None)) } , _);
                  pstr_loc }]
      ; _ } ->
      let text = if text = "" then "/**/" else "/**" ^ text ^ "*/" in
      makeList ~inline:(true, true) ~postSpace:true ~preSpace:true ~indent:0 ~break:IfNeed [atom ~loc:pstr_loc text]
    | { attr_name; attr_payload; _ } -> self#payload "@" attr_name attr_payload

  (* [@@ ... ] Attributes that occur after a major item in a structure/class *)
  method item_attribute = self#attribute

  (* [@@ ...] Attributes that occur not *after* an item in some structure/class/sig, but
     rather as their own standalone item. Note that syntactic distinction
     between item_attribute and floating_attribute is no longer necessary with
     Reason. Thank you semicolons. *)
  method floating_attribute = self#item_attribute

  method attributes l = List.map self#attribute l

  method attach_std_attrs l toThis =
    let l = extractStdAttrs l in
    match l with
      | [] -> toThis
      | _::_ -> makeList ~postSpace:true (List.concat [self#attributes l; [toThis]])

  method attach_std_item_attrs ?(allowUncurry=true) ?extension l toThis =
    let l = (partitionAttributes ~allowUncurry l).stdAttrs in
    match extension, l with
    | None, [] -> toThis
    | _, _ ->
      let extension = match extension with
        | None -> []
        | Some id -> [atom ("%" ^ id.txt)]
      in
      makeList
        ~postSpace:true ~indent:0 ~break:Layout.Always_rec ~inline:(true, true)
        (extension @ List.map self#item_attribute l @ [toThis])

  method exception_declaration ed =
    let pcd_name = ed.pext_name in
    let pcd_loc = ed.pext_loc in
    let pcd_attributes = [] in
    let exn_arg = match ed.pext_kind with
      | Pext_decl (args, type_opt) ->
          let pcd_args, pcd_res = args, type_opt in
          [self#type_variant_leaf_nobar {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes}]
      | Pext_rebind id ->
          [atom pcd_name.txt; atom "="; (self#longident_loc id)] in
    let {stdAttrs; docAttrs} =
      partitionAttributes ~partDoc:true ed.pext_attributes
    in
    let layout =
      self#attach_std_item_attrs
        stdAttrs
        (label ~space:true
          (atom "exception")
          (makeList ~postSpace:true ~inline:(true, true) exn_arg))
    in
    self#attachDocAttrsToLayout
      ~stdAttrs
      ~docAttrs
      ~loc:ed.pext_loc
      ~layout
      ()

  (*
    Note: that override doesn't appear in class_sig_field, but does occur in
    class/object expressions.
    TODO: TODOATTRIBUTES
   *)
  method method_sig_flags_for s = function
    | Virtual -> [atom "virtual"; atom s]
    | Concrete ->  [atom s]

  method value_type_flags_for s = function
    | (Virtual, Mutable) -> [atom "virtual"; atom "mutable"; atom s]
    | (Virtual, Immutable) -> [atom "virtual"; atom s]
    | (Concrete, Mutable) -> [atom "mutable"; atom s]
    | (Concrete, Immutable) -> [atom s]

  method class_sig_field x =
    match x.pctf_desc with
    | Pctf_inherit ct ->
      label ~space:true (atom "inherit") (self#class_constructor_type ct)
    | Pctf_val (s, mf, vf, ct) ->
      let valueFlags = self#value_type_flags_for (s.txt ^ ":") (vf, mf) in
      label
        ~space:true
        (
          label ~space:true
            (atom "val")
            (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed valueFlags)
        )
        (self#core_type ct)
    | Pctf_method (s, pf, vf, ct) ->
      let methodFlags = self#method_sig_flags_for (s.txt ^ ":") vf
      in
      let pubOrPrivate =
        match pf with
        | Private -> "pri"
        | Public -> "pub"
      in
      let m = label
        ~space:true
        (label ~space:true
            (atom pubOrPrivate)
            (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed methodFlags)
        )
        (self#core_type ct)
      in
      (self#attach_std_item_attrs x.pctf_attributes m)
    | Pctf_constraint (ct1, ct2) ->
      label
        ~space:true
        (atom "constraint")
        (label ~space:true
            (makeList ~postSpace:true [self#core_type ct1; atom "="])
            (self#core_type ct2)
        )
    | Pctf_attribute a -> self#floating_attribute a
    | Pctf_extension e -> self#item_extension e

  (*
    /** doc comment */                                    (* formattedDocs *)
    [@bs.val] [@bs.module "react-dom"]                    (* formattedAttrs *)
    external render : reactElement => element => unit =   (* frstHalf *)
      "render";                                           (* sndHalf *)

    To improve the formatting with breaking & indentation:
      * consider the part before the '=' as a label
      * combine that label with '=' in a list
      * consider the part after the '=' as a list
      * combine both parts as a label
      * format the doc comment with a ~postSpace:true (inline, not inline) list
      * format the attributes with a ~postSpace:true (inline, inline) list
      * format everything together in a ~postSpace:true (inline, inline) list
        for nicer breaking
  *)
  method primitive_declaration vd =
    let lblBefore =
      label
        ~space:true
        (makeList
           [(makeList ~postSpace:true [atom "external"; protectIdentifier vd.pval_name.txt]); (atom ":")])
        (self#core_type vd.pval_type)
    in
    let primDecl =
      match vd.pval_prim with
      | [""] -> lblBefore
      | _ ->
        let frstHalf = makeList ~postSpace:true [lblBefore; atom "="] in
        let sndHalf =
          makeSpacedBreakableInlineList (List.map self#constant_string_for_primitive vd.pval_prim) in
        label ~space:true frstHalf sndHalf
    in
    match vd.pval_attributes with
    | [] -> primDecl
    | attrs ->
        let {stdAttrs; docAttrs} = partitionAttributes ~partDoc:true attrs in
        let docs = List.map self#item_attribute docAttrs in
        let formattedDocs = makeList ~postSpace:true docs in
        let attrs = List.map self#item_attribute stdAttrs in
        let formattedAttrs = makeSpacedBreakableInlineList attrs in
        let layouts = match (docAttrs, stdAttrs) with
        | ([], _) -> [formattedAttrs; primDecl]
        | (_, []) -> [formattedDocs; primDecl]
        | _ -> [formattedDocs; formattedAttrs; primDecl] in
        makeSpacedBreakableInlineList layouts

  method classTypeSigsAndRest x =
    match x.pcty_desc with
    | Pcty_signature cs ->
      let {pcsig_self = ct; pcsig_fields = l} = cs in
      let instTypeFields = List.map self#class_sig_field l in
      let allItems = match ct.ptyp_desc with
        | Ptyp_any -> instTypeFields
        | _ ->
          label ~space:true (atom "as") (self#core_type ct) ::
          instTypeFields
      in
      allItems
    | _ -> [self#class_instance_type x]

  method class_instance_type x =
    match x.pcty_desc with
    | Pcty_signature _
    | Pcty_open _ ->
      let opens, rest = self#classTypeOpens x in
      let cs = self#classTypeSigsAndRest rest in
      self#attach_std_item_attrs ~allowUncurry:false x.pcty_attributes (
        makeList
          ~wrap:("{", "}")
          ~postSpace:true
          ~break:Layout.Always_rec
          (List.map semiTerminated (List.concat [opens; cs]))
      )
    | Pcty_constr (li, l) ->
      self#attach_std_attrs x.pcty_attributes (
        match l with
        | [] -> self#longident_loc li
        | _::_ ->
          label
            (self#longident_loc li)
            (makeList ~wrap:("(", ")") ~sep:commaTrail (List.map self#core_type l))
      )
    | Pcty_extension e ->
      self#attach_std_item_attrs x.pcty_attributes (self#extension e)
    | Pcty_arrow _ -> failwith "class_instance_type should not be printed with Pcty_arrow"

  method classTypeOpens x =
    let rec gatherOpens acc opn =
      match opn.pcty_desc with
      | Pcty_open (md, ct) ->
          let li = md.popen_expr in
          gatherOpens
            (source_map ~loc:li.loc
               (label ~space:true
                  (atom ("open" ^ (override md.popen_override)))
                  (self#longident_loc li))
             :: acc)
            ct
      | _ -> List.rev acc, opn
    in
    gatherOpens [] x

  method class_declaration_list l =
    let class_declaration ?(class_keyword=false)
        ({pci_params=ls; pci_name={txt}; pci_virt; pci_loc} as x) =
      let (firstToken, pattern, patternAux) = self#class_opening class_keyword txt pci_virt ls in
      let classBinding = self#wrappedClassBinding firstToken pattern patternAux x.pci_expr in
      source_map ~loc:pci_loc
        (self#attach_std_item_attrs x.pci_attributes classBinding)
    in
    (match l with
      | [] -> raise (NotPossible "Class definitions will have at least one item.")
      | x::rest ->
        makeNonIndentedBreakingList (
          class_declaration ~class_keyword:true x ::
          List.map class_declaration rest
        )
    )
  (* For use with [class type a = class_instance_type]. Class type
     declarations/definitions declare the types of instances generated by class
     constructors.
     We have to call self#class_instance_type because self#class_constructor_type
     would add a "new" before the type.
     TODO: TODOATTRIBUTES:
  *)
  method class_type_declaration_list l =
    let class_type_declaration kwd ({pci_params=ls;pci_name;pci_attributes} as x) =
      let opener = match x.pci_virt with
        | Virtual -> kwd ^ " " ^ "virtual"
        | Concrete -> kwd
      in

      let upToName =
        if ls == [] then
          label ~space:true (atom opener) (atom pci_name.txt)
        else
          label
            ~space:true
            (label ~space:true (atom opener) (atom pci_name.txt))
            (self#class_params_def ls)
      in
      let includingEqual = makeList ~postSpace:true [upToName; atom "="] in
      let {stdAttrs; docAttrs} =
        partitionAttributes ~partDoc:true pci_attributes
      in
      let layout =
        self#attach_std_item_attrs stdAttrs @@
        label ~space:true includingEqual (self#class_instance_type x.pci_expr)
      in
      self#attachDocAttrsToLayout
        ~stdAttrs
        ~docAttrs
        ~loc:pci_name.loc
        ~layout
        ()
    in
    match l with
    | [] -> failwith "Should not call class_type_declaration with no classes"
    | [x] -> class_type_declaration "class type" x
    | x :: xs ->
      makeList
        ~break:Always_rec
        ~indent:0
        ~inline:(true, true)
        (
          (class_type_declaration "class type" x)::
          List.map (class_type_declaration "and") xs
        )

  (*
     Formerly the [class_type]
     Notice how class_constructor_type doesn't have any type attributes -
     class_instance_type does.
     TODO: Divide into class_constructor_types that allow arrows and ones
     that don't.
   *)
  method class_constructor_type x =
    match x.pcty_desc with
    | Pcty_arrow _ ->
      let rec allArrowSegments acc = function
        | { pcty_desc = Pcty_arrow (l, ct1, ct2); } ->
            allArrowSegments (self#type_with_label (l, ct1, false) :: acc) ct2
        (* This "new" is unfortunate. See reason_parser.mly for details. *)
        | xx -> (List.rev acc, self#class_constructor_type xx)
      in
      let (params, return) = allArrowSegments [] x in
      let normalized =
        makeList ~break:IfNeed
          ~sep:(Sep "=>")
          ~preSpace:true ~postSpace:true ~inline:(true, true)
        [makeCommaBreakableListSurround "(" ")" params; return]
      in
      source_map ~loc:x.pcty_loc normalized
    | _ ->
      (* Unfortunately, we have to have final components of a class_constructor_type
         be prefixed with the `new` keyword.  Hopefully this is temporary. *)
      self#class_instance_type x

  method non_arrowed_class_constructor_type x =
    match x.pcty_desc with
    | Pcty_arrow _ ->
      source_map ~loc:x.pcty_loc
        (formatPrecedence (self#class_constructor_type x))
    | _ -> self#class_instance_type x

  method class_field x =
    let itm =
      match x.pcf_desc with
      | Pcf_inherit (ovf, ce, so) ->
        let inheritText = ("inherit" ^ override ovf) in
        let inheritExp = self#class_expr ce in
        label
          ~space:true
          (atom inheritText)
          (
            match so with
            | None -> inheritExp;
            | Some s -> label ~space:true inheritExp (atom ("as " ^ s.txt))
          )
      | Pcf_val (s, mf, Cfk_concrete (ovf, e)) ->
        let opening = match mf with
          | Mutable ->
            let mutableName = [atom "mutable"; atom s.txt] in
            label
              ~space:true
              (atom ("val" ^ override ovf))
              (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed mutableName)
          | Immutable -> label ~space:true (atom ("val" ^ override ovf)) (atom s.txt)
        in
        let valExprAndConstraint = match e.pexp_desc with
          | Pexp_constraint (ex, ct) ->
            let openingWithTypeConstraint = formatTypeConstraint opening (self#core_type ct) in
            label
              ~space:true
              (makeList ~postSpace:true [openingWithTypeConstraint; atom "="])
              (self#unparseExpr ex)
          | _ ->
            label ~space:true (makeList ~postSpace:true [opening; atom "="]) (self#unparseExpr e)
        in
        valExprAndConstraint
      | Pcf_val (s, mf, Cfk_virtual ct) ->
        let opening = match mf with
          | Mutable ->
            let mutableVirtualName = [atom "mutable"; atom "virtual"; atom s.txt] in
            let openingTokens =
              (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed mutableVirtualName) in
            label ~space:true (atom "val") openingTokens
          | Immutable ->
            let virtualName = [atom "virtual"; atom s.txt] in
            let openingTokens =
              (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed virtualName) in
            label ~space:true (atom "val") openingTokens
        in
        formatTypeConstraint opening (self#core_type ct)
      | Pcf_method (s, pf, Cfk_virtual ct) ->
        let opening = match pf with
          | Private ->
            let privateVirtualName = [atom "virtual"; atom s.txt] in
            let openingTokens =
              (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed privateVirtualName) in
            label ~space:true (atom "pri") openingTokens
          | Public ->
            let virtualName = [atom "virtual"; atom s.txt] in
            let openingTokens =
              (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed virtualName) in
            label ~space:true (atom "pub") openingTokens
        in
        formatTypeConstraint opening (self#core_type ct)
      | Pcf_method (s, pf, Cfk_concrete (ovf, e)) ->
        let methodText =
           let postFix = if ovf == Override then "!" else "" in
           (
           match pf with
           | Private -> "pri" ^ postFix
           | Public -> "pub" ^ postFix
           ) in
        (* Should refactor the binding logic so faking out the AST isn't needed,
           currently, it includes a ton of nuanced logic around recovering explicitly
           polymorphic type definitions, and that furthermore, that representation...
           Actually, let's do it.

           For some reason, concrete methods are only ever parsed as Pexp_poly.
           If there *is* no polymorphic function for the method, then the return
           value of the function is wrapped in a ghost Pexp_poly with [None] for
           the type vars.*)
        (match e.pexp_desc with
          | (Pexp_poly
              ({pexp_desc=Pexp_constraint (methodFunWithNewtypes, nonVarifiedExprType)},
                Some ({ptyp_desc=Ptyp_poly (typeVars, varifiedPolyType)})
              )
            ) when (
              let (leadingAbstractVars, _) =
                self#leadingCurriedAbstractTypes methodFunWithNewtypes in
              self#isRenderableAsPolymorphicAbstractTypes
                typeVars
                (* If even artificially varified. Don't know until this returns*)
                varifiedPolyType
                leadingAbstractVars
                nonVarifiedExprType
          ) ->
            let (leadingAbstractVars, _) =
              self#leadingCurriedAbstractTypes methodFunWithNewtypes in
            self#locallyAbstractPolymorphicFunctionBinding
              methodText
              (atom s.txt)
              methodFunWithNewtypes
              leadingAbstractVars
              nonVarifiedExprType
          | Pexp_poly (e, Some ct) ->
            self#formatSimplePatternBinding methodText (atom s.txt)
              (Some (source_map ~loc:ct.ptyp_loc (self#core_type ct)))
              (self#unparseExprApplicationItems e)
          (* This form means that there is no type constraint - it's a strange node name.*)
          | Pexp_poly (e, None) ->
            self#wrappedBinding methodText ~arrow:"=>" (atom s.txt) [] e
          | _ -> failwith "Concrete methods should only ever have Pexp_poly."
        )
      | Pcf_constraint (ct1, ct2) ->
        label
          ~space:true
          (atom "constraint")
          (
            makeList ~postSpace:true ~inline:(true, false) [
              makeList ~postSpace:true [self#core_type ct1; atom "="];
              self#core_type ct2
            ]
          )
      | Pcf_initializer e ->
        label
          ~space:true
          (atom "initializer")
          (self#simplifyUnparseExpr e)
      | Pcf_attribute a -> self#floating_attribute a
      | Pcf_extension e ->
        (* And don't forget, we still need to print post_item_attributes even for
           this case *)
        self#item_extension e
    in
    let layout = self#attach_std_attrs x.pcf_attributes itm in
    source_map ~loc:x.pcf_loc layout

  method class_self_pattern_and_structure {pcstr_self = p; pcstr_fields = l} =
    let fields = List.map self#class_field l in
    (* Recall that by default self is bound to "this" at parse time. You'd
       have to go out of your way to bind it to "_". *)
    match (p.ppat_attributes, p.ppat_desc) with
      | ([], Ppat_var ({txt = "this"})) -> fields
      | _ ->
        let field = label ~space:true (atom "as") (self#pattern p) in
        source_map ~loc:p.ppat_loc field :: fields

  method simple_class_expr x =
    let {stdAttrs} = partitionAttributes x.pcl_attributes in
    if stdAttrs != [] then
      formatSimpleAttributed
        (self#simple_class_expr {x with pcl_attributes=[]})
        (self#attributes stdAttrs)
    else
      let itm =
        match x.pcl_desc with
        | Pcl_constraint (ce, ct) ->
          formatTypeConstraint (self#class_expr ce) (self#class_constructor_type ct)
        (* In OCaml,
          - In the most recent version of OCaml, when in the top level of a
            module, let _ = ... is a PStr_eval.
          - When in a function, it is a Pexp_let PPat_any
          - When in class pre-member let bindings it is a Pcl_let PPat_any

           Reason normalizes all of these to be simple imperative expressions
           with trailing semicolons, *except* in the case of classes because it
           will likely introduce a conflict with some proposed syntaxes for
           objects.
        *)
        | Pcl_let _
        | Pcl_structure _
        | Pcl_open _ ->
          let opens, rest = self#classExprOpens x in
          let rows = self#classExprLetsAndRest rest in
          makeList ~wrap:("{", "}") ~inline:(true, false) ~postSpace:true ~break:Always_rec
            (List.map semiTerminated (List.concat [opens; rows]))
        | Pcl_extension e -> self#extension e
        | _ -> formatPrecedence (self#class_expr x)
     in source_map ~loc:x.pcl_loc itm

  method classExprLetsAndRest x =
    match x.pcl_desc with
      | Pcl_structure cs -> self#class_self_pattern_and_structure cs
      | Pcl_let (rf, l, ce) ->
        (* For "letList" bindings, the start/end isn't as simple as with
         * module value bindings. For "let lists", the sequences were formed
         * within braces {}. The parser relocates the first let binding to the
         * first brace. *)
        let binding =
          source_map ~loc:(self#bindingsLocationRange l)
            (self#bindings (rf, l))
        in
        (binding :: self#classExprLetsAndRest ce)
      | Pcl_open (_, ce) -> self#classExprLetsAndRest ce
      | _ -> [self#class_expr x]

  method classExprOpens x =
    let rec gatherOpens acc opn =
      match opn.pcl_desc with
      | Pcl_open (md, ce) ->
        let li = md.popen_expr in
        gatherOpens
          (source_map ~loc:li.loc
             (label ~space:true
                (atom ("open" ^ (override md.popen_override)))
                (self#longident_loc li))
           :: acc)
          ce
      | _ -> List.rev acc, opn
    in
    gatherOpens [] x

  method class_expr x =
    let {stdAttrs} = partitionAttributes x.pcl_attributes in
    (* We cannot handle the attributes here. Must handle them in each item *)
    if stdAttrs != [] then
      (* Do not need a "simple" attributes precedence wrapper. *)
      formatAttributed
        (self#simple_class_expr {x with pcl_attributes=[]})
        (self#attributes stdAttrs)
    else
      match x.pcl_desc with
      | Pcl_fun _ ->
        (match self#curriedConstructorPatternsAndReturnVal x with
         | None, _ ->
           (* x just matched Pcl_fun, there is at least one parameter *)
           assert false
         | Some args, e ->
           label ~space:true
             (makeList ~postSpace:true
                [label ~space:true (atom funToken) args; atom "=>"])
             (self#class_expr e))
      | Pcl_apply _ ->
        formatAttachmentApplication applicationFinalWrapping None
         (self#classExpressionToFormattedApplicationItems x, None)
      | Pcl_constr (li, []) ->
        label ~space:true (atom "class") (self#longident_loc li)
      | Pcl_constr (li, l) ->
        label
          (makeList ~postSpace:true [atom "class"; self#longident_loc li])
          (makeTup (List.map self#non_arrowed_non_simple_core_type l))
      | Pcl_open _
      | Pcl_constraint _
      | Pcl_extension _
      | Pcl_let _
      | Pcl_structure _ -> self#simple_class_expr x;

  method classStructure ?(forceBreak=false) ?(wrap=("", "")) cs =
    let (left, right) = wrap in
    makeList
      ~sep:(Layout.Sep ";")
      ~wrap:(left ^ "{", "}" ^ right)
      ~break:(if forceBreak then Layout.Always else Layout.IfNeed)
      ~postSpace:true
      ~inline:(true, false)
      (self#class_self_pattern_and_structure cs)

  method signature signatureItems =
    match signatureItems with
    | [] -> atom ""
    | first::_ as signatureItems ->
      let last = match (List.rev signatureItems) with | last::_ -> last | [] -> assert false in
      let loc_start = first.psig_loc.loc_start in
      let loc_end = last.psig_loc.loc_end in
      let items =
        groupAndPrint
          ~xf:self#signature_item
          ~getLoc:(fun x -> x.psig_loc)
          ~comments:self#comments
          signatureItems
      in
      source_map ~loc:{loc_start; loc_end; loc_ghost=false}
        (makeList
           ~postSpace:true
           ~break:Layout.Always_rec
           ~indent:0
           ~inline:(true, false)
           ~sep:(SepFinal (";", ";"))
           items)

  method signature_item x : Layout.t =
    let item: Layout.t =
      match x.psig_desc with
        | Psig_type (rf, l) ->
            self#type_def_list (rf, l)
        | Psig_value vd ->
            if vd.pval_prim != [] then
              self#primitive_declaration vd
            else
              let intro = atom "let" in
              let {stdAttrs; docAttrs} = partitionAttributes ~partDoc:true vd.pval_attributes in
              let layout = self#attach_std_item_attrs stdAttrs
                (formatTypeConstraint
                   (label ~space:true intro
                      (source_map ~loc:vd.pval_name.loc
                         (protectIdentifier vd.pval_name.txt)))
                  (self#core_type vd.pval_type))
              in
              self#attachDocAttrsToLayout
                ~stdAttrs
                ~docAttrs
                ~loc:vd.pval_loc
                ~layout
                ()

        | Psig_typext te ->
            self#type_extension te
        | Psig_exception ed ->
          self#exception_declaration
            { ed.ptyexn_constructor
              with pext_attributes = ed.ptyexn_attributes @ ed.ptyexn_constructor.pext_attributes}
        | Psig_class l ->
            let class_description
                ?(class_keyword=false)
                ({pci_params=ls; pci_name={txt}; pci_loc} as x) =
              let (firstToken, pattern, patternAux) = self#class_opening class_keyword txt x.pci_virt ls in
              let withColon = self#wrapCurriedFunctionBinding
                ~arrow:":"
                ~spaceBeforeArrow:false
                firstToken
                pattern
                patternAux
                ([(self#class_constructor_type x.pci_expr)], None)
              in
              let {stdAttrs; docAttrs} = partitionAttributes ~partDoc:true x.pci_attributes in
              let layout = self#attach_std_item_attrs stdAttrs withColon in
              source_map ~loc:pci_loc
                (self#attachDocAttrsToLayout
                  ~stdAttrs
                  ~docAttrs
                  ~loc:x.pci_name.loc
                  ~layout
                  ())
            in
            makeNonIndentedBreakingList (
              match l with
              | [] -> raise (NotPossible "No recursive class bindings")
              | [x] -> [class_description ~class_keyword:true x]
              | x :: xs ->
                 (class_description ~class_keyword:true x)::
                 (List.map class_description xs)
            )
        | Psig_module {pmd_name; pmd_type={pmty_desc=Pmty_alias alias}; pmd_attributes} ->
            let {stdAttrs; docAttrs} =
              partitionAttributes ~partDoc:true pmd_attributes
            in
            let layout =
              self#attach_std_item_attrs stdAttrs @@
              label ~space:true
                (makeList ~postSpace:true [
                   atom "module";
                   atom pmd_name.txt;
                   atom "="
                 ])
                (self#longident_loc alias)
            in
            self#attachDocAttrsToLayout
              ~stdAttrs
              ~docAttrs
              ~loc:pmd_name.loc
              ~layout
              ()
        | Psig_module pmd ->
            let {stdAttrs; docAttrs} =
              partitionAttributes ~partDoc:true pmd.pmd_attributes
            in
            let letPattern =
              makeList
                [makeList ~postSpace:true [atom "module"; (atom pmd.pmd_name.txt)];
                 atom ":"]
            in
            let layout =
              self#attach_std_item_attrs stdAttrs @@
              (self#module_type letPattern pmd.pmd_type)
            in
            self#attachDocAttrsToLayout
              ~stdAttrs
              ~docAttrs
              ~loc:pmd.pmd_name.loc
              ~layout
              ()
        | Psig_open od ->
          let {stdAttrs; docAttrs} =
            partitionAttributes ~partDoc:true od.popen_attributes
          in
          let layout =
            self#attach_std_item_attrs stdAttrs @@
            label ~space:true
              (atom ("open" ^ (override od.popen_override)))
              (self#longident_loc od.popen_expr)
          in
          self#attachDocAttrsToLayout
            ~stdAttrs
            ~docAttrs
            ~loc:od.popen_expr.loc
            ~layout
            ()
        | Psig_include incl ->
          let {stdAttrs; docAttrs} =
            partitionAttributes ~partDoc:true incl.pincl_attributes
          in
          let layout =
            self#attach_std_item_attrs stdAttrs @@
            (self#module_type (atom "include") incl.pincl_mod)
          in
          self#attachDocAttrsToLayout
            ~stdAttrs
            ~docAttrs
            ~loc:incl.pincl_mod.pmty_loc
            ~layout
            ()
        | Psig_modtype x ->
          let name = atom x.pmtd_name.txt in
          let letPattern = makeList ~postSpace:true [atom "module type"; name; atom "="] in
          let main = match x.pmtd_type with
            | None -> makeList ~postSpace:true [atom "module type"; name]
            | Some mt -> self#module_type letPattern mt
          in
          let {stdAttrs; docAttrs} =
            partitionAttributes ~partDoc:true x.pmtd_attributes
          in
          let layout =
            self#attach_std_item_attrs stdAttrs main
          in
          self#attachDocAttrsToLayout
            ~stdAttrs
            ~docAttrs
            ~loc:x.pmtd_name.loc
            ~layout
            ()
        | Psig_class_type l -> self#class_type_declaration_list l
        | Psig_recmodule decls ->
            let items = List.mapi (fun i xx ->
              let {stdAttrs; docAttrs} =
                partitionAttributes ~partDoc:true xx.pmd_attributes
              in
              let letPattern =
                makeList [
                  makeList ~postSpace:true [
                    atom (if i == 0 then "module rec" else "and");
                    atom xx.pmd_name.txt
                  ];
                 atom ":"
                ]
              in
              let layout =
                self#attach_std_item_attrs stdAttrs
                  (self#module_type ~space:true letPattern xx.pmd_type)
              in
              let layoutWithDocAttrs =
                self#attachDocAttrsToLayout
                 ~stdAttrs
                 ~docAttrs
                 ~loc:xx.pmd_name.loc
                 ~layout
                 ()
              in
              (extractLocModDecl xx, layoutWithDocAttrs)
            ) decls
            in
            makeNonIndentedBreakingList
              (groupAndPrint
                ~xf:(fun (_, layout) -> layout)
                ~getLoc:(fun (loc, _) -> loc)
                ~comments:self#comments
                items)

        | Psig_attribute a -> self#floating_attribute a
        | Psig_extension (({loc}, _) as ext, attrs) ->
          let {stdAttrs; docAttrs} =
            partitionAttributes ~partDoc:true attrs
          in
          let layout =
            self#attach_std_item_attrs stdAttrs (self#item_extension ext)
          in
          self#attachDocAttrsToLayout
            ~stdAttrs
            ~docAttrs
            ~loc
            ~layout
            ()
        | Psig_modsubst { pms_name; pms_manifest; pms_attributes; pms_loc } ->
          let name = atom pms_name.txt in
          let main = makeList ~postSpace:true
            [atom "module"; name; atom ":="; self#longident_loc pms_manifest]
          in
          let {stdAttrs; docAttrs} =
            partitionAttributes ~partDoc:true pms_attributes
          in
          let layout =
            self#attach_std_item_attrs stdAttrs main
          in
          self#attachDocAttrsToLayout
            ~stdAttrs
            ~docAttrs
            ~loc:pms_loc
            ~layout
            ()
        | Psig_typesubst l ->
          self#type_def_list ~eq_symbol:":=" (Recursive, l)
    in
    source_map ~loc:x.psig_loc item

  method non_arrowed_module_type ?(space=true) letPattern x =
    match x.pmty_desc with
      | Pmty_alias li ->
        label ~space
          letPattern
          (formatPrecedence (label ~space:true (atom "module") (self#longident_loc li)))
      | Pmty_typeof me ->
        let labelWithoutFinalWrap =
          label ~space:true
            (label ~space:true
               letPattern
               (makeList
                  ~inline:(false, false)
                  ~wrap:("(","")
                  ~postSpace:true
                  [atom "module type of"]))
            (self#module_expr me)
        in
        makeList ~wrap:("",")") [labelWithoutFinalWrap]
      | _ -> self#simple_module_type ~space letPattern x

  method simple_module_type ?(space=true) letPattern x =
    match x.pmty_desc with
      | Pmty_ident li -> label ~space letPattern (self#longident_loc li)
      | Pmty_signature s ->
        let items =
          groupAndPrint
          ~xf:self#signature_item
          ~getLoc:(fun x -> x.psig_loc)
          ~comments:self#comments
          s
        in
        let shouldBreakLabel = if List.length s > 1 then `Always else `Auto in
        label
          ~indent:0
          ~break:shouldBreakLabel
          (makeList
            [label
               ~break:shouldBreakLabel
                (makeList
                  ~postSpace:true
                  [letPattern; (atom "{")])
              (source_map
                 ~loc:x.pmty_loc
                 (makeList
                    ~break:(if List.length s > 1 then Always else IfNeed)
                    ~inline:(true, true)
                    ~postSpace:true
                    ~sep:(SepFinal (";", ";"))
                    items))])
          (atom "}")
      | Pmty_extension (s, e) -> label ~space letPattern (self#payload "%" s e)
      | _ ->
        makeList ~break:IfNeed ~wrap:("", ")")
          [self#module_type ~space:false (makeList ~pad:(false,true) ~wrap:("","(") [letPattern]) x]

  method module_type ?(space=true) letPattern x =
    let pmty = match x.pmty_desc with
      | Pmty_functor _ ->
        (* The segments that should be separated by arrows. *)
        let rec extract_args args xx = match xx.pmty_desc with
          | Pmty_functor (_, None, mt2) -> extract_args (`Unit :: args) mt2
          | Pmty_functor (s, Some mt1, mt2) ->
            let arg =
              if s.txt = "_"
              then self#module_type ~space:false (atom "") mt1
              else self#module_type ~space (makeList [(atom s.txt); atom ":"]) mt1
            in
            extract_args (`Arg arg :: args) mt2
          | _ ->
            let prepare_arg = function
              | `Unit -> atom "()"
              | `Arg x -> x
            in
            let args = match args with
              | [`Unit] -> []
              | _ -> List.rev_map prepare_arg args
            in
            (args, self#module_type (atom "") xx)
        in
        let args, ret = extract_args [] x in
        label ~space letPattern
          (makeList
             ~break:IfNeed
             ~sep:(Sep "=>")
             ~preSpace:true
             ~inline:(true, true)
             [makeTup args; ret])

      (* See comments in sugar_parser.mly about why WITH constraints aren't "non
       * arrowed" *)
      | Pmty_with (mt, l) ->
          let modSub atm li2 token = makeList ~postSpace:true [
            atom "module";
            atm;
            atom token;
            self#longident_loc li2
          ] in
          let typeAtom = atom "type" in
          let eqAtom = atom "=" in
          let destrAtom = atom ":=" in
          let with_constraint = function
            | Pwith_type (li, td) ->
                self#formatOneTypeDef
                  typeAtom
                  (makeList ~preSpace:true [(self#longident_loc li)])
                  eqAtom
                  td
            | Pwith_module (li, li2) ->
                modSub (self#longident_loc li) li2 "="
            | Pwith_typesubst (_, td) ->
                self#formatOneTypeDef
                  typeAtom
                  (atom ~loc:td.ptype_name.loc td.ptype_name.txt)
                  destrAtom
                  td
            | Pwith_modsubst (s, li2) -> modSub (self#longident s.txt) li2 ":="
          in
          (match l with
            | [] -> self#module_type ~space letPattern mt
            | _ ->
              label ~space letPattern
                (label ~space:true
                  (makeList ~preSpace:true [self#module_type ~space:false (atom "") mt; atom "with"])
                  (makeList
                     ~break:IfNeed
                     ~inline:(true, true)
                     ~sep:(Sep "and")
                     ~postSpace:true
                     ~preSpace:true
                     (List.map with_constraint l)))
          )
        (* Seems like an infinite loop just waiting to happen. *)
        | _ -> self#non_arrowed_module_type ~space letPattern x
    in
    source_map ~loc:x.pmty_loc pmty

  method simple_module_expr ?(hug=false) x = match x.pmod_desc with
    | Pmod_unpack e ->
        let exprLayout = match e.pexp_desc with
        | Pexp_constraint (e, {ptyp_desc = Ptyp_package(lid, cstrs)}) ->
          formatTypeConstraint
            (makeList ~postSpace:true [atom "val"; (self#unparseExpr e)])
            (self#typ_package ~mod_prefix:false lid cstrs)
        | _ -> makeList ~postSpace:true [atom "val"; (self#unparseExpr e)]
        in formatPrecedence exprLayout
    | Pmod_ident li ->
        ensureSingleTokenSticksToLabel (self#longident_loc li)
    | Pmod_constraint (unconstrainedRet, mt) ->
        let letPattern = makeList [(self#module_expr unconstrainedRet); atom ":"]
        in
        formatPrecedence (self#module_type letPattern mt)
    | Pmod_structure s ->
        let wrap = if hug then ("({", "})") else ("{", "}") in
        let items =
          groupAndPrint
            ~xf:self#structure_item
            ~getLoc:(fun x -> x.pstr_loc)
            ~comments:self#comments
            s
        in
        makeList
          ~break:Always_rec
          ~inline:(true, false)
          ~wrap
          ~postSpace:true
          ~sep:(SepFinal (";", ";"))
          items
    | _ ->
        (* For example, functor application will be wrapped. *)
        formatPrecedence (self#module_expr x)

  method module_expr x =
    match x.pmod_desc with
    | Pmod_functor _ ->
      let (argsList, return) = self#curriedFunctorPatternsAndReturnStruct x in
      (* See #19/20 in syntax.mls - cannot annotate return type at
               the moment. *)
      self#wrapCurriedFunctionBinding funToken ~sweet:true ~arrow:"=>" (makeTup argsList) []
        ([self#moduleExpressionToFormattedApplicationItems return], None)
    | Pmod_apply _ ->
      self#moduleExpressionToFormattedApplicationItems x
    | Pmod_extension (s, e) -> self#payload "%" s e
    | Pmod_unpack _
    | Pmod_ident _
    | Pmod_constraint _
    | Pmod_structure _ -> self#simple_module_expr x


  method structure structureItems =
    match structureItems with
    | [] -> atom ""
    | first::_ as structureItems ->
      let last = match (List.rev structureItems) with | last::_ -> last | [] -> assert false in
      let loc_start = first.pstr_loc.loc_start in
      let loc_end = last.pstr_loc.loc_end in
      let items =
        groupAndPrint
          ~xf:self#structure_item
          ~getLoc:(fun x -> x.pstr_loc)
          ~comments:self#comments
          structureItems
      in
      source_map ~loc:{loc_start; loc_end; loc_ghost = false}
        (makeList
           ~postSpace:true
           ~break:Always_rec
           ~indent:0
           ~inline:(true, false)
           ~sep:(SepFinal (";", ";"))
           items)

  (*
     How do modules become parsed?
     let module (X: sig) = blah;
       Will not parse! (Should just make it parse to let [X:sig =]).
     let module X: sig = blah;
       Becomes Pmod_constraint
     let module X: sig = (blah:sig);
       Becomes Pmod_constraint .. Pmod_constraint
     let module X = blah:typ;
       Becomes Pmod_constraint
     let module X (Y:y) (Z:z):r => Q
       Becomes Pmod_functor...=> Pmod_constraint

     let module X (Y:y) (Z:z):r => (Q:r2)
       Probably becomes Pmod_functor...=> (Pmod_constraint..
       Pmod_constraint)

    let (module X) =
      Is a *completely* different thing (unpacking/packing first class modules).
      We should make sure this is very well distinguished.
      - Just replace all "let module" with a new three letter keyword (mod)?
      - Reserve let (module X) for unpacking first class modules.

    See the notes about how Ppat_constraint become parsed and attempt to unify
    those as well.
  *)

  method let_module_binding prefixText bindingName moduleExpr =
    let (argsList, return) = self#curriedFunctorPatternsAndReturnStruct moduleExpr in (
      match (argsList, return.pmod_desc) with
        (* Simple module with type constraint, no functor args. *)
        | ([], Pmod_constraint (unconstrainedRetTerm, ct)) ->
            let letPattern =
              makeList
                [makeList ~postSpace:true [atom prefixText; bindingName];
                 atom ":"]
            in
            let typeConstraint = self#module_type letPattern ct in
            let includingEqual = makeList ~postSpace:true [typeConstraint; atom "="]
            in
            formatAttachmentApplication applicationFinalWrapping (Some (true, includingEqual))
              ([self#moduleExpressionToFormattedApplicationItems unconstrainedRetTerm], None)

        (* Simple module with type no constraint, no functor args. *)
        | ([], _) ->
          self#formatSimplePatternBinding prefixText bindingName None
            ([self#moduleExpressionToFormattedApplicationItems return], None)
        | (_, _) ->
            (* A functor *)
            let (argsWithConstraint, actualReturn) = (
              match return.pmod_desc with
                (* A functor with constrained return type:
                 *
                 * let module X = (A) (B) : Ret => ...
                 * *)
                | Pmod_constraint (me, ct) ->
                  ([makeTup argsList;
                    self#non_arrowed_module_type (atom ":") ct], me)
                | _ -> ([makeTup argsList], return)
            ) in
            self#wrapCurriedFunctionBinding prefixText ~arrow:"=>"
              (makeList [bindingName; atom " ="]) argsWithConstraint
              ([self#moduleExpressionToFormattedApplicationItems actualReturn], None)
    )

    method class_opening class_keyword name pci_virt ls =
      let firstToken = if class_keyword then "class" else "and" in
      match (pci_virt, ls) with
        (* When no class params, it's a very simple formatting for the
           opener - no breaking. *)
        | (Virtual, []) ->
          (firstToken, atom "virtual", [atom name])
        | (Concrete, []) ->
          (firstToken, atom name, [])
        | (Virtual, _::_) ->
          (firstToken, atom "virtual", [atom name; self#class_params_def ls])
        | (Concrete, _::_) ->
          (firstToken, atom name, [self#class_params_def ls])


  (* TODO: TODOATTRIBUTES: Structure items don't have attributes, but each
     pstr_desc *)
  method structure_item term =
    let item = (
      match term.pstr_desc with
        | Pstr_eval (e, attrs) ->
            let {stdAttrs; jsxAttrs; uncurried} = partitionAttributes attrs in
            if uncurried then Hashtbl.add uncurriedTable e.pexp_loc true;
            let layout = self#attach_std_item_attrs stdAttrs (self#unparseUnattributedExpr e) in
            (* If there was a JSX attribute BUT JSX component wasn't detected,
               that JSX attribute needs to be pretty printed so it doesn't get
               lost *)
            (match jsxAttrs with
            | [] -> layout
            | _::_ ->
              let jsxAttrNodes = List.map self#attribute jsxAttrs in
              makeList ~sep:(Sep " ") (jsxAttrNodes @ [layout]))
        | Pstr_type (_, []) -> assert false
        | Pstr_type (rf, l)  -> (self#type_def_list (rf, l))
        | Pstr_value (rf, l) -> (self#bindings (rf, l))
        | Pstr_typext te -> (self#type_extension te)
        | Pstr_exception ed ->
          self#exception_declaration
            { ed.ptyexn_constructor
              with pext_attributes = ed.ptyexn_attributes @ ed.ptyexn_constructor.pext_attributes}
        | Pstr_module x ->
            let bindingName = atom ~loc:x.pmb_name.loc x.pmb_name.txt in
            self#attach_std_item_attrs x.pmb_attributes @@
            self#let_module_binding "module" bindingName x.pmb_expr
        | Pstr_open od ->
            self#attach_std_item_attrs od.popen_attributes @@
            makeList ~postSpace:true [
              atom ("open" ^ (override od.popen_override));
              self#moduleExpressionToFormattedApplicationItems od.popen_expr;
            ]
        | Pstr_modtype x ->
            let name = atom x.pmtd_name.txt in
            let letPattern = makeList ~postSpace:true [atom "module type"; name; atom "="] in
            let main = match x.pmtd_type with
              | None -> makeList ~postSpace:true [atom "module type"; name]
              | Some mt -> self#module_type letPattern mt
            in
            self#attach_std_item_attrs x.pmtd_attributes main
        | Pstr_class l -> self#class_declaration_list l
        | Pstr_class_type l -> self#class_type_declaration_list l
        | Pstr_primitive vd -> self#primitive_declaration vd
        | Pstr_include incl ->
            self#attach_std_item_attrs incl.pincl_attributes @@
            (* Kind of a hack *)
            let moduleExpr = incl.pincl_mod in
            self#moduleExpressionToFormattedApplicationItems
              ~prefix:"include"
              moduleExpr

        | Pstr_recmodule decls -> (* 3.07 *)
            let items = List.mapi (fun i xx ->
              let {stdAttrs; docAttrs} =
                partitionAttributes ~partDoc:true xx.pmb_attributes
              in
              let layout =
                self#attach_std_item_attrs stdAttrs @@
                self#let_module_binding
                  (if i == 0 then "module rec" else "and")
                  (atom xx.pmb_name.txt)
                  xx.pmb_expr
              in
              let layoutWithDocAttrs =
                self#attachDocAttrsToLayout
                 ~stdAttrs
                 ~docAttrs
                 ~loc:xx.pmb_name.loc
                 ~layout
                 ()
              in
              (extractLocModuleBinding xx, layoutWithDocAttrs)
            ) decls
            in
            makeNonIndentedBreakingList
              (groupAndPrint
                ~xf:(fun (_, layout) -> layout)
                ~getLoc:(fun (loc, _) -> loc)
                ~comments:self#comments
                items)
        | Pstr_attribute a -> self#floating_attribute a
        | Pstr_extension ((extension, PStr [item]), a) ->
          begin match item.pstr_desc with
            | Pstr_value (rf, l) -> self#bindings ~extension (rf, l)
            | _ ->
                let {stdAttrs; docAttrs} =
                  partitionAttributes ~partDoc:true a
                in
                let layout =
                  self#attach_std_item_attrs ~extension stdAttrs
                     (self#structure_item item)
                in
                makeList ~inline:(true, true) ~break:Always
                  ((List.map self#attribute docAttrs)@[layout])
          end
        | Pstr_extension (e, a) ->
          (* Notice how extensions have attributes - but not every structure
             item does. *)
          self#attach_std_item_attrs a (self#item_extension e)
    ) in
    source_map ~loc:term.pstr_loc item

  method type_extension te =
    let formatOneTypeExtStandard prepend ({ptyext_path} as te) =
      let name = self#longident_loc ptyext_path in
      let item = self#formatOneTypeExt prepend name (atom "+=") te in
      let {stdAttrs; docAttrs} =
        partitionAttributes ~partDoc:true te.ptyext_attributes
      in
      let layout = self#attach_std_item_attrs stdAttrs item in
      self#attachDocAttrsToLayout
        ~stdAttrs
        ~docAttrs
        ~loc:ptyext_path.loc
        ~layout
        ()
    in
    formatOneTypeExtStandard (atom "type") te

  (* [allowUnguardedSequenceBodies] allows sequence expressions {} to the right of `=>` to not
     be guarded in `{}` braces. *)
  method case_list ?(allowUnguardedSequenceBodies=false) l =
    let rec appendLabelToLast items rhs =
      match items with
        | hd::[] -> (label ~indent:0 ~space:true hd rhs)::[]
        | hd::tl -> hd::(appendLabelToLast tl rhs)
        | [] -> raise (NotPossible "Cannot append to last of nothing")
    in

    let case_row {pc_lhs; pc_guard; pc_rhs} =
      let theOrs = orList pc_lhs in

      (* match x with *)
      (* | AnotherReallyLongVariantName (_, _, _)   *)
      (* | AnotherReallyLongVariantName2 (_, _, _)
           when true => {                           *)

      (*   }                                        *)

      (*<sbi><X>match x with</X>   *)
      (*     <Y>everythingElse</Y> *)
      (*</sbi>                     *)



      (*     ............................................................
             :    each or segment has a spaced list <> that ties its    :
             : bar "|" to its pattern                                   :
             ...:..........................................................:.....
             :  :  each or-patterned match is grouped in SpacedBreakableInline  :
             :  :                                                          :    :
             v  v                                                          v    v
             <sbi><>|<lb><A><>     FirstThingStandalone t =></A></><B>t</B></lb></></sbi>
             <sbi><>|<C>           AnotherReallyLongVariantName (_, _, _)</C></>
             ^    <>|<lb><><lb><D>AnotherReallyLongVariantNam2 (_, _, _)</D>             (label the last in or ptn for or and label it again for arrow)
             :        ^  ^   ^     <E>when true<E></lb> =></><F>{
             :        :  :   :    </F>}</lb></sbi> ^       ^
             :        :  :   :            ^     ^   :      :
             :        :  :   :            :     :   :      :
             :        :  :   :If there is :a WHERE  :      :
             :        :  :   :an extra    :label is :      :
             :        :  :   :inserted bef:ore the  :      :
             :        :  :   :arrow.      :     :   :      :
             :        :  :   :............:.....:...:      :
             :        :  :                :     :          :
             :        :  :                :     :          :
             :        :  :                :     :          :
             :        :  :The left side of:this final label:
             :        :  :uses a list to  :append the arrow:
             :        :  :................:.....:..........:
             :        :                   :     :
             :        :                   :     :
             :        :                   :     :
             :        :Final or segment is:     :
             :        :wrapped in lbl that:     :
             :        :partitions pattern :     :
             :        :and arrow from     :     :
             :        :expression.        :     :
             :        :                   :     :
             :        :...................:     :
             :     [orsWithWhereAndArrowOnLast] :
             :                                  :
             :..................................:
                         [row]

      *)
      let bar xx = makeList ~postSpace:true [atom "|"; xx] in
      let appendWhereAndArrow p = match pc_guard with
        | None -> makeList ~postSpace:true [p; atom "=>"]
        | Some g ->
          (* when x should break as a whole - extra list added around it to make it break as one *)
          let withWhen = label ~space:true p
              (makeList ~break:Layout.Never ~inline:(true, true) ~postSpace:true
                 [label ~space:true (atom "when") (self#unparseExpr g)])
          in
          makeList ~inline:(true, true) ~postSpace:true [withWhen; atom "=>"]
      in
      let rec appendWhereAndArrowToLastOr = function
        | [] -> []
        | hd::tl ->
          let formattedHd = self#pattern hd in
          let formattedHd =
            if tl == [] then appendWhereAndArrow formattedHd else formattedHd
          in
          (formattedHd :: appendWhereAndArrowToLastOr tl)
      in
      let orsWithWhereAndArrowOnLast = appendWhereAndArrowToLastOr theOrs in
      let rhs =
        if allowUnguardedSequenceBodies then
          match (self#under_pipe#letList pc_rhs) with
          (* TODO: Still render a list with located information here so that
               comments (eol) are interleaved *)
          | [hd] -> hd
          (* In this case, we don't need any additional indentation, because there aren't
             wrapping {} which would cause zero indentation to look strange. *)
          | lst -> makeUnguardedLetSequence lst
        else self#under_pipe#unparseExpr pc_rhs
      in
      source_map
        (* Fake shift the location to accommodate for the bar, to make sure
           * the wrong comments don't make their way past the next bar. *)
        ~loc:(expandLocation ~expand:(0, 0) {
            loc_start = pc_lhs.ppat_loc.loc_start;
            loc_end = pc_rhs.pexp_loc.loc_end;
            loc_ghost = false;
          })
        (makeList ~break:Always_rec ~inline:(true, true)
           (List.map bar (appendLabelToLast orsWithWhereAndArrowOnLast rhs)))
    in
    groupAndPrint
      ~xf:case_row
      ~getLoc:(fun {pc_lhs; pc_rhs} -> {pc_lhs.ppat_loc with loc_end = pc_rhs.pexp_loc.loc_end})
      ~comments:self#comments
      l

  (* Formats a list of a single expr param in such a way that the parens of the function or
   * (poly)-variant application and the wrapping of the param stick together when the layout breaks.
   *  Example: `foo({a: 1, b: 2})` needs to be formatted as
   *  foo({
   *    a: 1,
   *    b: 2
   *  })
   *  when the line length dictates breaking. Notice how `({` and `})` 'hug'.
   *  Also see "isSingleArgParenApplication" which determines if
   *  this kind of formatting should happen. *)
  method singleArgParenApplication ?(wrap=("", "")) ?(uncurried=false) es =
    let (lwrap, rwrap) = wrap in
    let lparen = lwrap ^ (if uncurried then  "(. " else "(") in
    let rparen = ")" ^ rwrap in
    match es with
    | [{pexp_attributes = []; pexp_desc = Pexp_record (l, eo)}] ->
      self#unparseRecord ~wrap:(lparen, rparen) l eo
    | [{pexp_attributes = []; pexp_desc = Pexp_tuple l}] ->
      self#unparseSequence ~wrap:(lparen, rparen) ~construct:`Tuple l
    | [{pexp_attributes = []; pexp_desc = Pexp_array l}] ->
      self#unparseSequence ~wrap:(lparen, rparen) ~construct:`Array l
    | [{pexp_attributes = []; pexp_desc = Pexp_object cs}] ->
      self#classStructure ~wrap:(lparen, rparen) cs
    | [{pexp_attributes = []; pexp_desc = Pexp_extension (s, p)}] when s.txt = "bs.obj" ->
      self#formatBsObjExtensionSugar ~wrap:(lparen, rparen) p
    | [({pexp_attributes = []} as exp)] when (is_simple_list_expr exp) ->
          (match view_expr exp with
          | `list xs ->
              self#unparseSequence ~construct:`List ~wrap:(lparen, rparen) xs
          | `cons xs ->
              self#unparseSequence ~construct:`ES6List ~wrap:(lparen, rparen) xs
          | _ -> assert false)
    | _ -> assert false

  method formatSingleArgLabelApplication labelTerm rightExpr =
    let layout_right = match rightExpr with
    | {pexp_desc = Pexp_let _} ->
      makeLetSequence ~wrap:("({", "})") (self#letList rightExpr)
    | e when isSingleArgParenApplication [rightExpr] ->
      self#singleArgParenApplication [e]
    | {pexp_desc = Pexp_construct ( {txt= Lident"()"},_)} ->
      (* special case unit such that we don't end up with double parens *)
      self#simplifyUnparseExpr rightExpr
    | _ ->
      formatPrecedence (self#unparseExpr rightExpr)
    in
    label labelTerm layout_right

  method label_x_expression_param (l, e) =
    let term = self#unparseProtectedExpr e in
    let param = match (l, e) with
      | (Nolabel, _) -> term
      | (Labelled lbl, _) when Reason_heuristics.is_punned_labelled_expression e lbl ->
        makeList [atom namedArgSym; term]
      | (Optional lbl, _) when Reason_heuristics.is_punned_labelled_expression e lbl ->
        makeList [atom namedArgSym; label term (atom "?")]
      | (Labelled lbl, _) ->
        label (atom (namedArgSym ^ lbl ^ "=")) term
      | (Optional lbl, _) ->
        label (atom (namedArgSym ^ lbl ^ "=?")) term
    in
    source_map ~loc:e.pexp_loc param

  method label_x_expression_params ?wrap ?(uncurried=false) xs =
    match xs with
      (* function applications with unit as only argument should be printed differently
       * e.g. print_newline(()) should be printed as print_newline() *)
      | [(Nolabel, {pexp_attributes = []; pexp_desc = Pexp_construct ( {txt= Lident "()"}, None)})]
          -> makeList
              ~break:Never
              ?wrap
              [if uncurried then atom "(.)" else atom "()"]

      (* The following cases provide special formatting when there's only one expr_param that is a tuple/array/list/record etc.
       *  e.g. foo({a: 1, b: 2})
       *  becomes ->
       *  foo({
       *    a: 1,
       *    b: 2,
       *  })
       *  when the line-length indicates breaking.
       *)
      | [(Nolabel, exp)] when isSingleArgParenApplication [exp] ->
          self#singleArgParenApplication ?wrap ~uncurried [exp]
      | params ->
          makeTup ?wrap ~uncurried (List.map self#label_x_expression_param params)

  (*
   * Prefix represents an optional layout. When passed it will be "prefixed" to
   * the funExpr. Example, given `bar(x, y)` with prefix `foo`, we get
   * foobar(x,y). When the arguments break, the closing `)` is nicely aligned
   * on the height of the prefix:
   *  foobar(
   *    x,
   *    y,
   *  )  --> notice how `)` sits on the height of `foo` instead of `bar`
   *
   *  ~wrap -> represents optional "wrapping", might be useful in context of jsx
   *  where braces are required:
   * prop={bar(   -> `{` is formatted before the funExpr
   *   x,
   *   y,
   * )}      -> notice how the closing brace hugs: `)}`
   *)
  method formatFunAppl ?(prefix=(atom "")) ?(wrap=("", "")) ~jsxAttrs ~args ~funExpr ~applicationExpr ?(uncurried=false) () =
    let (leftWrap, rightWrap) = wrap in
    let uncurriedApplication = uncurried in
    (* If there was a JSX attribute BUT JSX component wasn't detected,
       that JSX attribute needs to be pretty printed so it doesn't get
       lost *)
    let maybeJSXAttr = List.map self#attribute jsxAttrs in
    let categorizeFunApplArgs args =
      let reverseArgs = List.rev args in
      match reverseArgs with
      | ((_, {pexp_desc = Pexp_fun _}) as callback)::args
          when
            [] == List.filter (fun (_, e) -> match e.pexp_desc with Pexp_fun _ -> true | _ -> false) args
          (* default to normal formatting if there's more than one callback *)
          -> `LastArgIsCallback(callback, List.rev args)
      | _ -> `NormalFunAppl args
    in
    let formattedFunExpr = match funExpr.pexp_desc with
      (* pipe first chain or sharpop chain as funExpr, no parens needed, we know how to parse *)
      | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident s}}, _)
        when requireNoSpaceFor s ->
        self#unparseExpr funExpr
      | Pexp_field _ -> self#unparseExpr funExpr
      | _ -> self#simplifyUnparseExpr funExpr
    in
    let formattedFunExpr = makeList [prefix; atom leftWrap; formattedFunExpr] in
    begin match categorizeFunApplArgs args with
    | `LastArgIsCallback(callbackArg, args) ->
        (* This is the following case:
         * Thing.map(foo, bar, baz, (abc, z) =>
         *   MyModuleBlah.toList(argument)
         *)
        let (argLbl, cb) = callbackArg in
        let {stdAttrs; uncurried} = partitionAttributes cb.pexp_attributes in
        let cbAttrs = stdAttrs in
        if uncurried then Hashtbl.add uncurriedTable cb.pexp_loc true;
        let (cbArgs, retCb) = self#curriedPatternsAndReturnVal {cb with pexp_attributes = []} in
        let cbArgs = if cbAttrs != [] then
            makeList ~break:IfNeed ~inline:(true, true) ~postSpace:true
              (List.map self#attribute cbAttrs @ cbArgs)
        else makeList cbArgs in
        let theCallbackArg = match argLbl with
          | Optional s -> makeList ([atom namedArgSym; atom s; atom "=?"]@[cbArgs])
          | Labelled s -> makeList ([atom namedArgSym; atom s; atom "="]@[cbArgs])
          | Nolabel -> cbArgs
        in
        let theFunc =
          source_map ~loc:funExpr.pexp_loc
            (makeList
               ~wrap:("", (if uncurriedApplication then "(." else "("))
               [formattedFunExpr])
        in
        let formattedFunAppl = begin match self#letList retCb with
        | [x] ->
          (* force breaks for test assertion style callbacks, e.g.
           *  describe("App", () => test("math", () => Expect.expect(1 + 2) |> toBe(3)));
           * should always break for readability of the tests:
           *  describe("App", () =>
           *    test("math", () =>
           *      Expect.expect(1 + 2) |> toBe(3)
           *    )
           *  );
           *)
          let forceBreak = match funExpr.pexp_desc with
          | Pexp_ident ident when
              let lastIdent = Longident.last ident.txt in
              List.mem lastIdent ["test"; "describe"; "it"; "expect"] -> true
          | _ -> false
          in
          let (leftWrap, rightWrap) as wrap = ("=> ", ")" ^ rightWrap) in
          let wrap = if self#should_preserve_requested_braces retCb then
            (leftWrap ^ "{", "}" ^ rightWrap)
          else
            wrap
          in
          let returnValueCallback =
            makeList
              ~break:(if forceBreak then Always else IfNeed)
              ~wrap
              [x]
          in
          let argsWithCallbackArgs = List.concat [(List.map self#label_x_expression_param args); [theCallbackArg]] in
          let left = label
            theFunc
            (makeList
               ~pad:(uncurriedApplication, false)
               ~wrap:("", " ") ~break:IfNeed ~inline:(true, true) ~sep:(Sep ",") ~postSpace:true
              argsWithCallbackArgs)
          in
          label left returnValueCallback
        | xs ->
          let printWidthExceeded = Reason_heuristics.funAppCallbackExceedsWidth ~printWidth:settings.width ~args ~funExpr () in
          if not printWidthExceeded then
              (*
               * Thing.map(foo, bar, baz, (abc, z) =>
               *   MyModuleBlah.toList(argument)
               * )
               *
               * To get this kind of formatting we need to construct the following tree:
               * <Label>
               * <left>Thing.map(foo, bar, baz, (abc, z)</left><right>=>
               *   MyModuleBlah.toList(argument)
               * )</right>
               * </Label>
               *
               * where left is
               * <Label><left>Thing.map(</left></right>foo, bar, baz, (abc, z) </right></Label>
               *
               * The <right> part of that label could be a <List> with wrap:("", " ") break:IfNeed inline:(true, true)
               * with items: "foo", "bar", "baz", "(abc, z)", separated by commas.
               *
               * this is also necessary to achieve the following formatting where }) hugs :
               * test("my test", () => {
               *   let x = a + b;
               *   let y = z + c;
               *   x + y
               * });
               *)
            let (leftWrap, rightWrap) as wrap = ("=> ", ")" ^ rightWrap) in
            let wrap = if self#should_preserve_requested_braces retCb then
              (leftWrap ^ "{", "}" ^ rightWrap)
            else
              wrap
            in
            let right =
              source_map ~loc:retCb.pexp_loc
                (makeList ~break:Always_rec ~wrap ~sep:(SepFinal (";", ";")) xs)
            in
            let argsWithCallbackArgs =
              List.map self#label_x_expression_param args @ [theCallbackArg]
            in
            let left = label
                theFunc
                (makeList ~wrap:("", " ") ~break:IfNeed ~inline:(true, true) ~sep:(Sep ",") ~postSpace:true
                   argsWithCallbackArgs)
            in
            label left right
          else
            (* Since the heuristic says the line length is exceeded in this case,
             * we conveniently format everything as
             * <label><left>Thing.map(</left><right><list>
             *   foo,
             *   bar,
             *   baz,
             *   <label> <left>(abc) =></left> <right><list> {
             *     let x = 1;
             *     let y = 2;
             *     x + y
             *   }</list></right></label>
             * )</list></right></label>
            *)
            let args =
              makeList ~break:Always ~wrap:("", ")" ^ rightWrap) ~sep:commaTrail (
                (List.map self#label_x_expression_param args) @
                [label ~space:true (makeList ~wrap:("", " =>") [theCallbackArg])
                   (source_map ~loc:retCb.pexp_loc (makeLetSequence xs))]
              )
            in
            (* This will need to be (theFunc, args) *)
            label theFunc args
        end in
        maybeJSXAttr @ [formattedFunAppl]
    | `NormalFunAppl args ->
      let theFunc =
        source_map ~loc:funExpr.pexp_loc formattedFunExpr
      in
      (* reset here only because [function,match,try,sequence] are lower priority *)
      (* The "expression location" might be different than the location of the actual
       * function application because things like surrounding { } expand the
       * parsed location (in body of while loop for example).
       * We recover the most meaningful function application location we can.*)
      let (syntheticApplicationLocation, syntheticArgLoc) = match args with
        | [] -> (funExpr.pexp_loc, funExpr.pexp_loc)
        | _::_ ->
          {funExpr.pexp_loc with loc_end = applicationExpr.pexp_loc.loc_end},
          {funExpr.pexp_loc with loc_start = funExpr.pexp_loc.loc_end; loc_end = applicationExpr.pexp_loc.loc_end}
      in
      let theArgs = self#reset#label_x_expression_params ~wrap:("", rightWrap) ~uncurried args in
      maybeJSXAttr @ [source_map ~loc:syntheticApplicationLocation
                        (label theFunc (source_map ~loc:syntheticArgLoc theArgs))]
    end
end;;

let toplevel_phrase ppf x =
  match x with
  | Ptop_def s -> format_layout ppf (printer#structure s)
  | Ptop_dir _ -> print_string "(* top directives not supported *)"

let case_list ppf x =
  List.iter (format_layout ppf) (printer#case_list x)

(* Convert a Longident to a list of strings.
   E.g. M.Constructor will be ["Constructor"; "M.Constructor"]
   Also support ".Constructor" to specify access without a path.
 *)
let longident_for_arity lid =
  let rec toplevel = function
    | Lident s ->
        [s]
    | Ldot (lid, s) ->
        let append_s x = x ^ "." ^ s in
        s :: (List.map append_s (toplevel lid))
    | Lapply (_,s) ->
        toplevel s in
   match lid with
    | Lident s ->
        ("." ^ s) :: toplevel lid
    | _ ->
        toplevel lid

(* add expilcit_arity to a list of attributes
 *)
let add_explicit_arity loc attributes =
  { attr_name = { txt="explicit_arity"; loc }
  ; attr_payload = PStr []
  ; attr_loc = loc
  } :: normalized_attributes "explicit_arity" attributes

(* explicit_arity_exists check if expilcit_arity exists
 *)
let explicit_arity_not_exists attributes =
  not (attribute_exists "explicit_arity" attributes)

(* wrap_expr_with_tuple wraps an expression
 * with tuple as a sole argument.
 *)
let wrap_expr_with_tuple exp =
  {exp with pexp_desc = Pexp_tuple [exp]}

(* wrap_pat_with_tuple wraps an pattern
 * with tuple as a sole argument.
 *)
let wrap_pat_with_tuple pat =
  {pat with ppat_desc = Ppat_tuple [pat]}



(* explicit_arity_constructors is a set of constructors that are known to have
 * multiple arguments
 *
 *)

module StringSet = Set.Make(String);;

let built_in_explicit_arity_constructors = ["Some"; "Assert_failure"; "Match_failure"]

let explicit_arity_constructors = StringSet.of_list(built_in_explicit_arity_constructors @ (!configuredSettings).constructorLists)

let add_explicit_arity_mapper super =
  let super_expr = super.Ast_mapper.expr in
  let super_pat = super.Ast_mapper.pat in
  let expr mapper expr =
    let expr =
      match expr with
      | {pexp_desc=Pexp_construct(lid, Some sp);
         pexp_loc;
         pexp_attributes} when
          List.exists
            (fun c -> StringSet.mem c explicit_arity_constructors)
            (longident_for_arity lid.txt) &&
          explicit_arity_not_exists pexp_attributes ->
        {pexp_desc=Pexp_construct(lid, Some (wrap_expr_with_tuple sp));
         pexp_loc;
         pexp_attributes=add_explicit_arity pexp_loc pexp_attributes;
         pexp_loc_stack = []}
      | x -> x
    in
    super_expr mapper expr
  and pat mapper pat =
    let pat =
      match pat with
      | {ppat_desc=Ppat_construct(lid, Some sp);
         ppat_loc;
         ppat_attributes} when
          List.exists
            (fun c -> StringSet.mem c explicit_arity_constructors)
            (longident_for_arity lid.txt) &&
          explicit_arity_not_exists ppat_attributes ->
        {ppat_desc=Ppat_construct(lid, Some (wrap_pat_with_tuple sp));
         ppat_loc;
         ppat_attributes=add_explicit_arity ppat_loc ppat_attributes;
         ppat_loc_stack = []}
      | x -> x
    in
    super_pat mapper pat
  in
  { super with Ast_mapper. expr; pat }

let preprocessing_mapper =
  ml_to_reason_swap_operator_mapper
    (escape_stars_slashes_mapper
      (add_explicit_arity_mapper Ast_mapper.default_mapper))

let core_type ppf x =
  format_layout ppf
    (printer#core_type (apply_mapper_to_type x preprocessing_mapper))

let pattern ppf x =
  format_layout ppf
    (printer#pattern (apply_mapper_to_pattern x preprocessing_mapper))

let signature (comments : Comment.t list) ppf x =
  List.iter (fun comment -> printer#trackComment comment) comments;
  format_layout ppf ~comments
    (printer#signature (apply_mapper_to_signature x preprocessing_mapper))

let structure (comments : Comment.t list) ppf x =
  List.iter (fun comment -> printer#trackComment comment) comments;
  format_layout ppf ~comments
    (printer#structure (apply_mapper_to_structure x preprocessing_mapper))

let expression ppf x =
  format_layout ppf
    (printer#unparseExpr (apply_mapper_to_expr x preprocessing_mapper))

let case_list = case_list

end
in
object
  method core_type = Formatter.core_type
  method pattern = Formatter.pattern
  method signature = Formatter.signature
  method structure = Formatter.structure
  (* For merlin-destruct *)
  method toplevel_phrase = Formatter.toplevel_phrase
  method expression = Formatter.expression
  method case_list = Formatter.case_list
end
