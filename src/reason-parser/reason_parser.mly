/*
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
 */

/* The parser definition */

%{
open Migrate_parsetree.OCaml_404.Ast

(*
   TODO:
   - Remove open Migrate_parsetree.OCaml_404.Ast of the top of this file and
   fix all compilation issues. Use long indentifiers to make everything easier
   to reason about. Do it with care to avoid conflicts.
*)

(**

   location.ml:
   ------------
   let mkloc txt loc = { txt ; loc }
   let rhs_loc n = {
     loc_start = Parsing.rhs_start_pos n;
     loc_end = Parsing.rhs_end_pos n;
     loc_ghost = false;
   }
   let symbol_rloc () = {
     loc_start = Parsing.symbol_start_pos ();
     loc_end = Parsing.symbol_end_pos ();
     loc_ghost = false;
   }

   let symbol_gloc () = {
     loc_start = Parsing.symbol_start_pos ();
     loc_end = Parsing.symbol_end_pos ();
     loc_ghost = true;
   }

   ast_helper.ml:
   ------------
   module Typ = struct
    val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
    let mk ?(loc = !default_loc) ?(attrs = []) d =
       {Parsetree.ptyp_desc = d; Parsetree.ptyp_loc = loc; ptyp_attributes = attrs}
     ..
   end

   parse_tree.mli
   --------------
   and core_type = {
     Parsetree.ptyp_desc: core_type_desc;
     Parsetree.ptyp_loc: Location.t;
     ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
   }

   and core_type_desc =
     | Parsetree.Ptyp_any
           (*  _ *)
     | Ptyp_var of string
           (* 'a *)
     | Ptyp_arrow of label * core_type * core_type
           (* T1 -> T2       (label = "")
              ~l:T1 -> T2    (label = "l")
              ?l:T1 -> T2    (label = "?l")
            *)
     | Ptyp_tuple of core_type list
           (* T1 * ... * Tn   (n >= 2) *)

   reason_parser.mly
   ---------------
   In general:

                                          syntax variant          {pblah_desc: core_blah_desc
                                                                   pblah_loc: {txt, loc}
                                                                   pblah_attributes: ... }
                                         /              \            /       \
   val mkblah: ~loc -> ~attributes ->     core_blah_desc     ->      core_blah
   let mkblah = Blah.mk

*)


let dummy_loc () = {
  Location.loc_start = Lexing.dummy_pos;
  Location.loc_end = Lexing.dummy_pos;
  Location.loc_ghost = false;
}

let mklocation loc_start loc_end = {
  Location.loc_start = loc_start;
  Location.loc_end = loc_end;
  Location.loc_ghost = false;
}

let with_txt a txt = {
    a with Asttypes.txt = txt;
}

let make_real_loc loc = {
    loc with Location.loc_ghost = false
}

let make_ghost_loc loc = {
    loc with Location.loc_ghost = true
}

let ghloc ?(loc=dummy_loc ()) d = { Asttypes.txt = d; loc = (make_ghost_loc loc) }

(**
  * turn an object into a real
  *)
let make_real_exp exp = {
    exp with Parsetree.pexp_loc = make_real_loc exp.Parsetree.pexp_loc
}
let make_real_pat pat = {
    pat with Parsetree.ppat_loc = make_real_loc pat.Parsetree.ppat_loc
}
let make_real_cf cf = {
    cf with Parsetree.pcf_loc = make_real_loc cf.Parsetree.pcf_loc
}

(**
  * turn a object into ghost
  *)
let make_ghost_cf cf = {
    cf with Parsetree.pcf_loc = make_ghost_loc cf.Parsetree.pcf_loc
}
let make_ghost_exp exp = {
    exp with Parsetree.pexp_loc = make_ghost_loc exp.Parsetree.pexp_loc
}

let make_ghost_pat pat = {
    pat with Parsetree.ppat_loc = make_ghost_loc pat.Parsetree.ppat_loc
}

(**
  * change the location state to be a ghost location or real location
  *)
let set_loc_state is_ghost loc =
    if is_ghost then make_ghost_loc loc else make_real_loc loc

let mktyp ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Typ.mk ~loc d

let mkpat ?(attrs=[]) ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Pat.mk ~loc ~attrs d

let mkexp ?(attrs=[]) ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Exp.mk ~loc ~attrs d

let mkmty ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Mty.mk ~loc d

let mksig ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Sig.mk ~loc d

let mkmod ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Mod.mk ~loc d

let mkstr ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Str.mk ~loc d

let mkclass ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Cl.mk ~loc d

let mkcty ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Cty.mk ~loc d

let mkctf ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Ctf.mk ~loc d

let may_tuple startp endp = function
  | []  -> assert false
  | [x] -> {x with Parsetree.pexp_loc = mklocation startp endp}
  | xs  -> mkexp ~loc:(mklocation startp endp) (Pexp_tuple xs)

(**
  Make a core_type from a as_loc(LIDENT).
  Useful for record type punning.
  type props = {width: int, height: int};
  type state = {nbrOfClicks: int};
  type component = {props, state};
*)
let mkct lbl =
  let lident = Longident.Lident lbl.Asttypes.txt in
  let ttype = Parsetree.Ptyp_constr({txt = lident; loc = lbl.loc}, []) in
  {Parsetree.ptyp_desc = ttype; Parsetree.ptyp_loc = lbl.loc; ptyp_attributes = []}

let mkcf ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ast_helper.Cf.mk ~loc d

let simple_ghost_text_attr ?(loc=dummy_loc ()) txt =
  let loc = set_loc_state true loc in
  [({Asttypes.txt; loc}, Parsetree.PStr [])]

let mkExplicitArityTuplePat ?(loc=dummy_loc ()) pat =
  (* Tell OCaml type system that what this tuple construction represents is
     not actually a tuple, and should represent several constructor
     arguments.  This allows the syntax the ability to distinguish between:

     X (10, 20)  -- One argument constructor
     X 10 20     -- Multi argument constructor
  *)
  mkpat
    ~loc
    ~attrs:(simple_ghost_text_attr ~loc "explicit_arity")
    pat

let mkExplicitArityTupleExp ?(loc=dummy_loc ()) exp =
  mkexp
    ~loc
    ~attrs:(simple_ghost_text_attr ~loc "explicit_arity")
    exp

let is_pattern_list_single_any = function
  | [{Parsetree.ppat_desc=Ppat_any; ppat_attributes=[]} as onlyItem] -> Some onlyItem
  | _ -> None

let set_structure_item_location x loc = {x with Parsetree.pstr_loc = loc};;

let mkoperator {Location.txt; loc} =
  Ast_helper.Exp.mk ~loc (Parsetree.Pexp_ident(Location.mkloc (Longident.Lident txt) loc))

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.

  jordwalke: Noticed that ghost expressions are often used when inserting
   additional AST nodes from a parse rule. Either an extra wrapping one, or an
   additional inner node. This is consistent with the above description, I
   believe.
*)


let ghunit ?(loc=dummy_loc ()) () =
  mkexp ~ghost:true ~loc (Pexp_construct (Location.mknoloc (Longident.Lident "()"), None))

let mkinfixop arg1 op arg2 =
  mkexp(Pexp_apply(op, [Asttypes.Nolabel, arg1; Asttypes.Nolabel, arg2]))

let mkinfix arg1 name arg2 =
  mkinfixop arg1 (mkoperator name) arg2

let neg_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus name arg =
  match name.Asttypes.txt, arg.Parsetree.pexp_desc with
  | "-", Pexp_constant(Pconst_integer (n,m)) ->
      mkexp(Pexp_constant(Pconst_integer(neg_string n,m)))
  | ("-" | "-."), Pexp_constant(Pconst_float (f, m)) ->
      mkexp(Pexp_constant(Pconst_float(neg_string f, m)))
  | txt, _ ->
      let name = {name with Asttypes.txt = "~" ^ txt} in
      mkexp(Pexp_apply(mkoperator name, [Nolabel, arg]))

let prepare_functor_arg = function
  | Some name, mty -> (name, mty)
  | None, (Some {Parsetree.pmty_loc} as mty) ->
      (Location.mkloc "_" (make_ghost_loc pmty_loc), mty)
  | None, None -> assert false

let mk_functor_mod args body =
  let folder arg acc =
    let name, mty = prepare_functor_arg arg.Asttypes.txt in
    mkmod ~loc:arg.loc (Pmod_functor(name, mty, acc))
  in
  List.fold_right folder args body

let mk_functor_mty args body =
  let folder arg acc =
    let name, mty = prepare_functor_arg arg.Asttypes.txt in
    mkmty ~loc:arg.loc (Pmty_functor(name, mty, acc))
  in
  List.fold_right folder args body

let mkuplus name arg =
  match name.Asttypes.txt, arg.Parsetree.pexp_desc with
  | "+", Pexp_constant(Pconst_integer _)
  | ("+" | "+."), Pexp_constant(Pconst_float _) ->
      mkexp arg.Parsetree.pexp_desc
  | txt, _ ->
      let name = {name with Asttypes.txt = "~" ^ txt} in
      mkexp(Pexp_apply(mkoperator name, [Nolabel, arg]))

let mkexp_cons consloc args loc =
  mkexp ~loc (Pexp_construct(Location.mkloc (Longident.Lident "::") consloc, Some args))

let mkexp_constructor_unit consloc loc =
  mkexp ~loc (Pexp_construct(Location.mkloc (Longident.Lident "()") consloc, None))

let ghexp_cons consloc args loc =
  mkexp ~ghost:true ~loc (Pexp_construct(Location.mkloc (Longident.Lident "::") loc, Some args))

let mkpat_cons consloc args loc =
  mkpat ~loc (Ppat_construct(Location.mkloc (Longident.Lident "::") loc, Some args))

let ghpat_cons consloc args loc =
  mkpat ~ghost:true ~loc (Ppat_construct(Location.mkloc (Longident.Lident "::") loc, Some args))

let mkpat_constructor_unit consloc loc =
  mkpat ~loc (Ppat_construct(Location.mkloc (Longident.Lident "()") consloc, None))

let simple_pattern_list_to_tuple ?(loc=dummy_loc ()) = function
  | [] -> assert false
  | lst -> mkpat ~loc (Ppat_tuple lst)

let mktailexp_extension loc seq ext_opt =
  let rec handle_seq = function
    | [] ->
        let base_case = match ext_opt with
          | Some ext ->
            ext
          | None ->
            let loc = make_ghost_loc loc in
            let nil = { Asttypes.txt = Longident.Lident "[]"; loc } in
            Ast_helper.Exp.mk ~loc (Pexp_construct (nil, None)) in
        base_case
    | e1 :: el ->
        let exp_el = handle_seq el in
        let loc = mklocation e1.Parsetree.pexp_loc.loc_start exp_el.Parsetree.pexp_loc.loc_end in
        let arg = mkexp ~ghost:true ~loc (Pexp_tuple [e1; exp_el]) in
        ghexp_cons loc arg loc
  in
  handle_seq seq

let mktailpat_extension loc (seq, ext_opt) =
  let rec handle_seq = function
    [] ->
      let base_case = match ext_opt with
        | Some ext ->
          ext
        | None ->
          let loc = make_ghost_loc loc in
          let nil = { Asttypes.txt = Longident.Lident "[]"; loc } in
          mkpat ~loc (Ppat_construct (nil, None)) in
      base_case
  | p1 :: pl ->
      let pat_pl = handle_seq pl in
      let loc = mklocation p1.Parsetree.ppat_loc.loc_start pat_pl.Parsetree.ppat_loc.loc_end in
      let arg = mkpat ~ghost:true ~loc (Ppat_tuple [p1; pat_pl]) in
      ghpat_cons loc arg loc in
  handle_seq seq

let makeFrag loc body =
  let attribute = ({Asttypes.txt = "JSX"; loc = loc}, Parsetree.PStr []) in
  { body with Parsetree.pexp_attributes = attribute :: body.Parsetree.pexp_attributes }


(* Applies attributes to the structure item, not the expression itself. Makes
 * structure item have same location as expression. *)
let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); Parsetree.pstr_loc = e.Parsetree.pexp_loc }

let ghexp_constraint loc e (t1, t2) =
  match t1, t2 with
  | Some t, None -> mkexp ~ghost:true ~loc (Pexp_constraint(e, t))
  | _, Some t -> mkexp ~ghost:true ~loc (Pexp_coerce(e, t1, t))
  | None, None -> assert false

let array_function ?(loc=dummy_loc()) str name =
  ghloc ~loc (Longident.Ldot(Longident.Lident str, (if !Clflags.fast then "unsafe_" ^ name else name)))

let syntax_error_str loc msg =
  if !Reason_config.recoverable then
    Ast_helper.Str.mk ~loc:loc (Pstr_extension (Syntax_util.syntax_error_extension_node loc msg, []))
  else
    raise(Syntaxerr.Error(Syntaxerr.Other loc))

let syntax_error () =
  raise Syntaxerr.Escape_error

let syntax_error_exp loc msg =
  if !Reason_config.recoverable then
    Ast_helper.Exp.mk ~loc (Pexp_extension (Syntax_util.syntax_error_extension_node loc msg))
  else
    syntax_error ()

let syntax_error_pat loc msg =
  if !Reason_config.recoverable then
    Ast_helper.Pat.extension ~loc (Syntax_util.syntax_error_extension_node loc msg)
  else
    syntax_error ()

let syntax_error_typ loc msg =
  if !Reason_config.recoverable then
    Ast_helper.Typ.extension ~loc (Syntax_util.syntax_error_extension_node loc msg)
  else
    syntax_error ()

let syntax_error_mod loc msg =
  if !Reason_config.recoverable then
    Ast_helper.Mty.extension ~loc (Syntax_util.syntax_error_extension_node loc msg)
  else
    syntax_error ()

let unclosed opening closing =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(opening.Asttypes.loc, opening.txt,
                                           closing.Asttypes.loc, closing.txt)))

let unclosed_extension closing =
  Syntax_util.syntax_error_extension_node closing.Asttypes.loc ("Expecting \"" ^ closing.txt ^ "\"")

let unclosed_mod opening closing =
  if !Reason_config.recoverable then
    mkmod(Pmod_extension (unclosed_extension closing))
  else
    unclosed opening closing

let unclosed_cl opening closing =
  if !Reason_config.recoverable then
    mkclass(Pcl_extension (unclosed_extension closing))
  else
    unclosed opening closing

let unclosed_mty opening closing =
  if !Reason_config.recoverable then
    mkmty(Pmty_extension (unclosed_extension closing))
  else
    unclosed opening closing

let unclosed_cty opening closing =
  if !Reason_config.recoverable then
    mkcty(Pcty_extension (unclosed_extension closing))
  else
    unclosed opening closing

let unclosed_exp opening closing =
  if !Reason_config.recoverable then
    mkexp(Pexp_extension (unclosed_extension closing))
  else
    unclosed opening closing

let unclosed_pat opening closing =
  if !Reason_config.recoverable then
    mkpat(Ppat_extension (unclosed_extension closing))
  else
    unclosed opening closing

let expecting nonterm =
    raise Syntaxerr.(Error(Expecting(nonterm.Asttypes.loc, nonterm.txt)))

let expecting_pat nonterm =
  if !Reason_config.recoverable then
    mkpat(Ppat_extension (Syntax_util.syntax_error_extension_node nonterm.Asttypes.loc ("Expecting " ^ nonterm.txt)))
  else
    expecting nonterm

let not_expecting start_pos end_pos nonterm =
    raise Syntaxerr.(Error(Not_expecting(mklocation start_pos end_pos, nonterm)))

type labelled_parameter =
  | Term of Asttypes.arg_label * Parsetree.expression option * Parsetree.pattern
  | Type of string

let mkexp_fun {Location.txt; loc} body =
  let loc = mklocation loc.loc_start body.Parsetree.pexp_loc.loc_end in
  match txt with
  | Term (label, default_expr, pat) ->
    Ast_helper.Exp.fun_ ~loc label default_expr pat body
  | Type str ->
    Ast_helper.Exp.newtype ~loc str body

let mkclass_fun {Location.txt ; loc} body =
  let loc = mklocation loc.loc_start body.Parsetree.pcl_loc.loc_end in
  match txt with
  | Term (label, default_expr, pat) ->
    Ast_helper.Cl.fun_ ~loc label default_expr pat body
  | Type str ->
    let pat = syntax_error_pat loc "(type) not allowed in classes" in
    Ast_helper.Cl.fun_ ~loc Nolabel None pat body

let mktyp_arrow {Location.txt = (label, cod); loc} dom =
  mktyp ~loc:(mklocation loc.loc_start dom.Parsetree.ptyp_loc.loc_end)
    (Ptyp_arrow (label, cod, dom))

let mkcty_arrow {Location.txt = (label, cod); loc} dom =
  mkcty ~loc:(mklocation loc.loc_start dom.Parsetree.pcty_loc.loc_end)
    (Pcty_arrow (label, cod, dom))

let mkexp_app_rev startp endp (body, args) =
  let loc = mklocation startp endp in
  if args = [] then { body with Parsetree.pexp_loc = loc } else
    mkexp ~loc (Pexp_apply (body, List.rev args))

let mkmod_app mexp marg =
  mkmod ~loc:(mklocation mexp.Parsetree.pmod_loc.loc_start marg.Parsetree.pmod_loc.loc_end)
    (Pmod_apply (mexp, marg))

let bigarray_function ?(loc=dummy_loc()) str name =
  ghloc ~loc (Longident.Ldot(Longident.Ldot(Longident.Lident "Bigarray", str), name))

let bigarray_untuplify = function
    { Parsetree.pexp_desc = Pexp_tuple explist; Parsetree.pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get ?(loc=dummy_loc()) arr arg =
  let get = if !Clflags.fast then "unsafe_get" else "get" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Parsetree.Pexp_ident(bigarray_function ~loc "Array1" get)),
                       [Nolabel, arr; Nolabel, c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Parsetree.Pexp_ident(bigarray_function ~loc "Array2" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Parsetree.Pexp_ident(bigarray_function ~loc "Array3" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3]))
  | coords ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Parsetree.Pexp_ident(bigarray_function ~loc "Genarray" "get")),
                       [Nolabel, arr; Nolabel, mkexp ~ghost:true ~loc (Pexp_array coords)]))

let bigarray_set ?(loc=dummy_loc()) arr arg newval =
  let set = if !Clflags.fast then "unsafe_set" else "set" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Parsetree.Pexp_ident(bigarray_function ~loc "Array1" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Parsetree.Pexp_ident(bigarray_function ~loc "Array2" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Parsetree.Pexp_ident(bigarray_function ~loc "Array3" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3; Nolabel, newval]))
  | coords ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Parsetree.Pexp_ident(bigarray_function ~loc "Genarray" "set")),
                       [Nolabel, arr;
                        Nolabel, mkexp ~ghost:true ~loc (Pexp_array coords);
                        Nolabel, newval]))

let exp_of_label label =
  mkexp ~loc:label.Asttypes.loc (Parsetree.Pexp_ident {label with txt=Longident.Lident(Longident.last label.txt)})

let pat_of_label label =
  mkpat ~loc:label.Asttypes.loc (Parsetree.Ppat_var {label with txt=(Longident.last label.txt)})

let check_variable vl loc v =
  if List.mem v vl then
    raise Syntaxerr.(Error(Variable_in_scope(loc,v)))

let varify_constructors var_names t =
  let rec loop t =
    let desc =
      match t.Parsetree.ptyp_desc with
      | Parsetree.Ptyp_any -> Parsetree.Ptyp_any
      | Ptyp_var x ->
          check_variable var_names t.Parsetree.ptyp_loc x;
          Ptyp_var x
      | Ptyp_arrow (label,core_type,core_type') ->
          Ptyp_arrow(label, loop core_type, loop core_type')
      | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
      | Parsetree.Ptyp_constr( { txt = Longident.Lident s }, []) when List.mem s var_names ->
          Ptyp_var s
      | Parsetree.Ptyp_constr(longident, lst) ->
          Parsetree.Ptyp_constr(longident, List.map loop lst)
      | Ptyp_object (lst, o) ->
          Ptyp_object
            (List.map (fun (s, attrs, t) -> (s, attrs, loop t)) lst, o)
      | Ptyp_class (longident, lst) ->
          Ptyp_class (longident, List.map loop lst)
      | Ptyp_alias(core_type, string) ->
          check_variable var_names t.Parsetree.ptyp_loc string;
          Ptyp_alias(loop core_type, string)
      | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
          Ptyp_variant(List.map loop_row_field row_field_list,
                       flag, lbl_lst_option)
      | Ptyp_poly(string_lst, core_type) ->
          List.iter (check_variable var_names t.Parsetree.ptyp_loc) string_lst;
          Ptyp_poly(string_lst, loop core_type)
      | Ptyp_package(longident,lst) ->
          Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
      | Ptyp_extension (s, arg) ->
          Ptyp_extension (s, arg)
    in
    {t with Parsetree.ptyp_desc = desc}
  and loop_row_field  =
    function
      | Parsetree.Rtag(label,attrs,flag,lst) ->
          Parsetree.Rtag(label,attrs,flag,List.map loop lst)
      | Rinherit t ->
          Rinherit (loop t)
  in
  loop t

let pexp_newtypes ?loc newtypes exp =
  List.fold_right (fun newtype exp -> mkexp ?loc (Pexp_newtype (newtype, exp)))
    newtypes exp

(**
  I believe that wrap_type_annotation will automatically generate the type
  arguments (type a) (type b) based on what was listed before the dot in a
  polymorphic type annotation that uses locally abstract types.
 *)
let wrap_type_annotation newtypes core_type body =
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp = pexp_newtypes newtypes exp in
  let typ = mktyp ~ghost:true (Ptyp_poly(newtypes,varify_constructors newtypes core_type)) in
  (exp, typ)


let struct_item_extension (ext_attrs, ext_id) structure_items =
  mkstr ~ghost:true (Pstr_extension ((ext_id, Parsetree.PStr structure_items), ext_attrs))

let expression_extension (ext_attrs, ext_id) item_expr =
  mkexp ~ghost:true ~attrs:ext_attrs
    (Pexp_extension (ext_id, Parsetree.PStr [mkstrexp item_expr []]))

(* There's no more need for these functions - this was for the following:
 *
 *     fun % ext [@foo] arg => arg;
 *
 *   Becoming
 *
 *     [%ext  (fun arg => arg) [@foo]]
 *
 *   Which we no longer support.
 *)
(* Applies the attributes to the body, then wraps entire thing in an extension
 * expression, whose payload consists of a single structure item that is body
 *)
(* let wrap_exp_attrs body (ext, attrs) = *)
(*   (* todo: keep exact location for the entire attribute *) *)
(*   let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in *)
(*   match ext with *)
(*   | None -> body *)
(*   | Some id -> mkexp ~ghost:true (Pexp_extension (id, Parsetree.PStr [mkstrexp body []])) *)

(* Why not just mkexp with the right attributes in the first place? *)
(* let mkexp_attrs d attrs = *)
(*   wrap_exp_attrs (mkexp d) attrs *)

let mkcf_attrs ?(loc=dummy_loc()) d attrs =
  Ast_helper.Cf.mk ~loc ~attrs d

let mkctf_attrs d attrs =
  Ast_helper.Ctf.mk ~attrs d

type let_binding =
  { lb_pattern: Parsetree.pattern;
    lb_expression: Parsetree.expression;
    (* The meaning of lb_leading_attributes and lbs_extension are dependent on
     * the context of the let binding (module/expression etc) *)
    lb_attributes: Parsetree.attributes;
    (* lb_docs: docs Lazy.t; *)
    (* lb_text: text Lazy.t; *)
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: Asttypes.rec_flag;
    lbs_extension: (Parsetree.attributes * string Asttypes.loc) option;
    lbs_loc: Location.t }

let mklb (p, e) attrs loc =
  { lb_pattern = p;
    lb_expression = e;
    (* Only some individual let bindings are allowed to have attributes
     * depending on the context *)
    lb_attributes = attrs;
    lb_loc = loc; }

let mklbs ext rf lb loc =
  { lbs_bindings = [lb];
    lbs_rec = rf;
    lbs_extension = ext;
    lbs_loc = loc; }

let addlbs lbs lbs' =
  { lbs with lbs_bindings = lbs.lbs_bindings @ lbs' }

let val_of_let_bindings lbs =
  let bindings =
    List.map
      (fun lb ->
         Ast_helper.Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           (* ~docs:(Lazy.force lb.lb_docs) *)
           (* ~text:(Lazy.force lb.lb_text) *)
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
  let str = mkstr (Pstr_value(lbs.lbs_rec, bindings)) in
  match lbs.lbs_extension with
  | None -> str
  | Some ext -> struct_item_extension ext [str]

let expr_of_let_bindings lbs body =
  let bindings =
    List.map
      (fun lb ->
         (* Individual let bindings in an *expression* can't have item attributes. *)
         (*if lb.lb_attributes <> [] then
           raise Syntaxerr.(Error(Not_expecting(lb.lb_loc, "item attribute")));*)
         Ast_helper.Vb.mk ~attrs:lb.lb_attributes ~loc:lb.lb_loc lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
  (* The location of this expression unfortunately includes the entire rule,
   * which will include any preceeding extensions. *)
  let item_expr = mkexp (Pexp_let(lbs.lbs_rec, bindings, body)) in
  match lbs.lbs_extension with
  | None -> item_expr
  | Some ext -> expression_extension ext item_expr

let class_of_let_bindings lbs body =
  let bindings =
    List.map
      (fun lb ->
         (*if lb.lb_attributes <> [] then
           raise Syntaxerr.(Error(Not_expecting(lb.lb_loc, "item attribute")));*)
         Ast_helper.Vb.mk ~attrs:lb.lb_attributes ~loc:lb.lb_loc lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
    if lbs.lbs_extension <> None then
      raise Syntaxerr.(Error(Not_expecting(lbs.lbs_loc, "extension")));
    mkclass(Pcl_let (lbs.lbs_rec, bindings, body))

(*
 * arity_conflict_resolving_mapper is triggered when both "implicit_arity" "explicit_arity"
 * are in the attribtues. In that case we have to remove "explicit_arity"
 *
 * However, if we simply remove explicit_arity, we would end up with a
 * wrapping tuple which has only one component (inner tuple).
 * This is against the invariance where tuples must have 2+ components.
 * Therefore, in the case we have to remove explicit_arity, we also need to
 * unwrap the tuple to expose the inner tuple directly.
 *
 *)
let arity_conflict_resolving_mapper super =
{ super with
  Ast_mapper.expr = begin fun mapper expr ->
    match expr with
      | {Parsetree.pexp_desc=Pexp_construct(lid, args);
         Parsetree.pexp_loc;
         pexp_attributes} when Syntax_util.attributes_conflicted "implicit_arity" "explicit_arity" pexp_attributes ->
         let new_args =
           match args with
             | Some {Parsetree.pexp_desc = Pexp_tuple [sp]} -> Some sp
             | _ -> args in
         super.Ast_mapper.expr mapper
         {Parsetree.pexp_desc=Pexp_construct(lid, new_args); Parsetree.pexp_loc; pexp_attributes=
          Syntax_util.normalized_attributes "explicit_arity" pexp_attributes}
      | x -> super.expr mapper x
  end;
  pat = begin fun mapper pattern ->
    match pattern with
      | {Parsetree.ppat_desc=Ppat_construct(lid, args);
         Parsetree.ppat_loc;
         ppat_attributes} when Syntax_util.attributes_conflicted "implicit_arity" "explicit_arity" ppat_attributes ->
         let new_args =
           match args with
             | Some {Parsetree.ppat_desc = Ppat_tuple [sp]} -> Some sp
             | _ -> args in
         super.pat mapper
         {Parsetree.ppat_desc=Ppat_construct(lid, new_args); Parsetree.ppat_loc; ppat_attributes=
          Syntax_util.normalized_attributes "explicit_arity" ppat_attributes}
      | x -> super.pat mapper x
  end;
}

let reason_mapper =
  Ast_mapper.default_mapper
  |> Syntax_util.reason_to_ml_swap_operator_mapper
  |> arity_conflict_resolving_mapper

let rec string_of_longident = function
    | Longident.Lident s -> s
    | Longident.Ldot(longPrefix, s) ->
        s
    | Longident.Lapply (y,s) -> string_of_longident s

let built_in_explicit_arity_constructors = ["Some"; "Assert_failure"; "Match_failure"]

let jsx_component module_name attrs children loc =
  let firstPart = (List.hd (Longident.flatten module_name)) in
  let lident = if String.get firstPart 0 != '_' && firstPart = String.capitalize firstPart then
    (* firstPart will be non-empty so the 0th access is fine. Modules can't start with underscore *)
    Longident.Ldot(module_name, "createElement")
  else
    Longident.Lident firstPart
  in
  let ident = Location.mkloc lident loc in
  let body = mkexp(Pexp_apply(mkexp(Pexp_ident ident) ~loc, attrs @ children)) ~loc in
  let attribute = ({Asttypes.txt = "JSX"; Asttypes.loc = loc}, Parsetree.PStr []) in
  { body with pexp_attributes = attribute :: body.pexp_attributes }

(* We might raise some custom error messages in this file.
  Do _not_ directly raise a Location.Error. Our public interface guarantees that we only throw Syntaxerr or Syntax_util.Error *)
let raiseSyntaxErrorFromSyntaxUtils loc fmt =
  Printf.ksprintf
    (fun msg -> raise Syntax_util.(Error(loc, (Syntax_error msg))))
    fmt

let ensureTagsAreEqual startTag endTag loc =
  if startTag <> endTag then
     let startTag = (String.concat "" (Longident.flatten startTag)) in
     let endTag = (String.concat "" (Longident.flatten endTag)) in
     raiseSyntaxErrorFromSyntaxUtils loc
      "Start tag <%s> does not match end tag </%s>" startTag endTag

let prepare_immutable_labels labels =
  let prepare (label, attr) =
    if label.Parsetree.pld_mutable == Mutable then syntax_error();
    (label.pld_name.txt, attr, label.pld_type)
  in
  List.map prepare labels

type core_type_object =
  | Core_type of Parsetree.core_type
  | Record_type of Parsetree.label_declaration list

(* `{. "foo": bar}` -> `Js.t {. foo: bar}` and {.. "foo": bar} -> `Js.t {.. foo: bar} *)
let mkBsObjTypeSugar ~loc ~closed rows =
  let obj = mktyp ~loc (Ptyp_object (prepare_immutable_labels rows, closed)) in
  let jsDotTCtor = { Asttypes.txt = Longident.parse "Js.t"; loc } in
  Core_type(mktyp(Parsetree.Ptyp_constr(jsDotTCtor, [obj])))

let only_core_type t startp endp =
  match t with
  | Core_type ct -> ct
  | Record_type _ ->
    let loc = mklocation startp endp in
    raiseSyntaxErrorFromSyntaxUtils loc "Record type is not allowed"

let only_labels l =
  let rec loop label_declarations result =
    match label_declarations with
    | hd :: tail ->
      let (l, a) = hd in
      if (List.length a > 0) then
        syntax_error ()
      else
        loop tail (result @ [l] )
    | [] -> result
  in
  loop l []

let doc_loc = {Asttypes.txt = "ocaml.doc"; Asttypes.loc = Location.none}

let doc_attr text loc =
  let open Parsetree in
  let exp =
    { Parsetree.pexp_desc = Pexp_constant (Pconst_string(text, None));
      Parsetree.pexp_loc = loc;
      pexp_attributes = []; }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); Parsetree.pstr_loc = exp.Parsetree.pexp_loc }
  in
    (doc_loc, Parsetree.PStr [item])

%}



/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOTDOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string * char option> FLOAT
%token FOR
%token FUN ES6_FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
/* SLASHGREATER is an INFIXOP3 that is handled specially */
%token SLASHGREATER
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <string * char option> INT
%token <string> LABEL_WITH_EQUAL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
%token LESS
%token <string> LESSIDENT
%token LESSGREATER
%token LESSSLASHGREATER
%token LESSDOTDOTGREATER
%token EQUALGREATER
%token LET
%token <string> LIDENT
%token LPAREN
%token LBRACKETAT
%token OF
%token PRI
%token SWITCH
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token NONREC
%token OBJECT
%token OPEN
%token OR
/* %token PARSER */
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token <string> POSTFIXOP
%token PUB
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token <string> LESSSLASHIDENTGREATER
%token SEMI
%token SEMISEMI
%token SHARP
%token <string> SHARPOP
%token SIG
%token STAR
%token <string * string option> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT
%token <string> DOCSTRING

%token EOL

/* Precedences and associativities.

Tokens and rules have precedences and those precedences are used to
resolve what would otherwise be a conflict in the grammar.

Precedence and associativity/Resolving conflicts:
----------------------------
See [http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual026.html] section
about conflicts.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

*/
/* Question: Where is the SEMI explicit precedence? */
%nonassoc below_SEMI
%right    EQUALGREATER                  /* core_type2 (t => t => t) */
%right    COLON
%right    EQUAL                         /* below COLONEQUAL (lbl = x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc QUESTION
%nonassoc WITH             /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc AS
%nonassoc below_BAR                     /* Allows "building up" of many bars */
%left     BAR                           /* pattern (p|p|p) */

%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%left     INFIXOP0 LESS GREATER         /* expr (e OP e OP e) */
%left     LESSDOTDOTGREATER /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ /* expr (e OP e OP e) */
%left     PERCENT INFIXOP3 SLASHGREATER STAR          /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */

/**
 * With the way attributes are currently parsed, if we want consistent precedence for
 *
 * The OCaml parser parses the following attributes:
 *
 *    let x = true && (false [@attrOnFalse])
 *    let x = true && false [@attrOnFalse]
 *    let x = 10 + 20 [@attrOn20]
 *    let x = (10 + 20) [@attrEntireAddition]
 *
 * As:
 *
 *    let x = true && ((false)[@attrOnFalse ])
 *    let x = true && ((false)[@attrOnFalse ])
 *    let x = ((10 + 20)[@attrOn20 ])
 *    let x = ((10 + 20)[@attrEntireAddition ])
 *
 * That is because the precedence of tokens is configured as following, which
 * only serves to treat certain infix operators as different than others with
 * respect to attributes *only*.
 *
 *    %right    OR BARBAR
 *    %right    AMPERSAND AMPERAMPER
 *    %nonassoc below_EQUAL
 *    %left     INFIXOP0 EQUAL LESS GREATER
 *    %right    INFIXOP1
 *    %nonassoc LBRACKETAT
 *    %right    COLONCOLON
 *    %left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ
 *    %left     PERCENT INFIXOP3 SLASHGREATER STAR
 *    %right    INFIXOP4
 *
 * So instead, with Reason, we treat all infix operators identically w.r.t.
 * attributes. In expressions, they have the same precedence as function
 * arguments, as if they are additional arguments to a function application.
 *
 * Note that unary subtractive/plus parses with lower precedence than function
 * application (and attributes) This means that:
 *
 *   let = - something blah blah [@attr];
 *
 * Will have the attribute applied to the entire content to the right of the
 * unary minus, as if the attribute was merely another argument to the function
 * application.
 *
 *
 * To make the attribute apply to the unary -, wrap in parens.
 *
 *   let = (- something blah blah) [@attr];
 *
 * Where arrows occur, it will (as always) obey the rules of function/type
 * application.
 *
 *   type x = int => int [@onlyAppliedToTheInt];
 *   type x = (int => int) [@appliedToTheArrow];
 *
 * However, unary subtractive/plus parses with *higher* precedence than infix
 * application, so that
 *
 *   3 + - funcCall arg arg + 3;
 *
 * Is parsed as:
 *
 *   3 + (- (funcCall arg arg)) + 3;
 *
 * TODO:
 *
 * We would also like to bring this behavior to `!` as well, when ! becomes
 * "not". This is so that you may do !someFunction(arg, arg) and have the
 * entire function application negated. In fact, we may as well just have all
 * of PREFIXOP have the unary precedence parsing behavior for consistency.
 */

%nonassoc attribute_precedence

%nonassoc prec_unary /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
/* Now that commas require wrapping parens (for tuples), prec_constr_appl no
* longer needs to be above COMMA, but it doesn't hurt */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */

/* PREFIXOP and BANG precedence */
%nonassoc below_DOT_AND_SHARP           /* practically same as below_SHARP but we convey purpose */
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%left     SHARPOP
%nonassoc below_DOT
%nonassoc DOT POSTFIXOP

/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc LBRACKETLESS LBRACKET LBRACELESS LBRACE LPAREN

/* Entry points */

%start implementation                   /* for implementation files */
%type <Ast_404.Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Ast_404.Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Ast_404.Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Ast_404.Parsetree.toplevel_phrase list> use_file
%start parse_core_type
%type <Ast_404.Parsetree.core_type> parse_core_type
%start parse_expression
%type <Ast_404.Parsetree.expression> parse_expression
%start parse_pattern
%type <Ast_404.Parsetree.pattern> parse_pattern

/* Instead of reporting an error directly, productions specified
 * below will be reduced first and poped up in the stack to a higher
 * level production.
 *
 * This is essential to error reporting as it is much friendier to provide
 * a higher level error (e.g., "Expecting the parens to be closed" )
 * as opposed to a low-level one (e.g., "Expecting to finish
 * current type definition").
 *
 * See Menhir's manual for more details.
 */
%on_error_reduce structure_item let_binding_body
                 as_loc(attribute)+
                 type_longident
                 attribute+
                 constr_longident
                 pattern
                 nonrec_flag
                 val_ident
                 SEMI?
                 fun_def(EQUAL,core_type)
                 fun_def(EQUALGREATER,non_arrowed_core_type)
                 expr_optional_constraint
%%

/* Entry points */

implementation:
  structure EOF
  { Syntax_util.apply_mapper_to_structure $1 reason_mapper }
;

interface:
  signature EOF
  { Syntax_util.apply_mapper_to_signature $1 reason_mapper }
;

toplevel_phrase: embedded
  ( EOF                              { raise End_of_file}
  | structure_item     SEMI          { Parsetree.Ptop_def $1 }
  | toplevel_directive SEMI          { $1 }
  ) { Syntax_util.apply_mapper_to_toplevel_phrase $1 reason_mapper }
;

use_file: embedded
  ( EOF                              { [] }
  | structure_item     SEMI use_file { Parsetree.Ptop_def $1  :: $3 }
  | toplevel_directive SEMI use_file { $1 :: $3 }
  | structure_item     EOF           { [Parsetree.Ptop_def $1 ] }
  | toplevel_directive EOF           { [$1] }
  ) { Syntax_util.apply_mapper_to_use_file $1 reason_mapper }
;

parse_core_type:
  only_core_type(core_type) EOF
  { Syntax_util.apply_mapper_to_type $1 reason_mapper }
;

parse_expression:
  expr EOF
  { Syntax_util.apply_mapper_to_expr $1 reason_mapper }
;

parse_pattern:
  pattern EOF
  { Syntax_util.apply_mapper_to_pattern $1 reason_mapper }
;

/* Module expressions */

module_parameter:
as_loc
  ( LPAREN RPAREN
    { (Some (Location.mkloc "*" (mklocation $startpos $endpos)), None) }
  | as_loc(UIDENT {$1} | UNDERSCORE {"_"}) COLON module_type
    { (Some $1, Some $3) }
  | module_type
    { (None, Some $1) }
) {$1};

functor_parameters:
  | LPAREN RPAREN
    { let loc = mklocation $startpos $endpos in
      [Location.mkloc (Some (Location.mkloc "*" loc), None) loc]
    }
  | parenthesized(module_parameter) { [$1] }
  | parenthesized(lseparated_two_or_more(COMMA, module_parameter)) { $1 }
;

module_complex_expr:
mark_position_mod
  ( module_expr
    { $1 }
  | module_expr COLON module_type
    { mkmod(Pmod_constraint($1, $3)) }
  | VAL expr
    { mkmod(Pmod_unpack $2) }
  | VAL expr COLON package_type
    { let loc = mklocation $symbolstartpos $endpos in
      mkmod (Pmod_unpack(
             mkexp ~ghost:true ~loc (Pexp_constraint($2, mktyp ~ghost:true ~loc (Ptyp_package $4))))) }
  | VAL expr COLON package_type COLONGREATER package_type
    { let loc = mklocation $symbolstartpos $endpos in
      mkmod (Pmod_unpack(
             mkexp ~ghost:true ~loc (Pexp_coerce($2, Some(mktyp ~ghost:true ~loc (Ptyp_package $4)),
                                    mktyp ~ghost:true ~loc (Ptyp_package $6))))) }
  | VAL expr COLONGREATER package_type
    { let loc = mklocation $symbolstartpos $endpos and ghost = true in
      let mty = mktyp ~ghost ~loc (Ptyp_package $4) in
      mkmod (Pmod_unpack(mkexp ~ghost ~loc (Pexp_coerce($2, None, mty))))
    }
  ) {$1};

module_arguments:
  | module_expr_structure { [$1] }
  | parenthesized(separated_list(COMMA, module_complex_expr))
    { match $1 with
      | [] -> [mkmod ~loc:(mklocation $startpos $endpos) (Pmod_structure [])]
      | xs -> xs
    }
;

module_expr_body: preceded(EQUAL,module_expr) | module_expr_structure { $1 };

module_expr_structure:
  LBRACE structure RBRACE
  { mkmod ~loc:(mklocation $startpos $endpos) (Pmod_structure($2)) }
;

module_expr:
mark_position_mod
  ( as_loc(mod_longident)
    { mkmod(Pmod_ident $1) }
  | module_expr_structure { $1 }
  | as_loc(LPAREN) module_expr COLON module_type as_loc(error)
    { unclosed_mod (with_txt $1 "(") (with_txt $5 ")")}
  | LPAREN module_complex_expr RPAREN
    { $2 }
  | LPAREN RPAREN
    { mkmod (Pmod_structure []) }
  | extension
    { mkmod (Pmod_extension $1) }
  /**
   * Although it would be nice (and possible) to support annotated return value
   * here, that wouldn't be consistent with what is possible for functions.
   * Update: In upstream, it *is* possible to annotate return values for
   * lambdas.
   */
  | either(ES6_FUN,FUN) functor_parameters preceded(COLON,simple_module_type)? EQUALGREATER module_expr
    { let me = match $3 with
        | None -> $5
        | Some mt ->
          let loc = mklocation $startpos($3) $endpos in
          mkmod ~loc (Pmod_constraint($5, mt))
      in
      mk_functor_mod $2 me
    }
  | module_expr module_arguments
    { List.fold_left mkmod_app $1 $2 }
  | module_expr as_loc(LPAREN) module_expr as_loc(error)
    { unclosed_mod (with_txt $2 "(") (with_txt $4 ")") }
  | as_loc(LPAREN) module_expr as_loc(error)
    { unclosed_mod (with_txt $1 "(") (with_txt $3 ")") }
  | as_loc(LPAREN) VAL expr COLON as_loc(error)
    { unclosed_mod (with_txt $1 "(") (with_txt $5 ")") }
  | as_loc(LPAREN) VAL expr COLONGREATER as_loc(error)
    { unclosed_mod (with_txt $1 "(") (with_txt $5 ")") }
  | as_loc(LPAREN) VAL expr as_loc(error)
    { unclosed_mod (with_txt $1 "(") (with_txt $4 ")") }
  | attribute module_expr %prec attribute_precedence
    { {$2 with pmod_attributes = $1 :: $2.pmod_attributes} }
  ) {$1};

/**
 * Attributes/Extension points TODO:
 * - Faux-ternary to support extension points (printing/parsing).
 * - Audit to ensure every [item attributes] [item extensions] is supported.
 * - wrap_exp_attrs / mkexp_attrs cleanup - no need for the confusing
 * indirection.
 * - Ensure proper parsing as ensured by commit:
 *   4c48d802cb9e8110ab3b57ca0b6a02fdd5655283
 * - Support the Item Extension + Item Attributes pattern/sugar/unification for
 * all items in let sequences (let module etc / let open).
 */

/*
 * In OCaml there are (confusingly) two ways to execute imperitive code at the
 * top level:
 *
 *   doStuff();      (* parsed as let _ = ... *)
 *   doMoreStuff();  (* parsed as let _ = ... *)
 *   ;;              (* SEMISEMI *)
 *   let exportedThing = blah
 *
 *   let _ = doStuff()
 *   let _ = doStuff()
 *   let exportedThing = blah   (* SEMISEMI not needed if no leading seq_expr *)
 *
 *
 * SEMISEMI (and a bunch of other inconsistencies/suprises) are the price you
 * pay for not requiring a delimiter after each binding/imperitive action.
 *
 *    let myFn () =
 *       let x = 0 in
 *       let y = 10 in
 *       x + y
 *
 * Also, in OCaml, there is a different syntax for let bindings in function
 * bodies, where each let bindings group must end with IN.
 *
 * If we just *require* that every module export is terminated by a SEMI, and
 * require that sequence expressions are grouped by some surrounding tokens {},
 * then we can have a consistent, familiar way of executing imperitive code, or
 * let bindings (without introducing SEMISEMI and without forcing you to write
 * [let _ = imperitive()]).
 *
 *   doStuff();
 *   doStuff();
 *   let exportedThing = blah;   (* SEMISEMI not needed *)
 *
 * Also, we can then make function scoped let bindings have a consistent syntax
 * with the top level module syntax.
 *
 *    let myFn () => {
 *      let x = 0;
 *      let y = 10;
 *      x + y;
 *    };
 }
 *
 * There are other practical reasons to require each structure item (or record
 * item etc) to be terminated by a SEMI. It allows IDEs to correct indentation
 * as you type. Otherwise (as in OCaml) your editor has to wait until you type
 * `let` again to determine the indentation of the new line - for many editors,
 * achieving that configuration is not easy.
 *
 *  structure:
 *      seq_expr attribute* structure_tail { mkstrexp $1 $2 :: $3 }
 *    | structure_tail { $1 }
 *  ;
 *  structure_tail:
 *                           { [] }
 *    | SEMISEMI structure   { $2 }
 *    | structure_item SEMI structure_tail { $1 :: $3 }
 */

structure:
  | /* Empty */ { [] }
  | as_loc(error) structure
    { let menhirError = Syntax_util.findMenhirErrorMessage $1.loc in
      match menhirError with
      | MenhirMessagesError errMessage -> (syntax_error_str errMessage.loc errMessage.msg) :: $2
      | _ -> (syntax_error_str $1.loc "Invalid statement") :: $2
    }
  | as_loc(error) SEMI structure
    { let menhirError = Syntax_util.findMenhirErrorMessage $1.loc in
      match menhirError with
      | MenhirMessagesError errMessage -> (syntax_error_str errMessage.loc errMessage.msg) :: $3
      | _ -> (syntax_error_str $1.loc "Invalid statement") :: $3
    }
  | structure_item { $1 }
  | as_loc(structure_item) error structure
    { let menhirError = Syntax_util.findMenhirErrorMessage $1.loc in
      match menhirError with
      | MenhirMessagesError errMessage -> (syntax_error_str errMessage.loc errMessage.msg) :: $3
      | _ -> (syntax_error_str $1.loc "Statement has to end with a semicolon") :: $3
    }
  | structure_item SEMI structure
    { let rec prepend = function
        | [] -> assert false
        | [x] ->
           let effective_loc = mklocation x.Parsetree.pstr_loc.loc_start $endpos($2) in
           let x = set_structure_item_location x effective_loc in
           x :: $3
        | x :: xs -> x :: prepend xs
      in
      prepend $1
    }
;

structure_item:
  | mark_position_str
    /* We consider a floating expression to be equivalent to a single let binding
       to the "_" (any) pattern.  */
    ( item_attributes unattributed_expr
      { mkstrexp $2 $1 }
    | item_extension_sugar structure_item
      { struct_item_extension $1 $2 }
    | item_attributes
      EXTERNAL as_loc(val_ident) COLON only_core_type(core_type) EQUAL primitive_declaration
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr (Pstr_primitive (Ast_helper.Val.mk $3 $5 ~prim:$7 ~attrs:$1 ~loc)) }
    | type_declarations
      { let (nonrec_flag, tyl) = $1 in mkstr(Pstr_type (nonrec_flag, tyl)) }
    | str_type_extension
      { mkstr(Pstr_typext $1) }
    | str_exception_declaration
      { mkstr(Pstr_exception $1) }
    | item_attributes opt_LET_MODULE as_loc(UIDENT) module_binding_body
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr(Pstr_module (Ast_helper.Mb.mk $3 $4 ~attrs:$1 ~loc)) }
    | item_attributes opt_LET_MODULE REC as_loc(UIDENT) module_binding_body
      and_module_bindings*
      { let loc = mklocation $symbolstartpos $endpos($5) in
        mkstr (Pstr_recmodule ((Ast_helper.Mb.mk $4 $5 ~attrs:$1 ~loc) :: $6))
      }
    | item_attributes MODULE TYPE OF? as_loc(ident)
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr(Pstr_modtype (Ast_helper.Mtd.mk $5 ~attrs:$1 ~loc)) }
    | item_attributes MODULE TYPE OF? as_loc(ident) module_type_body(EQUAL)
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr(Pstr_modtype (Ast_helper.Mtd.mk $5 ~typ:$6 ~attrs:$1 ~loc)) }
    | open_statement
      { mkstr(Pstr_open $1) }
    | item_attributes CLASS class_declaration_details and_class_declaration*
      { let (ident, binding, virt, params) = $3 in
        let loc = mklocation $symbolstartpos $endpos($3) in
        let first = Ast_helper.Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc in
        mkstr (Pstr_class (first :: $4))
      }
    | class_type_declarations
        (* Each declaration has their own preceeding attribute* *)
      { mkstr(Pstr_class_type $1) }
    | item_attributes INCLUDE module_expr
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr(Pstr_include (Ast_helper.Incl.mk $3 ~attrs:$1 ~loc))
      }
    | item_attributes item_extension
      (* No sense in having item_extension_sugar for something that's already an
       * item_extension *)
      { mkstr(Pstr_extension ($2, $1)) }
    | let_bindings
      { val_of_let_bindings $1 }
    ) { [$1] }
 | located_attributes
   { List.map (fun x -> mkstr ~loc:x.Asttypes.loc (Pstr_attribute x.txt)) $1 }
;

module_binding_body:
  | loption(functor_parameters) module_expr_body
    { mk_functor_mod $1 $2 }
  | loption(functor_parameters) COLON module_type module_expr_body
    { let loc = mklocation $startpos($3) $endpos($4) in
      mk_functor_mod $1 (mkmod ~loc (Pmod_constraint($4, $3))) }
;

and_module_bindings:
  item_attributes AND as_loc(UIDENT) module_binding_body
  { Ast_helper.Mb.mk $3 $4 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos) }
;

/* Module types */

/*
(*
 * For now, WITH constraints on module types shouldn't be considered
 * "non-arrowed" because their WITH constraints themselves might contain arrows.
 * We could ensure that you may only supply *non* arrowed types (or wrap arrowed
 * types in parens) in thier WITH constraints, but that's just too difficult to
 * think about so we will simply not consider WITH constrained module types as
 * non-arrowed.
 *
 * Given this, we can probably take out the below_WITH precedence in the rules
 * down below.
 *)
*/

/* Allowed in curried let bidings */

simple_module_type:
mark_position_mty
  ( parenthesized(module_parameter)
    { match $1.txt with
      | (None, Some x) -> x
      | _ -> syntax_error_mod $1.Asttypes.loc "Expecting a simple module type"
    }
  | module_type_signature { $1 }
  | as_loc(LPAREN) module_type as_loc(error)
    { unclosed_mty (with_txt $1 "(") (with_txt $3 ")")}
  | as_loc(mty_longident)
    { mkmty (Pmty_ident $1) }
  | as_loc(LBRACE) signature as_loc(error)
    { unclosed_mty (with_txt $1 "{") (with_txt $3 "}")}
  | extension
    { mkmty (Pmty_extension $1) }
  ) {$1};

module_type_signature:
  LBRACE signature RBRACE
  { mkmty ~loc:(mklocation $startpos $endpos) (Pmty_signature $2) }
;

/*
(*
 * For module types, we have:
 *
 *  simple_module_type: (Non arrowed implied)
 *  non_arrowed_module_type
 *  module_type ::=
 *    module_type WITH ...
 *    non_arrowed_module_type
 *    (arg : module_type) => module_type
 *    module_type => module_type
 *)
*/

module_type:
mark_position_mty
  ( module_type WITH lseparated_nonempty_list(AND, with_constraint)
    (* See note above about why WITH constraints aren't considered
     * non-arrowed.
     * We might just consider unifying the syntax for record extension with
     * module extension/WITH constraints.
     *
     *    mod MyModule = {
     *       ModuleToInclude...
     *    };
     *
     *    let module CreateFactory
     *               (Spec: ContainerSpec)
     *               :{DescriptorFactoryIntf.S with
     *                  type props = Spec.props and type dependencies = Spec.props} =>
     *
     *)
    { mkmty (Pmty_with($1, $3)) }
  | simple_module_type
    {$1}
  | LPAREN MODULE TYPE OF module_expr RPAREN
    { mkmty (Pmty_typeof $5) }
  | attribute module_type %prec attribute_precedence
    { {$2 with pmty_attributes = $1 :: $2.pmty_attributes} }
  | functor_parameters EQUALGREATER module_type %prec below_SEMI
      (**
       * In OCaml, this is invalid:
       * module MyFunctor: functor MT -> (sig end) = functor MT -> (struct end);;
       *
       * Not only must curried functor args have annotations, but functor
       * annotations must include *names* for each argument in a functor type.
       *
       * module MyFunctor: functor (MT:MT) -> (sig end) = functor (MT:MT) -> (struct end)
       *
       * In Reason, we will parse the functor type:
       *
       *    (AB:MT) -> ReturnSig
       *
       * as in:
       *                   /----------------\
       * module MyFunctor: (A:B) => ReturnSig = functor (C:D) => {}
       *
       * But only for the sake of compatibility with existing OCaml code (the
       * ability to "view" OCaml code in Reason's syntax without loss of
       * information.) Do not write identifiers in functor argument type
       * positions - you wouldn't do it with functions, and they are
       * meaningless in functors.
       *
       *  But for sake of consistency (and for sake of a syntax that truly
       *  unifies functor syntax with function syntax, the following "sugars"
       *  will be parsed (and printed):
       *
       *   A => B => C
       *
       * Is parsed into:
       *
       * functor (_:A) -> functor (_:B) -> C
       *
       * And a dummy "_" is inserted into the parse tree where no name has been
       * provided.
       *
       *   {SomeSig} => {} => {}
       *
       * Is parsed into:
       *
       * (_:SomeSig) => (_:{}) => {}
       *
       *
       *)
    { mk_functor_mty $1 $3 }
  ) {$1};

signature:
  | /* Empty */ { [] }
  | signature_item { $1 }
  | signature_item SEMI signature { $1 @ $3 }
;

signature_item:
  | mark_position_sig
    ( item_attributes
      LET as_loc(val_ident) COLON only_core_type(core_type)
      { let loc = mklocation $symbolstartpos $endpos in
        mksig(Psig_value (Ast_helper.Val.mk $3 $5 ~attrs:$1 ~loc))
      }
    | item_attributes
      EXTERNAL as_loc(val_ident) COLON only_core_type(core_type) EQUAL primitive_declaration
      { let loc = mklocation $symbolstartpos $endpos in
        mksig(Psig_value (Ast_helper.Val.mk $3 $5 ~prim:$7 ~attrs:$1 ~loc))
      }
    | type_declarations
      { let (nonrec_flag, tyl) = $1 in mksig (Psig_type (nonrec_flag, tyl)) }
    | sig_type_extension
      { mksig(Psig_typext $1) }
    | sig_exception_declaration
      { mksig(Psig_exception $1) }
    | item_attributes opt_LET_MODULE as_loc(UIDENT) module_declaration
      { let loc = mklocation $symbolstartpos $endpos in
        mksig(Psig_module (Ast_helper.Md.mk $3 $4 ~attrs:$1 ~loc))
      }
    | item_attributes opt_LET_MODULE as_loc(UIDENT) EQUAL as_loc(mod_longident)
      { let loc = mklocation $symbolstartpos $endpos in
        let loc_mod = mklocation $startpos($5) $endpos($5) in
        mksig(
          Psig_module (
            Ast_helper.Md.mk
              $3
              (Ast_helper.Mty.alias ~loc:loc_mod $5)
              ~attrs:$1
              ~loc
          )
        )
      }
    | item_attributes opt_LET_MODULE REC as_loc(UIDENT)
      module_type_body(COLON) and_module_rec_declaration*
      { let loc = mklocation $symbolstartpos $endpos($5) in
        mksig (Psig_recmodule (Ast_helper.Md.mk $4 $5 ~attrs:$1 ~loc :: $6)) }
    | item_attributes MODULE TYPE as_loc(ident)
      { let loc = mklocation $symbolstartpos $endpos in
        mksig(Psig_modtype (Ast_helper.Mtd.mk $4 ~attrs:$1 ~loc))
      }
    | item_attributes MODULE TYPE as_loc(ident) module_type_body(EQUAL)
      { let loc = mklocation $symbolstartpos $endpos in
        mksig(Psig_modtype (Ast_helper.Mtd.mk $4 ~typ:$5 ~loc ~attrs:$1))
      }
    | open_statement
      { mksig(Psig_open $1) }
    | item_attributes INCLUDE module_type
      { let loc = mklocation $symbolstartpos $endpos in
        mksig(Psig_include (Ast_helper.Incl.mk $3 ~attrs:$1 ~loc))
      }
    | class_descriptions
      { mksig(Psig_class $1) }
    | class_type_declarations
      { mksig(Psig_class_type $1) }
    | item_attributes item_extension
      { mksig(Psig_extension ($2, $1)) }
    ) { [$1] }
  | located_attributes
    { List.map (fun x -> mksig ~loc:x.Asttypes.loc (Psig_attribute x.txt)) $1 }
;

open_statement:
  item_attributes OPEN override_flag as_loc(mod_longident)
  { Ast_helper.Opn.mk $4 ~override:$3 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos) }
;

module_declaration:
  loption(functor_parameters) module_type_body(COLON)
  { mk_functor_mty $1 $2 }
;

module_type_body(DELIM):
  | DELIM module_type { $2 }
  | module_type_signature { $1 }
;

and_module_rec_declaration:
  item_attributes AND as_loc(UIDENT) module_type_body(COLON)
  { Ast_helper.Md.mk $3 $4 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos) }
;

/* Class expressions */

and_class_declaration:
  item_attributes AND class_declaration_details
  { let (ident, binding, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Ast_helper.Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc
  }
;

class_declaration_details:
  virtual_flag as_loc(LIDENT) ioption(class_type_parameters)
  loption(labeled_pattern_list) class_declaration_body
  { let body = List.fold_right mkclass_fun $4 $5 in
    let params = match $3 with None -> [] | Some x -> x in
    ($2, body, $1, params)
  }
;

class_declaration_body:
  preceded(COLON, class_constructor_type)?
  either(preceded(EQUAL, class_expr), class_body_expr)
  { match $1 with
    | None -> $2
    | Some ct -> Ast_helper.Cl.constraint_ ~loc:(mklocation $symbolstartpos $endpos) $2 ct
  }
;

class_expr_lets_and_rest:
mark_position_cl
  ( class_expr { $1 }
  | let_bindings SEMI class_expr_lets_and_rest
    { class_of_let_bindings $1 $3 }
  | object_body { mkclass (Pcl_structure $1) }
  ) {$1};

object_body:
  | loption(located_attributes)
    mark_position_pat(delimited(AS,pattern,SEMI))
    lseparated_list(SEMI, class_field) SEMI?
    { let attrs = List.map (fun x -> mkcf ~loc:x.Asttypes.loc (Pcf_attribute x.txt)) $1 in
      Ast_helper.Cstr.mk $2 (attrs @ List.concat $3) }
  | lseparated_list(SEMI, class_field) SEMI?
    { let loc = mklocation $symbolstartpos $symbolstartpos in
      Ast_helper.Cstr.mk (mkpat ~loc (Ppat_var (Location.mkloc "this" loc))) (List.concat $1) }
;

class_expr:
mark_position_cl
  ( class_simple_expr
    { $1 }
  | either(ES6_FUN,FUN) labeled_pattern_list EQUALGREATER class_expr
    { List.fold_right mkclass_fun $2 $4 }
  | class_simple_expr labeled_arguments
      /**
       * This is an interesting way to "partially apply" class construction:
       *
       * let inst = new oldclass 20;
       * class newclass = oldclass withInitArg;
       * let inst = new newclass;
       */
    { mkclass(Pcl_apply($1, $2)) }
  | attribute class_expr
    { {$2 with pcl_attributes = $1 :: $2.pcl_attributes} }
  /*
    When referring to class expressions (not regular types that happen to be
    classes), you must refer to it as a class. This gives syntactic real estate
    to place type parameters which are distinguished from constructor application
    of arguments.

       class myClass 'x = (class yourClass 'x int) y z;
       inherit (class yourClass float int) initArg initArg;
       ...
       let myVal: myClass int = new myClass 10;

   */
  | CLASS as_loc(class_longident) loption(type_parameters)
    { mkclass(Pcl_constr($2, $3)) }
  | extension
    { mkclass(Pcl_extension $1) }
  ) {$1};

class_simple_expr:
mark_position_cl
  ( as_loc(class_longident)
    { mkclass(Pcl_constr($1, [])) }
  | class_body_expr
    { $1 }
  | as_loc(LBRACE) class_expr_lets_and_rest as_loc(error)
    { unclosed_cl (with_txt $1 "{") (with_txt $3 "}") }
  | LPAREN class_expr COLON class_constructor_type RPAREN
    { mkclass(Pcl_constraint($2, $4)) }
  | as_loc(LPAREN) class_expr COLON class_constructor_type as_loc(error)
    { unclosed_cl (with_txt $1 "(") (with_txt $5 ")") }
  | LPAREN class_expr RPAREN
    { $2 }
  | as_loc(LPAREN) class_expr as_loc(error)
    { unclosed_cl (with_txt $1 "(") (with_txt $3 ")") }
  ) {$1};

%inline class_body_expr: LBRACE class_expr_lets_and_rest RBRACE { $2 };

class_field:
  | mark_position_cf
    ( item_attributes INHERIT override_flag class_expr preceded(AS,LIDENT)?
      { mkcf_attrs (Pcf_inherit ($3, $4, $5)) $1 }
    | item_attributes VAL value
      { mkcf_attrs (Pcf_val $3) $1 }
    | item_attributes either(PUB {Asttypes.Public}, PRI {Asttypes.Private}) method_
      { let (a, b) = $3 in mkcf_attrs (Pcf_method (a, $2, b)) $1 }
    | item_attributes CONSTRAINT constrain_field
      { mkcf_attrs (Pcf_constraint $3) $1 }
    | item_attributes INITIALIZER mark_position_exp(simple_expr)
      { mkcf_attrs (Pcf_initializer $3) $1 }
    | item_attributes item_extension
      { mkcf_attrs (Pcf_extension $2) $1 }
    ) { [$1] }
  | located_attributes
    { List.map (fun x -> mkcf ~loc:x.Asttypes.loc (Pcf_attribute x.Asttypes.txt)) $1 }
;

value:
/* TODO: factorize these rules (also with method): */
  | override_flag MUTABLE VIRTUAL as_loc(label) COLON only_core_type(core_type)
    { if $1 = Override
      then not_expecting $symbolstartpos $endpos "members marked virtual may not also be marked overridden"
      else ($4, Mutable, Cfk_virtual $6)
    }
  | override_flag MUTABLE VIRTUAL label COLON only_core_type(core_type) EQUAL
    { not_expecting $startpos($7) $endpos($7) "not expecting equal - cannot specify value for virtual val" }
  | VIRTUAL mutable_flag as_loc(label) COLON only_core_type(core_type)
    { ($3, $2, Cfk_virtual $5) }
  | VIRTUAL mutable_flag label COLON core_type EQUAL
    { not_expecting $startpos($6) $endpos($6) "not expecting equal - cannot specify value for virtual val" }
  | override_flag mutable_flag as_loc(label) EQUAL expr
    { ($3, $2, Cfk_concrete ($1, $5)) }
  | override_flag mutable_flag as_loc(label) type_constraint EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      let e = ghexp_constraint loc $6 $4 in
      ($3, $2, Cfk_concrete ($1, e)) }
;

method_:
/* TODO: factorize those rules... */
  | override_flag VIRTUAL as_loc(label) COLON poly_type
    { if $1 = Override then syntax_error ();
      ($3, Cfk_virtual $5)
    }
  | override_flag as_loc(label) fun_def(EQUAL,core_type)
    { let loc = mklocation $symbolstartpos $endpos in
      ($2, Cfk_concrete ($1, mkexp ~ghost:true ~loc (Parsetree.Pexp_poly ($3, None))))
    }
  | override_flag as_loc(label) preceded(COLON,poly_type)?
    either(preceded(EQUAL,expr), braced_expr)
      /* Without locally abstract types, you'll see a Ptyp_poly in the Pexp_poly */
    { let loc = mklocation $symbolstartpos $endpos in
      ($2, Cfk_concrete ($1, mkexp ~ghost:true ~loc (Parsetree.Pexp_poly($4, $3))))
    }
  | override_flag as_loc(label) COLON TYPE LIDENT+ DOT only_core_type(core_type)
    either(preceded(EQUAL,expr), braced_expr)
    /* WITH locally abstract types, you'll see a Ptyp_poly in the Pexp_poly,
       but the expression will be a Pexp_newtype and type vars will be
       "varified". */
    {
      (* For non, methods we'd create a pattern binding:
         ((Ppat_constraint(mkpatvar ..., Ptyp_poly (typeVars, poly_type_varified))),
          exp_with_newtypes_constrained_by_non_varified)

         For methods, we create:
         Pexp_poly (Pexp_constraint (methodFunWithNewtypes, non_varified), Some (Ptyp_poly newTypes varified))
       *)
      let (exp_non_varified, poly_vars) = wrap_type_annotation $5 $7 $8 in
      let exp = Parsetree.Pexp_poly(exp_non_varified, Some poly_vars) in
      let loc = mklocation $symbolstartpos $endpos in
      ($2, Cfk_concrete ($1, mkexp ~ghost:true ~loc exp))
    }
;

/* A parsing of class_constructor_type is the type of the class's constructor - and if the
   constructor takes no arguments, then simply the class_instance_type of what is
   returned.

   The class_instance_type is type of the thing returned from a class constructor
   which can either be:
    - The identifier of another class.
    - Type construction of a class type. [v1, v2] classIdentifier
    - The type definition of the object returned (which is much like the type
      of anonymous object but with different syntax, and ability to specify
      inheritance):
      - The class signature body may contain the type of methods (just like object types do)
      - Inheritance of another class_instance_type.
      - The instance variable "val"s.
      - Constraints (same syntax that is used at class *definition* time)


     When In Modules:
     ================
     There are two primary language "class" constructs that are included in modules:
     Class Definitions, and Class Type Definitions.

     A Class Definition:
     -------------------
     Brings three things into the environment. For a class defined with name
     [myClass], the environment would then contain the following:
       1. A type called [myClass] which represents instances that have the same
       structure as what is returned from the constructor - and do not have
       any *more* members than what is returned from the constructor.
       2. A type called [#myClass] which loosely speaking represents objects
       that have the same structure as what is returned from the constructor,
       but can also have *additional* fields as well. This ability to have
       additional fields is accomplished via the same row-polymorphism of
       objects without classes. Any reference to [#myClass] implicitly includes
       a type variable, whose name we may not mention.
       3. The class constructor for [myClass] - a function that "new" can be
       invoked on. If the constructor is a function that takes arguments, then
       the type of the constructor is the type of that function (including
       the type of object returned). If it doesn't take arguments, then the
       type of the constructor is the type of the object returned. You can
       explicitly annotate/constrain the type of the constructor, and can
       annotate them in module signatures as well.

     The type constraint on a constructor has one syntactic nuance. The final
     item separated by arrow must be prefixed with "new". This is merely to
     resolve parsing conflicts. This can be fixed by making a parse rule for
     types that parses *either* non-arowed class_instance_type or core_types,
     but that means deferring the interpretation of patterns like "lowercase
     identifier" and "type application", until it is known if that item was the
     *final* arrow segment.

     A Class Type Definition:
     -----------------------
     Brings two things into the environment. For a class type definition with
     name [myClass], the environment would then contain the following:
       1. A type called [myClass] which describes any object having *exactly*
       the fields listed in the definition.
       2. A type called [#myClass] which loosely speaking represents objects
       that have the same structure as the definition, but can also have
       *additional* fields as well.


     When In Signatures:
     ================

     A Class Definition (in signature becomes a class "description"):
     -------------------
     A Class Definition merely includes the type of that classes constructor.
     [class myClass: args => new classType;] This merely admits that the module
     includes a class whose constructor has that type.

     A Class Type Definition is specified exactly as it is in a module (of
     course, it may be abstract).


     A Class Type Definition:
     -----------------------
     A Class Type Definition is specified exactly as it is in a module (of
     course, it may be abstract).
     Here are the parsing rules for everything discussed above:



    Overview of Parsing Rules:
    ==========================
     (Note: I have renamed class_type from the upstream compiler to
     class_constructor_type and class_signature to class_instance_type for
     clarity)

     constructor_type:
        | class_instance_type
        | constructorFunction => typeWithArrows => class_instance_type

     Modules
       Class Definitions
       class_declarations
         | class LIDENT = class_expr
         | class LIDENT = (class_expr : constructor_type)
         "Pstr_class"

       Class Type Definition
       class_type_declarations
         CLASS TYPE ident = class_instance_type
         "Pstr_class_type"

     Signatures
       Class Descriptions (describes Class Definitions in Module)
       class_descriptions
         CLASS ident : constructor_type
         "Psig_class"

       Class Type Declarations (subset of Class Type Definitions in Module)
       class_type_declarations
         CLASS TYPE ident = class_instance_type
        "Psig_class_type"

 */
class_constructor_type:
  | class_instance_type { $1 }
  | arrow_type_parameters EQUALGREATER class_constructor_type
    { List.fold_right mkcty_arrow $1 $3 }
;

class_instance_type:
mark_position_cty
  ( as_loc(clty_longident)
    loption(parenthesized(separated_nonempty_list(COMMA,only_core_type(core_type))))
    { mkcty (Pcty_constr ($1, $2)) }
  | attribute class_instance_type
    /* Note that this will compound attributes - so they will become
       attached to whatever */
    { {$2 with pcty_attributes = $1 :: $2.pcty_attributes} }
  | class_type_body
    { $1 }
  | extension
    { mkcty (Pcty_extension $1) }
  ) {$1};

class_type_body:
  | LBRACE class_sig_body RBRACE
    { mkcty ~loc:(mklocation $startpos $endpos) (Pcty_signature $2) }
  | as_loc(LBRACE) class_sig_body as_loc(error)
    { unclosed_cty (with_txt $1 "{") (with_txt $3 "}") }
;

class_sig_body:
  class_self_type lseparated_list(SEMI, class_sig_field) SEMI?
  { Ast_helper.Csig.mk $1 (List.concat $2) }
;

class_self_type:
  | AS only_core_type(core_type) SEMI { $2 }
  | /* empty */ { Ast_helper.Typ.mk ~loc:(mklocation $symbolstartpos $endpos) Parsetree.Ptyp_any }
;

class_sig_field:
  | mark_position_ctf
    ( item_attributes INHERIT class_instance_type
      { mkctf_attrs (Pctf_inherit $3) $1 }
    | item_attributes VAL value_type
      { mkctf_attrs (Pctf_val $3) $1 }
    | item_attributes PRI virtual_flag label COLON poly_type
      { mkctf_attrs (Pctf_method ($4, Private, $3, $6)) $1 }
    | item_attributes PUB virtual_flag label COLON poly_type
      { mkctf_attrs (Pctf_method ($4, Public, $3, $6)) $1 }
    | item_attributes CONSTRAINT constrain_field
      { mkctf_attrs (Pctf_constraint $3) $1 }
    | item_attributes item_extension
      { mkctf_attrs (Pctf_extension $2) $1 }
    ) { [$1] }
  | located_attributes
    { List.map (fun x -> mkctf ~loc:x.Asttypes.loc (Pctf_attribute x.Asttypes.txt)) $1 }
;

value_type:
  mutable_or_virtual_flags label COLON only_core_type(core_type)
  { let (mut, virt) = $1 in ($2, mut, virt, $4) }
;

constrain:
  only_core_type(core_type) EQUAL only_core_type(core_type)
  { ($1, $3, mklocation $symbolstartpos $endpos) }
;

constrain_field:
  only_core_type(core_type) EQUAL only_core_type(core_type)
  { ($1, $3) }
;

class_descriptions:
  item_attributes CLASS class_description_details and_class_description*
  { let (ident, binding, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    (Ast_helper.Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc :: $4)
  }
;

and_class_description:
  item_attributes AND class_description_details
  { let (ident, binding, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Ast_helper.Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc
  }
;

%inline class_type_parameters:
  parenthesized(lseparated_nonempty_list(COMMA, type_parameter))
  { $1 }
;

class_description_details:
  virtual_flag as_loc(LIDENT) loption(class_type_parameters) COLON class_constructor_type
  { ($2, $5, $1, $3) }
;

class_type_declarations:
  item_attributes CLASS TYPE class_type_declaration_details
  and_class_type_declaration*
  { let (ident, instance_type, virt, params) = $4 in
    let loc = mklocation $symbolstartpos $endpos in
    (Ast_helper.Ci.mk ident instance_type ~virt ~params ~attrs:$1 ~loc :: $5)
  }
;

and_class_type_declaration:
  item_attributes AND class_type_declaration_details
  { let (ident, instance_type, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Ast_helper.Ci.mk ident instance_type ~virt ~params ~attrs:$1 ~loc
  }
;

class_type_declaration_details:
  virtual_flag as_loc(LIDENT) loption(class_type_parameters)
  either(preceded(EQUAL,class_instance_type), class_type_body)
  { ($2, $4, $1, $3) }
;

/* Core expressions */

/* Note: If we will parse this as Pexp_apply, and it will
 * not be printed with the braces, except in a couple of cases such as if/while
 * loops.
 *
 *  let add a b = {
 *    a + b;
 *  };
 *  TODO: Rename to [semi_delimited_block_sequence]
 *
 * Since seq_expr doesn't require a final SEMI, then without
 * a final SEMI, a braced sequence with a single identifier is
 * indistinguishable from a punned record.
 *
 *   let myThing = {x};
 *
 * Is it {x:x} the record or x the identifier? We simply decided to break
 * the tie and say that it should be parsed as a single identifier because
 * single field records are incredibly rare. Apart from this one
 * disadvantage, there's no disadvantage to not requiring the final brace.
 *
 * For each valid sequence item, we must list three forms:
 *
 *   [item_extension_sugar] [nonempty_item_attributes] ITEM
 *   [nonempty_item_attributes] ITEM
 *   ITEM
 */
braced_expr:
mark_position_exp
  ( LBRACE seq_expr RBRACE
    { $2 }
  | LBRACE as_loc(seq_expr) error
    { syntax_error_exp $2.Asttypes.loc "SyntaxError in block" }

  | LBRACE DOTDOTDOT expr_optional_constraint COMMA? RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let msg = "Record construction must have at least one field explicitly set" in
      syntax_error_exp loc msg
    }
  | LBRACE record_expr RBRACE
    { let (exten, fields) = $2 in mkexp (Pexp_record(fields, exten)) }
  | as_loc(LBRACE) record_expr as_loc(error)
    { unclosed_exp (with_txt $1 "{") (with_txt $3 "}")}
  | LBRACE record_expr_with_string_keys RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let (exten, fields) = $2 in
      mkexp ~loc (Pexp_extension (Location.mkloc ("bs.obj") loc,
             Parsetree.PStr [mkstrexp (mkexp ~loc (Pexp_record(fields, exten))) []]))
    }
  | as_loc(LBRACE) record_expr_with_string_keys as_loc(error)
    { unclosed_exp (with_txt $1 "{") (with_txt $3 "}")}
  /* Todo: Why is this not a simple_expr? */
  | LBRACE object_body RBRACE
    { mkexp (Pexp_object $2) }
  | as_loc(LBRACE) object_body as_loc(error)
    { unclosed_exp (with_txt $1 "{") (with_txt $3 "}") }
) {$1};

seq_expr_no_seq:
| expr SEMI? { $1 }
| opt_LET_MODULE as_loc(UIDENT) module_binding_body SEMI seq_expr
  { mkexp (Pexp_letmodule($2, $3, $5)) }
| LET? OPEN override_flag as_loc(mod_longident) SEMI seq_expr
  { mkexp (Pexp_open($3, $4, $6)) }
| str_exception_declaration SEMI seq_expr {
   mkexp (Pexp_letexception ($1, $3)) }
| let_bindings SEMI seq_expr
  { expr_of_let_bindings $1 $3 }
| let_bindings SEMI?
  { let loc = mklocation $symbolstartpos $endpos in
    expr_of_let_bindings $1 @@ ghunit ~loc ()
  }
;

seq_expr:
mark_position_exp
  ( seq_expr_no_seq
    { $1 }
  | item_extension_sugar mark_position_exp(seq_expr_no_seq)
    { expression_extension $1 $2 }
  | expr SEMI seq_expr
    { mkexp (Pexp_sequence($1, $3)) }
  | item_extension_sugar expr SEMI seq_expr
    { mkexp (Pexp_sequence(expression_extension $1 $2, $4)) }
  ) { $1 }
;

/*


A:
let named                 a::a      b::b             => a + b;
type named =              a::int => b::int => int;
B:
let namedAlias            a::aa     b::bb  => aa + bb;
let namedAlias            a::aa     b::bb  => aa + bb;
type namedAlias =         a::int => b::int => int;
C:
let namedAnnot            a::(a:int) b::(b:int)   => 20;
D:
let namedAliasAnnot       a::(aa:int) b::(bb:int) => 20;
E:
let myOptional            a::a=?    b::b=?      ()  => 10;
type named =              a::int? => b::int? => unit => int;
F:
let optionalAlias         a::aa=?   b::bb=?   ()  => 10;
G:
let optionalAnnot         a::(a:int)=? b::(b:int)=? ()  => 10;
H:
let optionalAliasAnnot    a::(aa:int)=? b::(bb:int)=? ()  => 10;
I: :
let defOptional           a::a=10    b::b=10  ()  => 10;
type named =              a::int?  => b::int? => unit => int;
J:
let defOptionalAlias      a::aa=10      b::bb=10  ()  => 10;
K:
let defOptionalAnnot      a::(a:int)=10 b::(b:int)=10 ()  => 10;
L:
let defOptionalAliasAnnot a::(aa:int)=10 b::(bb:int)=10 ()  => 10;

M: Invoking them - Punned :
let resNotAnnotated = named a::a b::b;
N::
let resAnnotated    = (named a::a b::b :int);
O: Invoking them :
let resNotAnnotated = named a::a b::b;
P: Invoking them :
let resAnnotated    = (named a::a b::b :int);

Q: Here's why "punning" doesn't work!  :
 Is b:: punned with a final non-named arg, or is b:: supplied b as one named arg? :
let b = 20;
let resAnnotated    = (named a::a b:: b);

R: Proof that there are no ambiguities with return values being annotated :
let resAnnotated    = (named a::a b :ty);


S: Explicitly passed optionals are a nice way to say "use the default value":
let explicitlyPassed =          myOptional a::?None b::?None;
T: Annotating the return value of the entire function call :
let explicitlyPassedAnnotated = (myOptional a::?None b::?None :int);
U: Explicitly passing optional with identifier expression :
let a = None;
let explicitlyPassed =           myOptional a::?a b::?None;
let explicitlyPassedAnnotated = (myOptional a::?a b::?None :int);

*/

labeled_pattern_constraint:
  | AS pattern_optional_constraint { fun _punned -> $2 }
  | preceded(COLON, only_core_type(core_type))?
    { fun punned ->
      let pat = mkpat (Ppat_var punned) ~loc:punned.Asttypes.loc in
      match $1 with
      | None -> pat
      | Some typ ->
        let loc = mklocation punned.Asttypes.loc.loc_start $endpos in
        mkpat ~loc (Ppat_constraint(pat, typ))
    }
;

labeled_pattern:
as_loc
   ( TILDE as_loc(LIDENT) labeled_pattern_constraint
    { Term (Asttypes.Labelled $2.txt, None, $3 $2) }
  | TILDE as_loc(LIDENT) labeled_pattern_constraint EQUAL expr
    { Term (Asttypes.Optional $2.txt, Some $5, $3 $2) }
  | TILDE as_loc(LIDENT) labeled_pattern_constraint EQUAL QUESTION
    { Term (Asttypes.Optional $2.txt, None, $3 $2) }
  | as_loc(LABEL_WITH_EQUAL) expr
    { let loc = (mklocation $symbolstartpos $endpos) in
      Term (Asttypes.Optional $1.txt, Some $2, pat_of_label (Location.mkloc (Longident.parse $1.txt) loc)) }
  | as_loc(LABEL_WITH_EQUAL) QUESTION
    { let loc = (mklocation $symbolstartpos $endpos) in
      Term (Asttypes.Optional $1.txt, None, pat_of_label (Location.mkloc (Longident.parse $1.txt) loc)) } (* mkpat(Ppat_alias) *)
  | pattern_optional_constraint
    { Term (Nolabel, None, $1) }
  | TYPE LIDENT
    { Type $2 }
  ) { $1 }
;

%inline labeled_pattern_list:
  parenthesized(separated_list(COMMA, labeled_pattern))
  { match $1 with
    | [] ->
      let loc = mklocation $startpos $endpos in
      [Location.mkloc (Term (Nolabel, None, mkpat_constructor_unit loc loc)) loc]
    | pats -> pats
  }
;

es6_parameters:
  | labeled_pattern_list { $1 }
  | as_loc(val_ident)
    { [{$1 with txt = Term (Nolabel, None, mkpat ~loc:$1.loc (Ppat_var $1))}] }
;



// TODO: properly fix JSX labelled/optional stuff
jsx_arguments:
  /* empty */ { [] }
  | LIDENT EQUAL QUESTION simple_expr jsx_arguments
    { (* a=?b *)
      [(Asttypes.Optional $1, $4)] @ $5
    }
  | QUESTION LIDENT jsx_arguments
    { (* <Foo ?bar /> punning with explicitly passed optional *)
      let loc_lident = mklocation $startpos($2) $endpos($2) in
      [(Asttypes.Optional $2, mkexp (Parsetree.Pexp_ident {txt = Longident.Lident $2; loc = loc_lident}) ~loc:loc_lident)] @ $3
    }
  | LIDENT EQUAL simple_expr jsx_arguments
    { (* a=b *)
      [(Asttypes.Labelled $1, $3)] @ $4
    }
  | LIDENT jsx_arguments
    { (* a (punning) *)
      let loc_lident = mklocation $startpos($1) $endpos($1) in
      [(Asttypes.Labelled $1, mkexp (Parsetree.Pexp_ident {txt = Longident.Lident $1; loc = loc_lident}) ~loc:loc_lident)] @ $2
    }
;

jsx_start_tag_and_args:
  LESSIDENT jsx_arguments
  { let name = Longident.parse $1 in
    (jsx_component name $2, name)
  }
;

jsx_start_tag_and_args_without_leading_less:
    mod_longident jsx_arguments
    { (jsx_component $1 $2, $1) }
  | LIDENT jsx_arguments
    { let lident = Longident.Lident $1 in
      (jsx_component lident $2, lident) }
;

jsx:
  | LESSGREATER simple_expr_no_call* LESSSLASHGREATER
    { let loc = mklocation $symbolstartpos $endpos in
      let body = mktailexp_extension loc $2 None in
      makeFrag loc body
    }
  | jsx_start_tag_and_args SLASHGREATER
    { let (component, _) = $1 in
      let loc = mklocation $symbolstartpos $endpos in
      component [
        (Asttypes.Labelled "children", mktailexp_extension loc [] None);
        (Asttypes.Nolabel, mkexp_constructor_unit loc loc)
      ] loc
    }
  | jsx_start_tag_and_args GREATER simple_expr_no_call* LESSSLASHIDENTGREATER
    { let (component, start) = $1 in
      let loc = mklocation $symbolstartpos $endpos in
      (* TODO: Make this tag check simply a warning *)
      let endName = Longident.parse $4 in
      let _ = ensureTagsAreEqual start endName loc in
      let siblings = if List.length $3 > 0 then $3 else [] in
      component [
        (Asttypes.Labelled "children", mktailexp_extension loc siblings None);
        (Asttypes.Nolabel, mkexp_constructor_unit loc loc)
      ] loc
    }
   | jsx_start_tag_and_args GREATER DOTDOTDOT simple_expr_no_call LESSSLASHIDENTGREATER
     (* <Foo> ...bar </Foo> or <Foo> ...((a) => 1) </Foo> *)
    { let (component, start) = $1 in
      let loc = mklocation $symbolstartpos $endpos in
      (* TODO: Make this tag check simply a warning *)
      let endName = Longident.parse $5 in
      let _ = ensureTagsAreEqual start endName loc in
      let child = $4 in
      component [
        (Asttypes.Labelled "children", child);
        (Asttypes.Nolabel, mkexp_constructor_unit loc loc)
      ] loc
    }
;

jsx_without_leading_less:
  | GREATER simple_expr_no_call* LESSSLASHGREATER {
    let loc = mklocation $symbolstartpos $endpos in
    let body = mktailexp_extension loc $2 None in
    makeFrag loc body
  }
  | jsx_start_tag_and_args_without_leading_less SLASHGREATER {
    let (component, _) = $1 in
    let loc = mklocation $symbolstartpos $endpos in
    component [
      (Asttypes.Labelled "children", mktailexp_extension loc [] None);
      (Asttypes.Nolabel, mkexp_constructor_unit loc loc)
    ] loc
  }
  | jsx_start_tag_and_args_without_leading_less GREATER simple_expr_no_call* LESSSLASHIDENTGREATER {
    let (component, start) = $1 in
    let loc = mklocation $symbolstartpos $endpos in
    (* TODO: Make this tag check simply a warning *)
    let endName = Longident.parse $4 in
    let _ = ensureTagsAreEqual start endName loc in
    let siblings = if List.length $3 > 0 then $3 else [] in
    component [
      (Asttypes.Labelled "children", mktailexp_extension loc siblings None);
      (Asttypes.Nolabel, mkexp_constructor_unit loc loc)
    ] loc
  }
    | jsx_start_tag_and_args_without_leading_less GREATER DOTDOTDOT simple_expr_no_call LESSSLASHIDENTGREATER {
    let (component, start) = $1 in
    let loc = mklocation $symbolstartpos $endpos in
    (* TODO: Make this tag check simply a warning *)
    let endName = Longident.parse $5 in
    let _ = ensureTagsAreEqual start endName loc in
    let child = $4 in
    component [
      (Asttypes.Labelled "children", child);
      (Asttypes.Nolabel, mkexp_constructor_unit loc loc)
    ] loc
  }
;

optional_expr_extension:
  | (* empty *) { fun exp -> exp }
  | item_extension_sugar { fun exp -> expression_extension $1 exp  }
;

/*
 * Parsing of expressions is quite involved as it depends on context.
 * At the top-level of a structure, expressions can't have attributes
 * (those are attached to the structure).
 * In other places, attributes are allowed.
 *
 * The generic parts are represented by unattributed_expr_template(_).
 * Then unattributed_expr represents the concrete unattributed expr
 * while expr adds an attribute rule to unattributed_expr_template.
 */
%inline unattributed_expr_template(E):
mark_position_exp
  ( simple_expr
    { $1 }
  | FUN optional_expr_extension fun_def(EQUALGREATER,non_arrowed_core_type)
    { $2 $3 }
  | ES6_FUN es6_parameters EQUALGREATER expr
    { List.fold_right mkexp_fun $2 $4 }
  | ES6_FUN es6_parameters COLON only_core_type(non_arrowed_core_type) EQUALGREATER expr
    { List.fold_right mkexp_fun $2
        (ghexp_constraint (mklocation $startpos($4) $endpos) $6 (Some $4, None)) }
  /* List style rules like this often need a special precendence
     such as below_BAR in order to let the entire list "build up"
   */
  | FUN optional_expr_extension match_cases(expr) %prec below_BAR
    { $2 (mkexp (Pexp_function $3)) }
  | SWITCH optional_expr_extension simple_expr_no_constructor
    LBRACE match_cases(seq_expr) RBRACE
    { $2 (mkexp (Pexp_match ($3, $5))) }
  | TRY optional_expr_extension simple_expr_no_constructor
    LBRACE match_cases(seq_expr) RBRACE
    { $2 (mkexp (Pexp_try ($3, $5))) }
  | TRY optional_expr_extension simple_expr_no_constructor WITH error
    { syntax_error_exp (mklocation $startpos($5) $endpos($5)) "Invalid try with"}
  | IF optional_expr_extension parenthesized_expr
       simple_expr ioption(preceded(ELSE,expr))
    { $2 (mkexp (Pexp_ifthenelse($3, $4, $5))) }
  | WHILE optional_expr_extension parenthesized_expr simple_expr
    { $2 (mkexp (Pexp_while($3, $4))) }
  | FOR optional_expr_extension LPAREN pattern IN expr direction_flag expr RPAREN
    simple_expr
    { $2 (mkexp (Pexp_for($4, $6, $8, $7, $10))) }
  | LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
    { let loc_colon = mklocation $startpos($2) $endpos($2) in
      let loc = mklocation $symbolstartpos $endpos in
      mkexp_cons loc_colon (mkexp ~ghost:true ~loc (Pexp_tuple[$5;$7])) loc
    }
  | E as_loc(infix_operator) expr
    { mkinfix $1 $2 $3 }
  | as_loc(subtractive) expr %prec prec_unary
    { mkuminus $1 $2 }
  | as_loc(additive) expr %prec prec_unary
    { mkuplus $1 $2 }
  | as_loc(BANG {"!"}) expr %prec prec_unary
    { mkexp(Parsetree.Pexp_apply(mkoperator $1, [Asttypes.Nolabel,$2])) }
  | simple_expr DOT as_loc(label_longident) EQUAL expr
    { mkexp(Parsetree.Pexp_setfield($1, $3, $5)) }
  | simple_expr LBRACKET expr RBRACKET EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Parsetree.Pexp_ident(array_function ~loc "Array" "set") in
      mkexp(Parsetree.Pexp_apply(mkexp ~ghost:true ~loc exp,
                       [Asttypes.Nolabel,$1; Asttypes.Nolabel,$3; Asttypes.Nolabel,$6]))
    }
  | simple_expr DOT LBRACKET expr RBRACKET EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Parsetree.Pexp_ident(array_function ~loc "String" "set") in
      mkexp(Parsetree.Pexp_apply(mkexp ~ghost:true ~loc exp,
                       [Asttypes.Nolabel,$1; Asttypes.Nolabel,$4; Asttypes.Nolabel,$7]))
    }
  | simple_expr DOT LBRACE expr RBRACE EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      bigarray_set ~loc $1 $4 $7
    }
  | as_loc(label) EQUAL expr
    { mkexp(Pexp_setinstvar($1, $3)) }
  | ASSERT simple_expr
    { mkexp (Pexp_assert $2) }
  | LAZY simple_expr
    { mkexp (Pexp_lazy $2) }
  /*
   * Ternary is just a shortcut for:
   *
   *     switch expression { | true => expr1 | false => expr2 }
   *
   * The COLON token priority is below QUESTION so that the following parses:
   *
   *    x ? y :
   *    z ? q : r
   *
   *  As
   *
   *    x ? y :
   *    (z ? q : r)
   *
   *  Instead of:
   *
   *    (x ? y :
   *    z) ? q : r
   *
   * When a question mark is seen, *this* parsing rule has lower priority so
   * that we, instead, shift the qusetion mark so that the *latter* ternary is
   * recognized first on the top of the stack. (z ? q : r).
   */
  | E QUESTION expr COLON expr
    { (* Should use ghost expressions, but not sure how that would work with source maps *)
      (* So ? will become true and : becomes false for now*)
      let loc_question = mklocation $startpos($2) $endpos($2) in
      let loc_colon = mklocation $startpos($4) $endpos($4) in
      let fauxTruePat =
        Ast_helper.Pat.mk ~loc:loc_question (Ppat_construct({txt = Longident.Lident "true"; loc = loc_question}, None)) in
      let fauxFalsePat =
        Ast_helper.Pat.mk ~loc:loc_colon (Ppat_construct({txt = Longident.Lident "false"; loc = loc_colon}, None)) in
      let fauxMatchCaseTrue = Ast_helper.Exp.case fauxTruePat $3 in
      let fauxMatchCaseFalse = Ast_helper.Exp.case fauxFalsePat $5 in
      mkexp (Pexp_match ($1, [fauxMatchCaseTrue; fauxMatchCaseFalse]))
    }
  ) {$1};

/*
 * Much like how patterns are partitioned into pattern/simple_pattern,
 * expressions are divided into expr/simple_expr.
 * expr: contains function application, but simple_expr doesn't (unless it's
 * wrapped in parens).
 */
expr:
  | unattributed_expr_template(expr) { $1 }
  | mark_position_exp(
      attribute expr { {$2 with pexp_attributes = $1 :: $2.pexp_attributes} }
      %prec attribute_precedence
    )
    { $1 }
;

unattributed_expr:
  unattributed_expr_template(unattributed_expr) { $1 };

parenthesized_expr:
  | braced_expr
    { $1 }
  | LPAREN RPAREN
    { let loc = mklocation $startpos $endpos in
      mkexp_constructor_unit loc loc }
  | LPAREN expr_list RPAREN
    { may_tuple $startpos $endpos $2 }
;

/* The grammar of simple exprs changes slightly according to context:
 * - in most cases, calls (like f(x)) are allowed
 * - in some contexts, calls are forbidden
 *   (most notably JSX lists and rhs of a SHARPOP extension).
 *
 * simple_expr_template contains the generic parts,
 * simple_expr and simple_expr_no_call the specialized instances.
 */
%inline simple_expr_template(E):
  | as_loc(val_longident) { mkexp (Parsetree.Pexp_ident $1) }
  | constant              { mkexp (Parsetree.Pexp_constant $1) }
  | jsx                   { $1 }
  | simple_expr_direct_argument { $1 }
  | LBRACKETBAR expr_list BARRBRACKET
    { mkexp (Pexp_array $2) }
  | as_loc(LBRACKETBAR) expr_list as_loc(error)
    { unclosed_exp (with_txt $1 "[|") (with_txt $3 "|]") }
  | LBRACKETBAR BARRBRACKET
    { mkexp (Pexp_array []) }
  /* Not sure why this couldn't have just been below_SHARP (Answer: Being
   * explicit about needing to wait for "as") */
  | as_loc(constr_longident) %prec prec_constant_constructor
    { mkexp (Pexp_construct ($1, None)) }
  | name_tag %prec prec_constant_constructor
    { mkexp (Pexp_variant ($1, None)) }
  | LPAREN expr_list RPAREN
    { may_tuple $startpos $endpos $2 }
  | as_loc(LPAREN) expr_list as_loc(error)
    { unclosed_exp (with_txt $1 "(") (with_txt $3 ")") }
  | E as_loc(POSTFIXOP)
    { mkexp(Parsetree.Pexp_apply(mkoperator $2, [Asttypes.Nolabel, $1])) }
  | as_loc(mod_longident) DOT LPAREN expr_list RPAREN
    { mkexp(Parsetree.Pexp_open(Fresh, $1, may_tuple $startpos($3) $endpos($5) $4)) }
  | mod_longident DOT as_loc(LPAREN) expr_list as_loc(error)
    { unclosed_exp (with_txt $3 "(") (with_txt $5 ")") }
  | E DOT as_loc(label_longident)
    { mkexp(Parsetree.Pexp_field($1, $3)) }
  | as_loc(mod_longident) DOT LBRACE RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let pat = mkpat (Ppat_var (Location.mkloc "this" loc)) in
      mkexp(Parsetree.Pexp_open (Fresh, $1,
                       mkexp(Parsetree.Pexp_object(Ast_helper.Cstr.mk pat []))))
    }
  | E LBRACKET expr RBRACKET
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Parsetree.Pexp_ident(array_function ~loc "Array" "get") in
      mkexp(Parsetree.Pexp_apply(mkexp ~ghost:true ~loc exp, [Asttypes.Nolabel,$1; Asttypes.Nolabel,$3]))
    }
  | E as_loc(LBRACKET) expr as_loc(error)
    { unclosed_exp (with_txt $2 "(") (with_txt $4 ")") }
  | E DOT LBRACKET expr RBRACKET
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Parsetree.Pexp_ident(array_function ~loc "String" "get") in
      mkexp(Parsetree.Pexp_apply(mkexp ~ghost:true ~loc exp, [Asttypes.Nolabel,$1; Asttypes.Nolabel,$4]))
    }
  | E DOT as_loc(LBRACKET) expr as_loc(error)
    { unclosed_exp (with_txt $3 "[") (with_txt $5 "]") }
  | E DOT LBRACE expr RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      bigarray_get ~loc $1 $4
    }
  | as_loc(mod_longident) DOT LBRACE record_expr RBRACE
    { let (exten, fields) = $4 in
      let loc = mklocation $symbolstartpos $endpos in
      let rec_exp = mkexp ~loc (Pexp_record (fields, exten)) in
      mkexp(Pexp_open(Fresh, $1, rec_exp))
    }
  | mod_longident DOT as_loc(LBRACE) record_expr as_loc(error)
    { unclosed_exp (with_txt $3 "{") (with_txt $5 "}") }
  | as_loc(mod_longident) DOT LBRACKETBAR expr_list BARRBRACKET
    { let loc = mklocation $symbolstartpos $endpos in
      let rec_exp = Ast_helper.Exp.mk ~loc ~attrs:[] (Pexp_array $4) in
      mkexp(Pexp_open(Fresh, $1, rec_exp))
    }
  | mod_longident DOT as_loc(LBRACKETBAR) expr_list as_loc(error)
    { unclosed_exp (with_txt $3 "[|") (with_txt $5 "|]") }
  | as_loc(mod_longident) DOT LBRACKET expr_comma_seq_extension RBRACKET
    { let seq, ext_opt = $4 in
      let loc = mklocation $startpos($4) $endpos($4) in
      let list_exp = make_real_exp (mktailexp_extension loc seq ext_opt) in
      let list_exp = { list_exp with Parsetree.pexp_loc = loc } in
      mkexp (Pexp_open (Fresh, $1, list_exp))
    }
  | as_loc(PREFIXOP) E %prec below_DOT_AND_SHARP
    { mkexp(Pexp_apply(mkoperator $1, [Asttypes.Nolabel, $2])) }
  /**
   * Must be below_DOT_AND_SHARP so that the parser waits for several dots for
   * nested record access that the bang should apply to.
   *
   * !x.y.z should be parsed as !(((x).y).z)
   */
  (*| as_loc(BANG {"!"}) E %prec below_DOT_AND_SHARP
    { mkexp (Pexp_apply(mkoperator $1, [Nolabel,$2])) }*)
  | NEW as_loc(class_longident)
    { mkexp (Pexp_new $2) }
  | as_loc(mod_longident) DOT LBRACELESS field_expr_list COMMA? GREATERRBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Ast_helper.Exp.mk ~loc ~attrs:[] (Pexp_override $4) in
      mkexp (Pexp_open(Fresh, $1, exp))
    }
  | mod_longident DOT as_loc(LBRACELESS) field_expr_list COMMA? as_loc(error)
    { unclosed_exp (with_txt $3 "{<") (with_txt $6 ">}") }
  | E SHARP label
    { mkexp (Pexp_send($1, $3)) }
  | E as_loc(SHARPOP) simple_expr_no_call
    { mkinfixop $1 (mkoperator $2) $3 }
  | as_loc(mod_longident) DOT LPAREN MODULE module_expr COLON package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp (Pexp_open(Fresh, $1,
        mkexp ~loc (Pexp_constraint (mkexp ~ghost:true ~loc (Pexp_pack $5),
                                     mktyp ~ghost:true ~loc (Ptyp_package $7)))))
    }
  | mod_longident DOT as_loc(LPAREN) MODULE module_expr COLON as_loc(error)
    { unclosed_exp (with_txt $3 "(") (with_txt $7 ")")}
  | extension
    { mkexp (Pexp_extension $1) }
;

%inline simple_expr: simple_expr_call { mkexp_app_rev $startpos $endpos $1 };

simple_expr_no_constructor:
  mark_position_exp(simple_expr_template(simple_expr_no_constructor)) { $1 };

simple_expr_template_constructor:
  | as_loc(constr_longident)
    mark_position_exp
      ( non_labeled_argument_list   { mkexp (Pexp_tuple($1)) }
      | simple_expr_direct_argument { $1 }
      )
    { (*if List.mem (string_of_longident $1.txt)
         built_in_explicit_arity_constructors then
        (* unboxing the inner tupple *)
        match $2 with
          | [inner] -> mkexp (Pexp_construct($1, Some inner))
          | _ -> assert false
      else*)
      mkExplicitArityTupleExp (Pexp_construct($1, Some $2))
    }
  | name_tag
    mark_position_exp
      ( non_labeled_argument_list
        { (* only wrap in a tuple if there are more than one arguments *)
          match $1 with
          | [x] -> x
          | l -> mkexp (Pexp_tuple(l))
        }
      | simple_expr_direct_argument { $1 }
      )
    { mkexp(Pexp_variant($1, Some $2)) }
;

simple_expr_no_call:
  | mark_position_exp(simple_expr_template(simple_expr_no_call)) { $1 }
  | simple_expr_template_constructor { $1 }
;

simple_expr_call:
  | mark_position_exp(simple_expr_template(simple_expr)) { $1, [] }
  | simple_expr_call labeled_arguments
    { let body, args = $1 in (body, List.rev_append $2 args) }
  | LBRACKET expr_comma_seq_extension RBRACKET
    { let seq, ext_opt = $2 in
      let loc = mklocation $startpos($2) $endpos($2) in
      (make_real_exp (mktailexp_extension loc seq ext_opt), [])
    }
  | simple_expr_template_constructor { ($1, []) }
;

simple_expr_direct_argument:
  /*
     Because [< is a special token, the <ident and <> won't be picked up as separate
     tokens, when a list begins witha JSX tag. So we special case it.
     (todo: pick totally different syntax for polymorphic variance types to avoid
     the issue alltogether.

     first token
     /\
     [<ident    args />  , remainingitems ]
     [<>                 , remainingitems ]
   */
  | braced_expr { $1 }
  | LBRACKETLESS jsx_without_leading_less COMMA expr_comma_seq_extension RBRACKET
    { let entireLoc = mklocation $startpos($1) $endpos($4) in
      let (seq, ext_opt) = $4 in
      mktailexp_extension entireLoc ($2::seq) ext_opt
    }
  | LBRACKETLESS jsx_without_leading_less RBRACKET
    { let entireLoc = mklocation $startpos($1) $endpos($3) in
      mktailexp_extension entireLoc ($2::[]) None
    }
  | LBRACELESS field_expr_list COMMA? GREATERRBRACE
    { mkexp (Pexp_override $2) }
  | as_loc(LBRACELESS) field_expr_list COMMA? as_loc(error)
    { unclosed_exp (with_txt $1 "{<") (with_txt $4 ">}" ) }
  | LBRACELESS GREATERRBRACE
    { mkexp (Pexp_override [])}
  | LPAREN MODULE module_expr RPAREN
    { mkexp (Pexp_pack $3) }
  | LPAREN MODULE module_expr COLON package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp (Pexp_constraint (mkexp ~ghost:true ~loc (Pexp_pack $3),
                              mktyp ~ghost:true ~loc (Ptyp_package $5)))
    }
  | as_loc(LPAREN) MODULE module_expr COLON as_loc(error)
    { unclosed_exp (with_txt $1 "(") (with_txt $5 ")") }
;

non_labeled_argument_list:
  | parenthesized(separated_nonempty_list(COMMA, expr_optional_constraint)) { $1 }
  | LPAREN RPAREN
    { let loc = mklocation $startpos $endpos in
      [mkexp_constructor_unit loc loc] }
;

labeled_arguments:
  | mark_position_exp(simple_expr_direct_argument)
    { [(Asttypes.Nolabel, $1)] }
  | parenthesized(separated_list(COMMA, labeled_expr))
    { match $1 with
      | [] -> let loc = mklocation $startpos $endpos in
              [(Asttypes.Nolabel, mkexp_constructor_unit loc loc)]
      | xs -> xs
    }
;

labeled_expr_constraint:
  | expr_optional_constraint { fun _punned -> $1 }
  | type_constraint?
    { fun punned ->
      let exp = mkexp (Pexp_ident punned) ~loc:punned.loc in
      match $1 with
      | None -> exp
      | Some typ ->
        let loc = mklocation punned.loc.loc_start $endpos in
        ghexp_constraint loc exp typ
    }
;

labeled_expr:
  | expr_optional_constraint { (Asttypes.Nolabel, $1) }
  | TILDE as_loc(val_longident)
    { (* add(:a, :b)  -> parses :a & :b *)
      let exp = mkexp (Pexp_ident $2) ~loc:$2.loc in
      (Labelled (String.concat "" (Longident.flatten $2.txt)), exp)
    }
  | TILDE as_loc(val_longident) QUESTION
    { (* foo(:a?)  -> parses :a? *)
      let exp = mkexp (Pexp_ident $2) ~loc:$2.loc in
      (Asttypes.Optional (String.concat "" (Longident.flatten $2.txt)), exp)
    }
  | TILDE as_loc(val_longident) EQUAL optional labeled_expr_constraint
    { (* foo(:bar=?Some(1)) or add(:x=1, :y=2) -> parses :bar=?Some(1) & :x=1 & :y=1 *)
      ($4 (String.concat "" (Longident.flatten $2.txt)), $5 $2)
    }
  | as_loc(LABEL_WITH_EQUAL) optional labeled_expr_constraint
    {
      let loc = (mklocation $symbolstartpos $endpos) in
      ($2 $1.txt, $3 (Location.mkloc (Longident.parse $1.txt) loc))
    }
;

%inline and_let_binding:
  /* AND bindings don't accept a preceeding extension ID, but do accept
   * preceeding attribute*. These preceeding attribute* will cause an
   * error if this is an *expression * let binding. Otherwise, they become
   * attribute* on the structure item for the "and" binding.
   */
  item_attributes AND let_binding_body
  { mklb $3 $1 (mklocation $symbolstartpos $endpos) }
;

let_bindings: let_binding and_let_binding* { addlbs $1 $2 };

let_binding:
  /* Form with item extension sugar */
  item_attributes LET item_extension_sugar? rec_flag let_binding_body
  { let loc = mklocation $symbolstartpos $endpos in
    mklbs $3 $4 (mklb $5 $1 loc) loc }
;

let_binding_body:
  | with_patvar(val_ident) type_constraint EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      ($1, ghexp_constraint loc $4 $2) }
  | with_patvar(val_ident) fun_def(EQUAL,core_type)
    { ($1, $2) }
  | with_patvar(val_ident) COLON preceded(QUOTE,ident)+ DOT only_core_type(core_type)
      EQUAL mark_position_exp(expr)
    { let typ = mktyp ~ghost:true (Ptyp_poly($3, $5)) in
      let loc = mklocation $symbolstartpos $endpos in
      (mkpat ~ghost:true ~loc (Ppat_constraint($1, typ)), $7)
    }
  | with_patvar(val_ident) COLON TYPE LIDENT+ DOT only_core_type(core_type)
      EQUAL mark_position_exp(expr)
  /* Because core_type will appear to contain "type constructors" since the
   * type variables listed in LIDENT+ don't have leading single quotes, we
   * have to call [varify_constructors] (which is what [wrap_type_annotation]
   * does among other things) to turn those "type constructors" that correspond
   * to LIDENT+ into regular type variables. I don't think this should be
   * done in the parser!
   */
  /* In general, this is a very strange transformation that occurs in the
   * standard OCaml parser, but producing the same strange transformation, and
   * being able to recover original code from the result has the benefit that
   * this syntax will integrate seamlessly with ASTs produced by the standard
   * OCaml parser.
   *
   *  let s : type a . core_type = body
   *
   *  LET_BINDING:
   *  Ppat_constraint(Ppat_var s, Ptyp_poly(newtypes, varified_core_type))
   *  Pexp_newtype..(a, Pexp_constraint(body, NOT_varified_core_type))
   *
   *  When [a] is in the first arg to [PTyp_poly] the second arg [core_type]
   *  expects to see [a] and ['a] is not allowed anywhere.
   *  When pretty printing, we must reverse this entire process!
   *
   *  All of this is consistent with the Manual which states:
   *
   *    let rec f : type t1 t2. t1 * t2 list -> t1 = ...
   *
   *  is automatically expanded into
   *
   *    let rec f : 't1 't2. 't1 * 't2 list -> 't1 =
   *      fun (type t1) (type t2) -> (... : t1 * t2 list -> t1)
   *
   * So therefore we end up generating the following two forms of parse trees
   * for the two primary forms of explicitly polymorphic type annotations:
   *
   *
   *           /-----------Ppat_constraint--------\ /-PExp_constraint--\
   *       let x: locallyAbstractTypes . typeVars       =      exp
   *
   *     or
   *           /-------------Ppat_constraint-----------\     /----PExp_constraint------\
   *       let x: type abstractTypes . varified_core_type = (exp : core_type_non_varified)
   *       where carified_core_type must equal (varify_constructors core_type_non_varified)
   *
   *     And in the later case
   *
   */
   { let exp, poly = wrap_type_annotation $4 $6 $8 in
     let loc = mklocation $symbolstartpos $endpos in
     (mkpat ~ghost:true ~loc (Ppat_constraint($1, poly)), exp)
   }

  /* The combination of the following two rules encompass every type of
   * pattern *except* val_identifier. The fact that we want handle the
   * val_ident separately as a separate rule in let_binding_body alone justifies the
   * need for the strange structuring of
   * [pattern]/[simple_pattern_not_ident]/[simple_pattern] - it allows us
   * to isolate [val_ident] in the special case of [let_binding_body].
   *
   * TODO:
   * Unfortunately, it means we cannot do: let (myCurriedFunc: int -> int) a -> a;
   */
  | pattern EQUAL expr
    { ($1, $3) }
  | simple_pattern_not_ident COLON only_core_type(core_type) EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      (mkpat ~loc (Ppat_constraint($1, $3)), $5)
    }
;

/*
 * TODO:
 * In OCaml, the following function binding would be parsed by the function
 * parsers but would return a non function. Coincidentally, everything just worked.
 *   let x: int = 10;
 * Since in Reason, function bindings always use arrows, it's wrong to rely
 * on function parsers to return non function bindings:
 *   let x: int -> 10;
 *   let y (:returnType) -> 20;  (* wat *)
 */

%inline match_cases(EXPR): lnonempty_list(match_case(EXPR)) { $1 };

match_case(EXPR):
  BAR pattern preceded(WHEN,expr)? EQUALGREATER EXPR
  { Ast_helper.Exp.case $2 ?guard:$3 $5 }
;

fun_def(DELIM, typ):
  labeled_pattern_list
  preceded(COLON,only_core_type(typ))?
  either(preceded(DELIM, expr), braced_expr)
  { List.fold_right mkexp_fun $1
      (match $2 with
      | None -> $3
      | Some ct -> Ast_helper.Exp.constraint_ ~loc:(mklocation $startpos $endpos) $3 ct)
  }
;

/* At least one comma delimited: Each item optionally annotated. */
expr_list:
  lseparated_nonempty_list(COMMA, expr_optional_constraint) COMMA?
  { $1 }
;

/* [x, y, z, ...n] --> ([x,y,z], Some n) */
expr_comma_seq_extension:
  | DOTDOTDOT expr_optional_constraint
    { ([], Some $2) }
  | expr_optional_constraint COMMA?
    { ([$1], None) }
  | expr_optional_constraint COMMA expr_comma_seq_extension
    { let seq, ext = $3 in ($1::seq, ext) }
;

/* Same as expr_comma_seq_extension but occuring after an item.

  Used when parsing `[<> </>, remaining]`. We know that there is at
  least one item, so we either should have a comma + more, or nothing.

expr_comma_seq_extension_second_item:
  | DOTDOTDOT expr_optional_constraint RBRACKET
    { ([], Some $2) }
  | expr_optional_constraint COMMA? RBRACKET
    { ([$1], None) }
  | expr_optional_constraint COMMA expr_comma_seq_extension
    { let seq, ext = $3 in ($1::seq, ext) }
;

/**
 * See note about tuple patterns. There are few cases where expressions may be
 * type constrained without requiring additional parens, and inside of tuples
 * are one exception.
 */
expr_optional_constraint:
  | expr { $1 }
  | expr type_constraint
    { ghexp_constraint (mklocation $symbolstartpos $endpos) $1 $2 }
;

record_expr:
  | DOTDOTDOT expr_optional_constraint lnonempty_list(preceded(COMMA,lbl_expr))
    { (Some $2, $3) }
  | as_loc(label_longident) COLON expr
    { (None, [($1, $3)]) }
  | lseparated_two_or_more(COMMA, lbl_expr) COMMA?
    { (None, $1) }
;

lbl_expr:
  | as_loc(label_longident) COLON expr { ($1, $3) }
  | as_loc(label_longident)            { ($1, exp_of_label $1) }
;

record_expr_with_string_keys:
  | DOTDOTDOT expr_optional_constraint string_literal_exprs
    { (Some $2, $3) }
  | STRING COLON expr
    { let loc = mklocation $symbolstartpos $endpos in
      let (s, d) = $1 in
      let lident_lident_loc = Location.mkloc (Longident.Lident s) loc in
      (None, [(lident_lident_loc, $3)])
    }
  | string_literal_expr string_literal_exprs
    { (None, $1 :: $2) }
;

%inline string_literal_exprs:
  lnonempty_list(preceded(COMMA, string_literal_expr)) { $1 };

string_literal_expr:
  STRING preceded(COLON, expr)?
  { let loc = mklocation $startpos $endpos in
    let (s, d) = $1 in
    let lident_lident_loc = Location.mkloc (Longident.Lident s) loc in
    let exp = match $2 with
      | Some x -> x
      | None -> mkexp (Pexp_ident lident_lident_loc)
    in
    (lident_lident_loc, exp)
  }
;

/**
 * field_expr is distinct from record_expr because labels cannot/shouldn't be scoped.
 */
field_expr:
  /* Using LIDENT instead of label here, because a reduce/reduce conflict occurs on:
   *   {blah:x}
   *
   * After `blah`, the parser couldn't tell whether to reduce `label` or
   * `val_ident`. So inlining the terminal here to avoid the whole decision.
   * Another approach would have been to place the `label` rule at at a precedence
   * of below_COLON or something.
   */
 | as_loc(LIDENT) COLON expr
   { ($1, $3) }
 | LIDENT
   { let loc = mklocation $symbolstartpos $endpos in
     let lident_loc = Location.mkloc $1 loc in
     let lident_lident_loc = Location.mkloc (Longident.Lident $1) loc in
     (lident_loc, mkexp (Pexp_ident lident_lident_loc))
   }
;

%inline field_expr_list: lseparated_nonempty_list(COMMA, field_expr) { $1 };

type_constraint:
  | COLON only_core_type(core_type)
      preceded(COLONGREATER,only_core_type(core_type))?
    { (Some $2, $3) }
  | COLONGREATER only_core_type(core_type)
    { (None, Some $2) }
;

/* Patterns */

pattern:
  | pattern_without_or { $1 }
  | mark_position_pat(pattern BAR pattern { mkpat(Ppat_or($1, $3)) }) { $1 }
;

pattern_constructor_argument:
  | simple_pattern_direct_argument
    { [$1] }
  | parenthesized(separated_nonempty_list(COMMA, pattern_optional_constraint))
    { $1 }
;

/**
* Provides sugar for pattern matching on a constructor pattern with a 'direct' argument.
* Example:
* | Foo () => () is sugar for | Foo(()) => ()
* | Foo [a, b, c] => () is sugar for | Foo([a, b, c]) => ()
* | Foo [|x, y|] => () is sugar for | Foo([|x, y|]) => ()
* }
*/
simple_pattern_direct_argument:
mark_position_pat (
   as_loc(constr_longident)
    { mkpat(Ppat_construct(Location.mkloc $1.txt $1.loc, None)) }
  | record_pattern { $1 }
  | list_pattern { $1 }
  | array_pattern { $1 }
  ) {$1}
;

pattern_without_or:
mark_position_pat
  ( simple_pattern { $1 }

  | pattern_without_or AS as_loc(val_ident)
    { mkpat(Ppat_alias($1, $3)) }

  | pattern_without_or AS as_loc(error)
    { expecting_pat (with_txt $3 "identifier") }
  /**
    * Parses a (comma-less) list of patterns into a tuple, or a single pattern
    * (if there is only one item in the list). This is kind of sloppy as there
    * should probably be a different AST construct for the syntax construct this
    * is used in (multiple constructor arguments). The things passed to
    * constructors are not actually tuples either in underlying representation or
    * semantics (they are not first class).
    */
  | as_loc(constr_longident) pattern_constructor_argument
    /* the first case is `| Foo(_)` and doesn't need explicit_arity attached. Actually, something like `| Foo(1)` doesn't either, but we
      keep explicit_arity on the latter anyways because why not. But for `| Foo(_)` in particular, it's convenient to have explicit_arity
      removed, so that you can have the following shortcut:
      | Foo _ _ _ _ _
      vs.
      | Foo _
    */
    { match is_pattern_list_single_any $2 with
      | Some singleAnyPat ->
        mkpat (Ppat_construct($1, Some singleAnyPat))
      | None ->
        let loc = mklocation $symbolstartpos $endpos in
        let argPattern = simple_pattern_list_to_tuple ~loc $2 in
        mkExplicitArityTuplePat (Ppat_construct($1, Some argPattern))
    }

  | name_tag simple_pattern { mkpat (Ppat_variant($1, Some $2)) }

  | pattern_without_or as_loc(COLONCOLON) pattern_without_or
    { raiseSyntaxErrorFromSyntaxUtils $2.loc
        ":: is not supported in Reason, please use [hd, ...tl] instead" }

  | pattern_without_or COLONCOLON as_loc(error)
    { expecting_pat (with_txt $3 "pattern") }

  | LPAREN COLONCOLON RPAREN LPAREN pattern_without_or COMMA pattern_without_or RPAREN
    { let loc_coloncolon = mklocation $startpos($2) $endpos($2) in
      let loc = mklocation $symbolstartpos $endpos in
      mkpat_cons loc_coloncolon (mkpat ~ghost:true ~loc (Ppat_tuple[$5;$7])) loc
    }

  | as_loc(LPAREN) COLONCOLON RPAREN LPAREN
      pattern_without_or COMMA pattern_without_or as_loc(error)
    { unclosed_pat (with_txt $1 "(") (with_txt $8 ")") }

  | EXCEPTION pattern_without_or %prec prec_constr_appl
    { mkpat(Ppat_exception $2) }

  | LAZY simple_pattern { mkpat(Ppat_lazy $2) }

  /**
   * Attribute "attribute" everything to the left of the attribute,
   * up until the point of to the start of an expression, left paren, left
   * bracket, comma, bar - whichever comes first.
   */
  | attribute pattern_without_or %prec attribute_precedence
    { {$2 with ppat_attributes = $1 :: $2.ppat_attributes} }

  ) {$1};

/* A "simple pattern" is either a value identifier, or it is a
 * simple *non*value identifier.
 *
 * A "pattern" (the more general) is the set of all simple patterns,
 * but also with:
 * - more information such as `as X`, `lazy` etc.
 *
 * "labeled_simple_pattern"s contain the set of all simple_patterns, but also
 * include patterns suitable in function bindings where labeled arguments are
 * accepted.
 *
 * But, in all patterns, it seems type constraints must be grouped within some
 * parens or something.
 */
simple_pattern:
mark_position_pat
  ( as_loc(val_ident)        { mkpat(Ppat_var $1) }
  | simple_pattern_not_ident { $1 }
  ) {$1};

simple_pattern_not_ident:
mark_position_pat
  ( UNDERSCORE
    { mkpat (Ppat_any) }
  | signed_constant
    { mkpat (Ppat_constant $1) }
  | signed_constant DOTDOT signed_constant
    { mkpat (Ppat_interval ($1, $3)) }
  | as_loc(constr_longident)
    { mkpat (Ppat_construct ($1, None)) }
  | name_tag
    { mkpat (Ppat_variant ($1, None)) }
  | SHARP as_loc(type_longident)
    { mkpat (Ppat_type ($2)) }
  | as_loc(LBRACKETBAR) pattern_comma_list SEMI? as_loc(error)
    { unclosed_pat (with_txt $1 "[|") (with_txt $4 "|]") }
  | LPAREN pattern RPAREN
    { $2 }
  | LPAREN lseparated_two_or_more(COMMA, pattern_optional_constraint) RPAREN
    { mkpat (Ppat_tuple $2) }
  | as_loc(LPAREN) pattern as_loc(error)
    { unclosed_pat (with_txt $1 "(") (with_txt $3 ")") }
  | LPAREN pattern COLON only_core_type(core_type) RPAREN
    { mkpat(Ppat_constraint($2, $4)) }
  | as_loc(LPAREN) pattern COLON core_type as_loc(error)
    { unclosed_pat (with_txt $1 "(") (with_txt $5 ")") }
  | LPAREN pattern COLON as_loc(error)
    { expecting_pat (with_txt $4 "type") }
  | LPAREN MODULE as_loc(UIDENT) RPAREN
    { mkpat(Ppat_unpack $3) }
  | LPAREN MODULE as_loc(UIDENT) COLON package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkpat(Ppat_constraint(mkpat ~ghost:true ~loc (Ppat_unpack $3),
                            mktyp ~ghost:true ~loc (Ptyp_package $5)))
    }
  | as_loc(LPAREN) MODULE UIDENT COLON package_type as_loc(error)
    { unclosed_pat (with_txt $1 "(") (with_txt $6 ")") }
  | record_pattern { $1 }
  | list_pattern { $1 }
  | array_pattern { $1 }
  | extension
    { mkpat(Ppat_extension $1) }
  ) {$1};

%inline record_pattern:
    LBRACE lbl_pattern_list RBRACE
    { let (fields, closed) = $2 in mkpat (Ppat_record (fields, closed)) }
  | as_loc(LBRACE) lbl_pattern_list as_loc(error)
    { unclosed_pat (with_txt $1 "{") (with_txt $3 "}") }
;

%inline list_pattern:
    LBRACKET pattern_comma_list_extension RBRACKET
    { make_real_pat (mktailpat_extension (mklocation $startpos($2) $endpos($2)) $2) }
  | as_loc(LBRACKET) pattern_comma_list_extension as_loc(error)
    { unclosed_pat (with_txt $1 "[") (with_txt $3 "]") }
;

%inline array_pattern:
    LBRACKETBAR loption(terminated(pattern_comma_list,SEMI?)) BARRBRACKET
    { mkpat (Ppat_array $2) }
;

pattern_optional_constraint:
mark_position_pat
  ( pattern                                 { $1 }
  | pattern COLON only_core_type(core_type) { mkpat(Ppat_constraint($1, $3)) }
  ) {$1};
;

%inline pattern_comma_list:
  lseparated_nonempty_list(COMMA, pattern) { $1 };

/* [x, y, z, ...n] --> ([x,y,z], Some n) */
pattern_comma_list_extension:
  | pattern_comma_list COMMA DOTDOTDOT pattern { ($1, Some $4) }
  | pattern_comma_list COMMA?                  { ($1, None)    }
;

lbl_pattern_list:
  | lbl_pattern                            { ([$1], Asttypes.Closed) }
  | lbl_pattern COMMA                      { ([$1], Asttypes.Closed) }
  | lbl_pattern COMMA UNDERSCORE COMMA?    { ([$1], Asttypes.Open)   }
  | lbl_pattern COMMA lbl_pattern_list
    { let (fields, closed) = $3 in $1 :: fields, closed }
;

lbl_pattern:
  | as_loc(label_longident) COLON pattern { ($1,$3) }
  | as_loc(label_longident)               { ($1, pat_of_label $1) }
  | as_loc(label_longident) AS as_loc(val_ident)
    { (* punning with alias eg. {ReasonReact.state as prevState}
       *  -> {ReasonReact.state: state as prevState} *)
      ($1, mkpat(Ppat_alias(pat_of_label $1, $3)))
    }
;

/* Primitive declarations */

primitive_declaration: nonempty_list(STRING { fst $1 }) { $1 };

/* Type declarations

   The rule for declaring multiple types, 'type t = ... and u = ...', is
   written in a "continuation-passing-style". In pseudo-code:

   Rather than having rules like:
     (1) "TYPE one_type (AND one_type)*"

   It looks like:
     (2) "TYPE types" where "types = one_type (AND types)?".

   This give more context to prevent ambiguities when parsing attributes.
   Because attributes can appear before AND (1), the parser should
   decide very early whether to reduce "one_type", so that an attribute can
   get attached to AND.

   Previously, only [@..] could occur inside "one_type" and only [@@...] before
   AND.  Now we only have [@..].

   With style (2), we can inline the relevant part of "one_type"
   (see type_declaration_kind) to parse attributes as needed and take a
   decision only when arriving at AND.
*/

type_declarations:
  item_attributes TYPE nonrec_flag type_declaration_details
  { let (ident, params, constraints, kind, priv, manifest), endpos, and_types = $4 in
    let loc = mklocation $symbolstartpos endpos in
    let ty = Ast_helper.Type.mk ident ~params:params ~cstrs:constraints
             ~kind ~priv ?manifest ~attrs:$1 ~loc in
    ($3, ty :: and_types)
  }
;

and_type_declaration:
  | { [] }
  | item_attributes AND type_declaration_details
    { let (ident, params, cstrs, kind, priv, manifest), endpos, and_types = $3 in
      let loc = mklocation $symbolstartpos endpos in
      Ast_helper.Type.mk ident ~params ~cstrs ~kind ~priv ?manifest ~attrs:$1 ~loc
      :: and_types
    }
;

type_declaration_details:
  | as_loc(UIDENT) type_variables_with_variance type_declaration_kind
    { raiseSyntaxErrorFromSyntaxUtils $1.loc
        "a type name must start with a lower-case letter or an underscore" }
  | as_loc(LIDENT) type_variables_with_variance type_declaration_kind
    { let (kind, priv, manifest), constraints, endpos, and_types = $3 in
      (($1, $2, constraints, kind, priv, manifest), endpos, and_types) }
;

type_declaration_kind:
  | EQUAL private_flag constructor_declarations
    { let (cstrs, constraints, endpos, and_types) = $3 in
      ((Ptype_variant (cstrs), $2, None), constraints, endpos, and_types) }
  | EQUAL only_core_type(core_type) EQUAL private_flag constructor_declarations
    { let (cstrs, constraints, endpos, and_types) = $5 in
      ((Ptype_variant cstrs, $4, Some $2), constraints, endpos, and_types) }
  | type_other_kind constraints and_type_declaration
    { ($1, $2, $endpos($2), $3) }
;

%inline constraints:
  | { [] }
  | preceded(CONSTRAINT, constrain)+ { $1 }
;

type_other_kind:
  | /*empty*/
    { (Ptype_abstract, Public, None) }
  | EQUAL private_flag core_type
    { match $3 with
      | Core_type ct -> (Ptype_abstract, $2, Some ct)
      | Record_type rt -> (Ptype_record rt, $2, None)
    }
  | EQUAL DOTDOT
    { (Ptype_open, Public, None) }
  | EQUAL only_core_type(core_type) EQUAL DOTDOT
    { (Ptype_open, Public, Some $2) }
  | EQUAL only_core_type(core_type) EQUAL private_flag LBRACE label_declarations RBRACE
    { (Ptype_record (only_labels $6), $4, Some $2) }
;

%inline type_variables_with_variance:
  loption(parenthesized(separated_nonempty_list(COMMA, type_variable_with_variance)))
  { $1 }
;

type_variable_with_variance:
  embedded
  ( QUOTE ident       { (mktyp (Ptyp_var $2) , Asttypes.Invariant    ) }
  | UNDERSCORE        { (mktyp (Parsetree.Ptyp_any)    , Asttypes.Invariant    ) }
  | PLUS QUOTE ident  { (mktyp (Ptyp_var $3) , Asttypes.Covariant    ) }
  | PLUS UNDERSCORE   { (mktyp (Parsetree.Ptyp_any)    , Asttypes.Covariant    ) }
  | MINUS QUOTE ident { (mktyp (Ptyp_var $3) , Asttypes.Contravariant) }
  | MINUS UNDERSCORE  { (mktyp Parsetree.Ptyp_any      , Asttypes.Contravariant) }
  )
  { let first, second = $1 in
    let ptyp_loc =
        {first.Parsetree.ptyp_loc with loc_start = $symbolstartpos; loc_end = $endpos}
    in
    ({first with Parsetree.ptyp_loc}, second)
  }
;

type_parameter: type_variance type_variable { ($2, $1) };

type_variance:
  | /* empty */ { Asttypes.Invariant }
  | PLUS        { Asttypes.Covariant }
  | MINUS       { Asttypes.Contravariant }
;

type_variable:
mark_position_typ
  (QUOTE ident { mktyp (Ptyp_var $2) })
  { $1 };

%inline constructor_declarations:
  either(constructor_declaration,bar_constructor_declaration)
  constructor_declarations_aux
  { let (cstrs, constraints, endpos, and_types) = $2 in
    ($1 :: cstrs, constraints, endpos, and_types)
  }
;

constructor_declarations_aux:
  | bar_constructor_declaration constructor_declarations_aux
    { let (cstrs, constraints, endpos, and_types) = $2 in
      ($1 :: cstrs, constraints, endpos, and_types)
    }
  | constraints and_type_declaration
    { ([], $1, $endpos($1), $2) }
;

bar_constructor_declaration:
  item_attributes BAR constructor_declaration
  { {$3 with pcd_attributes = $1 @ $3.pcd_attributes} }
;

%inline constructor_declaration:
  item_attributes as_loc(constr_ident) generalized_constructor_arguments
  { let args, res = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Ast_helper.Type.constructor ~attrs:$1 $2 ~args ?res ~loc }
;

/* Why are there already attribute* on the extension_constructor_declaration? */
str_exception_declaration:
  item_attributes EXCEPTION
    either(extension_constructor_declaration, extension_constructor_rebind)
  { {$3 with pext_attributes = $3.pext_attributes @ $1} }
;

sig_exception_declaration:
  item_attributes EXCEPTION
    extension_constructor_declaration
  { {$3 with pext_attributes = $3.pext_attributes @ $1} }
;

generalized_constructor_arguments:
  constructor_arguments? preceded(COLON,only_core_type(core_type))?
  { ((match $1 with None -> Parsetree.Pcstr_tuple [] | Some x -> x), $2) }
;

constructor_arguments:
  | object_record_type
    { match $1 with
      | Core_type ct -> Parsetree.Pcstr_tuple [ct]
      | Record_type rt -> Pcstr_record rt
    }
  | parenthesized(separated_nonempty_list(COMMA, only_core_type(core_type)))
    { Parsetree.Pcstr_tuple $1 }
;

label_declaration:
  | item_attributes mutable_flag as_loc(LIDENT)
    { let loc = mklocation $symbolstartpos $endpos in
      (Ast_helper.Type.field $3 (mkct $3) ~mut:$2 ~loc, $1)
    }
  | item_attributes mutable_flag as_loc(LIDENT) COLON poly_type
    { let loc = mklocation $symbolstartpos $endpos in
      (Ast_helper.Type.field $3 $5 ~mut:$2 ~loc, $1)
    }
;

%inline string_literal_lbls:
  lseparated_nonempty_list(COMMA, string_literal_lbl) COMMA? { $1 };

string_literal_lbl:
  | item_attributes STRING COLON poly_type
    {
      let loc = mklocation $symbolstartpos $endpos in
      let (s, _) = $2 in
      (Ast_helper.Type.field (Location.mkloc s loc) $4 ~loc, $1)
    }
  ;

/* Type Extensions */

potentially_long_ident_and_optional_type_parameters:
  | LIDENT type_variables_with_variance
    { let loc = mklocation $startpos($1) $endpos($1) in
      let lident_lident_loc = Location.mkloc (Longident.Lident $1) loc in
      (lident_lident_loc, $2)
    }
  | as_loc(type_strictly_longident) type_variables_with_variance
    {($1, $2)}
;

str_type_extension:
  item_attributes
  TYPE nonrec_flag potentially_long_ident_and_optional_type_parameters
  PLUSEQ embedded(private_flag)
  attributed_ext_constructors(either(extension_constructor_declaration, extension_constructor_rebind))
  { if $3 <> Recursive then
      not_expecting $startpos($3) $endpos($3) "nonrec flag";
    let (potentially_long_ident, params) = $4 in
    Ast_helper.Te.mk potentially_long_ident $7 ~params ~priv:$6 ~attrs:$1
  }
;

sig_type_extension:
  item_attributes
  TYPE nonrec_flag potentially_long_ident_and_optional_type_parameters
  PLUSEQ embedded(private_flag)
  attributed_ext_constructors(extension_constructor_declaration)
  { if $3 <> Recursive then
      not_expecting $startpos($3) $endpos($3) "nonrec flag";
    let (potentially_long_ident, params) = $4 in
    Ast_helper.Te.mk potentially_long_ident $7 ~params ~priv:$6 ~attrs:$1
  }
;

%inline attributed_ext_constructor(X):
  item_attributes BAR item_attributes X { {$4 with pext_attributes = List.concat [$1; $3; $4.pext_attributes]} }
  /* Why is item_attributes duplicated?
     To be consistent with attributes on (poly)variants/gadts.
     So we can place the attribute after the BAR.
     Example:
      type water +=
        pri
        | [@foo] MineralWater;
  */
;

attributed_ext_constructors(X):
  | X attributed_ext_constructor(X)* { $1 :: $2 }
  | attributed_ext_constructor(X)+ { $1 }
;

extension_constructor_declaration:
  as_loc(constr_ident) generalized_constructor_arguments
  { let args, res = $2 in
    let loc = mklocation $symbolstartpos $endpos in
    Ast_helper.Te.decl $1 ~args ?res ~loc
  }
;

extension_constructor_rebind:
  as_loc(constr_ident) EQUAL as_loc(constr_longident)
  { let loc = mklocation $symbolstartpos $endpos in
    Ast_helper.Te.rebind $1 $3 ~loc
  }
;

/* "with" constraints (additional type equations over signature components) */

with_constraint:
  | TYPE as_loc(label_longident) type_variables_with_variance
      EQUAL embedded(private_flag) only_core_type(core_type) constraints
    { let loc = mklocation $symbolstartpos $endpos in
      let typ = Ast_helper.Type.mk {$2 with txt=Longident.last $2.txt}
                  ~params:$3 ~cstrs:$7 ~manifest:$6 ~priv:$5 ~loc in
      Pwith_type ($2, typ)
    }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | TYPE as_loc(label_longident) type_variables_with_variance
      COLONEQUAL only_core_type(core_type)
    { let last = match $2.txt with
        | Longident.Lident s -> s
        | _ -> not_expecting $startpos($2) $endpos($2) "Long type identifier"
      in
      let loc = mklocation $symbolstartpos $endpos in
      Pwith_typesubst (Ast_helper.Type.mk {$2 with txt=last} ~params:$3 ~manifest:$5 ~loc)
    }
  | MODULE as_loc(mod_longident) EQUAL as_loc(mod_ext_longident)
      { Pwith_module ($2, $4) }
  | MODULE as_loc(UIDENT) COLONEQUAL as_loc(mod_ext_longident)
      { Pwith_modsubst ($2, $4) }
;

/* Polymorphic types */

poly_type:
mark_position_typ
  ( only_core_type(core_type)
    { $1 }
  | preceded(QUOTE,ident)+ DOT only_core_type(core_type)
    { mktyp(Ptyp_poly($1, $3)) }
  ) {$1};

/**
  * OCaml types before:
  * -------------------
  * core_type ::=
  *   core_type2
  *   core_type2 as ident
  *
  * core_type2 ::=
  *   simple_core_type_or_tuple
  *   ?ident: core_type2 -> core_type2
  *   ?ident: core_type2 -> core_type2
  *   ident: core_type2 -> core_type2
  *   core_type2 -> core_type2
  *
  * simple_core_type_or_tuple ::=
  *   simple_core_type
  *   simple_core_type * simple_core_type_list   (*Reason deprecates this*)
  *
  * simple_core_type_list ::=                    (*Reason might be able to deprecate this*)
  *   simple_core_type
  *   simple_core_type_list STAR simple_core_type
  *
  * simple_core_type ::=
  *   simple_core_type
  *   simple_core_type2
  *   ( core_type)      (*core_type_comma_list of len 1*)
  *
  * simple_core_type2 ::=
  *  'ident
  *  Long.ident
  *  simple_core_type2 Long.ident
  *  (core_type, core_type) Long.ident
  *  <method1: ... method_2: ...>
  *  <>
  *  {method1: .. method2: ..}
  *  # class_longident #
  *  simple_core_type2 # class_longident
  *  (core_type, core_type) # class_longident
  *  (module package_type)
  *  [%]
  *  bunch of other stuff
  *
  * Reason types where tuples require grouping:
  * -------------------
  * Simple types are implicitly non-arrowed.
  * Non arrowed types may be shown in the trailing positino of the curried sugar
  * function/functor bindings, but it doesn't have to be simple.
  *
  * let x ... :non_arrowed_core_type => ..
  *
  * - A better name for core_type2 would be arrowed_core_type
  * - A better name for core_type would be aliased_arrowed_core_type
  * core_type ::=
  *   core_type2
  *   core_type2 as ident
  *
  * core_type2 ::=
  *   non_arrowed_core_type
  *   ident::? non_arrowed_core_type => non_arrowed_core_type
  *   ident:: non_arrowed_core_type => non_arrowed_core_type
  *   core_type2 => core_type2
  *
  * non_arrowed_core_type ::=
  *    non_arrowed_non_simple_core_type
  *    simple_core_type
  *
  * non_arrowed_non_simple_core_type ::=
  *   type_longident non_arrowed_simple_core_type_list
  *   # class_longident
  *   simple_core_type # class_longident
  *   [core_type_comma_list] # class_longident
  *
  * simple_core_type ::=
  *   <>
  *   ()
  *   {}
  *
  *
  *  'ident
  *  Long.ident
  *  simple_core_type Long.ident
  *  Long.ident non_arrowed_simple_core_type_list
  *  <method1: ... method_2: ...>
  *  <>
  *  {method1: .. method2: ..}
  *  # class_longident #
  *  simple_core_type # class_longident
  *  (core_type, core_type) # class_longident
  *  (module package_type)
  *  [%]
  *  bunch of other stuff
  */

/** Potentially includes:
 * - arrows
 * - space separated type applications
 * - "as" aliases
 */
core_type:
mark_position_typ2
  /* For some reason, when unifying Functor type syntax (using EQUALGREATER),
   * there was a shift reduce conflict likely caused by
   * type module MyFunctor = {type x = blah => foo } => SomeSig
   * That should *not* be a shift reduce conflict (on =>), and it's not clear
   * why the shift reduce conflict showed up in core_type2. Either way, this
   * seems to resolve the issue. If removing the below_EQUALGREATER, the shift
   * reduce conflict is actually caused by the new Functor type annotations
   * even though *nothing* will point towards that.
   * If switching to Menhir, this may not be needed.
   */
  ( core_type2
    { $1 }
  | only_core_type(core_type2) AS QUOTE ident
    { Core_type (mktyp(Ptyp_alias($1, $4))) }
  ) {$1};

/**
 *
 * core_type is basically just core_type2 but with a single AS x potentially
 * appended.
 * core_type2 Potentially includes:
 * - arrows
 * - space separated type applications
 * - Polymorphic type variable application
 */
core_type2:
  item_attributes unattributed_core_type
  { match $1, $2 with
    | [], result -> result
    | attrs, Record_type [] -> assert false
    | attrs, Core_type ct ->
      let loc_start = $symbolstartpos and loc_end = $endpos in
      let ptyp_loc = {ct.Parsetree.ptyp_loc with loc_start; loc_end} in
      let ptyp_attributes = attrs @ ct.ptyp_attributes in
      Core_type {ct with ptyp_attributes; Parsetree.ptyp_loc}
    | attrs, Record_type (label :: labels) ->
      let pld_attributes = attrs @ label.pld_attributes in
      Record_type ({label with pld_attributes} :: labels)
  }
;

unattributed_core_type:
  | non_arrowed_simple_core_type
    { $1 }
  | arrow_type_parameters EQUALGREATER only_core_type(core_type2)
    { Core_type (List.fold_right mktyp_arrow $1 $3) }
  | only_core_type(basic_core_type) EQUALGREATER only_core_type(core_type2)
    { Core_type (mktyp (Ptyp_arrow (Asttypes.Nolabel, $1, $3))) }
;

arrow_type_parameter:
  | only_core_type(core_type)
    { (Asttypes.Nolabel, $1) }
  | TILDE LIDENT COLON only_core_type(core_type)
    { (Asttypes.Labelled $2, $4) }
  | TILDE LIDENT COLON only_core_type(core_type) EQUAL optional
    {($6 $2, $4) }
;

arrow_type_parameters:
  | parenthesized(lseparated_nonempty_list(COMMA, as_loc(arrow_type_parameter)))
    { $1 }
;

/* Among other distinctions, "simple" core types can be used in Variant types:
 * type myType = Count of anySimpleCoreType. Core types (and simple core types)
 * don't include variant declarations (`constructor_declarations`) and don't
 * include the "faux curried" variant Constructor arguments list.
 *
 * In general, "simple" syntax constructs, don't need to be wrapped in
 * parens/braces when embedded in lists of those very constructs.
 *
 * A [simple_core_type] *can* be wrapped in parens, but
 * it doesn't have to be.
 */

/* The name [core_type] was taken. [non_arrowed_core_type] is the same as
 * [simple_core_type] but used in cases
 * where application needn't be wrapped in additional parens */
/* Typically, other syntax constructs choose to allow either
 * [simple_core_type] or
 * [non_arrowed_non_simple_core_type] depending on whether or not
 * they are in a context that expects space separated lists of types to carry
 * particular meaning outside of type constructor application.
 *
 * type x = SomeConstructor x y;
 */
non_arrowed_core_type:
  | non_arrowed_simple_core_type
    { $1 }
  | attribute only_core_type(non_arrowed_core_type)
    { Core_type {$2 with ptyp_attributes = $1 :: $2.ptyp_attributes} }
;

type_parameters:
  parenthesized(separated_nonempty_list(COMMA, only_core_type(core_type)))
  { $1 }
;

non_arrowed_simple_core_types:
mark_position_typ
  ( arrow_type_parameters
    { let prepare_arg {Location.txt = (label, ct); loc} = match label with
        | Asttypes.Nolabel -> ct
        | Asttypes.Optional _ | Asttypes.Labelled _ ->
            syntax_error_typ loc "Labels are not allowed inside a tuple"
      in
      match List.map prepare_arg $1 with
      | []    -> assert false
      | [one] -> one
      | many  -> mktyp (Ptyp_tuple many)
    }
  ) {$1};

non_arrowed_simple_core_type:
  | non_arrowed_simple_core_types       { Core_type $1 }
  | mark_position_typ2(basic_core_type) { $1 }
;

basic_core_type:
mark_position_typ2
  ( as_loc(type_longident) type_parameters
    { Core_type (mktyp(Parsetree.Ptyp_constr($1, $2))) }
  | SHARP as_loc(class_longident) type_parameters
    { Core_type (mktyp(Ptyp_class($2, $3))) }
  | QUOTE ident
    { Core_type (mktyp(Ptyp_var $2)) }
  | SHARP as_loc(class_longident)
    { Core_type (mktyp(Ptyp_class($2, []))) }
  | UNDERSCORE
    { Core_type (mktyp(Parsetree.Ptyp_any)) }
  | as_loc(type_longident)
    { Core_type (mktyp(Parsetree.Ptyp_constr($1, []))) }
  | object_record_type
    { $1 }
  | LBRACKET row_field_list RBRACKET
    { Core_type(mktyp(Ptyp_variant ($2, Asttypes.Closed, None))) }
  | LBRACKETGREATER loption(row_field_list) RBRACKET
    { Core_type(mktyp(Ptyp_variant ($2, Asttypes.Open, None))) }
  | LBRACKETLESS row_field_list loption(preceded(GREATER, name_tag+)) RBRACKET
    { Core_type(mktyp(Ptyp_variant ($2, Asttypes.Closed, Some $3))) }
  | LPAREN MODULE package_type RPAREN
    { Core_type(mktyp(Ptyp_package $3)) }
  | extension
    { Core_type(mktyp(Ptyp_extension $1)) }
  ) {$1};

object_record_type:
  | LBRACE RBRACE
    { syntax_error () }
  | LBRACE label_declarations RBRACE
    { Record_type (only_labels $2) }
  | LBRACE DOT string_literal_lbls RBRACE
    { (* `{. "foo": bar}` -> `Js.t {. foo: bar}` *)
      let loc = mklocation $symbolstartpos $endpos in
      mkBsObjTypeSugar ~loc ~closed:Asttypes.Closed $3
    }
  | LBRACE DOTDOT string_literal_lbls RBRACE
    { (* `{.. "foo": bar}` -> `Js.t {.. foo: bar}` *)
      let loc = mklocation $symbolstartpos $endpos in
      mkBsObjTypeSugar ~loc ~closed:Asttypes.Open $3
    }
  | LBRACE DOT loption(label_declarations) RBRACE
    { Core_type (mktyp (Ptyp_object (prepare_immutable_labels $3, Asttypes.Closed))) }
  | LBRACE DOTDOT loption(label_declarations) RBRACE
    { Core_type (mktyp (Ptyp_object (prepare_immutable_labels $3, Asttypes.Open))) }
;

%inline label_declarations:
  lseparated_nonempty_list(COMMA, label_declaration) COMMA? { $1 };

package_type:
  as_loc(mty_longident)
    loption(preceded(WITH, separated_nonempty_list(AND, package_type_cstr)))
  { ($1, $2) }
;

package_type_cstr:
  TYPE as_loc(label_longident) EQUAL only_core_type(core_type)
  { ($2, $4) }
;

row_field_list:
  | row_field bar_row_field* { $1 :: $2 }
  | bar_row_field bar_row_field* { $1 :: $2 }
;

row_field:
  | tag_field                             { $1 }
  | only_core_type(non_arrowed_core_type) { Rinherit $1 }
;

bar_row_field:
  item_attributes BAR row_field
  { match $3 with
    | Parsetree.Rtag (name, attrs, amp, typs) ->
        Parsetree.Rtag (name, $1 @ attrs, amp, typs)
    | Rinherit typ ->
        Rinherit {typ with ptyp_attributes = $1 @ typ.ptyp_attributes}
  }
;

tag_field:
  | item_attributes name_tag
      boption(AMPERSAND)
      separated_nonempty_list(AMPERSAND, non_arrowed_simple_core_types)
    { Parsetree.Rtag ($2, $1, $3, $4) }
  | item_attributes name_tag
    { Parsetree.Rtag ($2, $1, true, []) }
;

/* Constants */

constant:
  | INT          { let (n, m) = $1 in Parsetree.Pconst_integer (n, m) }
  | CHAR         { Pconst_char $1 }
  | STRING       { let (s, d) = $1 in Parsetree.Pconst_string (s, d) }
  | FLOAT        { let (f, m) = $1 in Parsetree.Pconst_float (f, m) }
;

signed_constant:
  | constant     { $1 }
  | MINUS INT    { let (n, m) = $2 in Parsetree.Pconst_integer("-" ^ n, m) }
  | MINUS FLOAT  { let (f, m) = $2 in Parsetree.Pconst_float("-" ^ f, m) }
  | PLUS INT     { let (n, m) = $2 in Parsetree.Pconst_integer (n, m) }
  | PLUS FLOAT   { let (f, m) = $2 in Parsetree.Pconst_float(f, m) }
;

/* Identifiers and long identifiers */

ident: UIDENT | LIDENT { $1 };

val_ident:
  | LIDENT                 { $1 }
  | LPAREN operator RPAREN { $2 }
;

%inline infix_operator:
  | INFIXOP0          { $1 }
  | INFIXOP1          { $1 }
  | INFIXOP2          { $1 }
  | INFIXOP3          { $1 }
  /* SLASHGREATER is INFIXOP3 but we needed to call it out specially */
  | SLASHGREATER      { "/>" }
  | INFIXOP4          { $1 }
  | PLUS              { "+" }
  | PLUSDOT           { "+." }
  | MINUS             { "-" }
  | MINUSDOT          { "-." }
  | STAR              { "*" }
  | LESS              { "<" }
  | GREATER           { ">" }
  | OR                { "or" }
  | BARBAR            { "||" }
  | AMPERSAND         { "&" }
  | AMPERAMPER        { "&&" }
  | COLONEQUAL        { ":=" }
  | PLUSEQ            { "+=" }
  | PERCENT           { "%" }
  /* We don't need to (and don't want to) consider </> an infix operator for now
     because our lexer requires that "</>" be expressed as "<\/>" because
     every star and slash after the first character must be escaped.
  */
  /* Also, we don't want to consider <> an infix operator because the Reason
     operator swapping requires that we express that as != */
  | LESSDOTDOTGREATER { "<..>" }
  | GREATER GREATER   { ">>" }
;

operator:
  | PREFIXOP          { $1 }
  | POSTFIXOP         { $1 }
  | BANG              { "!" }
  | infix_operator    { $1 }
;
%inline constr_ident:
  | UIDENT            { $1 }
  | LBRACKET RBRACKET { "[]" }
  | LPAREN RPAREN     { "()" }
  | COLONCOLON        { "::" }
/*  | LPAREN COLONCOLON RPAREN { "::" } */
  | FALSE             { "false" }
  | TRUE              { "true" }
;

val_longident:
  | val_ident                     { Longident.Lident $1 }
  | mod_longident DOT val_ident   { Longident.Ldot($1, $3) }
;

constr_longident:
  | mod_longident %prec below_DOT { $1 }
  | LBRACKET RBRACKET             { Longident.Lident "[]" }
  | LPAREN RPAREN                 { Longident.Lident "()" }
  | FALSE                         { Longident.Lident "false" }
  | TRUE                          { Longident.Lident "true" }
;

label_longident:
  | LIDENT                        { Longident.Lident $1 }
  | mod_longident DOT LIDENT      { Longident.Ldot($1, $3) }
;

type_longident:
  | LIDENT                        { Longident.Lident $1 }
  | mod_ext_longident DOT LIDENT  { Longident.Ldot($1, $3) }
;

/* Type long identifiers known to be "long". Only needed to resolve shift
 * reduce conflicts between `type myType 'a 'b = ..` and `type
 * Potentially.Long.myType 'a 'b += ..` - we needed to break the parsing of
 * Potentially.Long.myType into two cases, one that is long, and one that is
 * not.
 */
type_strictly_longident:
  mod_ext_longident DOT LIDENT
  { Longident.Ldot($1, $3) }
;

mod_longident:
  | UIDENT                        { Longident.Lident $1 }
  | mod_longident DOT UIDENT      { Longident.Ldot($1, $3) }
;

mod_ext_longident:
  | UIDENT                        { Longident.Lident $1 }
  | mod_ext_longident DOT UIDENT  { Longident.Ldot($1, $3) }
  | mod_ext2                      { $1 }
;

mod_ext2:
  anonymous( mod_ext_longident DOT UIDENT
             { Longident.Ldot($1, $3) }
           | mod_ext2
             { $1 }
           | UIDENT
             { Longident.Lident($1) }
           )
  parenthesized(lseparated_nonempty_list(COMMA, mod_ext_longident))
  { if not !Clflags.applicative_functors then
      raise Syntaxerr.(Error(Applicative_path(mklocation $startpos $endpos)));
    List.fold_left (fun p1 p2 -> Longident.Lapply (p1, p2)) $1 $2
  }
;

mty_longident:
  | ident                        { Longident.Lident $1 }
  | mod_ext_longident DOT ident  { Longident.Ldot($1, $3) }
;

clty_longident:
  | LIDENT                       { Longident.Lident $1 }
  | mod_ext_longident DOT LIDENT { Longident.Ldot($1, $3) }
;

class_longident:
  | LIDENT                       { Longident.Lident $1 }
  | mod_longident DOT LIDENT     { Longident.Ldot($1, $3) }
;

/* Toplevel directives */

toplevel_directive:
  SHARP ident embedded
          ( /* empty */   { Parsetree.Pdir_none }
          | STRING        { Parsetree.Pdir_string (fst $1) }
          | INT           { let (n, m) = $1 in Parsetree.Pdir_int (n, m) }
          | val_longident { Parsetree.Pdir_ident $1 }
          | mod_longident { Parsetree.Pdir_ident $1 }
          | FALSE         { Parsetree.Pdir_bool false }
          | TRUE          { Parsetree.Pdir_bool true }
          )
  { Ptop_dir($2, $3) }
;

/* Miscellaneous */

opt_LET_MODULE: MODULE { () } | LET MODULE { () };

%inline name_tag: BACKQUOTE ident { $2 };

%inline label: LIDENT { $1 };

rec_flag:
  | /* empty */   { Asttypes.Nonrecursive }
  | REC           { Asttypes.Recursive }
;

nonrec_flag:
  | /* empty */   { Asttypes.Recursive }
  | NONREC        { Asttypes.Nonrecursive }
;

direction_flag:
  | TO            { Asttypes.Upto }
  | DOWNTO        { Asttypes.Downto }
;

%inline private_flag:
  | /* empty */   { Asttypes.Public }
  | PRI           { Asttypes.Private }
;

mutable_flag:
  | /* empty */   { Asttypes.Immutable }
  | MUTABLE       { Asttypes.Mutable }
;

virtual_flag:
  | /* empty */   { Asttypes.Concrete }
  | VIRTUAL       { Asttypes.Virtual }
;

mutable_or_virtual_flags:
  | /* empty */   { Asttypes.Immutable, Asttypes.Concrete }
  | VIRTUAL mutable_flag { $2, Asttypes.Virtual }
  | MUTABLE virtual_flag { Asttypes.Mutable, $2 }
;

override_flag:
  | /* empty */   { Fresh }
  | BANG          { Override }
;

subtractive:
  | MINUS         { "-"  }
  | MINUSDOT      { "-." }
;

additive:
  | PLUS          { "+"  }
  | PLUSDOT       { "+." }
;

single_attr_id:
  | LIDENT      { $1 }
  | UIDENT      { $1 }
  | AND         { "and" }
  | AS          { "as" }
  | ASSERT      { "assert" }
  | BEGIN       { "begin" }
  | CLASS       { "class" }
  | CONSTRAINT  { "constraint" }
  | DO          { "do" }
  | DONE        { "done" }
  | DOWNTO      { "downto" }
  | ELSE        { "else" }
  | END         { "end" }
  | EXCEPTION   { "exception" }
  | EXTERNAL    { "external" }
  | FALSE       { "false" }
  | FOR         { "for" }
  | FUN         { "fun" }
  | FUNCTION    { "function" }
  | FUNCTOR     { "functor" }
  | IF          { "if" }
  | IN          { "in" }
  | INCLUDE     { "include" }
  | INHERIT     { "inherit" }
  | INITIALIZER { "initializer" }
  | LAZY        { "lazy" }
  | LET         { "let" }
  | SWITCH      { "switch" }
  | MODULE      { "module" }
  | MUTABLE     { "mutable" }
  | NEW         { "new" }
  | NONREC      { "nonrec" }
  | OBJECT      { "object" }
  | OF          { "of" }
  | OPEN        { "open" }
  | OR          { "or" }
  | PRI         { "private" }
  | REC         { "rec" }
  | SIG         { "sig" }
  | STRUCT      { "struct" }
  | THEN        { "then" }
  | TO          { "to" }
  | TRUE        { "true" }
  | TRY         { "try" }
  | TYPE        { "type" }
  | VAL         { "val" }
  | VIRTUAL     { "virtual" }
  | WHEN        { "when" }
  | WHILE       { "while" }
  | WITH        { "with" }
/* mod/land/lor/lxor/lsl/lsr/asr are not supported for now */
;

attr_id:
  | as_loc(single_attr_id) { $1 }
  | single_attr_id DOT attr_id { Location.mkloc ($1 ^ "." ^ $3.txt) (mklocation $symbolstartpos $endpos) }
;

attribute:
  | LBRACKETAT attr_id payload RBRACKET { ($2, $3) }
  | DOCSTRING { doc_attr $1 (mklocation $symbolstartpos $endpos) }
;

(* Inlined to avoid having to deal with buggy $symbolstartpos *)
%inline located_attributes: as_loc(attribute)+ { $1 }

(* Inlined to avoid having to deal with buggy $symbolstartpos *)
%inline item_attributes:
  | { [] }
  | located_attributes { List.map (fun x -> x.Asttypes.txt) $1 }
;

item_extension_sugar:
  PERCENT attr_id { ([], $2) }
;

extension:
  LBRACKETPERCENT attr_id payload RBRACKET { ($2, $3) }
;

item_extension:
  LBRACKETPERCENTPERCENT attr_id payload RBRACKET { ($2, $3) }
;

payload:
  | structure                       { Parsetree.PStr $1 }
  | COLON only_core_type(core_type) { PTyp $2 }
  | QUESTION pattern                { PPat ($2, None) }
  | QUESTION pattern WHEN expr      { PPat ($2, Some $4) }
;

optional:
  | { fun x -> Asttypes.Labelled x }
  | QUESTION { fun x -> Asttypes.Optional x }
;

%inline only_core_type(X): X { only_core_type $1 $symbolstartpos $endpos }

%inline mark_position_mod(X): x = X
  { {x with Parsetree.pmod_loc = {x.Parsetree.pmod_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_cty(X): x = X
  { {x with Parsetree.pcty_loc = {x.Parsetree.pcty_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_ctf(X): x = X
  { {x with pctf_loc = {x.pctf_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_exp(X): x = X
  { {x with Parsetree.pexp_loc = {x.Parsetree.pexp_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_typ(X): x = X
  { {x with Parsetree.ptyp_loc = {x.Parsetree.ptyp_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_typ2(X): x = X
  { match x with
    | Core_type ct ->
      let loc_start = $symbolstartpos and loc_end = $endpos in
      Core_type ({ct with Parsetree.ptyp_loc = {ct.Parsetree.ptyp_loc with loc_start; loc_end}})
    | Record_type _ -> x
  }
;

%inline mark_position_mty(X): x = X
  { {x with Parsetree.pmty_loc = {x.Parsetree.pmty_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_sig(X): x = X
  { {x with psig_loc = {x.psig_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_str(X): x = X
  { {x with Parsetree.pstr_loc = {x.Parsetree.pstr_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_cl(X): x = X
  { {x with Parsetree.pcl_loc = {x.Parsetree.pcl_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_cf(X): x = X
   { {x with pcf_loc = {x.pcf_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_pat(X): x = X
   { {x with Parsetree.ppat_loc = {x.Parsetree.ppat_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline as_loc(X): x = X
  { Location.mkloc x (mklocation $symbolstartpos $endpos) }
;

%inline with_patvar(X): x = X
  { let loc = mklocation $symbolstartpos $endpos in
    mkpat ~loc (Ppat_var (Location.mkloc x loc)) }

either(X,Y):
  | X { $1 }
  | Y { $1 }
;

%inline lnonempty_list(X): X llist_aux(X) { $1 :: List.rev $2 };

%inline llist(X): llist_aux(X) { List.rev $1 };

llist_aux(X):
  | /* empty */ { [] }
  | llist_aux(X) X { $2 :: $1 }
;

%inline lseparated_list(sep, X):
  | /* empty */ { [] }
  | lseparated_nonempty_list(sep, X) { $1 };

%inline lseparated_nonempty_list(sep, X):
  lseparated_nonempty_list_aux(sep, X) { List.rev $1 };

lseparated_nonempty_list_aux(sep, X):
  | X { [$1] }
  | lseparated_nonempty_list_aux(sep, X) sep X { $3 :: $1 }
;

%inline lseparated_two_or_more(sep, X):
  X sep lseparated_nonempty_list(sep, X) { $1 :: $3 };

%inline parenthesized(X): delimited(LPAREN, X, RPAREN) { $1 };

%%
