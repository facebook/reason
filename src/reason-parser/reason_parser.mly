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

(* The parser definition *)

%{
open Reason_migrate_parsetree
open OCaml_408.Ast
open Reason_syntax_util
open Location
open Asttypes
open Longident
open Parsetree
open Ast_helper
open Ast_mapper
open Reason_parser_def
open Reason_string
open Reason_errors

let raise_error error loc =
  raise_error (Ast_error error) loc

module Clflags = Reason_syntax_util.Clflags
(*
   TODO:
   - Remove all [open]s from the top of this file one by one and fix compilation
   failures that ensue by specifying the appropriate long identifiers. That
   will make the parser much easier to reason about.
   - Go back to trunk, do the same (remove [open]s, and fully specify long
   idents), to perform a clean diff.

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
       {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
     ..
   end

   parse_tree.mli
   --------------
   and core_type = {
     ptyp_desc: core_type_desc;
     ptyp_loc: Location.t;
     ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
   }

   and core_type_desc =
     | Ptyp_any
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

let make_floating_doc = function
  | {attr_name = {txt = "ocaml.doc"; _} as attr_name; _} as attr ->
      {attr with attr_name = {attr_name with txt = "ocaml.text"}}
  | attr -> attr

let uncurry_payload ?(name="bs") loc =
  { attr_name = {loc; txt = name};
    attr_payload = PStr [];
    attr_loc = loc
  }

let dummy_loc () = {
  loc_start = Lexing.dummy_pos;
  loc_end = Lexing.dummy_pos;
  loc_ghost = false;
}

let mklocation loc_start loc_end = {
  loc_start = loc_start;
  loc_end = loc_end;
  loc_ghost = false;
}

let make_real_loc loc = {
    loc with loc_ghost = false
}

let make_ghost_loc loc = {
    loc with loc_ghost = true
}

let ghloc ?(loc=dummy_loc ()) d = { txt = d; loc = (make_ghost_loc loc) }

let reloc_expr exp startpos endpos =
  {exp with pexp_loc = {exp.pexp_loc with loc_start = startpos; loc_end = endpos}}

(**
  * turn an object into a real
  *)
let make_real_exp exp = {
    exp with pexp_loc = make_real_loc exp.pexp_loc
}
let make_real_pat pat = {
    pat with ppat_loc = make_real_loc pat.ppat_loc
}
(*
 * change the location state to be a ghost location or real location
 *)
let set_loc_state is_ghost loc =
    if is_ghost then make_ghost_loc loc else make_real_loc loc

let mktyp ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Typ.mk ~loc d

let mkpat ?(attrs=[]) ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Pat.mk ~loc ~attrs d

let mkexp ?(attrs=[]) ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Exp.mk ~loc ~attrs d

let mkmty ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Mty.mk ~loc d

let mksig ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Sig.mk ~loc d

let mkmod ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Mod.mk ~loc d

let mkstr ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Str.mk ~loc d

let mkclass ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Cl.mk ~loc d

let mkcty ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Cty.mk ~loc d

let mkctf ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ctf.mk ~loc d

let may_tuple startp endp = function
  | []  -> assert false
  | [x] -> {x with pexp_loc = mklocation startp endp}
  | xs  -> mkexp ~loc:(mklocation startp endp) (Pexp_tuple xs)

(**
  Make a core_type from a as_loc(LIDENT).
  Useful for record type punning.
  type props = {width: int, height: int};
  type state = {nbrOfClicks: int};
  type component = {props, state};
*)
let mkct lbl =
  let lident = Lident lbl.txt in
  let ttype = Ptyp_constr({txt = lident; loc = lbl.loc}, []) in
  {ptyp_desc = ttype; ptyp_loc = lbl.loc; ptyp_attributes = []; ptyp_loc_stack =[]}

let mkcf ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Cf.mk ~loc d

let simple_ghost_text_attr ?(loc=dummy_loc ()) txt =
  let loc = set_loc_state true loc in
  [{ attr_name = {txt; loc};
     attr_payload = PStr [];
     attr_loc = loc;
  }]

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

let mkExplicitArityTupleExp ?(loc=dummy_loc ()) exp_desc =
  mkexp
    ~loc
    ~attrs:(simple_ghost_text_attr ~loc "explicit_arity")
    exp_desc

let is_pattern_list_single_any = function
  | [{ppat_desc=Ppat_any; ppat_attributes=[]} as onlyItem] -> Some onlyItem
  | _ -> None

let mkoperator {Location. txt; loc} =
  Exp.mk ~loc (Pexp_ident(mkloc (Lident txt) loc))

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
  mkexp ~ghost:true ~loc (Pexp_construct (mknoloc (Lident "()"), None))

let mkinfixop ?loc ?attrs arg1 op arg2 =
  mkexp ?loc ?attrs (Pexp_apply(op, [Nolabel, arg1; Nolabel, arg2]))

let mkinfix ?loc ?attrs arg1 name arg2 =
  mkinfixop ?loc ?attrs arg1 (mkoperator name) arg2

let neg_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus name arg =
  match name.txt, arg.pexp_desc with
  | "-", Pexp_constant(Pconst_integer (n,m)) ->
      mkexp(Pexp_constant(Pconst_integer(neg_string n,m)))
  | ("-" | "-."), Pexp_constant(Pconst_float (f, m)) ->
      mkexp(Pexp_constant(Pconst_float(neg_string f, m)))
  | txt, _ ->
      let name = {name with txt = "~" ^ txt} in
      mkexp(Pexp_apply(mkoperator name, [Nolabel, arg]))

let prepare_functor_arg = function
  | Some name, mty -> (name, mty)
  | None, (Some {pmty_loc} as mty) ->
      (mkloc "_" (make_ghost_loc pmty_loc), mty)
  | None, None -> assert false

let mk_functor_mod args body =
  let folder arg acc =
    let name, mty = prepare_functor_arg arg.txt in
    mkmod ~loc:arg.loc (Pmod_functor(name, mty, acc))
  in
  List.fold_right folder args body

let mk_functor_mty args body =
  let folder arg acc =
    let name, mty = prepare_functor_arg arg.txt in
    mkmty ~loc:arg.loc (Pmty_functor(name, mty, acc))
  in
  List.fold_right folder args body

let mkuplus name arg =
  match name.txt, arg.pexp_desc with
  | "+", Pexp_constant(Pconst_integer _)
  | ("+" | "+."), Pexp_constant(Pconst_float _) ->
      mkexp arg.pexp_desc
  | txt, _ ->
      let name = {name with txt = "~" ^ txt} in
      mkexp(Pexp_apply(mkoperator name, [Nolabel, arg]))

let mkexp_cons consloc args loc =
  mkexp ~loc (Pexp_construct(mkloc (Lident "::") consloc, Some args))

let mkexp_constructor_unit ?(uncurried=false) consloc loc =
  let attrs = if uncurried then [uncurry_payload ~name:"uncurry" loc] else [] in
  mkexp ~attrs ~loc (Pexp_construct(mkloc (Lident "()") consloc, None))

let ghexp_cons args loc =
  mkexp ~ghost:true ~loc (Pexp_construct(mkloc (Lident "::") loc, Some args))

let mkpat_cons args loc =
  mkpat ~loc (Ppat_construct(mkloc (Lident "::") loc, Some args))

let ghpat_cons args loc =
  mkpat ~ghost:true ~loc (Ppat_construct(mkloc (Lident "::") loc, Some args))

let mkpat_constructor_unit consloc loc =
  mkpat ~loc (Ppat_construct(mkloc (Lident "()") consloc, None))

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
            let nil = { txt = Lident "[]"; loc } in
            Exp.mk ~loc (Pexp_construct (nil, None)) in
        base_case
    | e1 :: el ->
        let exp_el = handle_seq el in
        let loc = mklocation e1.pexp_loc.loc_start exp_el.pexp_loc.loc_end in
        let arg = mkexp ~ghost:true ~loc (Pexp_tuple [e1; exp_el]) in
        ghexp_cons arg loc
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
          let nil = { txt = Lident "[]"; loc } in
          mkpat ~loc (Ppat_construct (nil, None)) in
      base_case
  | p1 :: pl ->
      let pat_pl = handle_seq pl in
      let loc = mklocation p1.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
      let arg = mkpat ~ghost:true ~loc (Ppat_tuple [p1; pat_pl]) in
      ghpat_cons arg loc in
  handle_seq seq

let makeFrag loc body =
  let attribute = {
    attr_name = {txt = "JSX"; loc = loc};
    attr_payload = PStr [];
    attr_loc = loc
  }
  in
  { body with pexp_attributes = attribute :: body.pexp_attributes }


(* Applies attributes to the structure item, not the expression itself. Makes
 * structure item have same location as expression. *)
let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let ghexp_constraint loc e (t1, t2) =
  match t1, t2 with
  | Some t, None -> mkexp ~ghost:true ~loc (Pexp_constraint(e, t))
  | _, Some t -> mkexp ~ghost:true ~loc (Pexp_coerce(e, t1, t))
  | None, None -> assert false

let mk_record_expr ?loc (exten, fields) =
  match fields, exten with
  | [], Some expr -> expr
  | _ -> mkexp ?loc (Pexp_record (fields, exten))

let array_function ?(loc=dummy_loc()) str name =
  ghloc ~loc (Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name)))

let syntax_error loc s =
  raise_error (Other_syntax_error s) loc

let syntax_error_exp loc msg =
  Exp.extension ~loc (Reason_errors.error_extension_node loc msg)

let syntax_error_pat loc msg =
  Pat.extension ~loc (Reason_errors.error_extension_node loc msg)

let syntax_error_mty loc msg =
  Mty.extension ~loc (Reason_errors.error_extension_node loc msg)

let syntax_error_typ loc msg =
  Typ.extension ~loc (Reason_errors.error_extension_node loc msg)

let not_expecting start_pos end_pos nonterm =
  let location = mklocation start_pos end_pos in
  raise_error (Not_expecting (location, nonterm)) location

(* Taken from the OCaml compiler. The next comment is also present there.

   This is somewhat hackish: we don't want to allow "type nonrec t := ...",
   because the definition is nonrecursive by default. Simply removing
   "nonrec_flag" from the rule results in a shift/reduce conflict:
       "TYPE . UNDERSCORE"
   can either be a shift in the type_subst_declaration rule, or a reduce of
   nonrec_flag in the type_declaration rule.
   To avoid it we could either %inline the nonrec_flag rule, but "meh", or we
   could add nonrec_flag to the type_subst_declaration rule, and explicitely
   check if it was passed. In which case we raise a proper error. *)
let check_nonrec_absent loc nonrec_flag =
  match nonrec_flag with
  | Recursive ->
    () (* nothing to do, this happens when "nonrec" is absent from the source *)
  | Nonrecursive ->
    let err = {|"nonrec", type substitutions are non recursive by default|} in
    raise Syntaxerr.(Error(Not_expecting(loc, err)))

let mkexp_fun {Location.txt; loc} body =
  let loc = mklocation loc.loc_start body.pexp_loc.loc_end in
  match txt with
  | Term (label, default_expr, pat) ->
    Exp.fun_ ~loc label default_expr pat body
  | Type str ->
    Exp.newtype ~loc (mkloc str loc) body

let mkclass_fun {Location. txt ; loc} body =
  let loc = mklocation loc.loc_start body.pcl_loc.loc_end in
  match txt with
  | Term (label, default_expr, pat) ->
    Cl.fun_ ~loc label default_expr pat body
  | Type _ ->
    let pat = syntax_error_pat loc "(type) not allowed in classes" in
    Cl.fun_ ~loc Nolabel None pat body

let mktyp_arrow ({Location.txt = (label, cod); loc}, uncurried) dom =
  let loc = mklocation loc.loc_start dom.ptyp_loc.loc_end in
  let typ = mktyp ~loc (Ptyp_arrow (label, cod, dom)) in
  {typ with ptyp_attributes = (if uncurried then [uncurry_payload loc] else [])}

let mkcty_arrow ({Location.txt = (label, cod); loc}, uncurried) dom =
  let loc = mklocation loc.loc_start dom.pcty_loc.loc_end in
  let ct = mkcty ~loc (Pcty_arrow (label, cod, dom)) in
  {ct with pcty_attributes = (if uncurried then [uncurry_payload loc] else [])}

(**
  * process the occurrence of _ in the arguments of a function application
  * replace _ with a new variable, currently __x, in the arguments
  * return a wrapping function that wraps ((__x) => ...) around an expression
  * e.g. foo(_, 3) becomes (__x) => foo(__x, 3)
  *)
let process_underscore_application args =
  let exp_question = ref None in
  let hidden_var = "__x" in
  let check_arg ((lab, exp) as arg) = match exp.pexp_desc with
    | Pexp_ident ({ txt = Lident "_"} as id) ->
        let new_id = mkloc (Lident hidden_var) id.loc in
        let new_exp = mkexp (Pexp_ident new_id) ~loc:exp.pexp_loc in
        exp_question := Some new_exp;
        (lab, new_exp)
    | _ ->
        arg in
  let args = List.map check_arg args in
  let wrap exp_apply = match !exp_question with
    | Some {pexp_loc=loc} ->
        let pattern = mkpat (Ppat_var (mkloc hidden_var loc)) ~loc in
        begin match exp_apply.pexp_desc with
        (* Transform pipe first with underscore application correct:
         * 5->doStuff(3, _, 7);
         * (5 |. doStuff)(3, _, 7)
         * 5 |. (__x => doStuff(3, __x, 7))
         *)
        | Pexp_apply(
          {pexp_desc= Pexp_apply(
            {pexp_desc = Pexp_ident({txt = Longident.Lident("|.")})} as pipeExp,
            [Nolabel, arg1; Nolabel, ({pexp_desc = Pexp_ident _} as arg2)]
            (*         5                            doStuff                   *)
          )},
          args (* [3, __x, 7] *)
          ) ->
            (* build `doStuff(3, __x, 7)` *)
            let innerApply = {arg2 with pexp_desc = Pexp_apply(arg2, args)} in
            (* build `__x => doStuff(3, __x, 7)` *)
            let innerFun =
              mkexp (Pexp_fun (Nolabel, None, pattern, innerApply)) ~loc
            in
            (* build `5 |. (__x => doStuff(3, __x, 7))` *)
            {exp_apply with pexp_desc =
              Pexp_apply(pipeExp, [Nolabel, arg1; Nolabel, innerFun])
            }
        | _ ->
          mkexp (Pexp_fun (Nolabel, None, pattern, exp_apply)) ~loc
        end
    | None ->
        exp_apply in
  (args, wrap)

(**
  * Joins a 'body' and it's 'args' to form a Pexp_apply.
  * Example:
  * 'add' (body) and '[1, 2]' (args) become a Pexp_apply representing 'add(1, 2)'
  *
  * Note that `add(. 1, 2)(. 3, 4)` & `add(. 1, 2, . 3, 4)` both
  * give `[[@uncurry] 1, 2, [@uncurry] 3, 4]]` as args.
  * The dot is parsed as [@uncurry] to distinguish between specific
  * uncurrying and [@bs]. They can appear in the same arg:
  * `add(. [@bs] 1)` is a perfectly valid, the dot indicates uncurrying
  * for the whole application of 'add' and [@bs] sits on the `1`.
  * Due to the dot of uncurried application possibly appearing in any
  * position of the args, we need to post-process the args and split
  * all args in groups that are uncurried (or not).
  * add(. 1, . 2) should be parsed as (add(. 1))(. 2)
  * The args can be splitted here in [1] & [2], based on those groups
  * we can recursively build the correct nested Pexp_apply here.
  *  -> Pexp_apply (Pexp_apply (add, 1), 2)   (* simplified ast *)
  *)
let mkexp_app_rev startp endp (body, args) =
  let loc = mklocation startp endp in
  if args = [] then {body with pexp_loc = loc}
  else
  (*
   * Post process the arguments and transform [@uncurry] into [@bs].
   * Returns a tuple with a boolean (was it uncurried?) and
   * the posible rewritten arg.
   *)
  let rec process_args acc es =
    match es with
    | (lbl, e)::es ->
        let attrs = e.pexp_attributes in
        let hasUncurryAttr = ref false in
        let newAttrs = List.filter (function
          | { attr_name = {txt = "uncurry"}; attr_payload = PStr []; _} ->
              hasUncurryAttr := true;
              false
          | _ -> true) attrs
        in
        let uncurried = !hasUncurryAttr in
        let newArg = (lbl, { e with pexp_attributes = newAttrs }) in
        process_args ((uncurried, newArg)::acc) es
    | [] -> acc
    in
    (*
     * Groups all uncurried args falling under the same Pexp_apply
     * Example:
     *    add(. 2, 3, . 4, 5) or add(. 2, 3)(. 4, 5)  (equivalent)
     * This results in two groups: (true, [2, 3]) & (true, [4, 5])
     * Both groups have 'true' as their first tuple element, because
     * they are uncurried.
     * add(2, 3, . 4) results in the groups (false, [2, 3]) & (true, [4])
     *)
    let rec group grp acc = function
    | (uncurried, arg)::xs ->
        let (_u, grp) = grp in
        if uncurried = true then begin
          group (true, [arg]) ((_u, (List.rev grp))::acc) xs
        end else begin
          group (_u, (arg::grp)) acc xs
        end
    | [] ->
        let (_u, grp) = grp in
        List.rev ((_u, (List.rev grp))::acc)
    in
    (*
     * Recursively transforms all groups into a (possibly uncurried)
     * Pexp_apply
     *
     * Example:
     *   Given the groups (true, [2, 3]) & (true, [4, 5]) and body 'add',
     *   we get the two nested Pexp_apply associated with
     *   (add(. 2, 3))(. 4, 5)
     *)
    let rec make_appl body = function
      | args::xs ->
          let (uncurried, args) = args in
          let expr = if args = [] then body
          else
            let (args, wrap) = process_underscore_application args in
            let args_loc = match args, List.rev args with
              | ((_, s)::_), ((_, e)::_) -> mklocation s.pexp_loc.loc_start e.pexp_loc.loc_end
              | _ -> assert false in
            let expr = mkexp ~loc:args_loc (Pexp_apply (body, args)) in
            let expr = if uncurried then {expr with pexp_attributes = [uncurry_payload loc]} else expr in
            wrap expr
          in
            make_appl expr xs
      | [] -> {body with pexp_loc = loc}
    in
    let processed_args = process_args [] args in
    let groups = group (false, []) [] processed_args in
    make_appl body groups

let mkmod_app mexp marg =
  mkmod ~loc:(mklocation mexp.pmod_loc.loc_start marg.pmod_loc.loc_end)
    (Pmod_apply (mexp, marg))

let bigarray_function ?(loc=dummy_loc()) str name =
  ghloc ~loc (Ldot(Ldot(Lident "Bigarray", str), name))

let bigarray_get ?(loc=dummy_loc()) arr arg =
  let get = if !Clflags.fast then "unsafe_get" else "get" in
  match arg with
    [c1] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array1" get)),
                       [Nolabel, arr; Nolabel, c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array2" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array3" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3]))
  | coords ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Genarray" "get")),
                       [Nolabel, arr; Nolabel, mkexp ~ghost:true ~loc (Pexp_array coords)]))

let bigarray_set ?(loc=dummy_loc()) arr arg newval =
  let set = if !Clflags.fast then "unsafe_set" else "set" in
  match arg with
    [c1] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array1" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array2" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array3" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3; Nolabel, newval]))
  | coords ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Genarray" "set")),
                       [Nolabel, arr;
                        Nolabel, mkexp ~ghost:true ~loc (Pexp_array coords);
                        Nolabel, newval]))

let exp_of_label label =
  mkexp ~loc:label.loc (Pexp_ident {label with txt=Lident(Longident.last label.txt)})

let pat_of_label label =
  mkpat ~loc:label.loc (Ppat_var {label with txt=(Longident.last label.txt)})

let check_variable vl loc v =
  if List.mem v vl then
    raise_error (Variable_in_scope (loc,v)) loc

let varify_constructors var_names t =
  let rec loop t =
    let desc =
      match t.ptyp_desc with
      | Ptyp_any -> Ptyp_any
      | Ptyp_var x ->
          check_variable var_names t.ptyp_loc x;
          Ptyp_var x
      | Ptyp_arrow (label,core_type,core_type') ->
          Ptyp_arrow(label, loop core_type, loop core_type')
      | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
      | Ptyp_constr( { txt = Lident s }, []) when List.mem s var_names ->
          Ptyp_var s
      | Ptyp_constr(longident, lst) ->
          Ptyp_constr(longident, List.map loop lst)
      | Ptyp_object (lst, o) ->
          Ptyp_object
            (List.map
               (fun ({ pof_desc; _ } as obj) ->
                 let pof_desc' = match pof_desc with
                   | Otag (s, t) -> Otag (s, loop t)
                   | Oinherit t -> Oinherit (loop t)
                 in
                 { obj with pof_desc = pof_desc' }) lst, o)
      | Ptyp_class (longident, lst) ->
          Ptyp_class (longident, List.map loop lst)
      | Ptyp_alias(core_type, string) ->
          check_variable var_names t.ptyp_loc string;
          Ptyp_alias(loop core_type, string)
      | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
          Ptyp_variant(List.map loop_row_field row_field_list,
                       flag, lbl_lst_option)
      | Ptyp_poly(string_lst, core_type) ->
          List.iter (fun x -> check_variable var_names t.ptyp_loc x.txt) string_lst;
          Ptyp_poly(string_lst, loop core_type)
      | Ptyp_package(longident,lst) ->
          Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
      | Ptyp_extension (s, arg) ->
          Ptyp_extension (s, arg)
    in
    {t with ptyp_desc = desc}
  and loop_row_field  =
    fun ({ prf_desc; _} as rf) ->
      let prf_desc' = match prf_desc with
        | Rtag(label, flag, lst) ->
          Rtag(label, flag, List.map loop lst)
        | Rinherit t ->
          Rinherit (loop t)
      in
      { rf with prf_desc = prf_desc' }
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
  let typ = mktyp ~ghost:true (Ptyp_poly(newtypes,varify_constructors (List.map (fun {txt} -> txt) newtypes) core_type)) in
  (exp, typ)


let struct_item_extension (ext_attrs, ext_id) structure_items =
  mkstr ~ghost:true (Pstr_extension ((ext_id, PStr structure_items), ext_attrs))

let expression_extension ?loc (ext_attrs, ext_id) item_expr =
  let extension = (ext_id, PStr [mkstrexp item_expr []]) in
  let loc = match loc with
    | Some loc -> loc
    | None -> make_ghost_loc (dummy_loc ())
  in
  Exp.extension ~loc ~attrs:ext_attrs extension

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
(*   | Some id -> mkexp ~ghost:true (Pexp_extension (id, PStr [mkstrexp body []])) *)

(* Why not just mkexp with the right attributes in the first place? *)
(* let mkexp_attrs d attrs = *)
(*   wrap_exp_attrs (mkexp d) attrs *)

let mkcf_attrs ?(loc=dummy_loc()) d attrs =
  Cf.mk ~loc ~attrs d

let mkctf_attrs d attrs =
  Ctf.mk ~attrs d

let mklbs ext rf lb loc =
  { lbs_bindings = [lb];
    lbs_rec = rf;
    lbs_extension = ext;
    lbs_loc = loc; }

let addlbs lbs lbs' =
  { lbs with lbs_bindings = lbs.lbs_bindings @ lbs' }

let val_of_let_bindings lbs =
  let str = Str.value lbs.lbs_rec lbs.lbs_bindings in
  match lbs.lbs_extension with
  | None -> str
  | Some ext -> struct_item_extension ext [str]

let expr_of_let_bindings ~loc lbs body =
  let item_expr = Exp.let_ ~loc lbs.lbs_rec lbs.lbs_bindings body in
  match lbs.lbs_extension with
  | None -> item_expr
  | Some ext -> expression_extension ~loc:(make_ghost_loc loc) ext item_expr

let class_of_let_bindings lbs body =
  if lbs.lbs_extension <> None then
    raise_error (Not_expecting (lbs.lbs_loc, "extension")) lbs.lbs_loc;
  Cl.let_ lbs.lbs_rec lbs.lbs_bindings body


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
  expr = begin fun mapper expr ->
    match expr with
      | {pexp_desc=Pexp_construct(lid, args);
         pexp_loc;
         pexp_attributes} when attributes_conflicted "implicit_arity" "explicit_arity" pexp_attributes ->
         let new_args =
           match args with
             | Some {pexp_desc = Pexp_tuple [sp]} -> Some sp
             | _ -> args in
         super.expr mapper
         { pexp_desc=Pexp_construct(lid, new_args);
           pexp_loc;
           pexp_attributes = normalized_attributes "explicit_arity" pexp_attributes;
           pexp_loc_stack = []
         }
      | x -> super.expr mapper x
  end;
  pat = begin fun mapper pattern ->
    match pattern with
      | {ppat_desc=Ppat_construct(lid, args);
         ppat_loc;
         ppat_attributes} when attributes_conflicted "implicit_arity" "explicit_arity" ppat_attributes ->
         let new_args =
           match args with
             | Some {ppat_desc = Ppat_tuple [sp]} -> Some sp
             | _ -> args in
         super.pat mapper
         { ppat_desc=Ppat_construct(lid, new_args);
           ppat_loc;
           ppat_attributes = normalized_attributes "explicit_arity" ppat_attributes;
           ppat_loc_stack = [];
         }
      | x -> super.pat mapper x
  end;
}

let reason_mapper =
  default_mapper
  |> reason_to_ml_swap_operator_mapper
  |> arity_conflict_resolving_mapper

let rewriteFunctorApp module_name elt loc =
  let rec applies = function
    | Lident _ -> false
    | Ldot (m, _) -> applies m
    | Lapply (_, _) -> true in
  let rec flattenModName = function
    | Lident id -> id
    | Ldot (m, id) -> flattenModName m ^ "." ^ id
    | Lapply (m1, m2) -> flattenModName m1 ^ "(" ^ flattenModName m2 ^ ")" in
  let rec mkModExp = function
    | Lident id -> mkmod ~loc (Pmod_ident {txt=Lident id; loc})
    | Ldot (m, id) -> mkmod ~loc (Pmod_ident {txt=Ldot (m, id); loc})
    | Lapply (m1, m2) -> mkmod ~loc (Pmod_apply (mkModExp m1, mkModExp m2)) in
  if applies module_name then
    let flat = flattenModName module_name in
    mkexp ~loc (Pexp_letmodule({txt=flat; loc},
                         mkModExp module_name,
                         mkexp(Pexp_ident {txt=Ldot (Lident flat, elt); loc})))
  else
    mkexp ~loc (Pexp_ident {txt=Ldot (module_name, elt); loc})

let jsx_component lid attrs children loc =
  let is_module_name = function
    | Lident s
    | Ldot (_, s) ->
      (* s will be non-empty so the 0th access is fine. Modules can't start with underscore *)
       String.get s 0 != '_' && s = String.capitalize_ascii s
    | Lapply (_, _) -> true
  in
  let element_fn = if is_module_name lid.txt then
      rewriteFunctorApp lid.txt "createElement" lid.loc
    else
      mkexp ~loc:lid.loc (Pexp_ident lid)
  in
  let body = mkexp(Pexp_apply(element_fn, attrs @ children)) ~loc in
  let attribute = {
    attr_name = {txt = "JSX"; loc = loc};
    attr_payload = PStr [];
    attr_loc = loc;
  }
  in
  { body with pexp_attributes = attribute :: body.pexp_attributes }

let rec ignoreLapply = function
  | Lident id -> Lident id
  | Ldot (lid, id) -> Ldot (ignoreLapply lid, id)
  | Lapply (m1, _) -> ignoreLapply m1

(* Like Longident.flatten, but ignores `Lapply`s. Useful because 1) we don't want to require `Lapply` in
   closing tags, and 2) Longident.flatten doesn't support `Lapply`. *)
let rec flattenWithoutLapply = function
  | Lident id -> [id]
  | Ldot (lid, id) -> flattenWithoutLapply lid @ [id]
  | Lapply (m1, _) -> flattenWithoutLapply m1

let ensureTagsAreEqual startTag endTag loc =
  if ignoreLapply startTag <> endTag then
    let startTag = String.concat "" (flattenWithoutLapply startTag) in
    let endTag = String.concat "" (flattenWithoutLapply endTag) in
    if endTag <> "" then
    Printf.ksprintf (syntax_error loc)
      "Start tag <%s> does not match end tag </%s>" startTag endTag

(* `{. "foo": bar}` -> `Js.t {. foo: bar}` and {.. "foo": bar} -> `Js.t {.. foo: bar} *)
let mkBsObjTypeSugar ~loc ~closed rows =
  let obj = mktyp ~loc (Ptyp_object (rows, closed)) in
  let jsDotTCtor = { txt = Longident.Ldot (Longident.Lident "Js", "t"); loc } in
  mktyp(Ptyp_constr(jsDotTCtor, [obj]))

let doc_loc loc = {txt = "ocaml.doc"; loc = loc}

let doc_attr text loc =
  (* Here is where we will convert from markdown to odoc - transform the "text" *)
  let open Parsetree in
  let exp =
    { pexp_desc = Pexp_constant (Pconst_string(text, None));
      pexp_loc = loc;
      pexp_attributes = [];
      pexp_loc_stack = [];
    }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
  in
  {
    attr_name = doc_loc loc;
    attr_payload = PStr [item];
    attr_loc = loc
  }

let prepend_attrs_to_labels attrs = function
  | [] -> [] (* not possible for valid inputs *)
  | x :: xs -> {x with pld_attributes = attrs @ x.pld_attributes} :: xs

let raise_record_trailing_semi_error loc =
  syntax_error_exp loc
    "Record entries are separated by comma; \
     we've found a semicolon instead."

let raise_record_trailing_semi_error' loc =
  (Some (raise_record_trailing_semi_error loc), [])

let record_exp_spread_msg =
  "Records can only have one `...` spread, at the beginning.
Explanation: since records have a known, fixed shape, a spread like `{a, ...b}` wouldn't make sense, as `b` would override every field of `a` anyway."

let record_pat_spread_msg =
  "Record's `...` spread is not supported in pattern matches.
Explanation: you can't collect a subset of a record's field into its own record, since a record needs an explicit declaration and that subset wouldn't have one.
Solution: you need to pull out each field you want explicitly."

let lowercase_module_msg =
  "Module names must start with an uppercase letter."

(* Handles "over"-parsing of spread syntax with `opt_spread`.
 * The grammar allows a spread operator at every position, when
 * generating the parsetree we raise a helpful error message. *)
let filter_raise_spread_syntax msg nodes =
  List.map (fun (dotdotdot, node) ->
      begin match dotdotdot with
        | Some dotdotdotLoc -> syntax_error dotdotdotLoc msg
        | None -> ()
      end;
      node
    ) nodes

(*
 * See https://github.com/ocaml/ocaml/commit/e1e03820e5fea322aa3156721bc1cc0231668101
 * Rely on the parsing rules for generic module types, and then
 * extract a package type, enabling more explicit error messages
 * *)
let package_type_of_module_type pmty =
  let map_cstr = function
    | Pwith_type (lid, ptyp) ->
        let loc = ptyp.ptype_loc in
        if ptyp.ptype_params <> [] then
          syntax_error loc "parametrized types are not supported";
        if ptyp.ptype_cstrs <> [] then
          syntax_error loc "constrained types are not supported";
        if ptyp.ptype_private <> Public then
          syntax_error loc "private types are not supported";

        (* restrictions below are checked by the 'with_constraint' rule *)
        assert (ptyp.ptype_kind = Ptype_abstract);
        assert (ptyp.ptype_attributes = []);
        let ty =
          match ptyp.ptype_manifest with
          | Some ty -> ty
          | None -> assert false
        in
        [lid, ty]
    | _ ->
       syntax_error pmty.pmty_loc "only 'with type t =' constraints are supported";
       []
  in
  match pmty with
  | {pmty_desc = Pmty_ident lid} -> Some (lid, [])
  | {pmty_desc = Pmty_with({pmty_desc = Pmty_ident lid}, cstrs)} ->
    Some (lid, List.flatten (List.map map_cstr cstrs))
  | _ -> None

let add_brace_attr expr =
  let attr = {
    attr_name = Location.mknoloc "reason.preserve_braces";
    attr_payload = PStr [];
    attr_loc = Location.none
  }
  in
  { expr with pexp_attributes= attr :: expr.pexp_attributes }

%}

%[@recover.prelude

  open Reason_migrate_parsetree.OCaml_408.Ast
  open Parsetree
  open Ast_helper

  let default_loc = ref Location.none

  let default_expr () =
    let id = Location.mkloc "merlin.hole" !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))

  let default_pattern () = Pat.any ~loc:!default_loc ()

  let default_module_expr () = Mod.structure ~loc:!default_loc[]
  let default_module_type () = Mty.signature ~loc:!default_loc[]

]

(* Tokens *)

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token <string> NAMETAG [@recover.expr ""] [@recover.cost 2]
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
  [@recover.expr ("0.0", None)] [@recover.cost 2]
%token FOR
%token FUN ES6_FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERDOTDOTDOT
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0 [@recover.expr ""] [@recover.cost 2]
%token <string> INFIXOP1 [@recover.expr ""] [@recover.cost 2]
%token <string> INFIXOP2 [@recover.expr ""] [@recover.cost 2]
%token <string> INFIXOP3 [@recover.expr ""] [@recover.cost 2]
(* SLASHGREATER is an INFIXOP3 that is handled specially *)
%token SLASHGREATER
%token <string> INFIXOP4
%token <string> LETOP
%token <string> ANDOP
%token INHERIT
%token INITIALIZER
%token <string * char option> INT
  [@recover.expr ("0", None)] [@recover.cost 2]
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
%token <string> LESSIDENT [@recover.expr ""] [@recover.cost 2]
%token <string> LESSUIDENT [@recover.expr ""] [@recover.cost 2]
%token LESSGREATER
%token LESSSLASHGREATER
%token LESSDOTDOTGREATER
%token EQUALGREATER
%token LET
%token <string> LIDENT [@recover.expr ""] [@recover.cost 2]
%token LPAREN
%token LBRACKETAT
%token <int * int> VERSION_ATTRIBUTE
%token OF
%token PRI
%token SWITCH
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT [@recover.expr 0n] [@recover.cost 2]
%token NEW
%token NONREC
%token OBJECT
%token OPEN
%token OR
(* %token PARSER *)
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP [@recover.expr ""] [@recover.cost 2]
%token <string> POSTFIXOP [@recover.expr ""] [@recover.cost 2]
%token PUB
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token <string> LESSSLASHIDENTGREATER [@recover.expr ""] [@recover.cost 2]
%token SEMI
%token SEMISEMI
%token SHARP
%token <string> SHARPOP
%token SHARPEQUAL
%token SIG
%token STAR
%token <string * string option * string option> STRING
  [@recover.expr ("", None, None)] [@recover.cost 2]

%token <string> STRING_TEMPLATE_TERMINATED
  [@recover.expr ("")] [@recover.cost 2]
%token <string> STRING_TEMPLATE_SEGMENT_LBRACE
  [@recover.expr ("")] [@recover.cost 2]

%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT [@recover.expr ""] [@recover.cost 2]
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT
%token <string> DOCSTRING

%token EOL

(* Precedences and associativities.

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

*)
(* Question: Where is the SEMI explicit precedence? *)
%nonassoc below_SEMI
%right    EQUALGREATER                  (* core_type2 (t => t => t) *)
%right    COLON
%right    EQUAL                         (* below COLONEQUAL (lbl = x := e) *)
%right    COLONEQUAL                    (* expr (e := e := e) *)
%nonassoc QUESTION
%nonassoc WITH             (* below BAR  (match ... with ...) *)
%nonassoc AND             (* above WITH (module rec A: SIG with ... and ...) *)
%nonassoc ELSE                          (* (if ... then ... else ...) *)
%nonassoc AS
%nonassoc below_BAR                     (* Allows "building up" of many bars *)
%left     BAR                           (* pattern (p|p|p) *)

%right    OR BARBAR                     (* expr (e || e || e) *)
%right    AMPERSAND AMPERAMPER          (* expr (e && e && e) *)
%left     INFIXOP0 LESS GREATER GREATERDOTDOTDOT (* expr (e OP e OP e) *)
%left     LESSDOTDOTGREATER (* expr (e OP e OP e) *)
%right    INFIXOP1                      (* expr (e OP e OP e) *)
%right    COLONCOLON                    (* expr (e :: e :: e) *)
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ (* expr (e OP e OP e) *)
%left     PERCENT INFIXOP3 SLASHGREATER STAR          (* expr (e OP e OP e) *)
%right    INFIXOP4                      (* expr (e OP e OP e) *)

(**
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
 *)

%nonassoc attribute_precedence

%nonassoc prec_unary (* unary - *)
%nonassoc prec_constant_constructor     (* cf. simple_expr (C versus C x) *)
(* Now that commas require wrapping parens (for tuples), prec_constr_appl no
* longer needs to be above COMMA, but it doesn't hurt *)
%nonassoc prec_constr_appl              (* above AS BAR COLONCOLON COMMA *)

(* PREFIXOP and BANG precedence *)
%nonassoc below_DOT_AND_SHARP           (* practically same as below_SHARP but we convey purpose *)
%nonassoc SHARP                         (* simple_expr/toplevel_directive *)
%nonassoc below_DOT

(* We need SHARPEQUAL to have lower precedence than `[` to make e.g.
   this work: `foo #= bar[0]`. Otherwise it would turn into `(foo#=bar)[0]` *)
%left     SHARPEQUAL
%nonassoc POSTFIXOP
(* LBRACKET and DOT are %nonassoc in OCaml, because the left and right sides
   are never the same, therefore there doesn't need to be a precedence
   disambiguation. This could also work in Reason, but by grouping the tokens
   below into a single precedence rule it becomes clearer that they all have the
   same precedence. *)
%left     SHARPOP MINUSGREATER LBRACKET DOT
(* Finally, the first tokens of simple_expr are above everything else. *)
%nonassoc LBRACKETLESS LBRACELESS LBRACE LPAREN



(* Entry points *)

%start implementation                   (* for implementation files *)
%type <Reason_migrate_parsetree.Ast_408.Parsetree.structure> implementation
%start interface                        (* for interface files *)
%type <Reason_migrate_parsetree.Ast_408.Parsetree.signature> interface
%start toplevel_phrase                  (* for interactive use *)
%type <Reason_migrate_parsetree.Ast_408.Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         (* for the #use directive *)
%type <Reason_migrate_parsetree.Ast_408.Parsetree.toplevel_phrase list> use_file
%start parse_core_type
%type <Reason_migrate_parsetree.Ast_408.Parsetree.core_type> parse_core_type
%start parse_expression
%type <Reason_migrate_parsetree.Ast_408.Parsetree.expression> parse_expression
%start parse_pattern
%type <Reason_migrate_parsetree.Ast_408.Parsetree.pattern> parse_pattern

(* Instead of reporting an error directly, productions specified
 * below will be reduced first and popped up in the stack to a higher
 * level production.
 *
 * This is essential to error reporting as it is much friendier to provide
 * a higher level error (e.g., "Expecting the parens to be closed" )
 * as opposed to a low-level one (e.g., "Expecting to finish
 * current type definition").
 *
 * See Menhir's manual for more details.
 *)
%on_error_reduce structure_item
                 let_binding_body
                 letop_bindings
                 letop_binding_body
                 as_loc(attribute)+
                 type_longident
                 constr_longident
                 pattern
                 nonrec_flag
                 val_ident
                 SEMI?
                 fun_def(EQUAL,core_type)
                 fun_def(EQUALGREATER,non_arrowed_core_type)
                 expr_optional_constraint
%%

(* Entry points *)

implementation:
  structure EOF
  {
    let itms = Reason_version.Ast_nodes.inject_attr_from_version_impl $1 in
    apply_mapper_to_structure itms reason_mapper
  }
;

interface:
  signature EOF
  {
    let itms = Reason_version.Ast_nodes.inject_attr_from_version_intf $1 in
    apply_mapper_to_signature itms reason_mapper
  }
;

toplevel_phrase: embedded
  ( EOF                              { raise End_of_file }
  | structure_item     SEMI          { Ptop_def $1 }
  | toplevel_directive SEMI          { $1 }
  ) { apply_mapper_to_toplevel_phrase $1 reason_mapper }
;

use_file_no_mapper: embedded
( EOF                              { [] }
  | structure_item     SEMI use_file_no_mapper { Ptop_def $1  :: $3 }
  | toplevel_directive SEMI use_file_no_mapper { $1 :: $3 }
  | structure_item     EOF           { [Ptop_def $1 ] }
  | toplevel_directive EOF           { [$1] }
  ) { $1 }
;

use_file:
  use_file_no_mapper { apply_mapper_to_use_file $1 reason_mapper }
;

parse_core_type:
  core_type EOF
  { apply_mapper_to_type $1 reason_mapper }
;

parse_expression:
  expr EOF
  { apply_mapper_to_expr $1 reason_mapper }
;

parse_pattern:
  pattern EOF
  { apply_mapper_to_pattern $1 reason_mapper }
;

(* Module expressions *)

module_parameter:
as_loc
  ( LPAREN RPAREN
    { (Some (mkloc "*" (mklocation $startpos $endpos)), None) }
  | as_loc(UIDENT {$1} | UNDERSCORE {"_"}) COLON module_type
    { (Some $1, Some $3) }
  | module_type
    { (None, Some $1) }
) {$1};

%inline two_or_more_module_parameters_comma_list:
  lseparated_two_or_more(COMMA, module_parameter) COMMA? {$1}
;

functor_parameters:
  | LPAREN RPAREN
    { let loc = mklocation $startpos $endpos in
      [mkloc (Some (mkloc "*" loc), None) loc]
    }
  (* This single parameter case needs to be explicitly specified so that
   * menhir can automatically remove the conflict between sigature:
   *
   *   include (X)
   *   include (X, Y) => Z
   *
   * Even though the later is non-sensical (maybe it won't be one day?)
   *)
  | LPAREN module_parameter RPAREN {[$2]}
  | LPAREN module_parameter COMMA RPAREN {[$2]}
  | parenthesized(two_or_more_module_parameters_comma_list) { $1 }
;

module_complex_expr:
mark_position_mod
  ( module_expr
    { $1 }
  | module_expr COLON module_type
    { mkmod(Pmod_constraint($1, $3)) }
  | VAL expr
    { mkmod(Pmod_unpack $2) }
  | VAL expr COLON MODULE? package_type
    { let loc = mklocation $symbolstartpos $endpos in
      mkmod (Pmod_unpack(
           mkexp ~ghost:true ~loc (Pexp_constraint($2, $5))))
    }
  | VAL expr COLON MODULE? package_type COLONGREATER MODULE? package_type
    { let loc = mklocation $symbolstartpos $endpos in
      mkmod (Pmod_unpack(mkexp ~ghost:true ~loc (Pexp_coerce($2, Some $5, $8)))) }
  | VAL expr COLONGREATER MODULE? package_type
    { let loc = mklocation $symbolstartpos $endpos in
      mkmod (Pmod_unpack(mkexp ~ghost:true ~loc (Pexp_coerce($2, None, $5))))
    }
  ) {$1};

module_arguments_comma_list:
  lseparated_list(COMMA, module_complex_expr) COMMA? {$1}
;
module_arguments:
  | module_expr_structure { [$1] }
  | parenthesized(module_arguments_comma_list)
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
  | LPAREN module_complex_expr RPAREN
    { $2 }
  | LPAREN RPAREN
    { mkmod (Pmod_structure []) }
  | extension
    { mkmod (Pmod_extension $1) }
  (**
   * Although it would be nice (and possible) to support annotated return value
   * here, that wouldn't be consistent with what is possible for functions.
   * Update: In upstream, it *is* possible to annotate return values for
   * lambdas.
   *)
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
  | attribute module_expr %prec attribute_precedence
    { {$2 with pmod_attributes = $1 :: $2.pmod_attributes} }
  ) {$1};

(**
 * Attributes/Extension points TODO:
 * - Faux-ternary to support extension points (printing/parsing).
 * - Audit to ensure every [item attributes] [item extensions] is supported.
 * - wrap_exp_attrs / mkexp_attrs cleanup - no need for the confusing
 * indirection.
 * - Ensure proper parsing as ensured by commit:
 *   4c48d802cb9e8110ab3b57ca0b6a02fdd5655283
 * - Support the Item Extension + Item Attributes pattern/sugar/unification for
 * all items in let sequences (let module etc / let open).
 *)

(*
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
 *)

structure:
  | (* Empty *) { [] }
  | structure_item { $1 }
  | structure_item SEMI structure { $1 @ $3 }
;

opt_LET_MODULE_ident:
  | opt_LET_MODULE as_loc(UIDENT) { $2 }
  | opt_LET_MODULE as_loc(LIDENT)
    { syntax_error $2.loc lowercase_module_msg; $2 }
;

opt_LET_MODULE_REC_ident:
  | opt_LET_MODULE REC as_loc(UIDENT) { $3 }
  | opt_LET_MODULE REC as_loc(LIDENT)
    { syntax_error $3.loc lowercase_module_msg; $3 }
;

structure_item:
  | mark_position_str
    (* We consider a floating expression to be equivalent to a single let binding
       to the "_" (any) pattern.  *)
    ( item_attributes unattributed_expr
      { mkstrexp $2 $1 }
    | item_attributes item_extension_sugar structure_item
      { let (ext_attrs, ext_id) = $2 in
        struct_item_extension ($1@ext_attrs, ext_id) $3 }
    | item_attributes
      EXTERNAL as_loc(val_ident) COLON core_type EQUAL primitive_declaration
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr (Pstr_primitive (Val.mk $3 $5 ~prim:$7 ~attrs:$1 ~loc)) }
    | item_attributes
      EXTERNAL as_loc(val_ident) COLON core_type SEMI
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr (Pstr_primitive (Val.mk $3 $5 ~prim:[""] ~attrs:$1 ~loc)) }
    | type_declarations
      { let (nonrec_flag, tyl) = $1 in mkstr(Pstr_type (nonrec_flag, tyl)) }
    | str_type_extension
      { mkstr(Pstr_typext $1) }
    | str_exception_declaration
      { mkstr(Pstr_exception (Te.mk_exception ~loc:$1.pext_loc $1)) }
    | item_attributes opt_LET_MODULE_ident module_binding_body
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr(Pstr_module (Mb.mk $2 $3 ~attrs:$1 ~loc)) }
    | item_attributes opt_LET_MODULE_REC_ident module_binding_body
      and_module_bindings*
      { let loc = mklocation $symbolstartpos $endpos($2) in
        mkstr (Pstr_recmodule ((Mb.mk $2 $3 ~attrs:$1 ~loc) :: $4))
      }
    | item_attributes MODULE TYPE OF? as_loc(ident)
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr(Pstr_modtype (Mtd.mk $5 ~attrs:$1 ~loc)) }
    | item_attributes MODULE TYPE OF? as_loc(ident) module_type_body(EQUAL)
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr(Pstr_modtype (Mtd.mk $5 ~typ:$6 ~attrs:$1 ~loc)) }
    | open_declaration
      { mkstr(Pstr_open $1) }
    | item_attributes CLASS class_declaration_details and_class_declaration*
      { let (ident, binding, virt, params) = $3 in
        let loc = mklocation $symbolstartpos $endpos($3) in
        let first = Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc in
        mkstr (Pstr_class (first :: $4))
      }
    | class_type_declarations
        (* Each declaration has their own preceeding attribute* *)
      { mkstr(Pstr_class_type $1) }
    | item_attributes INCLUDE module_expr
      { let loc = mklocation $symbolstartpos $endpos in
        mkstr(Pstr_include (Incl.mk $3 ~attrs:$1 ~loc))
      }
    | item_attributes item_extension
      (* No sense in having item_extension_sugar for something that's already an
       * item_extension *)
      { mkstr(Pstr_extension ($2, $1)) }
    | let_bindings
      { val_of_let_bindings $1 }
    ) { [$1] }
   | located_attributes
     {
       List.map
        (fun x -> mkstr ~loc:x.loc (Pstr_attribute (make_floating_doc x.txt)))
        $1
     }
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
  { Mb.mk $3 $4 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos) }
;

(* Module types *)

(*
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
*)

(* Allowed in curried let bidings *)

simple_module_type:
mark_position_mty
  ( parenthesized(module_parameter)
    { match $1.txt with
      | (None, Some x) -> x
      | _ -> syntax_error_mty $1.loc "Expecting a simple module type"
    }
  | module_type_signature { $1 }
  | as_loc(mty_longident)
    { mkmty (Pmty_ident $1) }
  | extension
    { mkmty (Pmty_extension $1) }
  | LPAREN MODULE TYPE OF module_expr RPAREN
    { mkmty (Pmty_typeof $5) }
  ) {$1};

module_type_signature:
  LBRACE signature RBRACE
  { mkmty ~loc:(mklocation $startpos $endpos) (Pmty_signature $2) }
;

(*
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
*)

%inline with_constraints:
  WITH lseparated_nonempty_list(AND, with_constraint) { $2 }

module_type:
mark_position_mty
  ( module_type with_constraints
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
    { mkmty (Pmty_with($1, $2)) }
  | simple_module_type
    {$1}
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
  | (* Empty *) { [] }
  | signature_items { $1 }
  | signature_items SEMI signature { $1 @ $3 }
;


signature_item:
  | item_attributes
    LET as_loc(val_ident) COLON core_type
    { let loc = mklocation $startpos($2) $endpos in
      Psig_value (Val.mk $3 $5 ~attrs:$1 ~loc)
    }
  | item_attributes
    EXTERNAL as_loc(val_ident) COLON core_type EQUAL primitive_declaration
    { let loc = mklocation $symbolstartpos $endpos in
      Psig_value (Val.mk $3 $5 ~prim:$7 ~attrs:$1 ~loc)
    }
  | item_attributes
    EXTERNAL as_loc(val_ident) COLON core_type SEMI
    { let loc = mklocation $symbolstartpos $endpos in
      Psig_value (Val.mk $3 $5 ~prim:[""] ~attrs:$1 ~loc)
    }
  | type_declarations
    { let (nonrec_flag, tyl) = $1 in Psig_type (nonrec_flag, tyl) }
   | type_subst_declarations
    { Psig_typesubst $1 }
  | sig_type_extension
    { Psig_typext $1 }
  | sig_exception_declaration
    { Psig_exception $1 }
  | item_attributes opt_LET_MODULE_ident module_declaration
    { let loc = mklocation $symbolstartpos $endpos in
      Psig_module (Md.mk $2 $3 ~attrs:$1 ~loc)
    }
  | item_attributes opt_LET_MODULE_ident EQUAL as_loc(mod_longident)
    { let loc = mklocation $symbolstartpos $endpos in
      let loc_mod = mklocation $startpos($4) $endpos($4) in
      Psig_module (
        Md.mk
            $2
            (Mty.alias ~loc:loc_mod $4)
            ~attrs:$1
            ~loc
            )
    }
  | item_attributes opt_LET_MODULE as_loc(UIDENT) COLONEQUAL as_loc(mod_ext_longident)
    { Psig_modsubst (Ms.mk $3 $5 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos))}
  | item_attributes opt_LET_MODULE_REC_ident module_type_body(COLON)
    and_module_rec_declaration*
    { let loc = mklocation $symbolstartpos $endpos($3) in
      Psig_recmodule (Md.mk $2 $3 ~attrs:$1 ~loc :: $4) }
  | item_attributes MODULE TYPE as_loc(ident)
    { let loc = mklocation $symbolstartpos $endpos in
      Psig_modtype (Mtd.mk $4 ~attrs:$1 ~loc)
    }
  | item_attributes MODULE TYPE as_loc(ident) module_type_body(EQUAL)
    { let loc = mklocation $symbolstartpos $endpos in
      Psig_modtype (Mtd.mk $4 ~typ:$5 ~loc ~attrs:$1)
    }
  | open_description
    { Psig_open $1 }
  | item_attributes INCLUDE module_type
    { let loc = mklocation $symbolstartpos $endpos in
      Psig_include (Incl.mk $3 ~attrs:$1 ~loc)
    }
  | class_descriptions
    { Psig_class $1 }
  | class_type_declarations
    { Psig_class_type $1 }
  | item_attributes item_extension
    { Psig_extension ($2, $1) }
;

signature_items:
  | as_loc(signature_item) { [mksig ~loc:$1.loc $1.txt] }
  | located_attributes
    { List.map
      (fun x -> mksig ~loc:x.loc (Psig_attribute (make_floating_doc x.txt)))
      $1 }
;

open_declaration:
  item_attributes OPEN override_flag module_expr
  { Opn.mk $4 ~override:$3 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos) }
;

open_description:
  item_attributes OPEN override_flag as_loc(mod_longident)
  { Opn.mk $4 ~override:$3 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos) }
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
  { Md.mk $3 $4 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos) }
;

(* Class expressions *)

and_class_declaration:
  item_attributes AND class_declaration_details
  { let (ident, binding, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc
  }
;

class_declaration_details:
  virtual_flag as_loc(LIDENT) optional_type_params(type_variable_without_underscore)
  ioption(labeled_pattern_list) class_declaration_body
  {
    let tree = match $4 with
    | None -> []
    | Some (lpl, _uncurried) -> lpl
    in
    let body = List.fold_right mkclass_fun tree $5 in
    let params = $3 in
    ($2, body, $1, params)
  }
;

class_declaration_body:
  preceded(COLON, class_constructor_type)?
  either(preceded(EQUAL, class_expr), class_body_expr)
  { match $1 with
    | None -> $2
    | Some ct -> Cl.constraint_ ~loc:(mklocation $symbolstartpos $endpos) $2 ct
  }
;

class_expr_lets_and_rest:
mark_position_cl
  ( class_expr { $1 }
  | let_bindings SEMI class_expr_lets_and_rest
    { class_of_let_bindings $1 $3 }
  | object_body { mkclass (Pcl_structure $1) }
  | LET? OPEN override_flag as_loc(mod_longident) SEMI class_expr_lets_and_rest
    { let loc = mklocation $startpos($2) $endpos($3) in
      let od = Opn.mk ~override:$3 ~loc $4 in
      mkclass (Pcl_open (od, $6)) }
  ) {$1};

object_body_class_fields:
  | lseparated_list(SEMI, class_field) SEMI? { List.concat $1 }

object_body:
  | loption(located_attributes)
    mark_position_pat(class_self_expr)
    { let attrs = List.map (fun x -> mkcf ~loc:x.loc (Pcf_attribute x.txt)) $1 in
      Cstr.mk $2 attrs }
  | loption(located_attributes)
    mark_position_pat(class_self_expr) SEMI
    object_body_class_fields
    { let attrs = List.map (fun x -> mkcf ~loc:x.loc (Pcf_attribute x.txt)) $1 in
      Cstr.mk $2 (attrs @ $4) }
  | object_body_class_fields
    { let loc = mklocation $symbolstartpos $symbolstartpos in
      Cstr.mk (mkpat ~loc (Ppat_var (mkloc "this" loc))) $1 }
;

class_self_expr:
  | AS pattern { $2 }

class_expr:
mark_position_cl
  ( class_simple_expr
    { $1 }
  | either(ES6_FUN,FUN) labeled_pattern_list EQUALGREATER class_expr
    { let (lp, _) = $2 in
      List.fold_right mkclass_fun lp $4 }
  | class_simple_expr labeled_arguments
      (**
       * This is an interesting way to "partially apply" class construction:
       *
       * let inst = new oldclass 20;
       * class newclass = oldclass withInitArg;
       * let inst = new newclass;
       *)
    { mkclass(Pcl_apply($1, $2)) }
  | attribute class_expr
    { {$2 with pcl_attributes = $1 :: $2.pcl_attributes} }
  (*
    When referring to class expressions (not regular types that happen to be
    classes), you must refer to it as a class. This gives syntactic real estate
    to place type parameters which are distinguished from constructor application
    of arguments.

       class myClass 'x = (class yourClass 'x int) y z;
       inherit (class yourClass float int) initArg initArg;
       ...
       let myVal: myClass int = new myClass 10;

   *)
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
  | LPAREN class_expr COLON class_constructor_type RPAREN
    { mkclass(Pcl_constraint($2, $4)) }
  | LPAREN class_expr RPAREN
    { $2 }
  ) {$1};

%inline class_body_expr: LBRACE class_expr_lets_and_rest RBRACE { $2 };

class_field:
  | mark_position_cf
    ( item_attributes INHERIT override_flag class_expr as_loc(preceded(AS,LIDENT))?
      { mkcf_attrs (Pcf_inherit ($3, $4, $5)) $1 }
    | item_attributes VAL value
      { mkcf_attrs (Pcf_val $3) $1 }
    | item_attributes either(PUB {Public}, PRI {Private}) method_
      { let (a, b) = $3 in mkcf_attrs (Pcf_method (a, $2, b)) $1 }
    | item_attributes CONSTRAINT constrain_field
      { mkcf_attrs (Pcf_constraint $3) $1 }
    | item_attributes INITIALIZER mark_position_exp(simple_expr)
      { mkcf_attrs (Pcf_initializer $3) $1 }
    | item_attributes item_extension
      { mkcf_attrs (Pcf_extension $2) $1 }
    ) { [$1] }
  | located_attributes
    { List.map
      (fun x -> mkcf ~loc:x.loc (Pcf_attribute (make_floating_doc x.txt)))
      $1 }
;

value:
(* TODO: factorize these rules (also with method): *)
  | override_flag MUTABLE VIRTUAL as_loc(label) COLON core_type
    { if $1 = Override then
        not_expecting $symbolstartpos $endpos
          "members marked virtual may not also be marked overridden";
      ($4, Mutable, Cfk_virtual $6)
    }
  | override_flag MUTABLE VIRTUAL as_loc(label) type_constraint EQUAL expr
    { not_expecting $startpos($6) $endpos($6)
        "not expecting equal - cannot specify value for virtual val";
      let loc = mklocation $symbolstartpos $endpos in
      let e = ghexp_constraint loc $7 $5 in
      ($4, Mutable, Cfk_concrete ($1, e)) }
  | VIRTUAL mutable_flag as_loc(label) COLON core_type
    { ($3, $2, Cfk_virtual $5) }
  | VIRTUAL mutable_flag as_loc(label) type_constraint EQUAL expr
    { not_expecting $startpos($5) $endpos($5)
        "not expecting equal - cannot specify value for virtual val";
      let loc = mklocation $symbolstartpos $endpos in
      let e = ghexp_constraint loc $6 $4 in
      ($3, $2, Cfk_concrete (Fresh, e)) }
  | override_flag mutable_flag as_loc(label) EQUAL expr
    { ($3, $2, Cfk_concrete ($1, $5)) }
  | override_flag mutable_flag as_loc(label) type_constraint EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      let e = ghexp_constraint loc $6 $4 in
      ($3, $2, Cfk_concrete ($1, e)) }
;

method_:
(* TODO: factorize those rules... *)
  | override_flag VIRTUAL as_loc(label) COLON poly_type
    { if $1 = Override then
        syntax_error (mklocation $startpos $endpos)
          "cannot override a virtual method";
      ($3, Cfk_virtual $5)
    }
  | override_flag as_loc(label) fun_def(EQUAL,core_type)
    { let loc = mklocation $symbolstartpos $endpos in
      ($2, Cfk_concrete ($1, mkexp ~ghost:true ~loc (Pexp_poly ($3, None))))
    }
  | override_flag as_loc(label) preceded(COLON,poly_type)?
    either(preceded(EQUAL,expr), braced_expr)
      (* Without locally abstract types, you'll see a Ptyp_poly in the Pexp_poly *)
    { let loc = mklocation $symbolstartpos $endpos in
      ($2, Cfk_concrete ($1, mkexp ~ghost:true ~loc (Pexp_poly($4, $3))))
    }
  | override_flag as_loc(label) COLON TYPE as_loc(LIDENT)+ DOT core_type
    either(preceded(EQUAL,expr), braced_expr)
    (* WITH locally abstract types, you'll see a Ptyp_poly in the Pexp_poly,
       but the expression will be a Pexp_newtype and type vars will be
       "varified". *)
    {
      (* For non, methods we'd create a pattern binding:
         ((Ppat_constraint(mkpatvar ..., Ptyp_poly (typeVars, poly_type_varified))),
          exp_with_newtypes_constrained_by_non_varified)

         For methods, we create:
         Pexp_poly (Pexp_constraint (methodFunWithNewtypes, non_varified), Some (Ptyp_poly newTypes varified))
       *)
      let (exp_non_varified, poly_vars) = wrap_type_annotation $5 $7 $8 in
      let exp = Pexp_poly(exp_non_varified, Some poly_vars) in
      let loc = mklocation $symbolstartpos $endpos in
      ($2, Cfk_concrete ($1, mkexp ~ghost:true ~loc exp))
    }
;

(* A parsing of class_constructor_type is the type of the class's constructor - and if the
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

 *)
class_constructor_type:
  | class_instance_type { $1 }
  | arrow_type_parameters EQUALGREATER class_constructor_type
    { List.fold_right mkcty_arrow $1 $3 }
;

class_instance_type:
mark_position_cty
  ( as_loc(clty_longident)
    loptioninline(type_parameters)
    { mkcty (Pcty_constr ($1, $2)) }
  | attribute class_instance_type
    (* Note that this will compound attributes - so they will become
       attached to whatever *)
    { {$2 with pcty_attributes = $1 :: $2.pcty_attributes} }
  | class_type_body
    { $1 }
  | extension
    { mkcty (Pcty_extension $1) }
  ) {$1};

class_type_body:
  | LBRACE class_sig_body_cty RBRACE
    { mkcty ~loc:(mklocation $startpos $endpos) $2 }
  | LBRACE DOT class_sig_body_cty RBRACE
    { let loc = mklocation $startpos $endpos in
      let ct = mkcty ~loc $3 in
      {ct with pcty_attributes = [uncurry_payload loc]}
    }
;

class_sig_body_fields:
  lseparated_list(SEMI, class_sig_field) SEMI? { List.concat $1 }
;

class_sig_body_cty:
  | class_sig_body { Pcty_signature $1 }
  | LET? OPEN override_flag as_loc(mod_longident) SEMI as_loc(class_sig_body_cty)
    { let {txt; loc} = $6 in
      let od = Opn.mk ~override:$3 ~loc:(mklocation $startpos($2) $endpos($3)) $4 in
      Pcty_open (od, mkcty ~loc txt) }
;

class_sig_body:
  | class_self_type
  { Csig.mk $1 [] }
  | class_self_type SEMI class_sig_body_fields
  { Csig.mk $1 $3 }
  | class_sig_body_fields
  { Csig.mk (Typ.mk ~loc:(mklocation $symbolstartpos $endpos) Ptyp_any) $1 }
;

class_self_type:
  | AS core_type { $2 }
;

class_sig_field:
  | mark_position_ctf
    ( item_attributes INHERIT class_instance_type
      { mkctf_attrs (Pctf_inherit $3) $1 }
    | item_attributes VAL value_type
      { mkctf_attrs (Pctf_val $3) $1 }
    | item_attributes PRI virtual_flag as_loc(label) COLON poly_type
      { mkctf_attrs (Pctf_method ($4, Private, $3, $6)) $1 }
    | item_attributes PUB virtual_flag as_loc(label) COLON poly_type
      { mkctf_attrs (Pctf_method ($4, Public, $3, $6)) $1 }
    | item_attributes CONSTRAINT constrain_field
      { mkctf_attrs (Pctf_constraint $3) $1 }
    | item_attributes item_extension
      { mkctf_attrs (Pctf_extension $2) $1 }
    ) { [$1] }
  | located_attributes
    { List.map
      (fun x -> mkctf ~loc:x.loc (Pctf_attribute (make_floating_doc x.txt)))
      $1 }
;

value_type:
  mutable_or_virtual_flags as_loc(label) COLON core_type
  { let (mut, virt) = $1 in ($2, mut, virt, $4) }
;

constrain:
  core_type EQUAL core_type
  { ($1, $3, mklocation $symbolstartpos $endpos) }
;

constrain_field:
  core_type EQUAL core_type
  { ($1, $3) }
;

class_descriptions:
  item_attributes CLASS class_description_details and_class_description*
  { let (ident, binding, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    (Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc :: $4)
  }
;

and_class_description:
  item_attributes AND class_description_details
  { let (ident, binding, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc
  }
;

%inline class_description_details:
  virtual_flag as_loc(LIDENT) optional_type_params(type_variable_without_underscore) COLON class_constructor_type
  { ($2, $5, $1, $3) }
;

class_type_declarations:
  item_attributes CLASS TYPE class_type_declaration_details
  and_class_type_declaration*
  { let (ident, instance_type, virt, params) = $4 in
    let loc = mklocation $symbolstartpos $endpos in
    (Ci.mk ident instance_type ~virt ~params ~attrs:$1 ~loc :: $5)
  }
;

and_class_type_declaration:
  item_attributes AND class_type_declaration_details
  { let (ident, instance_type, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Ci.mk ident instance_type ~virt ~params ~attrs:$1 ~loc
  }
;

%inline class_type_declaration_details:
  virtual_flag as_loc(LIDENT) optional_type_params(type_variable_with_variance)
  either(preceded(EQUAL,class_instance_type), class_type_body)
  { ($2, $4, $1, $3) }
;

%inline open_dot_declaration: as_loc(mod_longident)
  { let loc = mklocation $startpos($1) $endpos($1) in
    let me = Mod.ident ~loc $1 in
    Opn.mk ~loc me }
;

(* Core expressions *)

(* Note: If we will parse this as Pexp_apply, and it will
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
 *)
braced_expr:
mark_position_exp
  ( LBRACE seq_expr RBRACE
    { add_brace_attr $2 }
  | LBRACE DOTDOTDOT expr_optional_constraint COMMA? RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      syntax_error_exp loc
        "Record construction must have at least one field explicitly set" }
  | LBRACE DOTDOTDOT expr_optional_constraint SEMI RBRACE
    { let loc = mklocation $startpos($4) $endpos($4) in
      raise_record_trailing_semi_error loc }
  | LBRACE record_expr RBRACE
    { mk_record_expr $2 }
  | LBRACE record_expr_with_string_keys RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let (exten, fields) = $2 in
      mkexp ~loc (Pexp_extension (mkloc ("bs.obj") loc,
             PStr [mkstrexp (mkexp ~loc (Pexp_record(fields, exten))) []]))
    }
  (* Todo: Why is this not a simple_expr? *)
  | LBRACE object_body RBRACE
    { mkexp (Pexp_object $2) }
) {$1};

seq_expr_no_seq [@recover.expr default_expr ()]:
| expr SEMI? { $1 }
| opt_LET_MODULE_ident module_binding_body SEMI seq_expr
  { mkexp (Pexp_letmodule($1, $2, $4)) }
| item_attributes LET? OPEN override_flag as_loc(mod_longident) SEMI seq_expr
  { let loc = (mklocation $startpos($1) $endpos($4)) in
    let me = Mod.ident ~loc $5 in
    let od = Opn.mk ~override:$4 ~loc me in
    let exp = mkexp (Pexp_open(od, $7)) in
    { exp with pexp_attributes = $1 }
  }
| str_exception_declaration SEMI seq_expr {
   mkexp (Pexp_letexception ($1, $3)) }
| let_bindings SEMI seq_expr
  { let loc = mklocation $startpos($1) $endpos($3) in
    expr_of_let_bindings ~loc $1 $3
  }
| let_bindings SEMI?
  { let loc = mklocation $symbolstartpos $endpos in
    expr_of_let_bindings ~loc $1 (ghunit ~loc ())
  }
| as_loc(LETOP) letop_bindings SEMI seq_expr
  { let (pbop_pat, pbop_exp, rev_ands) = $2 in
    let ands = List.rev rev_ands in
    let pbop_loc = mklocation $symbolstartpos $endpos($2) in
    let let_ = {pbop_op = $1; pbop_pat; pbop_exp; pbop_loc} in
    mkexp ~loc:pbop_loc (Pexp_letop { let_; ands; body = $4}) }
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
    { let loc = mklocation $startpos($1) $endpos($2) in
      mkexp (Pexp_sequence(expression_extension ~loc $1 $2, $4)) }
  ) { $1 }
;

(*


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

*)

labeled_pattern_constraint:
  | AS pattern_optional_constraint { fun _punned -> $2 }
  | preceded(COLON, core_type)?
    { fun punned ->
      let pat = mkpat (Ppat_var punned) ~loc:punned.loc in
      match $1 with
      | None -> pat
      | Some typ ->
        let loc = mklocation punned.loc.loc_start $endpos in
        mkpat ~loc (Ppat_constraint(pat, typ))
    }
;

labeled_pattern:
as_loc
   ( TILDE as_loc(LIDENT) labeled_pattern_constraint
    { Term (Labelled $2.txt, None, $3 $2) }
  | TILDE as_loc(LIDENT) labeled_pattern_constraint EQUAL expr
    { Term (Optional $2.txt, Some $5, $3 $2) }
  | TILDE as_loc(LIDENT) labeled_pattern_constraint EQUAL QUESTION
    { Term (Optional $2.txt, None, $3 $2) }
  | pattern_optional_constraint
    { Term (Nolabel, None, $1) }
  | TYPE LIDENT
    { Type $2 }
  ) { $1 }
;


%inline labelled_pattern_comma_list:
  lseparated_nonempty_list(COMMA, labeled_pattern) COMMA? { $1 };

%inline labeled_pattern_list:
  | LPAREN RPAREN {
    let loc = mklocation $startpos $endpos in
    ([mkloc (Term (Nolabel, None, mkpat_constructor_unit loc loc)) loc], false)
  }
  | parenthesized(labelled_pattern_comma_list) {
    ($1, false)
  }
  | LPAREN DOT RPAREN {
      let loc = mklocation $startpos $endpos in
      ([mkloc (Term (Nolabel, None, mkpat_constructor_unit loc loc)) loc], true)
  }
  | LPAREN DOT labelled_pattern_comma_list RPAREN {
      ($3, true)
  }
;

es6_parameters:
  | labeled_pattern_list { $1 }
  | as_loc(UNDERSCORE)
    { ([{$1 with txt = Term (Nolabel, None, mkpat ~loc:$1.loc Ppat_any)}], false) }
  | simple_pattern_ident
    { ([Location.mkloc (Term (Nolabel, None, $1)) $1.ppat_loc], false) }
;

(* TODO: properly fix JSX labelled/optional stuff *)
jsx_arguments:
  (* empty *) { [] }
  | LIDENT EQUAL QUESTION simple_expr jsx_arguments
    { (* a=?b *)
      [(Optional $1, $4)] @ $5
    }
  | QUESTION LIDENT jsx_arguments
    { (* <Foo ?bar /> punning with explicitly passed optional *)
      let loc_lident = mklocation $startpos($2) $endpos($2) in
      [(Optional $2, mkexp (Pexp_ident {txt = Lident $2; loc = loc_lident}) ~loc:loc_lident)] @ $3
    }
  | LIDENT EQUAL simple_expr jsx_arguments
    { (* a=b *)
      [(Labelled $1, $3)] @ $4
    }
  | LIDENT jsx_arguments
    { (* a (punning) *)
      let loc_lident = mklocation $startpos($1) $endpos($1) in
      [(Labelled $1, mkexp (Pexp_ident {txt = Lident $1; loc = loc_lident}) ~loc:loc_lident)] @ $2
    }
  | as_loc(INFIXOP3)
    (* extra rule to provide nice error messages in the case someone
     * wrote: <Description term=<Text text="Age" />> child </Description>
     * or <Foo bar=<Baz />/>
     *  />> & />/> are lexed as infix tokens *)
  {
    begin match $1.txt with
      | "/>>" ->
         syntax_error $1.loc
          {|JSX in a JSX-argument needs to be wrapped in braces.
      If you wrote:
        <Description term=<Text text="Age" />> child </Description>
      Try wrapping <Text /> in braces.
        <Description term={<Text text="Age" />}> child </Description>|}
      | "/>/>" ->
         syntax_error $1.loc
           {|JSX in a JSX-argument needs to be wrapped in braces.
      If you wrote:
        <Description term=<Text text="Age" />/>
      Try wrapping <Text /> in braces.
        <Description term={<Text text="Age" />} />|}
      | _ -> syntax_error $1.loc "Syntax error"
    end;
    []
  }
;

jsx_start_tag_and_args:
  as_loc(LESSIDENT) jsx_arguments
     { let name = parse_lid $1.txt in
      (jsx_component {$1 with txt = name} $2, name)
    }
  | LESS as_loc(LIDENT) jsx_arguments
    { let name = parse_lid $2.txt in
      (jsx_component {$2 with txt = name} $3, name)
    }
  | LESS as_loc(mod_ext_longident) jsx_arguments
    { jsx_component $2 $3, $2.txt }
  | as_loc(mod_ext_lesslongident) jsx_arguments
    { jsx_component $1 $2, $1.txt }
;

jsx_start_tag_and_args_without_leading_less:
    as_loc(mod_ext_longident) jsx_arguments
    { (jsx_component $1 $2, $1.txt) }
  | as_loc(LIDENT) jsx_arguments
    { let lident = Longident.Lident $1.txt in
      (jsx_component {$1 with txt = lident } $2, lident)
    }
;

greater_spread:
  | GREATERDOTDOTDOT
  | GREATER DOTDOTDOT { ">..." }

jsx:
  | LESSGREATER simple_expr_no_call* LESSSLASHGREATER
    { let loc = mklocation $symbolstartpos $endpos in
      let body = mktailexp_extension loc $2 None in
      makeFrag loc body
    }
  (* There is not need yet for <> ...expr </> because it would currently
   * parse as expr.
  | LESSGREATER DOTDOTDOT simple_expr_no_call LESSSLASHGREATER
    { let loc = mklocation $symbolstartpos $endpos in
      makeFrag loc $3
    }
  *)
  | jsx_start_tag_and_args SLASHGREATER
    { let (component, _) = $1 in
      let loc = mklocation $symbolstartpos $endpos in
      component [
        (Labelled "children", mktailexp_extension loc [] None);
        (Nolabel, mkexp_constructor_unit loc loc)
      ] loc
    }
  | jsx_start_tag_and_args GREATER simple_expr_no_call* LESSSLASHIDENTGREATER
    { let (component, start) = $1 in
      let loc = mklocation $startpos($4) $endpos in
      (* TODO: Make this tag check simply a warning *)
      let endName = parse_lid $4 in
      let _ = ensureTagsAreEqual start endName loc in
      let siblings = if List.length $3 > 0 then $3 else [] in
      component [
        (Labelled "children", mktailexp_extension loc siblings None);
        (Nolabel, mkexp_constructor_unit loc loc)
      ] loc
    }
   | jsx_start_tag_and_args greater_spread simple_expr_no_call LESSSLASHIDENTGREATER
     (* <Foo> ...bar </Foo> or <Foo> ...((a) => 1) </Foo> *)
    { let (component, start) = $1 in
      let loc = mklocation $symbolstartpos $endpos in
      (* TODO: Make this tag check simply a warning *)
      let endName = parse_lid $4 in
      let _ = ensureTagsAreEqual start endName loc in
      let child = $3 in
      component [
        (Labelled "children", child);
        (Nolabel, mkexp_constructor_unit loc loc)
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
      (Labelled "children", mktailexp_extension loc [] None);
      (Nolabel, mkexp_constructor_unit loc loc)
    ] loc
  }
  | jsx_start_tag_and_args_without_leading_less GREATER simple_expr_no_call* LESSSLASHIDENTGREATER {
    let (component, start) = $1 in
    let loc = mklocation $symbolstartpos $endpos in
    (* TODO: Make this tag check simply a warning *)
    let endName = parse_lid $4 in
    let _ = ensureTagsAreEqual start endName loc in
    let siblings = if List.length $3 > 0 then $3 else [] in
    component [
      (Labelled "children", mktailexp_extension loc siblings None);
      (Nolabel, mkexp_constructor_unit loc loc)
    ] loc
  }
    | jsx_start_tag_and_args_without_leading_less greater_spread simple_expr_no_call LESSSLASHIDENTGREATER {
    let (component, start) = $1 in
    let loc = mklocation $symbolstartpos $endpos in
    (* TODO: Make this tag check simply a warning *)
    let endName = parse_lid $4 in
    let _ = ensureTagsAreEqual start endName loc in
    let child = $3 in
    component [
      (Labelled "children", child);
      (Nolabel, mkexp_constructor_unit loc loc)
    ] loc
  }
;

optional_expr_extension:
  | (* empty *) { fun exp -> exp }
  | item_extension_sugar { fun exp -> expression_extension $1 exp  }
;

(*
 * Parsing of expressions is quite involved as it depends on context.
 * At the top-level of a structure, expressions can't have attributes
 * (those are attached to the structure).
 * In other places, attributes are allowed.
 *
 * The generic parts are represented by unattributed_expr_template(_).
 * Then unattributed_expr represents the concrete unattributed expr
 * while expr adds an attribute rule to unattributed_expr_template.
 *)
%inline unattributed_expr_template(E):
mark_position_exp
  ( simple_expr
    { $1 }
  | FUN optional_expr_extension fun_def(EQUALGREATER,non_arrowed_core_type)
    { $2 $3 }
  | ES6_FUN es6_parameters EQUALGREATER expr
    { let (ps, uncurried) = $2 in
      let exp = List.fold_right mkexp_fun ps $4 in
      if uncurried then
        let loc = mklocation $startpos $endpos in
        {exp with pexp_attributes = (uncurry_payload loc)::exp.pexp_attributes}
      else exp
    }
  | ES6_FUN es6_parameters COLON non_arrowed_core_type EQUALGREATER expr
    { let (ps, uncurried) = $2 in
    let exp = List.fold_right mkexp_fun ps
        (ghexp_constraint (mklocation $startpos($4) $endpos) $6 (Some $4, None))  in
    if uncurried then
      let loc = mklocation $startpos $endpos in
      {exp with pexp_attributes = (uncurry_payload loc)::exp.pexp_attributes}
    else exp
    }

  (* List style rules like this often need a special precendence
     such as below_BAR in order to let the entire list "build up"
   *)
  | FUN optional_expr_extension match_cases(expr) %prec below_BAR
    { $2 (mkexp (Pexp_function $3)) }
  | SWITCH optional_expr_extension simple_expr_no_constructor
    LBRACE match_cases(seq_expr) RBRACE
    { $2 (mkexp (Pexp_match ($3, $5))) }
  | TRY optional_expr_extension simple_expr_no_constructor
    LBRACE match_cases(seq_expr) RBRACE
    { $2 (mkexp (Pexp_try ($3, $5))) }
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
    { let op = match $2.txt with
      | "->" -> {$2 with txt = "|."}
      | _ -> $2
      in mkinfix $1 op $3
    }
  | as_loc(subtractive) expr %prec prec_unary
    { mkuminus $1 $2 }
  | as_loc(additive) expr %prec prec_unary
    { mkuplus $1 $2 }
  | as_loc(BANG {"!"}) expr %prec prec_unary
    { mkexp(Pexp_apply(mkoperator $1, [Nolabel,$2])) }
  | simple_expr DOT as_loc(label_longident) EQUAL expr
    { mkexp(Pexp_setfield($1, $3, $5)) }
  | simple_expr LBRACKET expr RBRACKET EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Pexp_ident(array_function ~loc "Array" "set") in
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc exp,
                       [Nolabel,$1; Nolabel,$3; Nolabel,$6]))
    }
  | simple_expr DOT LBRACKET expr RBRACKET EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Pexp_ident(array_function ~loc "String" "set") in
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc exp,
                       [Nolabel,$1; Nolabel,$4; Nolabel,$7]))
    }
  | simple_expr bigarray_access EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      bigarray_set ~loc $1 $2 $4
    }
  | as_loc(label) EQUAL expr
    { mkexp(Pexp_setinstvar($1, $3)) }
  | ASSERT simple_expr
    { mkexp (Pexp_assert $2) }
  | LAZY simple_expr
    { mkexp (Pexp_lazy $2) }
  (*
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
   *)
  | E QUESTION expr COLON expr
    { (* Should use ghost expressions, but not sure how that would work with source maps *)
      (* So ? will become true and : becomes false for now*)
      let loc_question = mklocation $startpos($2) $endpos($2) in
      let loc_colon = mklocation $startpos($4) $endpos($4) in
      let fauxTruePat =
        Pat.mk ~loc:loc_question (Ppat_construct({txt = Lident "true"; loc = loc_question}, None)) in
      let fauxFalsePat =
        Pat.mk ~loc:loc_colon (Ppat_construct({txt = Lident "false"; loc = loc_colon}, None)) in
      let fauxMatchCaseTrue = Exp.case fauxTruePat $3 in
      let fauxMatchCaseFalse = Exp.case fauxFalsePat $5 in
      mkexp (Pexp_match ($1, [fauxMatchCaseTrue; fauxMatchCaseFalse]))
    }
  ) {$1};

(*
 * Much like how patterns are partitioned into pattern/simple_pattern,
 * expressions are divided into expr/simple_expr.
 * expr: contains function application, but simple_expr doesn't (unless it's
 * wrapped in parens).
 *)
expr [@recover.expr default_expr ()]:
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
  | LPAREN DOT RPAREN
    { let loc = mklocation $startpos $endpos in
      mkexp_constructor_unit ~uncurried:true loc loc }
  | LPAREN expr_list RPAREN
    { may_tuple $startpos $endpos $2 }
;

%inline array_expr_list:
  LBRACKETBAR
  lseparated_list(COMMA, opt_spread(expr_optional_constraint))
  COMMA?
  BARRBRACKET
  { let msg = "Arrays can't use the `...` spread currently. Please use `concat` or other Array helpers." in
    filter_raise_spread_syntax msg $2
  };

%inline bigarray_access:
  DOT LBRACE lseparated_nonempty_list(COMMA, expr) COMMA? RBRACE { $3 }

(* The grammar of simple exprs changes slightly according to context:
 * - in most cases, calls (like f(x)) are allowed
 * - in some contexts, calls are forbidden
 *   (most notably JSX lists and rhs of a SHARPOP extension).
 *
 * simple_expr_template contains the generic parts,
 * simple_expr, simple_expr_no_call and simple_expr_no_constructor the specialized instances.
 *)
%inline simple_expr_template(E):
  | as_loc(val_longident) { mkexp (Pexp_ident $1) }
  | template_string
    {
      let (_indent, expr) = $1 in
      expr
    }
  | constant
    { let attrs, cst = $1 in mkexp ~attrs (Pexp_constant cst) }
  | jsx                   { $1 }
  | simple_expr_direct_argument { $1 }
  | array_expr_list
    { mkexp (Pexp_array $1) }
  (* Not sure why this couldn't have just been below_SHARP (Answer: Being
   * explicit about needing to wait for "as") *)
  | as_loc(constr_longident) %prec prec_constant_constructor
    { mkexp (Pexp_construct ($1, None)) }
  | name_tag %prec prec_constant_constructor
    { mkexp (Pexp_variant ($1, None)) }
  | LPAREN expr_list RPAREN
    { may_tuple $startpos $endpos $2 }
  | E as_loc(POSTFIXOP)
    { mkexp(Pexp_apply(mkoperator $2, [Nolabel, $1])) }
  | od=open_dot_declaration DOT LPAREN expr_list RPAREN
    { mkexp(Pexp_open(od, may_tuple $startpos($3) $endpos($5) $4)) }
  | E DOT as_loc(label_longident)
    { mkexp(Pexp_field($1, $3)) }
  | od=open_dot_declaration DOT LBRACE RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let pat = mkpat (Ppat_var (mkloc "this" loc)) in
      mkexp(Pexp_open (od, mkexp(Pexp_object(Cstr.mk pat []))))
    }
  | E LBRACKET expr RBRACKET
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Pexp_ident(array_function ~loc "Array" "get") in
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc exp, [Nolabel,$1; Nolabel,$3]))
    }
  | E DOT LBRACKET expr RBRACKET
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Pexp_ident(array_function ~loc "String" "get") in
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc exp, [Nolabel,$1; Nolabel,$4]))
    }
  | E bigarray_access
    { let loc = mklocation $symbolstartpos $endpos in
      bigarray_get ~loc $1 $2 }
  | od=open_dot_declaration DOT LBRACE record_expr RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp(Pexp_open(od, mk_record_expr ~loc $4))
    }
  | od=open_dot_declaration DOT LBRACE record_expr_with_string_keys RBRACE
    { let (exten, fields) = $4 in
      let loc = mklocation $symbolstartpos $endpos in
      let rec_exp = mkexp ~loc (Pexp_extension (mkloc ("bs.obj") loc,
             PStr [mkstrexp (mkexp ~loc (Pexp_record(fields, exten))) []]))
      in
      mkexp(Pexp_open(od, rec_exp))
    }
  | od=open_dot_declaration DOT LBRACKETBAR expr_list BARRBRACKET
    { let loc = mklocation $symbolstartpos $endpos in
      let rec_exp = Exp.mk ~loc ~attrs:[] (Pexp_array $4) in
      mkexp(Pexp_open(od, rec_exp))
    }
  (* Parse Module.[<Component> <div/> </Component>] *)
  | od=open_dot_declaration DOT LBRACKETLESS jsx_without_leading_less RBRACKET
    { let seq, ext_opt = [$4], None in
      let loc = mklocation $startpos($4) $endpos($4) in
      let list_exp = make_real_exp (mktailexp_extension loc seq ext_opt) in
      let list_exp = { list_exp with pexp_loc = loc } in
      mkexp (Pexp_open (od, list_exp))
    }
  | od=open_dot_declaration DOT LBRACKET RBRACKET
    { let loc = mklocation $startpos($3) $endpos($4) in
      let list_exp = make_real_exp (mktailexp_extension loc [] None) in
      let list_exp = { list_exp with pexp_loc = loc } in
      mkexp (Pexp_open (od, list_exp))
    }
  | od=open_dot_declaration DOT LBRACKET expr_comma_seq_extension RBRACKET
    { let seq, ext_opt = $4 in
      let loc = mklocation $startpos($4) $endpos($4) in
      let list_exp = make_real_exp (mktailexp_extension loc seq ext_opt) in
      let list_exp = { list_exp with pexp_loc = loc } in
      mkexp (Pexp_open (od, list_exp))
    }
  | as_loc(PREFIXOP) E %prec below_DOT_AND_SHARP
    { mkexp(Pexp_apply(mkoperator $1, [Nolabel, $2])) }
  (**
   * Must be below_DOT_AND_SHARP so that the parser waits for several dots for
   * nested record access that the bang should apply to.
   *
   * !x.y.z should be parsed as !(((x).y).z)
   *)
  (*| as_loc(BANG {"!"}) E %prec below_DOT_AND_SHARP
    { mkexp (Pexp_apply(mkoperator $1, [Nolabel,$2])) }*)
  | NEW as_loc(class_longident)
    { mkexp (Pexp_new $2) }
  | od=open_dot_declaration DOT LBRACELESS field_expr_list COMMA? GREATERRBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Exp.mk ~loc ~attrs:[] (Pexp_override $4) in
      mkexp (Pexp_open(od, exp))
    }
  | E SHARP as_loc(label)
    { mkexp (Pexp_send($1, $3)) }
  | E as_loc(SHARPOP) simple_expr_no_call
    { mkinfixop $1 (mkoperator $2) $3 }
  | E as_loc(SHARPEQUAL) simple_expr
    { let op = { $2 with txt = "#=" } in
      mkinfixop $1 (mkoperator op) $3 }
  | E as_loc(MINUSGREATER) simple_expr_no_call
    { mkinfixop $1 (mkoperator {$2 with txt = "|."}) $3 }
  | od=open_dot_declaration DOT LPAREN MODULE module_expr COLON package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp (Pexp_open(od,
        mkexp ~loc (Pexp_constraint (mkexp ~ghost:true ~loc (Pexp_pack $5), $7))))
    }
  | extension
    { mkexp (Pexp_extension $1) }
;

%inline simple_expr: simple_expr_call { mkexp_app_rev $startpos $endpos $1 };

simple_expr_no_constructor [@recover.expr default_expr ()]:
  mark_position_exp(simple_expr_template(simple_expr_no_constructor)) { $1 };

simple_expr_template_constructor:
  | as_loc(constr_longident)
    mark_position_exp
      ( non_labeled_argument_list   { mkexp (Pexp_tuple($1)) }
      | simple_expr_direct_argument { $1 }
      )
    { mkExplicitArityTupleExp (Pexp_construct($1, Some $2))
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

simple_expr_no_call [@recover.expr default_expr ()]:
  | mark_position_exp(simple_expr_template(simple_expr_no_call)) { $1 }
  | simple_expr_template_constructor { $1 }
;

simple_expr_call [@recover.expr (default_expr (), [])]:
  | mark_position_exp(simple_expr_template(simple_expr)) { ($1, []) }
  | simple_expr_call labeled_arguments
    { let (body, args) = $1 in
      (body, List.rev_append $2 args) }
  | LBRACKET expr_comma_seq_extension RBRACKET
    { let seq, ext_opt = $2 in
      let loc = mklocation $startpos($2) $endpos($2) in
      (make_real_exp (mktailexp_extension loc seq ext_opt), [])
    }
  | simple_expr_template_constructor { ($1, []) }
;

simple_expr_direct_argument:
  (*
     Because [< is a special token, the <ident and <> won't be picked up as separate
     tokens, when a list begins witha JSX tag. So we special case it.
     (todo: pick totally different syntax for polymorphic variance types to avoid
     the issue alltogether.

     first token
     /\
     [<ident    args />  , remainingitems ]
     [<>                 , remainingitems ]
   *)
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
  | LBRACKETLESS jsx_without_leading_less COMMA RBRACKET
    { let entireLoc = mklocation $startpos($1) $endpos($4) in
      mktailexp_extension entireLoc [$2] None
    }
  | LBRACELESS field_expr_list COMMA? GREATERRBRACE
    { mkexp (Pexp_override $2) }
  | LBRACELESS GREATERRBRACE
    { mkexp (Pexp_override [])}
  | LPAREN MODULE module_expr RPAREN
    { mkexp (Pexp_pack $3) }
  | LPAREN MODULE module_expr COLON package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp (Pexp_constraint (mkexp ~ghost:true ~loc (Pexp_pack $3), $5))
    }
;

%inline non_labelled_expr_comma_list:
  lseparated_nonempty_list(COMMA, expr_optional_constraint) COMMA? { $1 };

non_labeled_argument_list:
  | parenthesized(non_labelled_expr_comma_list) { $1 }
  | LPAREN RPAREN
    { let loc = mklocation $startpos $endpos in
      [mkexp_constructor_unit loc loc] }
;

%inline labelled_expr_comma_list:
  lseparated_list(COMMA, uncurried_labeled_expr) COMMA? { $1 };

labeled_arguments:
  | mark_position_exp(simple_expr_direct_argument)
    { [(Nolabel, $1)] }
  | parenthesized(labelled_expr_comma_list)
    { match $1 with
      | [] -> let loc = mklocation $startpos $endpos in
              [(Nolabel, mkexp_constructor_unit loc loc)]
      | xs -> xs
    }
  | LPAREN DOT RPAREN
    { let loc = mklocation $startpos $endpos in
      [(Nolabel, mkexp_constructor_unit ~uncurried:true loc loc)]
    }
;

labeled_expr_constraint:
  | expr_optional_constraint { fun _punned -> $1 }
  | type_constraint
    { fun punned ->
      let exp = mkexp (Pexp_ident punned) ~loc:punned.loc in
      match $1 with
      | typ ->
        let loc = mklocation punned.loc.loc_start $endpos in
        ghexp_constraint loc exp typ
    }
;

%inline uncurried_labeled_expr:
  | DOT? labeled_expr {
    let uncurried = match $1 with | Some _ -> true | None -> false in
    if uncurried then
      let (lbl, argExpr) = $2 in
      let loc = mklocation $startpos $endpos in
      let up = uncurry_payload ~name:"uncurry" loc in
      (lbl, {argExpr with pexp_attributes = up::argExpr.pexp_attributes})
    else $2
  }
;

longident_type_constraint:
 | as_loc(val_longident) type_constraint?
 { $1, $2 }

labeled_expr:
  | expr_optional_constraint { (Nolabel, $1) }
  | TILDE as_loc(either(parenthesized(longident_type_constraint), longident_type_constraint))
    { (* add(~a, ~b) -> parses ~a & ~b *)
      let lident_loc, maybe_typ = $2.txt in
      let exp = mkexp (Pexp_ident lident_loc) ~loc:lident_loc.loc in
      let labeled_exp = match maybe_typ with
      | None -> exp
      | Some typ ->
          ghexp_constraint $2.loc exp typ
      in
      (Labelled (Longident.last lident_loc.txt), labeled_exp)
    }
  | TILDE as_loc(val_longident) QUESTION
    { (* foo(~a?)  -> parses ~a? *)
      let exp = mkexp (Pexp_ident $2) ~loc:$2.loc in
      (Optional (Longident.last $2.txt), exp)
    }
  | TILDE as_loc(LIDENT) EQUAL optional labeled_expr_constraint
    { (* foo(~bar=?Some(1)) or add(~x=1, ~y=2) -> parses ~bar=?Some(1) & ~x=1 & ~y=1 *)
      ($4 $2.txt, $5 { $2 with txt = Lident $2.txt })
    }
  | TILDE as_loc(LIDENT) EQUAL optional as_loc(UNDERSCORE)
    { (* foo(~l =_) *)
      let loc = $5.loc in
      let exp = mkexp (Pexp_ident (mkloc (Lident "_") loc)) ~loc in
      ($4 $2.txt, exp)
    }
  | as_loc(UNDERSCORE)
    { (* foo(_) *)
      let loc = $1.loc in
      let exp = mkexp (Pexp_ident (mkloc (Lident "_") loc)) ~loc in
      (Nolabel, exp)
    }
;

%inline and_let_binding:
  (* AND bindings don't accept a preceeding extension ID, but do accept
   * preceeding attribute*. These preceeding attribute* will cause an
   * error if this is an *expression * let binding. Otherwise, they become
   * attribute* on the structure item for the "and" binding.
   *)
  item_attributes AND let_binding_body
  { let pat, expr = $3 in
    Vb.mk ~loc:(mklocation $symbolstartpos $endpos) ~attrs:$1 pat expr }
;

let_bindings: let_binding and_let_binding* { addlbs $1 $2 };

let_binding:
  (* Form with item extension sugar *)
  item_attributes LET item_extension_sugar? rec_flag let_binding_body
  { let loc = mklocation $symbolstartpos $endpos in
    let pat, expr = $5 in
    mklbs $3 $4 (Vb.mk ~loc ~attrs:$1 pat expr) loc }
;

let_binding_body:
  | simple_pattern_ident type_constraint EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      ($1, ghexp_constraint loc $4 $2) }
  | simple_pattern_ident fun_def(EQUAL,core_type)
    { ($1, $2) }
  | simple_pattern_ident COLON as_loc(preceded(QUOTE,ident))+ DOT core_type
      EQUAL mark_position_exp(expr)
    { let typ = mktyp ~ghost:true (Ptyp_poly($3, $5)) in
      let loc = mklocation $symbolstartpos $endpos in
      (mkpat ~ghost:true ~loc (Ppat_constraint($1, typ)), $7)
    }
  | simple_pattern_ident COLON TYPE as_loc(LIDENT)+ DOT core_type
      EQUAL mark_position_exp(expr)
  (* Because core_type will appear to contain "type constructors" since the
   * type variables listed in LIDENT+ don't have leading single quotes, we
   * have to call [varify_constructors] (which is what [wrap_type_annotation]
   * does among other things) to turn those "type constructors" that correspond
   * to LIDENT+ into regular type variables. I don't think this should be
   * done in the parser!
   *)
  (* In general, this is a very strange transformation that occurs in the
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
   *)
   { let exp, poly = wrap_type_annotation $4 $6 $8 in
     let loc = mklocation $symbolstartpos $endpos in
     (mkpat ~ghost:true ~loc (Ppat_constraint($1, poly)), exp)
   }

  (* The combination of the following two rules encompass every type of
   * pattern *except* val_identifier. The fact that we want handle the
   * val_ident separately as a separate rule in let_binding_body alone justifies the
   * need for the strange structuring of
   * [pattern]/[simple_pattern_not_ident]/[simple_pattern] - it allows us
   * to isolate [val_ident] in the special case of [let_binding_body].
   *
   * TODO:
   * Unfortunately, it means we cannot do: let (myCurriedFunc: int -> int) a -> a;
   *)
  | pattern EQUAL expr
    { ($1, $3) }
  | simple_pattern_not_ident COLON core_type EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      (mkpat ~loc (Ppat_constraint($1, $3)), $5)
    }
;

letop_binding_body:
  | pat = simple_pattern_ident exp = expr
    { (pat, exp) }
  | pat = simple_pattern COLON typ = core_type EQUAL exp = expr
    { let loc = mklocation $startpos(pat) $endpos(typ) in
      (mkpat ~ghost:true ~loc (Ppat_constraint(pat, typ)), exp) }
  | pat = pattern EQUAL exp = expr
    { (pat, exp) }
;

letop_bindings:
    body = letop_binding_body
      { let let_pat, let_exp = body in
        let_pat, let_exp, [] }
  | bindings = letop_bindings pbop_op = as_loc(ANDOP) body = let_binding_body
      { let let_pat, let_exp, rev_ands = bindings in
        let pbop_pat, pbop_exp = body in
        let pbop_loc = mklocation $symbolstartpos $endpos in
        let and_ = {pbop_op; pbop_pat; pbop_exp; pbop_loc} in
        let_pat, let_exp, and_ :: rev_ands }
;

(*
 * TODO:
 * In OCaml, the following function binding would be parsed by the function
 * parsers but would return a non function. Coincidentally, everything just worked.
 *   let x: int = 10;
 * Since in Reason, function bindings always use arrows, it's wrong to rely
 * on function parsers to return non function bindings:
 *   let x: int -> 10;
 *   let y (:returnType) -> 20;  (* wat *)
 *)

%inline match_cases(EXPR): lnonempty_list(match_case(EXPR)) { $1 };

match_case(EXPR):
  as_loc(BAR) pattern preceded(WHEN,expr)? EQUALGREATER EXPR
  { let pat = {$2 with ppat_loc =
      { $2.ppat_loc with
        loc_start = $1.loc.loc_start
      }
    } in
    Exp.case pat ?guard:$3 $5 }
;

fun_def(DELIM, typ):
  labeled_pattern_list
  preceded(COLON,typ)?
  either(preceded(DELIM, expr), braced_expr)
  { let loc = mklocation $startpos $endpos in
    let (pl, uncurried) = $1 in
    let exp = List.fold_right mkexp_fun pl
        (match $2 with
        | None -> $3
        | Some ct -> Exp.constraint_ ~loc $3 ct)
    in
    if uncurried then
      {exp with pexp_attributes = (uncurry_payload loc)::exp.pexp_attributes}
    else exp
  }
;

(* At least one comma delimited: Each item optionally annotated. *)
expr_list:
  lseparated_nonempty_list(COMMA, expr_optional_constraint) COMMA?
  { $1 }
;

(* [x, y, z, ...n] --> ([x,y,z], Some n) *)
expr_comma_seq_extension:
  lseparated_nonempty_list(COMMA, opt_spread(expr_optional_constraint)) COMMA?
  { match List.rev $1 with
    (* Check if the last expr has been spread with `...` *)
    | ((dotdotdot, e) as hd)::es ->
      let (es, ext) = match dotdotdot with
      | Some _ -> (es, Some e)
      | None -> (hd::es, None)
      in
      let msg = "Lists can only have one `...` spread, at the end.
Explanation: lists are singly-linked list, where a node contains a value and points to the next node. `[a, ...bc]` efficiently creates a new item and links `bc` as its next nodes. `[...bc, a]` would be expensive, as it'd need to traverse `bc` and prepend each item to `a` one by one. We therefore disallow such syntax sugar.
Solution: directly use `concat` or other List helpers." in
      let exprList = filter_raise_spread_syntax msg es in
      (List.rev exprList, ext)
    | [] -> [], None
  }
;

(**
 * See note about tuple patterns. There are few cases where expressions may be
 * type constrained without requiring additional parens, and inside of tuples
 * are one exception.
 *)
expr_optional_constraint:
  | expr { $1 }
  | expr type_constraint
    { ghexp_constraint (mklocation $symbolstartpos $endpos) $1 $2 }
;

record_expr:
  | DOTDOTDOT expr_optional_constraint lnonempty_list(preceded(COMMA,
    opt_spread(lbl_expr))) COMMA?
    { let exprList = filter_raise_spread_syntax record_exp_spread_msg $3
      in (Some $2, exprList)
    }
  | DOTDOTDOT
    expr_optional_constraint SEMI
    lseparated_nonempty_list(COMMA, opt_spread(lbl_expr)) COMMA?
    { raise_record_trailing_semi_error'
        (mklocation $startpos($3) $endpos($3)) }
  | DOTDOTDOT
    expr_optional_constraint
    lnonempty_list(preceded(COMMA, opt_spread(lbl_expr))) SEMI
    { raise_record_trailing_semi_error'
        (mklocation $startpos($4) $endpos($4)) }
  | non_punned_lbl_expr COMMA?
    { (None, [$1]) }
  | non_punned_lbl_expr SEMI
    { raise_record_trailing_semi_error'
        (mklocation $startpos($2) $endpos($2)) }
  | lbl_expr lnonempty_list(preceded(COMMA, opt_spread(lbl_expr))) COMMA?
    { let exprList = filter_raise_spread_syntax record_exp_spread_msg $2 in
      (None, $1 :: exprList) }
  | lbl_expr lnonempty_list(preceded(COMMA, opt_spread(lbl_expr))) SEMI
    { raise_record_trailing_semi_error'
        (mklocation $startpos($3) $endpos($3)) }
;

%inline non_punned_lbl_expr:
  | as_loc(label_longident) COLON expr { ($1, $3) }
;

%inline punned_lbl_expr:
  | as_loc(label_longident) { ($1, exp_of_label $1) }
;

%inline lbl_expr:
  | non_punned_lbl_expr {$1}
  | punned_lbl_expr {$1}
;

record_expr_with_string_keys:
  | DOTDOTDOT expr_optional_constraint COMMA string_literal_exprs_maybe_punned
    { (Some $2, $4) }
  | STRING COLON expr COMMA?
    { let loc = mklocation $symbolstartpos $endpos in
      let (s, _, _) = $1 in
      let lident_lident_loc = mkloc (Lident s) loc in
      (None, [(lident_lident_loc, $3)])
    }
  | string_literal_expr_maybe_punned_with_comma string_literal_exprs_maybe_punned {
    (None, $1 :: $2)
  }
;

string_literal_exprs_maybe_punned:
  lseparated_nonempty_list(COMMA, string_literal_expr_maybe_punned) COMMA? { $1 };

(* Had to manually inline these two forms for some reason *)
string_literal_expr_maybe_punned_with_comma:
  | STRING COMMA
  { let loc = mklocation $startpos $endpos in
    let (s, _, _) = $1 in
    let lident_lident_loc = mkloc (Lident s) loc in
    let exp = mkexp ~loc (Pexp_ident lident_lident_loc) in
    (lident_lident_loc, exp)
  }
  | STRING COLON expr COMMA
  { let loc = mklocation $startpos $endpos in
    let (s, _, _) = $1 in
    let lident_lident_loc = mkloc (Lident s) loc in
    let exp = $3 in
    (lident_lident_loc, exp)
  }
;
string_literal_expr_maybe_punned:
  STRING preceded(COLON, expr)?
  { let loc = mklocation $startpos $endpos in
    let (s, _, _) = $1 in
    let lident_lident_loc = mkloc (Lident s) loc in
    let exp = match $2 with
      | Some x -> x
      | None -> mkexp ~loc (Pexp_ident lident_lident_loc)
    in
    (lident_lident_loc, exp)
  }
;

(**
 * field_expr is distinct from record_expr because labels cannot/shouldn't be scoped.
 *)
field_expr:
  (* Using LIDENT instead of label here, because a reduce/reduce conflict occurs on:
   *   {blah:x}
   *
   * After `blah`, the parser couldn't tell whether to reduce `label` or
   * `val_ident`. So inlining the terminal here to avoid the whole decision.
   * Another approach would have been to place the `label` rule at at a precedence
   * of below_COLON or something.
   *)
 | as_loc(LIDENT) COLON expr
   { ($1, $3) }
 | LIDENT
   { let loc = mklocation $symbolstartpos $endpos in
     let lident_loc = mkloc $1 loc in
     let lident_lident_loc = mkloc (Lident $1) loc in
     (lident_loc, mkexp (Pexp_ident lident_lident_loc))
   }
;

%inline field_expr_list: lseparated_nonempty_list(COMMA, field_expr) { $1 };

(* Allows for Ptyp_package core types without parens in the context
 * of a "type_constraint":
 * let x: module Foo.Bar.Baz = (module FirstClass)
 *        ^^^^^^^^^^^^^^^^^^
 *)
%inline module_constraint_type:
  mark_position_typ (preceded(MODULE, package_type)) {$1}
;

type_constraint:
  | COLON core_type
      preceded(COLONGREATER,core_type)?
    { (Some $2, $3) }
  | COLONGREATER core_type
    { (None, Some $2) }
  | COLON module_constraint_type
    { (Some $2, None) }
;

(* Patterns *)

pattern:
  | pattern_without_or { $1 }
  | mark_position_pat(pattern BAR pattern { mkpat(Ppat_or($1, $3)) }) { $1 }
;

%inline pat_comma_list:
  lseparated_nonempty_list(COMMA, pattern_optional_constraint) COMMA? { $1 };

pattern_constructor_argument:
  | simple_pattern_direct_argument
    { [$1] }
  | parenthesized(pat_comma_list)
    { $1 }
;

(**
* Provides sugar for pattern matching on a constructor pattern with a 'direct' argument.
* Example:
* | Foo () => () is sugar for | Foo(()) => ()
* | Foo [a, b, c] => () is sugar for | Foo([a, b, c]) => ()
* | Foo [|x, y|] => () is sugar for | Foo([|x, y|]) => ()
* }
*)
simple_pattern_direct_argument:
mark_position_pat (
   as_loc(constr_longident)
    { mkpat(Ppat_construct(mkloc $1.txt $1.loc, None)) }
  | simple_delimited_pattern { $1 }
  ) {$1}
;

pattern_without_or:
mark_position_pat
  ( simple_pattern { $1 }

  | pattern_without_or AS as_loc(val_ident)
    { mkpat(Ppat_alias($1, $3)) }

  (**
    * Parses a (comma-less) list of patterns into a tuple, or a single pattern
    * (if there is only one item in the list). This is kind of sloppy as there
    * should probably be a different AST construct for the syntax construct this
    * is used in (multiple constructor arguments). The things passed to
    * constructors are not actually tuples either in underlying representation or
    * semantics (they are not first class).
    *)
  | as_loc(constr_longident) pattern_constructor_argument
    (* the first case is `| Foo(_)` and doesn't need explicit_arity attached. Actually, something like `| Foo(1)` doesn't either, but we
      keep explicit_arity on the latter anyways because why not. But for `| Foo(_)` in particular, it's convenient to have explicit_arity
      removed, so that you can have the following shortcut:
      | Foo _ _ _ _ _
      vs.
      | Foo _
    *)
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
    { syntax_error $2.loc
        ":: is not supported in Reason, please use [hd, ...tl] instead";
      let loc = mklocation $symbolstartpos $endpos in
      mkpat_cons (mkpat ~ghost:true ~loc (Ppat_tuple[$1;$3])) loc
    }

  | LPAREN COLONCOLON RPAREN LPAREN pattern_without_or COMMA pattern_without_or RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkpat_cons (mkpat ~ghost:true ~loc (Ppat_tuple[$5;$7])) loc
    }

  | EXCEPTION pattern_without_or %prec prec_constr_appl
    { mkpat(Ppat_exception $2) }

  | LAZY simple_pattern { mkpat(Ppat_lazy $2) }

  (**
   * Attribute "attribute" everything to the left of the attribute,
   * up until the point of to the start of an expression, left paren, left
   * bracket, comma, bar - whichever comes first.
   *)
  | attribute pattern_without_or %prec attribute_precedence
    { {$2 with ppat_attributes = $1 :: $2.ppat_attributes} }

  ) {$1};

(* A "simple pattern" is either a value identifier, or it is a
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
 *)
simple_pattern:
  | simple_pattern_ident
  | simple_pattern_not_ident { $1 }
;

simple_pattern_ident:
  as_loc(val_ident) { mkpat ~loc:$1.loc (Ppat_var $1) }
;

simple_pattern_not_ident:
mark_position_pat
  ( UNDERSCORE
    { mkpat (Ppat_any) }
  | signed_constant
    { let attrs, cst = $1 in mkpat ~attrs (Ppat_constant cst) }
  | signed_constant DOTDOT signed_constant
    { mkpat (Ppat_interval (snd $1, snd $3)) }
  | signed_constant DOT signed_constant {
    syntax_error (mklocation $startpos $endpos) "Constant ranges must be separated with spaces around the ..";
     mkpat (Ppat_interval (snd $1, snd $3))
  }
  | as_loc(constr_longident)
    { mkpat (Ppat_construct ($1, None)) }
  | name_tag
    { mkpat (Ppat_variant ($1, None)) }
  | SHARP type_longident
    { mkpat (Ppat_type ($2)) }
  | LPAREN lseparated_nonempty_list(COMMA, pattern_optional_constraint) COMMA? RPAREN
    { match $2 with
      | [] -> (* This shouldn't be possible *)
        let loc = mklocation $startpos $endpos in
        mkpat_constructor_unit loc loc
      | [hd] -> hd
      | _ :: _ -> mkpat (Ppat_tuple $2)
    }
  | LPAREN MODULE as_loc(UIDENT) RPAREN
    { mkpat(Ppat_unpack($3)) }
  | simple_pattern_not_ident_
    { $1 }
  | extension
    { mkpat(Ppat_extension $1) }
  ) {$1};

simple_pattern_not_ident_:
  | simple_delimited_pattern
    { $1 }
  | as_loc(mod_longident) DOT simple_delimited_pattern
    { let loc = mklocation $symbolstartpos $endpos in
      mkpat ~loc (Ppat_open ($1, $3))
    }
  | as_loc(mod_longident) DOT LPAREN pattern RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkpat ~loc (Ppat_open ($1, $4)) }
  | as_loc(mod_longident) DOT as_loc(LBRACKET RBRACKET {Lident "[]"})
    { let loc = mklocation $symbolstartpos $endpos in
      mkpat ~loc (Ppat_open($1, mkpat ~loc:$3.loc (Ppat_construct($3, None)))) }
  | as_loc(mod_longident) DOT as_loc(LPAREN RPAREN {Lident "()"})
    { let loc = mklocation $symbolstartpos $endpos in
      mkpat ~loc (Ppat_open($1, mkpat ~loc:$3.loc (Ppat_construct($3, None)))) }

%inline simple_delimited_pattern:
  | record_pattern { $1 }
  | list_pattern   { $1 }
  | array_pattern  { $1 }

%inline record_pattern:
    LBRACE lbl_pattern_list RBRACE
    { let (fields, closed) = $2 in mkpat (Ppat_record (fields, closed)) }
;

%inline list_pattern:
    LBRACKET pattern_comma_list_extension RBRACKET
    { make_real_pat (mktailpat_extension (mklocation $startpos($2) $endpos($2)) $2) }
;

%inline array_pattern:
    LBRACKETBAR loption(terminated(pattern_comma_list,COMMA?)) BARRBRACKET
    { mkpat (Ppat_array $2) }
;

pattern_optional_constraint:
mark_position_pat
  ( pattern                 { $1 }
  | pattern COLON core_type
    { mkpat(Ppat_constraint($1, $3)) }
  (* If we kill the `let module …` syntax, this can be placed inside pattern.
   * Allows parsing of
   *  let foo = (type a, module X: X_t with type t = a) => X.a;
   *                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   * The `module` keyword after the colon is optional, because `module X`
   * clearly indicates that we're dealing with a Ppat_unpack here.
   *)
  | MODULE as_loc(UIDENT) COLON mark_position_typ(preceded(MODULE?, package_type))
    { mkpat (Ppat_constraint (mkpat (Ppat_unpack $2), $4)) }
  ) {$1};
;

%inline pattern_comma_list:
  lseparated_nonempty_list(COMMA, opt_spread(pattern))
  { let msg = "Array's `...` spread is not supported in pattern matches.
Explanation: such spread would create a subarray; out of performance concern, our pattern matching currently guarantees to never create new intermediate data.
Solution: if it's to validate the first few elements, use a `when` clause + Array size check + `get` checks on the current pattern. If it's to obtain a subarray, use `Array.sub` or `Belt.Array.slice`." in
    filter_raise_spread_syntax msg $1 };

(* [x, y, z, ...n] --> ([x,y,z], Some n) *)
pattern_comma_list_extension:
  lseparated_nonempty_list(COMMA, opt_spread(pattern)) COMMA?
  { match List.rev $1 with
    (* spread syntax is only allowed at the end *)
    | ((dotdotdot, p) as hd)::ps ->
      let (ps, spreadPat) = match dotdotdot with
      | Some _ -> (ps, Some p)
      | None -> (hd::ps, None)
      in
      let msg = "List pattern matches only supports one `...` spread, at the end.
Explanation: a list spread at the tail is efficient, but a spread in the middle would create new list(s); out of performance concern, our pattern matching currently guarantees to never create new intermediate data." in
      let patList = filter_raise_spread_syntax msg ps in
      (List.rev patList, spreadPat)
    | [] -> [], None
  };
;

_lbl_pattern_list:
  | opt_spread(lbl_pattern)                            { ([$1], Closed) }
  | opt_spread(lbl_pattern) COMMA                      { ([$1], Closed) }
  | opt_spread(lbl_pattern) COMMA UNDERSCORE COMMA?    { ([$1], Open)   }
  | opt_spread(lbl_pattern) COMMA _lbl_pattern_list
    { let (fields, closed) = $3 in $1 :: fields, closed }
;

%inline lbl_pattern_list:
  _lbl_pattern_list
  { let (fields, closed) = $1 in
    (filter_raise_spread_syntax record_pat_spread_msg fields, closed)
  }
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

(* Primitive declarations *)

primitive_declaration: nonempty_list(STRING { let (s, _, _) = $1 in s }) {$1};

(* Type declarations

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
*)

type_declarations:
  item_attributes TYPE nonrec_flag type_declaration_details
  { let (ident, params, constraints, kind, priv, manifest), endpos, and_types = $4 in
    let loc = mklocation $startpos($2) endpos in
    let ty = Type.mk ident ~params:params ~cstrs:constraints
             ~kind ~priv ?manifest ~attrs:$1 ~loc in
    ($3, ty :: and_types)
  }
;

and_type_declaration:
  | { [] }
  | item_attributes AND type_declaration_details
    { let (ident, params, cstrs, kind, priv, manifest), endpos, and_types = $3 in
      let loc = mklocation $symbolstartpos endpos in
      Type.mk ident ~params ~cstrs ~kind ~priv ?manifest ~attrs:$1 ~loc
      :: and_types
    }
;

%inline type_declaration_details:
  | as_loc(UIDENT) optional_type_params(type_variable_with_variance) type_declaration_kind
    { syntax_error $1.loc
        "a type name must start with a lower-case letter or an underscore";
      let (kind, priv, manifest), constraints, endpos, and_types = $3 in
      (($1, $2, constraints, kind, priv, manifest), endpos, and_types) }
  | as_loc(LIDENT) optional_type_params(type_variable_with_variance) type_declaration_kind
    { let (kind, priv, manifest), constraints, endpos, and_types = $3 in
      (($1, $2, constraints, kind, priv, manifest), endpos, and_types) }
;

type_declaration_kind:
  | EQUAL private_flag constructor_declarations
    { let (cstrs, constraints, endpos, and_types) = $3 in
      ((Ptype_variant (cstrs), $2, None), constraints, endpos, and_types) }
  | EQUAL core_type EQUAL private_flag constructor_declarations
    { let (cstrs, constraints, endpos, and_types) = $5 in
      ((Ptype_variant cstrs, $4, Some $2), constraints, endpos, and_types) }
  | type_other_kind constraints and_type_declaration
    { ($1, $2, $endpos($2), $3) }
;

type_subst_kind:
  | COLONEQUAL private_flag type_subst_constructor_declarations
    { let (cstrs, constraints, endpos, and_types) = $3 in
      ((Ptype_variant (cstrs), $2, None), constraints, endpos, and_types) }
  | COLONEQUAL core_type EQUAL private_flag type_subst_constructor_declarations
    { let (cstrs, constraints, endpos, and_types) = $5 in
      ((Ptype_variant cstrs, $4, Some $2), constraints, endpos, and_types) }
  | type_subst_other_kind constraints and_type_subst_declaration
    { ($1, $2, $endpos($2), $3) }
;

type_subst_declarations:
    item_attributes TYPE nrf=nonrec_flag name=as_loc(LIDENT)
     params=optional_type_params(type_variable_with_variance) kind_priv_man=type_subst_kind
      { check_nonrec_absent (mklocation $startpos(nrf) $endpos(nrf)) nrf;
        let (kind, priv, manifest), cstrs, endpos, and_types = kind_priv_man in
        let ty =
          Type.mk name ~params ~cstrs ~kind ~priv
            ?manifest ~attrs:$1
            ~loc:(mklocation $symbolstartpos endpos)
        in
        ty :: and_types }
;

and_type_subst_declaration:
  | { [] }
  | item_attributes AND name=as_loc(LIDENT)
    params=optional_type_params(type_variable_with_variance) kind_priv_man=type_subst_kind
    { let (kind, priv, manifest), cstrs, endpos, and_types = kind_priv_man in
      Type.mk name ~params ~cstrs
        ~kind ~priv ?manifest
        ~attrs:$1
        ~loc:(mklocation $symbolstartpos endpos)
      :: and_types
    }
;

%inline constraints:
  | { [] }
  | preceded(CONSTRAINT, constrain)+ { $1 }
;

type_subst_other_kind:
    nonempty_type_other_kind(COLONEQUAL)
      { $1 }

type_other_kind:
  | (*empty*)
    { (Ptype_abstract, Public, None) }
  | nonempty_type_other_kind(EQUAL)
    { $1 }
;

%inline nonempty_type_other_kind(eq_symbol):
  | eq_symbol private_flag core_type
    { (Ptype_abstract, $2, Some $3) }
  | eq_symbol private_flag item_attributes record_declaration
    { (Ptype_record (prepend_attrs_to_labels $3 $4), $2, None) }
  | eq_symbol private_flag DOTDOT
    { (Ptype_open, $2, None) }
  | eq_symbol core_type EQUAL DOTDOT
    { (Ptype_open, Public, Some $2) }
  | eq_symbol core_type EQUAL private_flag item_attributes record_declaration
    { (Ptype_record (prepend_attrs_to_labels $5 $6), $4, Some $2) }
;

/**
 * Class syntax cannot accept an underscore for type parameters.
 * There may be type checking problems, but at the very least it causes
 * a grammar conflict. The grammar conflict would go away if type parameters
 * *required* <> instead of also allowing ().
 */
%inline type_variable_without_underscore:
  embedded
  ( QUOTE ident       { (mktyp (Ptyp_var $2) , Invariant    ) }
  | PLUS QUOTE ident  { (mktyp (Ptyp_var $3) , Covariant    ) }
  | PLUS UNDERSCORE   { (mktyp (Ptyp_any)    , Covariant    ) }
  | MINUS QUOTE ident { (mktyp (Ptyp_var $3) , Contravariant) }
  | MINUS UNDERSCORE  { (mktyp Ptyp_any      , Contravariant) }
  )
  { let first, second = $1 in
    let ptyp_loc =
        {first.ptyp_loc with loc_start = $symbolstartpos; loc_end = $endpos}
    in
    ({first with ptyp_loc}, second)
  }
;


type_variable_with_variance:
  | type_variable_without_underscore { $1 }
  | UNDERSCORE        {
    let first = mktyp Ptyp_any in
    let second = Invariant in
    let ptyp_loc = {first.ptyp_loc with loc_start = $symbolstartpos; loc_end = $endpos} in
    ({first with ptyp_loc}, second)
  }
;

constructor_declarations:
  | BAR and_type_declaration { ([], [], $endpos, $2) }
  | either(constructor_declaration,bar_constructor_declaration)
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

type_subst_constructor_declarations:
  either(constructor_declaration,bar_constructor_declaration)
  type_subst_constructor_declarations_aux
  { let (cstrs, constraints, endpos, and_types) = $2 in
    ($1 :: cstrs, constraints, endpos, and_types)
  }
;

type_subst_constructor_declarations_aux:
  | bar_constructor_declaration type_subst_constructor_declarations_aux
    { let (cstrs, constraints, endpos, and_types) = $2 in
      ($1 :: cstrs, constraints, endpos, and_types)
    }
  | constraints and_type_subst_declaration
    { ([], $1, $endpos($1), $2) }
;

bar_constructor_declaration:
  item_attributes BAR constructor_declaration
  { {$3 with pcd_attributes = $1 @ $3.pcd_attributes} }
;

constructor_declaration:
  item_attributes as_loc(constr_ident) generalized_constructor_arguments
  { let args, res = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Type.constructor ~attrs:$1 $2 ~args ?res ~loc }
;

(* Why are there already attribute* on the extension_constructor_declaration? *)
str_exception_declaration:
  item_attributes EXCEPTION
    either(extension_constructor_declaration, extension_constructor_rebind)
  { { $3 with pext_attributes = $3.pext_attributes @ $1} }
;

sig_exception_declaration:
  item_attributes EXCEPTION
    extension_constructor_declaration
  { let decl = { $3 with pext_attributes = $3.pext_attributes @ $1} in
    Te.mk_exception ~loc:decl.pext_loc decl
  }
;

generalized_constructor_arguments:
  constructor_arguments? preceded(COLON,core_type)?
  { ((match $1 with None -> Pcstr_tuple [] | Some x -> x), $2) }
;

constructor_arguments_comma_list:
  lseparated_nonempty_list(COMMA, core_type) COMMA? {$1}
;

constructor_arguments:
  | object_record_type { Pcstr_tuple [$1] }
    (* XXX(anmonteiro): parse both parenthesized and non-parenthesized record
     * declaration for backwards compatibility. This could probably be removed
     * in a later (major) version if properly documented. *)
  | record_declaration { Pcstr_record $1 }
  | parenthesized(record_declaration) { Pcstr_record $1 }
  | parenthesized(constructor_arguments_comma_list)
    { Pcstr_tuple $1 }
;

record_label_declaration:
  | item_attributes mutable_flag as_loc(LIDENT)
    { let loc = mklocation $symbolstartpos $endpos in
      Type.field $3 (mkct $3) ~attrs:$1 ~mut:$2 ~loc
    }
  | item_attributes mutable_flag as_loc(LIDENT) COLON poly_type
    { let loc = mklocation $symbolstartpos $endpos in
      Type.field $3 $5 ~attrs:$1 ~mut:$2 ~loc
    }
;

record_declaration:
  LBRACE lseparated_nonempty_list(COMMA, record_label_declaration) COMMA? RBRACE
  { $2 }
;


(* Type Extensions *)

str_type_extension:
  attrs = item_attributes
  TYPE flag = nonrec_flag
    ident = as_loc(itype_longident)
    params = optional_type_params(type_variable_with_variance)
  PLUSEQ priv = embedded(private_flag)
  constructors =
    attributed_ext_constructors(either(extension_constructor_declaration, extension_constructor_rebind))
  { if flag <> Recursive then
      not_expecting $startpos(flag) $endpos(flag) "nonrec flag";
    Te.mk ~params ~priv ~attrs ident constructors
  }
;

sig_type_extension:
  attrs = item_attributes
  TYPE flag = nonrec_flag
    ident = as_loc(itype_longident)
    params = optional_type_params(type_variable_with_variance)
  PLUSEQ priv = embedded(private_flag)
  constructors =
    attributed_ext_constructors(extension_constructor_declaration)
  { if flag <> Recursive then
      not_expecting $startpos(flag) $endpos(flag) "nonrec flag";
    Te.mk ~params ~priv ~attrs ident constructors
  }
;

%inline attributed_ext_constructor(X):
  item_attributes BAR item_attributes X { {$4 with pext_attributes = List.concat [$1; $3; $4.pext_attributes]} }
  (* Why is item_attributes duplicated?
     To be consistent with attributes on (poly)variants/gadts.
     So we can place the attribute after the BAR.
     Example:
      type water +=
        pri
        | [@foo] MineralWater;
  *)
;

attributed_ext_constructors(X):
  | X attributed_ext_constructor(X)* { $1 :: $2 }
  | attributed_ext_constructor(X)+ { $1 }
;

extension_constructor_declaration:
  as_loc(constr_ident) generalized_constructor_arguments
  { let args, res = $2 in
    let loc = mklocation $symbolstartpos $endpos in
    Te.decl $1 ~args ?res ~loc
  }
;

extension_constructor_rebind:
  as_loc(constr_ident) EQUAL as_loc(constr_longident)
  { let loc = mklocation $symbolstartpos $endpos in
    Te.rebind $1 $3 ~loc
  }
;

(* "with" constraints (additional type equations over signature components) *)

with_constraint:
  | TYPE as_loc(label_longident) optional_type_params(type_variable_with_variance)
      EQUAL embedded(private_flag) core_type constraints
    { let loc = mklocation $symbolstartpos $endpos in
      let typ = Type.mk {$2 with txt=Longident.last $2.txt}
                  ~params:$3 ~cstrs:$7 ~manifest:$6 ~priv:$5 ~loc in
      Pwith_type ($2, typ)
    }
    (* used label_longident instead of type_longident to disallow
       functor applications in type path *)
  | TYPE as_loc(label_longident) optional_type_params(type_variable_with_variance)
      COLONEQUAL core_type
    { let last = match $2.txt with
        | Lident s -> s
        | other ->
          not_expecting $startpos($2) $endpos($2) "Long type identifier";
          let rec fallback = function
            | Lident s -> s
            | Ldot (_, s) -> s
            | Lapply (l, _) -> fallback l
          in
          fallback other
      in
      let loc = mklocation $symbolstartpos $endpos in
      Pwith_typesubst ($2, Type.mk {$2 with txt=last} ~params:$3 ~manifest:$5 ~loc)
    }
  | MODULE as_loc(mod_longident) EQUAL as_loc(mod_ext_longident)
      { Pwith_module ($2, $4) }
  | MODULE as_loc(UIDENT) COLONEQUAL as_loc(mod_ext_longident)
      { let lident = {$2 with txt=Lident $2.txt} in
        Pwith_modsubst (lident, $4) }
;

(* Polymorphic types *)
poly_type:
mark_position_typ
  ( core_type
    { $1 }
  | as_loc(preceded(QUOTE,ident))+ DOT core_type
    { mktyp(Ptyp_poly($1, $3)) }
  ) {$1};

(**
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
  *)

(** Potentially includes:
 * - arrows
 * - space separated type applications
 * - "as" aliases
 *)
core_type:
mark_position_typ
  (* For some reason, when unifying Functor type syntax (using EQUALGREATER),
   * there was a shift reduce conflict likely caused by
   * type module MyFunctor = {type x = blah => foo } => SomeSig
   * That should *not* be a shift reduce conflict (on =>), and it's not clear
   * why the shift reduce conflict showed up in core_type2. Either way, this
   * seems to resolve the issue. If removing the below_EQUALGREATER, the shift
   * reduce conflict is actually caused by the new Functor type annotations
   * even though *nothing* will point towards that.
   * If switching to Menhir, this may not be needed.
   *)
  (
    core_type2
    { $1 }
  | core_type2 AS QUOTE ident
    { mktyp(Ptyp_alias($1, $4)) }
  ) {$1};

(**
 *
 * core_type is basically just core_type2 but with a single AS x potentially
 * appended.
 * core_type2 Potentially includes:
 * - arrows
 * - space separated type applications
 * - Polymorphic type variable application
 *)
core_type2:
  item_attributes ct = unattributed_core_type
  { match $1 with
    | [] -> ct
    | attrs ->
      let loc_start = $symbolstartpos and loc_end = $endpos in
      let ptyp_loc = {ct.ptyp_loc with loc_start; loc_end} in
      let ptyp_attributes = attrs @ ct.ptyp_attributes in
      {ct with ptyp_attributes; ptyp_loc}
  }
;

unattributed_core_type:
  | non_arrowed_simple_core_type { $1 }
  | arrowed_simple_core_type { $1 }
;

(* arrowed: because it contains =>
 * simple: it doesn't need to be wrapped in parens *)
arrowed_simple_core_type:
  | ES6_FUN arrow_type_parameters EQUALGREATER core_type2
    { List.fold_right mktyp_arrow $2 $4 }
  | as_loc(labelled_arrow_type_parameter_optional) EQUALGREATER core_type2
    { mktyp_arrow ($1, false) $3 }
  | basic_core_type EQUALGREATER core_type2
    { mktyp (Ptyp_arrow (Nolabel, $1, $3)) }
;

labelled_arrow_type_parameter_optional:
  | TILDE LIDENT COLON protected_type EQUAL optional
    { ($6 $2, $4) }
;

arrow_type_parameter:
  | protected_type  { (Nolabel, $1) }
  | TILDE LIDENT COLON protected_type
    { (Labelled $2, $4) }
  | labelled_arrow_type_parameter_optional { $1 }
;

%inline uncurried_arrow_type_parameter:
    DOT? as_loc(arrow_type_parameter)
  { let uncurried = match $1 with | Some _ -> true | None -> false in
    ($2, uncurried)
  }

%inline arrow_type_parameter_comma_list:
    | lseparated_nonempty_list(COMMA, uncurried_arrow_type_parameter) COMMA? {$1}

arrow_type_parameters:
 | LPAREN arrow_type_parameter_comma_list RPAREN { $2 }
;

(* Among other distinctions, "simple" core types can be used in Variant types:
 * type myType = Count of anySimpleCoreType. Core types (and simple core types)
 * don't include variant declarations (`constructor_declarations`) and don't
 * include the "faux curried" variant Constructor arguments list.
 *
 * In general, "simple" syntax constructs, don't need to be wrapped in
 * parens/braces when embedded in lists of those very constructs.
 *
 * A [simple_core_type] *can* be wrapped in parens, but
 * it doesn't have to be.
 *)

(* The name [core_type] was taken. [non_arrowed_core_type] is the same as
 * [simple_core_type] but used in cases
 * where application needn't be wrapped in additional parens *)
(* Typically, other syntax constructs choose to allow either
 * [simple_core_type] or
 * [non_arrowed_non_simple_core_type] depending on whether or not
 * they are in a context that expects space separated lists of types to carry
 * particular meaning outside of type constructor application.
 *
 * type x = SomeConstructor x y;
 *)
non_arrowed_core_type:
  | non_arrowed_simple_core_type
    { $1 }
  | attribute non_arrowed_core_type
    { {$2 with ptyp_attributes = $1 :: $2.ptyp_attributes} }
;

%inline type_parameter_comma_list:
  | lseparated_nonempty_list(COMMA, protected_type) COMMA? {$1}
;

%inline first_less_than_type_ident:
  LESSIDENT { Lident $1 }

(* Since the <xyz token is parsed as a single token we need to catch that case here *)
%inline first_less_than_type_param:
mark_position_typ
  (  as_loc(first_less_than_type_ident)
      { mktyp(Ptyp_constr($1, [])) }
    | as_loc(first_less_than_type_ident) type_parameters
      { mktyp(Ptyp_constr($1, $2)) }
  ) { $1 }

type_parameters:
  | parenthesized(type_parameter_comma_list) { $1 }
  | lessthangreaterthanized(type_parameter_comma_list) { $1 }
  | first_less_than_type_param COMMA? GREATER { [$1] }
  | first_less_than_type_param COMMA type_parameter_comma_list GREATER
    {
      $1 :: $3
    }
;

(* "protected" stands for an environment where non-simple grammar
 * is actually simple. non-simple => parens, simple => no-parens necessary
 * For examples, in lists the ( and ) combined with , form a "protected"
 * environment for non-simple types.
 * in tuples: (int, string, float) ||-> int, string, float are protected
 * in functions args: (int, string) => float ||-> int, string are protected *)
protected_type:
  | module_constraint_type | core_type { $1 }
;

non_arrowed_simple_core_types:
mark_position_typ
  ( type_parameters
    { match $1 with
      | [one] -> one
      | many -> mktyp (Ptyp_tuple many)
    }
  ) {$1};

non_arrowed_simple_core_type:
  | non_arrowed_simple_core_types       { $1 }
  | mark_position_typ(basic_core_type) { $1 }
;

basic_core_type:
mark_position_typ
  ( type_longident type_parameters
    { mktyp(Ptyp_constr($1, $2)) }
  | SHARP as_loc(class_longident) type_parameters
    { mktyp(Ptyp_class($2, $3)) }
  | QUOTE ident
    { mktyp(Ptyp_var $2) }
  | SHARP as_loc(class_longident)
    { mktyp(Ptyp_class($2, [])) }
  | UNDERSCORE
    { mktyp(Ptyp_any) }
  | type_longident
    { mktyp(Ptyp_constr($1, [])) }
  | object_record_type
    { $1 }
  | LBRACKET row_field_list RBRACKET
    { mktyp(Ptyp_variant ($2, Closed, None)) }
  | LBRACKETGREATER loption(row_field_list) RBRACKET
    { mktyp(Ptyp_variant ($2, Open, None)) }
  | LBRACKETLESS row_field_list loption(preceded(GREATER, name_tag+)) RBRACKET
    { mktyp(Ptyp_variant ($2, Closed, Some $3)) }
  | extension
    { mktyp(Ptyp_extension $1) }
  ) {$1};

object_record_type:
  | LBRACE RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      syntax_error_typ loc "an object type cannot be empty" }
  | LBRACE DOT string_literal_labels RBRACE
    { (* `{. "foo": bar}` -> `Js.t({. foo: bar})` *)
      let loc = mklocation $symbolstartpos $endpos in
      mkBsObjTypeSugar ~loc ~closed:Closed $3
    }
  | LBRACE DOTDOT string_literal_labels RBRACE
    { (* `{.. "foo": bar}` -> `Js.t({.. foo: bar})` *)
      let loc = mklocation $symbolstartpos $endpos in
      mkBsObjTypeSugar ~loc ~closed:Open $3
    }
  | LBRACE DOT loption(object_label_declarations) RBRACE
    { mktyp (Ptyp_object ($3, Closed)) }
  | LBRACE DOTDOT loption(object_label_declarations) RBRACE
    { mktyp (Ptyp_object ($3, Open)) }
;

object_label_declaration:
  | item_attributes as_loc(LIDENT)
    { Of.tag ~attrs:$1 $2 (mkct $2) }
  | item_attributes as_loc(LIDENT) COLON poly_type
    { Of.tag ~attrs:$1 $2 $4 }
  | DOTDOTDOT basic_core_type
    { Of.inherit_ ~loc:(mklocation $symbolstartpos $endpos) $2 }
;

object_label_declarations:
  lseparated_nonempty_list(COMMA, object_label_declaration) COMMA? { $1 };

string_literal_label:
  item_attributes STRING COLON poly_type
  { let (label, _raw, _delim) = $2 in
    let lblloc = mkloc label (mklocation $startpos($2) $endpos($2)) in
    Of.tag ~loc:(mklocation $symbolstartpos $endpos) ~attrs:$1 lblloc $4 }
;

string_literal_labels:
  lseparated_nonempty_list(COMMA, string_literal_label) COMMA? { $1 };

package_type:
  module_type
  { let loc = mklocation $startpos $endpos in
    match package_type_of_module_type $1 with
    | Some result -> mktyp ~loc (Ptyp_package result)
    | None ->
       syntax_error_typ $1.pmty_loc
         "only module type identifier and 'with type' constraints are supported"
  }
;

row_field_list:
  | row_field bar_row_field* { $1 :: $2 }
  | bar_row_field bar_row_field* { $1 :: $2 }
;

row_field:
  | tag_field             { $1 }
  | non_arrowed_core_type
    { Rf.inherit_ ~loc:(mklocation $symbolstartpos $endpos) $1 }
;

(* TODO::: *)
bar_row_field:
  item_attributes BAR row_field
  { let loc = (mklocation $symbolstartpos $endpos) in
    match $3.prf_desc with
    | Rtag (name, amp, typs) ->
      Rf.tag ~loc ~attrs:($1 @ $3.prf_attributes) name amp typs
    | Rinherit typ ->
      Rf.inherit_ ~loc { typ with ptyp_attributes = ($1 @ typ.ptyp_attributes) }
  }
;

tag_field:
  | item_attributes as_loc(name_tag)
      boption(AMPERSAND)
      separated_nonempty_list(AMPERSAND, non_arrowed_simple_core_types)
    { Rf.tag ~loc:(mklocation $symbolstartpos $endpos) ~attrs:$1 $2 $3 $4 }
  | item_attributes as_loc(name_tag)
    { Rf.tag ~loc:(mklocation $symbolstartpos $endpos) ~attrs:$1 $2 true [] }
;

(* Constants *)

constant:
  | INT          { let (n, m) = $1 in ([], Pconst_integer (n, m)) }
  | CHAR         { ([], Pconst_char $1) }
  | FLOAT        { let (f, m) = $1 in ([], Pconst_float (f, m)) }
  | STRING       {
    let (s, raw, d) = $1 in
    let attr = match raw with
      | None -> []
      | Some raw ->
        let constant = Ast_helper.Exp.constant (Pconst_string (raw, None)) in
        [ { attr_name = Location.mknoloc "reason.raw_literal";
            attr_payload = PStr [mkstrexp constant []];
            attr_loc = Location.none
          } ]
    in
    (attr, Pconst_string (s, d))
  }
;

(*
 * Important: Read docs/TEMPLATE_LITERALS.md to understand this.
 * TODO: In STRING_TEMPLATE_TERMINATED case, detect empty string and return a
 * None.
 *)
template_string:
  | STRING_TEMPLATE_TERMINATED {
    let split_by_newlines = Reason_syntax_util.split_by_newline ~keep_empty:true $1 in
    let revLines = List.rev split_by_newlines in
    let (indent, revLines) =
      Reason_template.Parse.normalize_or_remove_last_line revLines in
    let txt =
      Reason_template.Parse.strip_leading_for_non_last ~indent "" revLines in
    ( indent,
      Ast_helper.Exp.constant (Pconst_string (txt, Some "reason.template")))
  }
  | STRING_TEMPLATE_SEGMENT_LBRACE seq_expr RBRACE template_string
  {
    let indent, tmplt = $4 in
    let op1 = mkloc "++" (mklocation $endpos($1) $startpos($2)) in
    let op2 = mkloc "++" (mklocation $startpos($3) $startpos($4)) in
    (* Right associative, unlike the future ++ will be parsed in next breaking
     * change. We will keep this right assoc though to make it easy to print *)
    let attrs = simple_ghost_text_attr "reason.template" in
    let seq_expr = reloc_expr $2 $endpos($1) $startpos($3) in
    if String.length $1 == 0 then
      (indent, mkinfix ~attrs seq_expr op2 tmplt)
    else
      let split_by_newlines = Reason_syntax_util.split_by_newline ~keep_empty:true $1 in
      let revLines = List.rev split_by_newlines in
      let txt =
        Reason_template.Parse.strip_leading_for_non_last ~indent "" revLines in
      let left = (Ast_helper.Exp.constant (Pconst_string (txt, None))) in
      (indent, mkinfix ~attrs left op1 (mkinfix ~attrs seq_expr op2 tmplt))
    (* TODO: Perform the string concat or printf depending *)
  }
;

signed_constant:
  | constant     { $1 }
  | MINUS INT    { let (n, m) = $2 in ([], Pconst_integer("-" ^ n, m)) }
  | MINUS FLOAT  { let (f, m) = $2 in ([], Pconst_float("-" ^ f, m)) }
  | PLUS INT     { let (n, m) = $2 in ([], Pconst_integer (n, m)) }
  | PLUS FLOAT   { let (f, m) = $2 in ([], Pconst_float(f, m)) }
;

(* Identifiers and long identifiers *)

ident: UIDENT | LIDENT { $1 };

val_ident:
  | LIDENT                 { $1 }
  | LPAREN operator RPAREN { $2 }
;

%inline infix_operator:
  | GREATER           { ">" }
  | INFIXOP0          { $1 }
  | INFIXOP1          { $1 }
  | INFIXOP2          { $1 }
  | INFIXOP3          { $1 }
  (* SLASHGREATER is INFIXOP3 but we needed to call it out specially *)
  | SLASHGREATER      { "/>" }
  | INFIXOP4          { $1 }
  | PLUS          { "+"  }
  | PLUSDOT       { "+." }
  | MINUS         { "-"  }
  | MINUSDOT      { "-." }
  | STAR              { "*" }
  | LESS              { "<" }
  | OR                { "or" }
  | BARBAR            { "||" }
  | AMPERSAND         { "&" }
  | AMPERAMPER        { "&&" }
  | COLONEQUAL        { ":=" }
  | PLUSEQ            { "+=" }
  | PERCENT           { "%" }
  | GREATERDOTDOTDOT { ">..." }
  (* We don't need to (and don't want to) consider </> an infix operator for now
     because our lexer requires that "</>" be expressed as "<\/>" because
     every star and slash after the first character must be escaped.
  *)
  (* Also, we don't want to consider <> an infix operator because the Reason
     operator swapping requires that we express that as != *)
  | LESSDOTDOTGREATER { "<..>" }
  | GREATER GREATER   { ">>" }

operator:
  | PREFIXOP          { $1 }
  | POSTFIXOP         { $1 }
  | BANG              { "!" }
  | infix_operator    { $1 }
  | LETOP             { $1 }
  | ANDOP             { $1 }
;
%inline constr_ident:
  | UIDENT            { $1 }
  | LBRACKET RBRACKET { "[]" }
  | LPAREN RPAREN     { "()" }
  | COLONCOLON        { "::" }
(*  | LPAREN COLONCOLON RPAREN { "::" } *)
  | FALSE             { "false" }
  | TRUE              { "true" }
;

val_longident:
  | val_ident                     { Lident $1 }
  | mod_longident DOT val_ident   { Ldot($1, $3) }
;

constr_longident:
  | mod_longident %prec below_DOT { $1 }
  | LBRACKET RBRACKET             { Lident "[]" }
  | LPAREN RPAREN                 { Lident "()" }
  | FALSE                         { Lident "false" }
  | TRUE                          { Lident "true" }
;

label_longident:
  | LIDENT                        { Lident $1 }
  | mod_longident DOT LIDENT      { Ldot($1, $3) }
;

type_longident: as_loc(itype_longident) { $1 };

%inline itype_longident:
  | LIDENT                        { Lident $1 }
  | mod_ext_longident DOT LIDENT  { Ldot($1, $3) }
;

mod_longident:
  | UIDENT                        { Lident $1 }
  | mod_longident DOT UIDENT      { Ldot($1, $3) }
;

/*
mod_less_uident_ext_longident:
  imod_less_uident_ext_longident { $1 }
;

%inline imod_less_uident_ext_longident:
  | LESSUIDENT                    { Lident $1 }
  | mod_ext_longident DOT UIDENT  { Ldot($1, $3) }
;
*/

mod_ext_longident: imod_ext_longident { $1 }

%inline imod_ext_longident:
  | UIDENT                        { Lident $1 }
  | mod_ext_longident DOT UIDENT  { Ldot($1, $3) }
  | mod_ext_apply                 { $1 }
;

mod_ext_apply:
  imod_ext_longident
  parenthesized(lseparated_nonempty_list(COMMA, mod_ext_longident))
  { if not !Clflags.applicative_functors then (
      let loc = mklocation $startpos $endpos in
      raise_error (Applicative_path loc) loc
    );
    List.fold_left (fun p1 p2 -> Lapply (p1, p2)) $1 $2
  }
;

mod_ext_lesslongident: imod_ext_lesslongident { $1 }

%inline imod_ext_lesslongident:
  | LESSUIDENT                        { Lident $1 }
  | mod_ext_lesslongident DOT UIDENT  { Ldot($1, $3) }
  | mod_ext_less_apply                 { $1 }
;

mod_ext_less_apply:
  imod_ext_lesslongident
  parenthesized(lseparated_nonempty_list(COMMA, mod_ext_longident))
  { if not !Clflags.applicative_functors then (
      let loc = mklocation $startpos $endpos in
      raise_error (Applicative_path loc) loc
    );
    List.fold_left (fun p1 p2 -> Lapply (p1, p2)) $1 $2
  }
;








mty_longident:
  | ident                        { Lident $1 }
  | mod_ext_longident DOT ident  { Ldot($1, $3) }
;

clty_longident:
  | LIDENT                       { Lident $1 }
  | mod_ext_longident DOT LIDENT { Ldot($1, $3) }
;

class_longident:
  | LIDENT                       { Lident $1 }
  | mod_longident DOT LIDENT     { Ldot($1, $3) }
;

(* Toplevel directives *)

toplevel_directive:
  SHARP as_loc(ident) embedded
          ( (* empty *)   { None }
          | STRING        { let (s, _, _) = $1 in Some(Pdir_string s) }
          | INT           { let (n, m) = $1 in Some(Pdir_int (n, m)) }
          | val_longident { Some(Pdir_ident $1) }
          | mod_longident { Some(Pdir_ident $1) }
          | FALSE         { Some(Pdir_bool false) }
          | TRUE          { Some(Pdir_bool true) }
          )
  {
    let pdir_arg = match $3 with
    | None -> None
    | Some pdira_desc -> Some {
      pdira_desc;
      pdira_loc = mklocation $startpos($3) $endpos($3);
    }
    in
    Ptop_dir
      { pdir_name = $2
      ; pdir_arg
      ; pdir_loc = $2.loc
      }
  }
;

(* Miscellaneous *)

opt_LET_MODULE: MODULE { () } | LET MODULE { () };

%inline name_tag: NAMETAG { $1 };

%inline label: LIDENT { $1 };

rec_flag:
  | (* empty *)   { Nonrecursive }
  | REC           { Recursive }
;

nonrec_flag:
  | (* empty *)   { Recursive }
  | NONREC        { Nonrecursive }
;

direction_flag:
  | TO            { Upto }
  | DOWNTO        { Downto }
;

%inline private_flag:
  | (* empty *)   { Public }
  | PRI           { Private }
;

mutable_flag:
  | (* empty *)   { Immutable }
  | MUTABLE       { Mutable }
;

virtual_flag:
  | (* empty *)   { Concrete }
  | VIRTUAL       { Virtual }
;

mutable_or_virtual_flags:
  | (* empty *)   { Immutable, Concrete }
  | VIRTUAL mutable_flag { $2, Virtual }
  | MUTABLE virtual_flag { Mutable, $2 }
;

override_flag:
  | (* empty *)   { Fresh }
  | BANG          { Override }
;

%inline subtractive:
  | MINUS         { "-"  }
  | MINUSDOT      { "-." }
;

%inline additive:
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
(* mod/land/lor/lxor/lsl/lsr/asr are not supported for now *)
;

attr_id:
  | as_loc(single_attr_id) { $1 }
  | single_attr_id DOT attr_id { mkloc ($1 ^ "." ^ $3.txt) (mklocation $symbolstartpos $endpos) }
;

attribute:
  | VERSION_ATTRIBUTE
    {
      (* Just ignore the attribute in the AST at this point, but record its version,
       * then we wil add it back at the "top" of the file. *)
      let major, minor = $1 in
      Reason_version.set_explicit (major, minor);
      let attr_payload = Reason_version.Ast_nodes.mk_version_attr_payload major minor in
      let loc = mklocation $symbolstartpos $endpos in
      { attr_name = {loc; txt="reason.version"};
        attr_payload;
        attr_loc = loc
      }
    }
  | LBRACKETAT attr_id payload RBRACKET
    {
      { attr_name = $2;
        attr_payload = $3;
        attr_loc = mklocation $symbolstartpos $endpos
      }
    }
  | DOCSTRING {
    (* Here is where we will make another copy of doc_attr but with
     * reason.doc/text instead of ocaml.doc/text and _that_ is the one that the
     * printer should pay attention to, completely ignoring the ocaml.doc/text
     * ones.  The ocaml.doc/text ones would only be received by odoc. *)
    doc_attr $1 (mklocation $symbolstartpos $endpos)
  }
;

(* Inlined to avoid having to deal with buggy $symbolstartpos *)
%inline located_attributes: as_loc(attribute)+ { $1 }

(* Inlined to avoid having to deal with buggy $symbolstartpos *)
%inline item_attributes:
  | { [] }
  | located_attributes { List.map (fun x -> x.txt) $1 }
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
  | structure                       { PStr $1 }
  | COLON signature                 { PSig $2 }
  | COLON core_type                 { PTyp $2 }
  | QUESTION pattern                { PPat ($2, None) }
  | QUESTION pattern WHEN expr      { PPat ($2, Some $4) }

  (* Allow parsing of [@test.call x => x]
   * By putting this rule here, a reduce/reduce conflict can be avoided
   * If we put the "simple_pattern_ident EQUALGREATER expr" inside the
   * unattributed_expr_template, the following ambiguity pops up:
   *  BAR pattern option(preceded(WHEN,expr)) EQUALGREATER expr
   *        WHEN expr
   *             simple_pattern_ident EQUALGREATER expr // lookahead token appears
   *             val_ident .
   *
   *  BAR pattern option(preceded(WHEN,expr)) EQUALGREATER expr // lookahead token appears
   *        WHEN expr // lookahead token is inherited
   *             simple_expr_call // lookahead token is inherited
   *             val_longident // lookahead token is inherited
   *             val_ident .
   *  Since where in a payload here, there's no ambiguity when putting
   *  the es6-style function (single arg, no parens) at this level.
   *)
  | simple_pattern_ident EQUALGREATER expr
    { let loc = mklocation $symbolstartpos $endpos in
      let expr = Exp.fun_ ~loc Nolabel None $1 $3 in
      PStr([mkstrexp expr []])
    }
;

optional:
  | { fun x -> Labelled x }
  | QUESTION { fun x -> Optional x }
;

%inline mark_position_mod(X): x = X
  { {x with pmod_loc = {x.pmod_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_cty(X): x = X
  { {x with pcty_loc = {x.pcty_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_ctf(X): x = X
  { {x with pctf_loc = {x.pctf_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_exp(X): x = X
  { {x with pexp_loc = {x.pexp_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_typ(X): x = X
  { {x with ptyp_loc = {x.ptyp_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_mty(X): x = X
  { {x with pmty_loc = {x.pmty_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_str(X): x = X
  { {x with pstr_loc = {x.pstr_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_cl(X): x = X
  { {x with pcl_loc = {x.pcl_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_cf(X): x = X
   { {x with pcf_loc = {x.pcf_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_pat(X): x = X
   { {x with ppat_loc = {x.ppat_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline as_loc(X): x = X
  { mkloc x (mklocation $symbolstartpos $endpos) }
;

either(X,Y):
  | X { $1 }
  | Y { $1 }
;

%inline opt_spread(X):
  | DOTDOTDOT? X
    { let dotdotdot = match $1 with
      | Some _ -> Some (mklocation $startpos($1) $endpos($2))
      | None -> None
      in
      (dotdotdot, $2)
    }
  ;

%inline lnonempty_list(X): X llist_aux(X) { $1 :: List.rev $2 };

%inline llist(X): llist_aux(X) { List.rev $1 };

llist_aux(X):
  | (* empty *) { [] }
  | llist_aux(X) X { $2 :: $1 }
;

%inline lseparated_list(sep, X):
  | (* empty *) { [] }
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

(*Less than followed by one or more X, then greater than *)
%inline lessthangreaterthanized(X): delimited(LESS, X, GREATER) { $1 };

(*Less than followed by one or more X, then greater than *)
%inline loptioninline(X): ioption(X) { match $1 with None -> [] | Some x -> x};

%inline nonempty_comma_list(X):
  lseparated_nonempty_list(COMMA, X) COMMA? {$1}
;

(* Allows defining type variable regions that allow certain *kinds* of type
 * variables depending on context *)
%inline type_param_group(X):
  | parenthesized(X)
  { $1 }
  (* No need to parse LESSIDENT here, because for
   * type_param_group, you'll never have an identifier in any of
   * the type parameters*)
  | lessthangreaterthanized(X)
  { $1 }
;

%inline optional_type_params(X):
  | loptioninline(type_param_group(nonempty_comma_list(X))) { $1 }

%%
