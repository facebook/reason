(*
 *  Copyright (c) 2015-present, Facebook, Inc.
 *  All rights reserved.
 *
 *  This source code is licensed under the BSD-style license found in the
 *  LICENSE file in the root directory of this source tree. An additional grant
 *  of patent rights can be found in the PATENTS file in the same directory.
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

(* Hello! Welcome to the Reason "outcome printer" logic. This logic takes the
  AST nodes and turn them into text, for Merlin, rtop and terminal errors
  reporting to be in Reason syntax.

  If you've navigated around in the Reason codebase, you might have seen the
  other printer called reason_pprint_ast, our actual, main pretty-printer. Why
  is this one separated from reason_pprint_ast? Because the outcome printer's
  use-case is a bit different and needs different entry points blablabla...
  These are mostly excuses. But for example, currently, `Js.t {. foo: bar}` by
  itself is *invalid syntax* for a pretty printer (the correct, minimal valid
  code would be `type myObject = Js.t {. foo: bar}`), but the terminal error
  report do want to provide just that snippet and have you print it. Hopefully
  OCaml can unify actual code pretty-printing and terminal type info pretty-
  printing one day.

  This also means the outcome printer doesn't use the normal Parsetree,
  Ast_helper and others you might have seen in other files. It has its own
  small AST definition here:
  https://github.com/ocaml/ocaml/blob/4.04/typing/outcometree.mli

  The rest of this file's logic is just pattern-matching on these tree node
  variants & using Format to pretty-print them nicely.
  *)

(*
  This file's shared between the Reason repo and the BuckleScript repo. In
  Reason, it's in src/reason_oprint.ml. In BuckleScript, it's in
  jscomp/outcome_printer/tweaked_reason_oprint.ml. We periodically copy
  this file from Reason (the source of truth) to BuckleScript, then uncomment
  the #if #else #end cppo macros you see in the file. That's because
  BuckleScript's on OCaml 4.02 while Reason's on 4.04; so the #if macros
  surround the pieces of code that are different between the two compilers.

  When you modify this file, please make sure you're not dragging in too many
  things. You don't necessarily have to test the file on both Reason and
  BuckleScript; ping @chenglou and a few others and we'll keep them synced up by
  patching the right parts, through the power of types(tm)
*)

(* #if defined BS_NO_COMPILER_PATCH then *)
open Ast_404
(* #end *)

open Format
open Outcometree

exception Ellipsis

let cautious f ppf arg =
  try f ppf arg with
    Ellipsis -> fprintf ppf "..."

(* #if defined BS_NO_COMPILER_PATCH then *)
let rec print_ident ppf =
  function
    Oide_ident s -> pp_print_string ppf s
  | Oide_dot (id, s) ->
      print_ident ppf id; pp_print_char ppf '.'; pp_print_string ppf s
  | Oide_apply (id1, id2) ->
      fprintf ppf "%a(%a)" print_ident id1 print_ident id2
(* #else *)
(* let rec print_ident ppf = *)
  (* function *)
    (* Oide_ident s -> !Oprint.out_ident ppf s *)
  (* | Oide_dot (id, s) -> *)
      (* print_ident ppf id; pp_print_char ppf '.'; !Oprint.out_ident ppf s *)
  (* | Oide_apply (id1, id2) -> *)
      (* fprintf ppf "%a(%a)" print_ident id1 print_ident id2 *)
(* #end *)

let parenthesized_ident name =
  (List.mem name ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"])
  ||
  (match name.[0] with
      'a'..'z' | 'A'..'Z' | '\223'..'\246' | '\248'..'\255' | '_' ->
        false
    | _ -> true)

let value_ident ppf name =
  if parenthesized_ident name then
    fprintf ppf "( %s )" name
  else
    pp_print_string ppf name

(* Values *)

let valid_float_lexeme s =
  let l = String.length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | '0' .. '9' | '-' -> loop (i+1)
    | _ -> s
  in loop 0

let float_repres f =
  match classify_float f with
    FP_nan -> "nan"
  | FP_infinite ->
      if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
      let float_val =
        let s1 = Printf.sprintf "%.12g" f in
        if f = float_of_string s1 then s1 else
        let s2 = Printf.sprintf "%.15g" f in
        if f = float_of_string s2 then s2 else
        Printf.sprintf "%.18g" f
      in valid_float_lexeme float_val

let parenthesize_if_neg ppf fmt v isneg =
  if isneg then pp_print_char ppf '(';
  fprintf ppf fmt v;
  if isneg then pp_print_char ppf ')'



let print_out_value ppf tree =
  let rec print_tree_1 ppf =
    function
    (* for the next few cases, please see context at https://github.com/facebook/reason/pull/1516#issuecomment-337069150 *)
    | Oval_constr (name, [Oval_constr ((Oide_ident "()"), [])]) ->
      (* for normal variants, but sugar Foo(()) to Foo() *)
       fprintf ppf "@[<1>%a()@]" print_ident name
    | Oval_constr (name, [param]) ->
      (* for normal variants *)
       fprintf ppf "@[<1>%a(%a)@]" print_ident name print_constr_param param
    | Oval_constr (name, (_ :: _ as params)) ->
        fprintf ppf "@[<1>%a(%a)@]" print_ident name
            (print_tree_list print_tree_1 ",") params
    | Oval_variant (name, Some (Oval_constr ((Oide_ident "()"), []))) ->
      (* for polymorphic variants, but sugar `foo(()) to `foo() *)
      fprintf ppf "@[<2>`%s()@]" name
    | Oval_variant (name, Some param) ->
      (* for polymorphic variants *)
      fprintf ppf "@[<2>`%s(%a)@]" name print_constr_param param
    | tree -> print_simple_tree ppf tree
  and print_constr_param ppf = function
    | Oval_int i -> parenthesize_if_neg ppf "%i" i (i < 0)
    | Oval_int32 i -> parenthesize_if_neg ppf "%lil" i (i < 0l)
    | Oval_int64 i -> parenthesize_if_neg ppf "%LiL" i (i < 0L)
    | Oval_nativeint i -> parenthesize_if_neg ppf "%nin" i (i < 0n)
    | Oval_float f -> parenthesize_if_neg ppf "%s" (float_repres f) (f < 0.0)
    | tree -> print_simple_tree ppf tree
  and print_simple_tree ppf =
    function
      Oval_int i -> fprintf ppf "%i" i
    | Oval_int32 i -> fprintf ppf "%lil" i
    | Oval_int64 i -> fprintf ppf "%LiL" i
    | Oval_nativeint i -> fprintf ppf "%nin" i
    | Oval_float f -> pp_print_string ppf (float_repres f)
    | Oval_char c -> fprintf ppf "%C" c
    | Oval_string s ->
        begin try [@ocaml.warning "-52"] fprintf ppf "%S" s with
          Invalid_argument  "String.create" -> fprintf ppf "<huge string>"
        end
    | Oval_list tl ->
        fprintf ppf "@[<1>[%a]@]" (print_tree_list print_tree_1 ",") tl
    | Oval_array tl ->
        fprintf ppf "@[<2>[|%a|]@]" (print_tree_list print_tree_1 ",") tl
    | Oval_constr (name, []) -> print_ident ppf name
    | Oval_variant (name, None) -> fprintf ppf "`%s" name
    | Oval_stuff s -> pp_print_string ppf s
    | Oval_record fel ->
        fprintf ppf "@[<1>{%a}@]" (cautious (print_fields true)) fel
    | Oval_ellipsis -> raise Ellipsis
    | Oval_printer f -> f ppf
    | Oval_tuple tree_list ->
        fprintf ppf "@[<1>(%a)@]" (print_tree_list print_tree_1 ",") tree_list
    | tree -> fprintf ppf "@[<1>(%a)@]" (cautious print_tree_1) tree
  and print_fields first ppf =
    function
      [] -> ()
    | (name, tree) :: fields ->
        if not first then fprintf ppf ",@ ";
        fprintf ppf "@[<1>%a:@ %a@]" print_ident name (cautious print_tree_1)
          tree;
        print_fields false ppf fields
  and print_tree_list print_item sep ppf tree_list =
    let rec print_list first ppf =
      function
        [] -> ()
      | tree :: tree_list ->
          if not first then fprintf ppf "%s@ " sep;
          print_item ppf tree;
          print_list false ppf tree_list
    in
    cautious (print_list true) ppf tree_list
  in
  cautious print_tree_1 ppf tree

(* Types *)

let rec print_list_init pr sep ppf =
  function
    [] -> ()
  | a :: l -> sep ppf; pr ppf a; print_list_init pr sep ppf l

let rec print_list pr sep ppf =
  function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l

let pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")

let pr_vars =
  print_list (fun ppf s -> fprintf ppf "'%s" s) (fun ppf -> fprintf ppf "@ ")

type label =
  | Nonlabeled
  | Labeled of string
  | Optional of string

let get_label lbl =
  if lbl = "" then Nonlabeled
  else if String.get lbl 0 = '?' then
    Optional (String.sub lbl 1 @@ String.length lbl - 1)
  else Labeled lbl

let rec print_out_type ppf =
  function
  | Otyp_alias (ty, s) ->
      fprintf ppf "@[%a@ as '%s@]" print_out_type ty s
  | Otyp_poly (sl, ty) ->
      fprintf ppf "@[<hov 2>%a.@ %a@]"
        pr_vars sl
        print_out_type ty
  | ty ->
      print_out_type_1 ppf ty

and print_arg ppf (lab, typ) =
  let suffix =
    match get_label lab with
    | Nonlabeled -> ""
    | Labeled lab ->
        pp_print_string ppf "~";
        pp_print_string ppf lab;
        pp_print_string ppf ": ";
        ""
    | Optional lab ->
        pp_print_string ppf "~";
        pp_print_string ppf lab;
        pp_print_string ppf ": ";
        "=?"
  in
  print_out_type_2 ppf typ;
  pp_print_string ppf suffix;

and print_out_type_1 ppf =
  function
    Otyp_arrow (lab, ty1, ty2) ->
      let rec collect_args args typ = match typ with
        | Otyp_arrow (lab, ty1, ty2) -> collect_args (args @ [(lab, ty1)]) ty2
        | _ -> (args, typ)
      in
      pp_open_box ppf 0;
      pp_print_string ppf "(";
      let (args, result) = collect_args [(lab, ty1)] ty2 in
      print_list print_arg (fun ppf -> fprintf ppf ",@ ") ppf args;
      pp_print_string ppf ")";
      pp_print_string ppf " =>";
      pp_print_space ppf ();
      print_out_type_1 ppf result;
      pp_close_box ppf ()
  | ty -> print_out_type_2 ppf ty
and print_out_type_2 ppf =
  function
    Otyp_tuple tyl ->
      fprintf ppf "@[<0>(%a)@]" (print_typlist print_simple_out_type ",") tyl
  | ty -> print_simple_out_type ppf ty
and print_simple_out_type ppf =
  function
    Otyp_class (ng, id, tyl) ->
      fprintf ppf "@[%s#%a%a@]" (if ng then "_" else "")
        print_ident id print_typargs tyl

  (* BuckleScript-specific external. See the manual for the usage of [@bs]. This
    [@bs] is processed into a type that looks like `Js.Internal.fn ...`. This
    leaks during error reporting, where the type is printed. Here, we print it
    back from `Js.Internal.fn [ `Arity_2 ('c, 'd) ] 'e` into `('a => 'b => int) [@bs]` *)
  (* same for `Js_internal.fn ...`. Either might shown *)
  | Otyp_constr (
      (Oide_dot (
        (Oide_dot ((Oide_ident "Js"), "Internal") | Oide_ident "Js_internal"),
        ("fn" | "meth" as name)
      ) as id),
      ([Otyp_variant(_, Ovar_fields [variant, _, tys], _, _); result] as tyl)
    ) ->
      (* Otyp_arrow *)
      let make tys result =
        if tys = [] then
          Otyp_arrow ("", Otyp_constr (Oide_ident "unit", []),result)
        else
          match tys with
          | [ Otyp_tuple tys as single] ->
            if variant = "Arity_1" then
              Otyp_arrow ("", single, result)
            else
              List.fold_right (fun x acc -> Otyp_arrow ("", x, acc)) tys result
          | [single] ->
            Otyp_arrow ("", single, result)
          | _ ->
            raise_notrace Not_found
      in
      begin match (make tys result) with
      | exception _ ->
        begin
          pp_open_box ppf 0;
          print_typargs ppf tyl;
          print_ident ppf id;
          pp_close_box ppf ()
        end
      | res ->
        begin match name  with
        | "fn" -> fprintf ppf "@[<0>(%a)@ [@bs]@]" print_out_type_1 res
        | "meth" -> fprintf ppf "@[<0>(%a)@ [@bs.meth]@]" print_out_type_1 res
        | _ -> assert false
        end
      end
  (* also BuckleScript-specific. See the comment in the previous pattern *)
  | Otyp_constr (
      (Oide_dot (
        (Oide_dot ((Oide_ident "Js"), "Internal") | Oide_ident "Js_internal"), "meth_callback" ) as id
      ),
      ([Otyp_variant(_, Ovar_fields [variant, _, tys], _,_); result] as tyl)
    ) ->
      let make tys result =
        match tys with
        | [Otyp_tuple tys as single] ->
          if variant = "Arity_1" then
            Otyp_arrow ("", single, result)
          else
            List.fold_right (fun x acc  -> Otyp_arrow("", x, acc) ) tys result
        | [single] ->
          Otyp_arrow ("", single, result)
        | _ ->
          raise_notrace Not_found
      in
      begin match (make tys result) with
      | exception _ ->
        begin
          pp_open_box ppf 0;
          print_typargs ppf tyl;
          print_ident ppf id;
          pp_close_box ppf ()
        end
      | res ->
        fprintf ppf "@[<0>(%a)@ [@bs.this]@]" print_out_type_1 res
      end

  | Otyp_constr (id, tyl) ->
      pp_open_box ppf 0;
      print_ident ppf id;
      begin match tyl with
      | [] -> ()
      | _ ->
        print_typargs ppf tyl;
      end;
      pp_close_box ppf ()
  | Otyp_object (fields, rest) ->
    let dot = match rest with
      Some non_gen -> (if non_gen then "_" else "") ^ ".."
    | None -> "."
    in
    fprintf ppf "@[<2>{%s %a }@]" dot print_object_fields fields
  | Otyp_stuff s -> pp_print_string ppf s
  | Otyp_var (ng, s) -> fprintf ppf "'%s%s" (if ng then "_" else "") s
  | Otyp_variant (non_gen, row_fields, closed, tags) ->
      let print_present ppf =
        function
          None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
      in
      let print_fields ppf =
        function
          Ovar_fields fields ->
            print_list print_row_field (fun ppf -> fprintf ppf "@;<1 -2>| ")
              ppf fields
        | Ovar_name (id, tyl) ->
            fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
      in
      fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a ]@]" (if non_gen then "_" else "")
        (if closed then if tags = None then " " else "< "
         else if tags = None then "> " else "? ")
        print_fields row_fields
        print_present tags
  | Otyp_alias _ | Otyp_poly _ as ty ->
      fprintf ppf "@[<1>(%a)@]" print_out_type ty;
  | Otyp_tuple _ | Otyp_arrow _ as ty ->
    (* no parentheses needed; the callsites already wrap these *)
      fprintf ppf "@[<1>%a@]" print_out_type ty;
  | Otyp_abstract | Otyp_open
  | Otyp_sum _ | Otyp_record _ | Otyp_manifest (_, _) -> ()
  | Otyp_module (p, n, tyl) ->
      fprintf ppf "@[<1>(module %s" p;
      let first = ref true in
      List.iter2
        (fun s t ->
          let sep = if !first then (first := false; "with") else "and" in
          fprintf ppf " %s type %s = %a" sep s print_out_type t
        )
        n tyl;
      fprintf ppf ")@]"
(* #if defined BS_NO_COMPILER_PATCH then *)
  | Otyp_attribute (t, attr) ->
        fprintf ppf "@[<1>(%a [@@%s])@]" print_out_type t attr.oattr_name
(* #end *)

and print_object_fields ppf =
  function
    [] -> ()
  | [s, t] ->
    fprintf ppf "%s : %a" s print_out_type t;
    print_object_fields ppf []
  | (s, t) :: l ->
    fprintf ppf "%s : %a,@ %a" s print_out_type t print_object_fields l
and print_row_field ppf (l, opt_amp, tyl) =
  let pr_of ppf =
    if opt_amp then fprintf ppf " &@ "
    else if tyl <> [] then fprintf ppf " "
    else fprintf ppf ""
  in
  fprintf ppf "@[<hv 2>`%s%t%a@]" l pr_of (print_typlist print_out_type " &")
    tyl
and print_typlist print_elem sep ppf =
  function
    [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
      print_elem ppf ty;
      pp_print_string ppf sep;
      pp_print_space ppf ();
      print_typlist print_elem sep ppf tyl
and print_out_wrap_type ppf =
  function
  | (Otyp_constr (_, _::_)) as ty ->
      print_out_type ppf ty
  | ty -> print_simple_out_type ppf ty
and print_typargs ppf =
  function
    [] -> ()
  | [ty1] ->
      pp_print_string ppf "(";
      print_out_wrap_type ppf ty1;
      pp_print_string ppf ")"
  | tyl ->
      pp_print_space ppf ();
      pp_print_string ppf "(";
      pp_open_box ppf 1;
      print_typlist print_out_wrap_type ", " ppf tyl;
      pp_close_box ppf ();
      pp_print_string ppf ")"

let out_type = ref print_out_type

(* Class types *)

let type_parameter ppf (ty, (co, cn)) =
  fprintf ppf "%s%s"
    (if not cn then "+" else if not co then "-" else "")
    (if ty = "_" then ty else "'"^ty)

let print_out_class_params ppf =
  function
    [] -> ()
  | tyl ->
      fprintf ppf "(@[<1>%a@])@ "
        (print_list type_parameter (fun ppf -> fprintf ppf ",@ "))
        tyl

let rec print_out_class_type ppf =
  function
    Octy_constr (id, tyl) ->
      let pr_tyl ppf =
        function
          [] -> ()
        | tyl ->
            fprintf ppf "@[<1> %a@]" (print_typlist print_out_wrap_type "") tyl
      in
      fprintf ppf "@[%a%a@]" print_ident id pr_tyl tyl
  | Octy_arrow (lab, argument_type, return_class_type) ->
      (* class arrow types need to be printed differently. For one, you can't do:

        class a: a => b

        because due to existing parsing issues, the `a` neds to be wrapped in parens (unlike normal arrow types).
        We can change this logic once this is no longer true
       *)
      let rec print_class_type_arguments_that_might_be_arrow ppf = function
        | Otyp_arrow ("", typ1, typ2) ->
          fprintf ppf "@[%a,@ %a@]"
            print_out_type typ1
            print_class_type_arguments_that_might_be_arrow typ2
        | Otyp_arrow (_actual_label, typ1, typ2) ->
          fprintf ppf "@[~%s: %a,@ %a@]"
            lab
            print_out_type typ1
            print_class_type_arguments_that_might_be_arrow typ2
        | argument_not_arrow -> fprintf ppf "%a" print_out_type argument_not_arrow
      in
      fprintf ppf "@[(%a) =>@ %a@]"
        print_class_type_arguments_that_might_be_arrow argument_type
        print_out_class_type return_class_type
  | Octy_signature (self_ty, csil) ->
      let pr_param ppf =
        function
          Some ty -> fprintf ppf "@ @[(%a)@]" print_out_type ty
        | None -> ()
      in
      fprintf ppf "@[<hv 2>@[<2>{%a@]@ %a@;<1 -2>}@]" pr_param self_ty
        (print_list print_out_class_sig_item (fun ppf -> fprintf ppf ";@ "))
        csil
and print_out_class_sig_item ppf =
  function
    Ocsg_constraint (ty1, ty2) ->
      fprintf ppf "@[<2>as %a =@ %a@]" print_out_type ty1
        print_out_type ty2
  | Ocsg_method (name, priv, virt, ty) ->
      fprintf ppf "@[<2>%s%s%s:@ %a@]"
        (if priv then "pri " else "pub ") (if virt then "virtual " else "")
        name print_out_type ty
  | Ocsg_value (name, mut, vr, ty) ->
      fprintf ppf "@[<2>val %s%s%s:@ %a@]"
        (if mut then "mutable " else "")
        (if vr then "virtual " else "")
        name print_out_type ty

(* Signature *)

let is_rec_next = function
  | Osig_class (_, _, _, _, Orec_next)::_
  | Osig_class_type (_, _, _, _, Orec_next)::_
  | Osig_module (_, _, Orec_next)::_
  | Osig_type (_, Orec_next)::_ -> true
  | _ -> false

let rec print_out_functor ppf =
  function
    Omty_functor (_, None, mty_res) ->
      fprintf ppf "() %a" print_out_functor mty_res
  | Omty_functor (name , Some mty_arg, mty_res) ->
      fprintf ppf "(%s : %a) => %a" name
        print_out_module_type mty_arg print_out_functor mty_res
  | m -> fprintf ppf "%a" print_out_module_type m
and print_out_module_type ppf =
  function
    Omty_abstract -> ()
  | Omty_functor _ as t ->
      fprintf ppf "@[<2>%a@]" print_out_functor t
  | Omty_ident id -> fprintf ppf "%a" print_ident id
  | Omty_signature sg ->
      fprintf ppf "@[<hv 2>{@ %a@;<1 -2>}@]" print_out_signature sg
  | Omty_alias id -> fprintf ppf "(module %a)" print_ident id
and print_out_signature ppf =
  function
    [] -> ()
  | [item] ->
      fprintf ppf "%a;" print_out_sig_item item
  | Osig_typext(ext, Oext_first) :: items ->
      (* Gather together the extension constructors *)
      let rec gather_extensions acc items =
        match items with
            Osig_typext(ext, Oext_next) :: items ->
              gather_extensions
                ((ext.oext_name, ext.oext_args, ext.oext_ret_type) :: acc)
                items
          | _ -> (List.rev acc, items)
      in
      let exts, items =
        gather_extensions
          [(ext.oext_name, ext.oext_args, ext.oext_ret_type)]
          items
      in
      let te =
        { otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private }
      in
      let sep = if is_rec_next items then "" else ";" in
      fprintf ppf "%a%s@ %a" print_out_type_extension te sep print_out_signature items
  | item :: items ->
      let sep = if is_rec_next items then "" else ";" in
      fprintf ppf "%a%s@ %a" print_out_sig_item item sep print_out_signature items
and print_out_sig_item ppf =
  function
    Osig_class (vir_flag, name, params, clt, rs) ->
      fprintf ppf "@[<2>%s%s@ %s %a@,:@ %a@]"
        (if rs = Orec_next then "and" else "class")
        (if vir_flag then " virtual" else "") name print_out_class_params params
        print_out_class_type clt
  | Osig_class_type (vir_flag, name, params, clt, rs) ->
      fprintf ppf "@[<2>%s%s@ %s %a@,=@ %a@]"
        (if rs = Orec_next then "and" else "class type")
        (if vir_flag then " virtual" else "") name print_out_class_params params
        print_out_class_type clt
  | Osig_typext (ext, Oext_exception) ->
      fprintf ppf "@[<2>exception %a@]"
        print_out_constr (ext.oext_name, ext.oext_args, ext.oext_ret_type)
  | Osig_typext (ext, _) ->
      print_out_extension_constructor ppf ext
  | Osig_modtype (name, Omty_abstract) ->
      fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype (name, mty) ->
      fprintf ppf "@[<2>module type %s =@ %a@]" name print_out_module_type mty
  | Osig_module (name, Omty_alias id, _) ->
      fprintf ppf "@[<2>module %s =@ %a@]" name print_ident id
  | Osig_module (name, mty, rs) ->
      fprintf ppf "@[<2>%s %s:@ %a@]"
        (match rs with Orec_not -> "module"
                    | Orec_first -> "module rec"
                    | Orec_next -> "and")
        name print_out_module_type mty
  | Osig_type(td, rs) ->
      print_out_type_decl
        (match rs with
          | Orec_not   -> "type nonrec"
          | Orec_first -> "type"
          | Orec_next  -> "and")
        ppf td
(* #if defined BS_NO_COMPILER_PATCH then *)
  | Osig_value {oval_name; oval_type; oval_prims; oval_attributes} ->
    let kwd = if oval_prims = [] then "let" else "external" in
    let pr_prims ppf =
      function
        [] -> ()
      | s :: sl ->
          fprintf ppf "@ = \"%s\"" s;
          List.iter (fun s -> fprintf ppf "@ \"%s\"" s) sl
    in
    fprintf ppf "@[<2>%s %a:@ %a%a%a@]" kwd value_ident oval_name
        !out_type oval_type pr_prims oval_prims
        (fun ppf -> List.iter (fun a -> fprintf ppf "@ [@@@@%s]" a.oattr_name))
        oval_attributes
  | Osig_ellipsis ->
    fprintf ppf "..."
(* #else *)
  (* | Osig_value(oval_name, oval_type, oval_prims) -> *)
    (* let kwd = if oval_prims = [] then "let" else "external" in *)
    (* let pr_prims ppf = *)
      (* function *)
        (* [] -> () *)
      (* | s :: sl -> *)
          (* fprintf ppf "@ = \"%s\"" s; *)
          (* List.iter (fun s ->  *)
    (* TODO: in general, we should print bs attributes, some attributes like
      bs.splice does need it *)
    (* let len = String.length s in *)
    (* if len >= 3 && s.[0] = 'B' && s.[1] = 'S' && s.[2] = ':' then *)
      (* fprintf ppf "@ \"BuckleScript External\""  *)
    (* else *)
      (* fprintf ppf "@ \"%s\"" s *)
    (* ) sl *)
    (* in *)
    (* fprintf ppf "@[<2>%s %a:@ %a%a@]" kwd value_ident oval_name *)
        (* !out_type oval_type pr_prims oval_prims *)
(* #end *)

and print_out_type_decl kwd ppf td =
  let print_constraints ppf =
    List.iter
      (fun (ty1, ty2) ->
         fprintf ppf "@ @[<2>constraint %a =@ %a@]" print_out_type ty1
           print_out_type ty2)
      td.otype_cstrs
  in
  let type_defined ppf =
    match td.otype_params with
      [] -> pp_print_string ppf td.otype_name
    | [param] -> fprintf ppf "@[%s(%a)@]" td.otype_name type_parameter param
    | _ ->
        fprintf ppf "@[%s(@[%a@])@]"
          td.otype_name
          (print_list type_parameter (fun ppf -> fprintf ppf ",@ "))
          td.otype_params
  in
  let print_manifest ppf =
    function
      Otyp_manifest (ty, _) -> fprintf ppf " =@ %a" print_out_type ty
    | _ -> ()
  in
  let print_name_params ppf =
    fprintf ppf "%s %t%a" kwd type_defined print_manifest td.otype_type
  in
  let ty =
    match td.otype_type with
      Otyp_manifest (_, ty) -> ty
    | _ -> td.otype_type
  in
  let print_private ppf = function
    Asttypes.Private -> fprintf ppf " pri"
  | Asttypes.Public -> ()
  in
  let print_out_tkind ppf = function
  | Otyp_abstract -> ()
  | Otyp_record lbls ->
      fprintf ppf " =%a {%a@;<1 -2>}"
        print_private td.otype_private
        (print_list_init print_out_label (fun ppf -> fprintf ppf "@ ")) lbls
  | Otyp_sum constrs ->
      fprintf ppf " =%a@;<1 2>%a"
        print_private td.otype_private
        (print_list print_out_constr (fun ppf -> fprintf ppf "@ | ")) constrs
  | Otyp_open ->
      fprintf ppf " = .."
  | ty ->
      fprintf ppf " =%a@;<1 2>%a"
        print_private td.otype_private
        print_out_type ty
  in
  fprintf ppf "@[<2>@[<hv 2>%t%a@]%t@]"
    print_name_params
    print_out_tkind ty
    print_constraints

and print_out_constr ppf (name, tyl,ret_type_opt) =
  match ret_type_opt with
  | None ->
      begin match tyl with
      | [] ->
          pp_print_string ppf name
      | _ ->
          fprintf ppf "@[<2>%s(%a)@]" name
            (print_typlist print_simple_out_type ",") tyl
      end
  | Some ret_type ->
      begin match tyl with
      | [] ->
          fprintf ppf "@[<2>%s:@ %a@]" name print_simple_out_type ret_type
      | _ ->
          fprintf ppf "@[<2>%s(%a) :%a@]" name
            (print_typlist print_simple_out_type ",") tyl
            print_simple_out_type ret_type
      end


and print_out_label ppf (name, mut, arg) =
  fprintf ppf "@[<2>%s%s:@ %a@]," (if mut then "mutable " else "") name
    print_out_type arg

and print_out_extension_constructor ppf ext =
  let print_extended_type ppf =
    let print_type_parameter ppf ty =
      fprintf ppf "%s"
        (if ty = "_" then ty else "'"^ty)
    in
      match ext.oext_type_params with
        [] -> fprintf ppf "%s" ext.oext_type_name
      | [ty_param] ->
        fprintf ppf "@[%a@ %s@]"
          print_type_parameter
          ty_param
          ext.oext_type_name
      | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list print_type_parameter (fun ppf -> fprintf ppf ",@ "))
          ext.oext_type_params
          ext.oext_type_name
  in
  fprintf ppf "@[<hv 2>type %t +=%s@;<1 2>%a@]"
    print_extended_type
    (if ext.oext_private = Asttypes.Private then " pri" else "")
    print_out_constr (ext.oext_name, ext.oext_args, ext.oext_ret_type)

and print_out_type_extension ppf te =
  let print_extended_type ppf =
    let print_type_parameter ppf ty =
      fprintf ppf "%s"
        (if ty = "_" then ty else "'"^ty)
    in
    match te.otyext_params with
      [] -> fprintf ppf "%s" te.otyext_name
    | [param] ->
      fprintf ppf "@[%a@ %s@]"
        print_type_parameter param
        te.otyext_name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list print_type_parameter (fun ppf -> fprintf ppf ",@ "))
          te.otyext_params
          te.otyext_name
  in
  fprintf ppf "@[<hv 2>type %t +=%s@;<1 2>%a@]"
    print_extended_type
    (if te.otyext_private = Asttypes.Private then " pri" else "")
    (print_list print_out_constr (fun ppf -> fprintf ppf "@ | "))
    te.otyext_constructors

(* Phrases *)

let print_out_exception ppf exn outv =
  match exn with
    Sys.Break -> fprintf ppf "Interrupted.@."
  | Out_of_memory -> fprintf ppf "Out of memory during evaluation.@."
  | Stack_overflow ->
      fprintf ppf "Stack overflow during evaluation (looping recursion?).@."
  | _ -> fprintf ppf "@[Exception:@ %a.@]@." print_out_value outv

let rec print_items ppf =
  function
    [] -> ()
  | (Osig_typext(ext, Oext_first), None) :: items ->
      (* Gather together extension constructors *)
      let rec gather_extensions acc items =
        match items with
            (Osig_typext(ext, Oext_next), None) :: items ->
              gather_extensions
                ((ext.oext_name, ext.oext_args, ext.oext_ret_type) :: acc)
                items
          | _ -> (List.rev acc, items)
      in
      let exts, items =
        gather_extensions
          [(ext.oext_name, ext.oext_args, ext.oext_ret_type)]
          items
      in
      let te =
        { otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private }
      in
        fprintf ppf "@[%a@]" print_out_type_extension te;
        if items <> [] then fprintf ppf "@ %a" print_items items
  | (tree, valopt) :: items ->
      begin match valopt with
        Some v ->
          fprintf ppf "@[<2>%a =@ %a;@]" print_out_sig_item tree
            print_out_value v
      | None -> fprintf ppf "@[%a;@]" print_out_sig_item tree
      end;
      if items <> [] then fprintf ppf "@ %a" print_items items

let print_out_phrase ppf =
  function
    Ophr_eval (outv, ty) ->
      fprintf ppf "@[- : %a@ =@ %a@]@." print_out_type ty print_out_value outv
  | Ophr_signature [] -> ()
  | Ophr_signature items -> fprintf ppf "@[<v>%a@]@." print_items items
  | Ophr_exception (exn, outv) -> print_out_exception ppf exn outv
