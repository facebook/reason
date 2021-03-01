open Reason_migrate_parsetree
open Ast_408
open Location
open Parsetree

(** Kinds of attributes *)
type attributesPartition = {
  arityAttrs : attributes;
  docAttrs : attributes;
  stdAttrs : attributes;
  jsxAttrs : attributes;
  stylisticAttrs : attributes;
  uncurried : bool
}

let is_stylistic_attr = function
  | { attr_name = {txt="reason.raw_literal"}; _}
  (* Consider warnings to be "stylistic" attributes - attributes that do not
   * affect printing *)
  | { attr_name = {txt="ocaml.ppwarn"}; _}
  | { attr_name = {txt="reason.preserve_braces"}; _} -> true
  | { attr_name = {txt="reason.template"}; _} -> true
  | _ -> false


(** Partition attributes into kinds *)
let rec partitionAttributes ?(partDoc=false) ?(allowUncurry=true) attrs : attributesPartition =
  match attrs with
  | [] ->
    {arityAttrs=[]; docAttrs=[]; stdAttrs=[]; jsxAttrs=[]; stylisticAttrs=[]; uncurried = false}
  | ({ attr_name = {txt = "bs"}; attr_payload = PStr []; _ } as attr)::atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    if allowUncurry then
      {partition with uncurried = true}
    else {partition with stdAttrs=attr::partition.stdAttrs}
  | ({ attr_name = {txt="JSX"}; _ } as jsx)::atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with jsxAttrs=jsx::partition.jsxAttrs}
  | ({ attr_name = {txt="explicit_arity"}; _} as arity_attr)::atTl
  | ({ attr_name = {txt="implicit_arity"}; _} as arity_attr)::atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with arityAttrs=arity_attr::partition.arityAttrs}
  | ({ attr_name = {txt="ocaml.text"}; _} as doc)::atTl when partDoc = true ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with docAttrs=doc::partition.docAttrs}
  | ({ attr_name = {txt="ocaml.doc" | "ocaml.text"}; _} as doc)::atTl when partDoc = true ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with docAttrs=doc::partition.docAttrs}
  | attr :: atTl when is_stylistic_attr attr ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with stylisticAttrs=attr::partition.stylisticAttrs}
  | atHd :: atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with stdAttrs=atHd::partition.stdAttrs}

let extractStdAttrs attrs =
  (partitionAttributes attrs).stdAttrs

let extract_raw_literal attrs =
  let rec loop acc = function
    | { attr_name = {txt="reason.raw_literal"};
        attr_payload =
          PStr [{pstr_desc = Pstr_eval({pexp_desc = Pexp_constant(Pconst_string(text, None))}, _)}]}
      :: rest ->
      (Some text, List.rev_append acc rest)
    | [] -> (None, List.rev acc)
    | attr :: rest -> loop (attr :: acc) rest
  in
  loop [] attrs

let without_stylistic_attrs attrs =
  let rec loop acc = function
    | attr :: rest when is_stylistic_attr attr -> loop acc rest
    | [] -> List.rev acc
    | attr :: rest -> loop (attr :: acc) rest
  in
  loop [] attrs

let is_jsx_attribute { attr_name = {txt}; _} = txt = "JSX"

(* TODO: Make this fast and not filter *)
let has_jsx_attributes attrs = List.exists is_jsx_attribute attrs

let is_preserve_braces_attr { attr_name = {txt}; _} =
  txt = "reason.preserve_braces"

let has_preserve_braces_attrs stylisticAttrs =
  List.exists is_preserve_braces_attr stylisticAttrs

let maybe_remove_stylistic_attrs attrs should_preserve =
  if should_preserve then
    attrs
  else
    List.filter (function
      | { attr_name = {txt="reason.raw_literal"}; _} -> true
      | _ -> false)
      attrs
