open Migrate_parsetree
open Ast_404

open Parsetree
open Asttypes
open Ast_mapper
open Ast_helper

let fail loc txt = raise (Location.Error (Location.error ~loc txt))

let rec process_arguments func loc =
  match func with
  | Pexp_fun (arg_label, eo, {ppat_desc = Ppat_constraint (_, ct); ppat_loc}, exp) ->
    Typ.arrow arg_label ct (process_arguments exp.pexp_desc exp.pexp_loc)
  (* An unlabeled () unit argument *)
  | Pexp_fun (arg_label, eo, {ppat_desc = Ppat_construct (({txt = Longident.Lident "()"; _}), None)}, exp) ->
    let unit_type = (Typ.constr (Location.mkloc (Longident.Lident "unit") (Location.symbol_gloc ())) []) in
    Typ.arrow arg_label unit_type (process_arguments exp.pexp_desc exp.pexp_loc)
  | Pexp_fun (arg_label, eo, {ppat_loc}, exp) ->
    fail ppat_loc "All arguments must have type annotations"
  | Pexp_constraint (_, ct) ->
    let (const_type, c) =
      match ct.ptyp_desc with
      | Ptyp_constr (loc, ct) -> (loc.txt, ct)
      | _ -> fail ct.ptyp_loc "Function return value must be annotated"
    in
    (Typ.constr (Location.mkloc const_type (Location.symbol_gloc ())) c)
  | _ -> 
    fail loc "Function return value must be annotated"

let loc_of_constant const =
  let const_type =
    match const with
    | Pconst_integer _ -> "int"
    | Pconst_char _ -> "char"
    | Pconst_string _ -> "string"
    | Pconst_float _ -> "float"
  in
  (Location.mkloc (Longident.Lident const_type) (Location.symbol_gloc ()))

let type_of_constant const = (Typ.constr (loc_of_constant const) [])

let is_my_attribute ({txt}, _) = txt = "export"

let filter_attrs = List.filter (fun a -> not (is_my_attribute a))

let disallow_my_attributes = List.filter (fun (loc, payload) ->
  if (is_my_attribute (loc, payload)) then
    fail loc.loc "Export not allowed here"
  else 
    false
  )

let signature_of_value_binding {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} = 
  (* TODO just pass in *)
  let other_attrs = filter_attrs pvb_attributes in
  match pvb_pat.ppat_desc with
    | Ppat_var loc -> (
      match pvb_expr.pexp_desc with
      (* let%export a = 2 -- unannotated export of a constant *)
      | Pexp_constant const ->
        let value = Val.mk ~attrs:other_attrs loc (type_of_constant const)
        in Sig.mk (Psig_value value)

      (* let%export a:int = 2 -- an annotated value export *)
      | Pexp_constraint (_, const) -> (
        let value = Val.mk
          ~attrs:other_attrs
          loc
          const
        in Sig.mk (Psig_value value)
      )

      (* let%export a (b:int) (c:char) : string = "boe" -- function export *)
      | Pexp_fun _ as arrow ->
        Sig.mk (Psig_value (Val.mk ~attrs:other_attrs loc (process_arguments arrow loc.loc)))

      | _ -> fail loc.loc "Non-constant exports must be annotated"
    )
    (* What would it mean to export complex patterns? *)
    | _ -> fail pvb_loc "Let export only supports simple patterns"


let rec functor_to_type expr loc = (
  match expr with
  | Pmod_functor (name, type_, {pmod_desc; pmod_loc}) ->
    Mty.functor_ name type_ (functor_to_type pmod_desc pmod_loc)
  | Pmod_constraint (expr, type_) -> type_
  | _ -> fail loc "Exported functors must be annotated"
)

let rec fun_type label {ppat_desc; ppat_loc} {pexp_desc; pexp_loc; pexp_attributes} = (
  let pattern_type = match ppat_desc with
  | Ppat_constraint (_, type_) -> type_
  | _ -> fail ppat_loc "All arguments must be annotated"
  in
  let result_type = match pexp_desc with
  | Pexp_fun (label, _, pattern, expr) -> {
    ptyp_desc=fun_type label pattern expr;
    ptyp_loc=pexp_loc;
    ptyp_attributes=pexp_attributes
  }
  | Pexp_constraint (_, type_) -> type_
  | _ -> fail pexp_loc "Return value must be constrained"
  in
  Ptyp_arrow (label, pattern_type, result_type)
)

let fold_optionals items = List.fold_right (fun x y -> match x with | None -> y | Some x -> x::y) items []

let class_desc_to_type desc = (
  match desc with
  | Pcf_val (name, mutable_flag, class_field_kind) -> (
    let (is_virtual, type_) = match class_field_kind with
    | Cfk_virtual t -> (Virtual, t)
    | Cfk_concrete (override, {pexp_desc; pexp_loc}) -> (match pexp_desc with
      | Pexp_constraint (expr, type_) -> (Concrete, type_)
      | Pexp_constant const -> (Concrete, type_of_constant const)
      | _ -> fail pexp_loc "Exported class value must be constrained"
    )
    in
    Some (Pctf_val (name.txt, mutable_flag, is_virtual, type_))
  )

  | Pcf_method (name, private_flag, class_field_kind) -> 
    let (is_virtual, type_) = match class_field_kind with
      | Cfk_virtual t -> (Virtual, t)
      | Cfk_concrete (override, {pexp_desc; pexp_loc}) -> match pexp_desc with
        | Pexp_poly (expr, Some type_) -> (Concrete, type_)
        | Pexp_poly ({pexp_desc=Pexp_fun (label, _, pattern, expr); pexp_loc; pexp_attributes}, None) ->
          (Concrete, {
            ptyp_desc=fun_type label pattern expr;
            ptyp_loc=pexp_loc;
            ptyp_attributes=pexp_attributes
          })
        | _ -> fail pexp_loc "Exported class methods must be fully annotated"
    in
    Some (Pctf_method (name.txt, private_flag, is_virtual, type_))

  | Pcf_inherit (override_flag, {pcl_desc; pcl_loc; pcl_attributes}, maybe_rename) -> (
    let res = match pcl_desc with
    | Pcl_constraint (_, type_) -> Pctf_inherit type_

    | Pcl_apply ({pcl_desc=Pcl_constr (name, types)}, _)
    | Pcl_constr (name, types) -> Pctf_inherit ({
      pcty_desc=Pcty_constr (name, types);
      pcty_loc=pcl_loc;
      pcty_attributes=pcl_attributes
    })
    | _ -> fail pcl_loc "Inheritance must be type annotated"
    in Some res
  )

  | Pcf_constraint (t1, t2) -> Some (Pctf_constraint (t1, t2))
  | Pcf_attribute attr -> Some (Pctf_attribute attr)
  | Pcf_extension ext -> Some (Pctf_extension ext)

  | Pcf_initializer expr -> None
)

let class_structure_to_class_signature {pcstr_self={ppat_desc; ppat_loc; ppat_attributes}; pcstr_fields} = (
  {
    pcsig_self={ptyp_desc=Ptyp_any; ptyp_loc=ppat_loc; ptyp_attributes=ppat_attributes}; (* TODO figure out when this is ever not any *)
    pcsig_fields=List.map (fun {pcf_desc; pcf_loc; pcf_attributes} ->
      match (class_desc_to_type pcf_desc) with
      | None -> None
      | Some desc ->
      Some {
        pctf_desc=desc;
        pctf_loc=pcf_loc;
        pctf_attributes=pcf_attributes
      }
    ) pcstr_fields |> fold_optionals
  }
)

let rec class_fun_type label {ppat_desc; ppat_loc} result = (
  let argtype = match ppat_desc with
  | Ppat_constraint (_, type_) -> type_
  | _ -> fail ppat_loc "All arguments of exported class must be constrained"
  in
  let result_type = class_expr_to_class_type result
  in
  Pcty_arrow (label, argtype, result_type)
)

and class_expr_to_class_type {pcl_desc; pcl_loc; pcl_attributes}: class_type = (
  let desc = match pcl_desc with
  | Pcl_constr (name, types) -> (Pcty_constr (name, types))
  | Pcl_structure body -> (Pcty_signature (class_structure_to_class_signature body))
  | Pcl_fun (name, _, argument, expr) -> (class_fun_type name argument expr)
  | Pcl_constraint (_, {pcty_desc}) -> pcty_desc
  | Pcl_extension ext -> Pcty_extension ext
  | Pcl_let (_, _, expr) -> 
    let {pcty_desc} = class_expr_to_class_type expr in pcty_desc

  | Pcl_apply _ -> fail pcl_loc "Class application expressions must be type annotated"
  in 
  {pcty_desc=desc; pcty_loc=pcl_loc; pcty_attributes=pcl_attributes}
)

let class_declaration_to_class_description {pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes}: class_description =
  let description = class_expr_to_class_type pci_expr in
  {pci_virt; pci_params; pci_name; pci_expr=description; pci_loc; pci_attributes}

(** The new stuff **)

type exportT =
  | NotExported
  | Exported
  | Abstract
  | ExportedAsType of core_type
  | ExportedAsSig of signature_item

let attribute_to_export ({txt; loc}, str) =
  if txt <> "export" then
    NotExported
  else
    match str with
    | PStr [] -> Exported (* [@export] *)
    | PSig [] -> Exported (* [@export:] *)
    (* [@export abstract] *)
    | PStr [{pstr_desc=Pstr_eval ({
        pexp_desc=Pexp_ident {txt=Longident.Lident "abstract"}
      }, _)}] -> Abstract
    | PStr _ -> fail loc "export with a value must be followed by a colon"
    | PTyp t -> ExportedAsType t (* [@export: t] *)
    | PSig [sig_] -> ExportedAsSig sig_ (* [@export: val m: t] *)
    (* TODO we could relax this if it makes sense? *)
    | PSig _ -> fail loc "can only export a single signature"
    | PPat _ -> fail loc "cannot export patterns"

let rec get_export attributes =
  match attributes with
  | [] -> (NotExported, [])
  | hd::tail ->
    let new_exp = attribute_to_export hd in
    let (final, others) = get_export tail in
    match (new_exp, final) with
    | (NotExported, _) -> (final, hd::others)
    | (_, NotExported) -> (new_exp, others)
    | _ -> let ({loc}, _) = hd in fail loc "Cannot have multiple export annotations"




let double_fold fn items =
  let rec inner items a b = (
    match items with
    | [] -> (a, b)
    | hd::tail ->
      let (a1, b1) = fn hd in
      inner tail (a @ a1) (b @ b1)
  ) in
  inner items [] []

let process_binding binding =
  let (export, attrs) = get_export binding.pvb_attributes in
  let sigs = match export with
  | Exported -> [signature_of_value_binding binding]
  | NotExported -> []
  | ExportedAsSig sig_ -> [sig_]
  | Abstract -> fail binding.pvb_loc "Cannot export value as abstract"
  | ExportedAsType t ->
    let name = match binding.pvb_pat.ppat_desc with
    | Ppat_var {txt} -> txt
    | _ -> fail binding.pvb_loc "let pattern must be a simple name when specifying the export type"
    in
    [Sig.mk (Psig_value (Val.mk ~attrs:attrs {loc=t.ptyp_loc;txt=name} t))]
  in
  (sigs, [{binding with pvb_attributes=attrs}])

let process_type type_ =
  let (export, attrs) = get_export type_.ptype_attributes in
  let sigtypes = match export with 
  | Exported -> [{type_ with ptype_attributes=attrs}]
  | NotExported -> []
  | ExportedAsSig sig_ -> (match sig_.psig_desc with
    | Psig_type (_, t) -> t
    | _ -> fail sig_.psig_loc "Type exported signature must be a type"
  )
  | Abstract -> 
    [{
      (* fully abstract *)
      type_ with
      ptype_attributes=attrs;
      ptype_kind=Ptype_abstract;
      ptype_manifest=None;
      ptype_cstrs=[];
      ptype_params=[]
    }]
  | ExportedAsType t -> [{
      type_ with
      ptype_manifest=Some t;
      ptype_attributes=attrs;
      ptype_cstrs=[];
      ptype_params=[]
    }]
  in
  (sigtypes, [{type_ with ptype_attributes=attrs}])

let module_sig module_ attrs get_signatures =
  match module_.pmb_expr.pmod_desc with
  | Pmod_structure inner_structures ->
    let {pmb_name; pmb_expr={pmod_loc; pmod_attributes}} = module_ in
    let (child_signatures, child_structures) = double_fold get_signatures inner_structures in

    let module_expr = Mod.structure child_structures ~loc:pmod_loc ~attrs:pmod_attributes in
    let module_ = (Mb.mk ~attrs:attrs pmb_name module_expr) in
    let sigModule_ = (Md.mk pmb_name (Mty.signature child_signatures)) in
    (* let _ = {ex with pmb_expr = a} in *)
    ([sigModule_], module_)
  | Pmod_constraint (_, module_type) ->
    (* a constrained module `module X: Type = ...` *)
    let {pmb_name; pmb_loc; pmb_expr={pmod_loc}} = module_ in
    ([(Md.mk pmb_name module_type)], {module_ with pmb_attributes=attrs})
  | Pmod_functor (arg, type_, {pmod_desc; pmod_loc}) ->
    (* a functor! module W (X: Y) : Z = ... *)
    let {pmb_name; pmb_loc; pmb_expr={pmod_loc}} = module_ in
    ([(Md.mk pmb_name (Mty.functor_ arg type_ (functor_to_type pmod_desc pmod_loc)))], {module_ with pmb_attributes=attrs})
  | _ -> fail module_.pmb_loc "Cannot determine a type for this exported module"

let process_class cl =
  let (export, attrs) = get_export cl.pci_attributes in
  let sigs = match export with
  | Exported -> [class_declaration_to_class_description {cl with pci_attributes=attrs}]
  | NotExported -> []
  | ExportedAsSig sig_ -> (match sig_.psig_desc with
    | Psig_class [cls] -> [cls]
    | _ -> fail sig_.psig_loc "Invalid class export"
  )
  | ExportedAsType t -> fail cl.pci_loc "Class types don't work as types"
  | Abstract -> fail cl.pci_loc "Classes cannot be abstract"
  in
  (sigs, [{cl with pci_attributes=attrs}])

(* TODO *)
let process_class_type clt =
  let (export, attrs) = get_export clt.pci_attributes in
  let sigs = match export with
  | Exported -> [clt]
  | NotExported -> []
  | ExportedAsSig sig_ -> (match sig_.psig_desc with
    | Psig_class_type [clt] -> [clt]
    | _ -> fail sig_.psig_loc "Invalid class type export")
  | ExportedAsType t -> fail clt.pci_loc "Class types don't work as types"
  | Abstract -> fail clt.pci_loc "Class types cannot be abstract"
  in
  (sigs, [{clt with pci_attributes=attrs}])



let process_module m get_signatures =
  let (export, attrs) = get_export m.pmb_attributes in
  let newmod = {m with pmb_attributes=attrs} in
  match export with
  | Exported -> module_sig m attrs get_signatures
  | NotExported -> ([], newmod)
  | ExportedAsSig sig_ -> (match sig_.psig_desc with
    | Psig_module m -> ([m], newmod)
    | _ -> fail sig_.psig_loc "Must export as module"
  )
  | ExportedAsType t -> fail m.pmb_loc "Module types don't work as types"
  | Abstract -> fail m.pmb_loc "Module cannot be exported as abstract"

let rec get_signatures structure =
  let (sigs, new_desc) = match structure.pstr_desc with
  (* Multiple exportables *)
  | Pstr_value (r, bindings) ->
    let (signatures, bindings) = double_fold process_binding bindings in
    (signatures, Pstr_value (r, bindings))

  | Pstr_class cls -> 
    let (signatures, classes) = double_fold process_class cls in
    (
      match signatures with
      | [] -> ([], Pstr_class classes)
      | _ -> ([Sig.mk (Psig_class signatures)], Pstr_class classes)
    )

  | Pstr_class_type clt -> 
    let (signatures, class_types) = double_fold process_class_type clt in
    (
      match signatures with
      | [] -> ([], Pstr_class_type class_types)
      | _ -> ([Sig.mk (Psig_class_type signatures)], Pstr_class_type class_types)
    )

  | Pstr_type (r, types) ->
    let (sigtypes, types) = double_fold process_type types in
    (
      match sigtypes with 
      | [] -> ([], Pstr_type (r, types))
      | _ -> ([Sig.mk (Psig_type (r, sigtypes))], Pstr_type (r, types))
    )

  | Pstr_recmodule modules -> 
    let (declarations, modules) = double_fold (fun m ->
      let (sigs, m) = process_module m get_signatures in (sigs, [m])
    ) modules in
    (match declarations with
    | [] -> ([], Pstr_recmodule modules)
    | _ -> ([Sig.mk (Psig_recmodule declarations)], Pstr_recmodule modules)
    )

  (* Single exportable *)
  | Pstr_module m ->
    let (declarations, m) = (process_module m get_signatures) in
    (List.map (fun d -> Sig.mk (Psig_module d)) declarations, Pstr_module m)

  | Pstr_typext t ->
    let (export, attrs) = (get_export t.ptyext_attributes) in
    let sigs = match export with
    | Exported -> [Sig.mk (Psig_typext {t with ptyext_attributes=attrs})]
    | NotExported -> []
    | ExportedAsSig sig_ -> [sig_]
    (* TODO? *)
    | Abstract -> fail t.ptyext_path.loc "Cannot export typext as abstract"
    | ExportedAsType _ -> fail t.ptyext_path.loc "Cannot export typext as type"
    in
    (sigs, Pstr_typext {t with ptyext_attributes=attrs})

  | Pstr_exception e -> 
    let (export, attrs) = (get_export e.pext_attributes) in
    let sigs = match export with
    | Exported -> [Sig.mk (Psig_exception {e with pext_attributes=attrs})]
    | NotExported -> []
    | ExportedAsSig sig_ -> [sig_]
    (* TODO? *)
    | Abstract
    | ExportedAsType _ -> fail e.pext_loc "Invalid exception export"
    in
    (sigs, Pstr_exception {e with pext_attributes=attrs})

  | Pstr_modtype mt ->
    let (export, attrs) = (get_export mt.pmtd_attributes) in
    let sigs = match export with
    | Exported -> [Sig.mk (Psig_modtype mt)]
    | NotExported -> []
    | ExportedAsSig sig_ -> [sig_]
    | Abstract -> fail mt.pmtd_loc "Module type cannot be abstract" (* TODO? *)
    | ExportedAsType _ -> fail mt.pmtd_loc "Must be a module type"
    in
    (sigs, Pstr_modtype {mt with pmtd_attributes=attrs})

  | Pstr_open _
  | Pstr_include _
  | Pstr_attribute _
  | Pstr_extension _ -> ([], structure.pstr_desc)
  | Pstr_eval (e, attrs) -> ([], Pstr_eval (e, disallow_my_attributes attrs))

  (* TODO maybe support primitives? *)
  | Pstr_primitive desc ->
    ([], Pstr_primitive {desc with pval_attributes=disallow_my_attributes desc.pval_attributes})
  in
  (sigs, [{structure with pstr_desc=new_desc}])

let export =
  Reason_toolchain.To_current.copy_mapper
  {
    Ast_mapper.default_mapper with
    structure = (fun mapper structure  ->
      let (sig_, str_) = double_fold get_signatures structure
      in
        if List.length sig_ > 0 then
          let processed_structure = Str.include_  {
            pincl_mod = Mod.constraint_
              (Mod.structure str_)
              (Mty.signature sig_);
              pincl_loc = (Location.symbol_rloc ());
              pincl_attributes = [];
            }
          in
          Ast_mapper.default_mapper.structure mapper [processed_structure]
        else
          Ast_mapper.default_mapper.structure mapper str_
      );
    }

  let _ = Compiler_libs.Ast_mapper.register "export"
      (fun _argv -> export)
