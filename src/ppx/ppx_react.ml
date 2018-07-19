open Migrate_parsetree
open Ast_404

open Parsetree
open Asttypes
open Ast_mapper
open Ast_helper

let lid txt =
  { txt = Longident.Lident txt; loc = Location.none }

let str txt = { txt; loc = Location.none }

let expandReactMacro =
  {
    default_mapper with
    structure = (fun mapper structure  ->
     match structure with
     | { pstr_loc; pstr_desc = Pstr_eval ({pexp_desc = (Pexp_extension(_,(PStr({pstr_desc = (Pstr_eval({pexp_desc = (Pexp_construct({txt = Longident.Lident module_name },_))},_))}::[]))))},_)}::_ ->
         [Str.module_
            {
              pmb_name = { txt = module_name; loc = pstr_loc };
              pmb_expr =
                Mod.mk
                   (Pmod_structure
                       ([Str.include_
                           {
                             pincl_mod = (Mod.mk ((Pmod_ident (lid "ReactRe.Component"))));
                             pincl_loc = pstr_loc;
                             pincl_attributes = []
                           };
                        Str.value Nonrecursive
                          [Vb.mk (Pat.var (str "name"))
                             (Exp.constant
                                (Pconst_string (module_name, None)))
                          ];
                        Str.type_
                          Recursive
                          [{
                             ptype_name = (str "props");
                             ptype_params = [];
                             ptype_cstrs = [];
                             ptype_kind =
                               Ptype_record
                                   [{
                                       pld_name = (str "message");
                                       pld_mutable = Immutable;
                                       pld_type =
                                         (Typ.constr (lid "string") []);
                                       pld_loc = pstr_loc;
                                       pld_attributes = []
                                     }];
                             ptype_private = Public;
                             ptype_manifest = None;
                             ptype_attributes = [];
                             ptype_loc = pstr_loc
                           }];
                        Str.value Nonrecursive
                          [Vb.mk (Pat.var (str "render"))
                             (Exp.fun_ Nolabel None (Pat.mk Ppat_any)
                                (Exp.apply (Exp.ident (lid "div"))
                                   [(Labelled "children",
                                     (Exp.construct (lid "[]") None));
                                    (Nolabel,
                                     (Exp.construct (lid "()") None))]
                                   ~attrs:[((str "JSX"),
                                            PStr ([]))]))]]))
                   ;
              pmb_attributes = [];
              pmb_loc = pstr_loc
            };
         Str.include_
           {
             pincl_mod =
               (Mod.apply
                  (Mod.mk (Pmod_ident (lid "ReactRe.CreateComponent")))
                  (Mod.mk (Pmod_ident (lid module_name))));
             pincl_loc = pstr_loc;
             pincl_attributes = []
           };
         Str.value Nonrecursive
           [Vb.mk (Pat.var (str "createElement"))
              (Exp.fun_ (Labelled "message") None (Pat.var (str "message"))
                 (Exp.fun_ (Labelled "children") None (Pat.var (str "children"))
                    (Exp.apply (Exp.ident (lid "wrapProps"))
                       [(Nolabel,
                         (Exp.record
                            [((lid "message"),
                              (Exp.ident (lid "message")))] None));
                        (Labelled "children", (Exp.ident (lid "children")))])))]]
     | _ -> structure)
  }

let () =
  Driver.register ~name:"editor.rehydrate"
    Versions.ocaml_404 (fun _config _cookies -> expandReactMacro)
