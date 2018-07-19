(*
  This is the file that handles turning Reason JSX' agnostic function call into
  a ReasonReact-specific function call. Aka, this is a macro, using OCaml's ppx
  facilities; https://whitequark.org/blog/2014/04/16/a-guide-to-extension-
  points-in-ocaml/

  You wouldn't use this file directly; it's used by BuckleScript's
  bsconfig.json. Specifically, there's a field called `react-jsx` inside the
  field `reason`, which enables this ppx through some internal call in bsb
*)

(*
  The actual transform:

  transform `div props1::a props2::b children::[foo, bar] () [@JSX]` into
  `ReactDOMRe.createElement "div" props::[%bs.obj {props1: 1, props2: b}] [|foo,
  bar|]`.

  transform the upper-cased case
  `Foo.createElement key::a ref::b foo::bar children::[] () [@JSX]` into
  `ReasonReact.element key::a ref::b (Foo.make foo::bar [||] [@JSX])`
*)

(*
  This file's shared between the Reason repo and the BuckleScript repo. In
  Reason, it's in src. In BuckleScript, it's in vendor/reason We periodically
  copy this file from Reason (the source of truth) to BuckleScript, then
  uncomment the #if #else #end cppo macros you see in the file. That's because
  BuckleScript's on OCaml 4.02 while Reason's on 4.04; so the #if macros
  surround the pieces of code that are different between the two compilers.

  When you modify this file, please make sure you're not dragging in too many
  things. You don't necessarily have to test the file on both Reason and
  BuckleScript; ping @chenglou and a few others and we'll keep them synced up by
  patching the right parts, through the power of types(tm)
*)

(* #if defined BS_NO_COMPILER_PATCH then *)
open Migrate_parsetree
open Ast_404

let nolabel = Ast_404.Asttypes.Nolabel
let labelled str = Ast_404.Asttypes.Labelled str
let argIsKeyRef = function
  | (Asttypes.Labelled ("key" | "ref"), _) | (Asttypes.Optional ("key" | "ref"), _) -> true
  | _ -> false
let constantString ~loc str = Ast_helper.Exp.constant ~loc (Parsetree.Pconst_string (str, None))
(* #else *)
(* let nolabel = "" *)
(* let labelled str = str *)
(* let argIsKeyRef = function *)
  (* | (("key" | "ref"), _) | (("?key" | "?ref"), _) -> true *)
  (* | _ -> false *)
(* let constantString ~loc str = Ast_helper.Exp.constant ~loc (Asttypes.Const_string (str, None)) *)
(* #end *)

open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

let transformChildren ~loc ~mapper theList =
  let rec transformChildren' theList accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match theList with
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} ->
      List.rev accum |> Exp.array ~loc
    | {pexp_desc = Pexp_construct (
        {txt = Lident "::"},
        Some {pexp_desc = Pexp_tuple (v::acc::[])}
      )} ->
      transformChildren' acc ((mapper.expr mapper v)::accum)
    | notAList -> mapper.expr mapper notAList
  in
  transformChildren' theList []

let extractChildrenForDOMElements ?(removeLastPositionUnit=false) ~loc propsAndChildren =
  let rec allButLast_ lst acc = match lst with
    | [] -> []
(* #if defined BS_NO_COMPILER_PATCH then *)
    | (Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})::[] -> acc
    | (Nolabel, _)::rest -> raise (Invalid_argument "JSX: found non-labelled argument before the last position")
(* #else *)
    (* | ("", {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})::[] -> acc *)
    (* | ("", _)::rest -> raise (Invalid_argument "JSX: found non-labelled argument before the last position") *)
(* #end *)
    | arg::rest -> allButLast_ rest (arg::acc)
  in
  let allButLast lst = allButLast_ lst [] |> List.rev in
  match (List.partition (fun (label, expr) -> label = labelled "children") propsAndChildren) with
  | ((label, childrenExpr)::[], props) ->
    (childrenExpr, if removeLastPositionUnit then allButLast props else props)
  | ([], props) ->
    (* no children provided? Place a placeholder list (don't forgot we're talking about DOM element conversion here only) *)
    (Exp.construct ~loc {loc; txt = Lident "[]"} None, if removeLastPositionUnit then allButLast props else props)
  | (moreThanOneChild, props) -> raise (Invalid_argument "JSX: somehow there's more than one `children` label")

(* TODO: some line number might still be wrong *)
let jsxMapper () =

  let jsxTransformV3 modulePath mapper loc attrs callExpression callArguments =
    let (children, argsWithLabels) =
      extractChildrenForDOMElements ~loc ~removeLastPositionUnit:true callArguments in
    let (argsKeyRef, argsForMake) = List.partition argIsKeyRef argsWithLabels in
    let childrenExpr = match children with
    (* if it's a single, non-jsx item, keep it so (remove the list wrapper, don't add the array wrapper) *)
    | {pexp_desc = Pexp_construct (
        {txt = Lident "::"; loc},
        Some {pexp_desc = Pexp_tuple [
          ({pexp_attributes} as singleItem);
          {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)}
        ]}
      )} when List.for_all (fun (attribute, _) -> attribute.txt <> "JSX") pexp_attributes ->
      mapper.expr mapper singleItem
    (* if it's a single jsx item, or multiple items, turn list into an array *)
    | nonEmptyChildren -> transformChildren ~loc ~mapper nonEmptyChildren
    in
    let recursivelyTransformedArgsForMake = argsForMake |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)) in
    let args = recursivelyTransformedArgsForMake @ [ (nolabel, childrenExpr) ] in
    let wrapWithReasonReactElement e = (* ReasonReact.element ::key ::ref (...) *)
      Exp.apply
        ~loc
        (Exp.ident ~loc {loc; txt = Ldot (Lident "ReasonReact", "element")})
        (argsKeyRef @ [(nolabel, e)]) in
    Exp.apply
      ~loc
      ~attrs
      (* Foo.make *)
      (Exp.ident ~loc {loc; txt = Ldot (modulePath, "make")})
      args
    |> wrapWithReasonReactElement in

  let jsxTransformV2 modulePath mapper loc attrs callExpression callArguments =
    let (children, argsWithLabels) =
      extractChildrenForDOMElements ~loc ~removeLastPositionUnit:true callArguments in
    let (argsKeyRef, argsForMake) = List.partition argIsKeyRef argsWithLabels in
    let childrenExpr = transformChildren ~loc ~mapper children in
    let recursivelyTransformedArgsForMake = argsForMake |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)) in
    let args = recursivelyTransformedArgsForMake @ [ (nolabel, childrenExpr) ] in
    let wrapWithReasonReactElement e = (* ReasonReact.element ::key ::ref (...) *)
      Exp.apply
        ~loc
        (Exp.ident ~loc {loc; txt = Ldot (Lident "ReasonReact", "element")})
        (argsKeyRef @ [(nolabel, e)]) in
    Exp.apply
      ~loc
      ~attrs
      (* Foo.make *)
      (Exp.ident ~loc {loc; txt = Ldot (modulePath, "make")})
      args
    |> wrapWithReasonReactElement in

  let lowercaseCaller mapper loc attrs callArguments id  =
    let (children, propsWithLabels) =
      extractChildrenForDOMElements ~loc callArguments in
    let componentNameExpr = constantString ~loc id in
    let childrenExpr = transformChildren ~loc ~mapper children in
    let args = match propsWithLabels with
      | [theUnitArgumentAtEnd] ->
        [
          (* "div" *)
          (nolabel, componentNameExpr);
          (* [|moreCreateElementCallsHere|] *)
          (nolabel, childrenExpr)
        ]
      | nonEmptyProps ->
        let propsCall =
          Exp.apply
            ~loc
            (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "props")})
            (nonEmptyProps |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)))
        in
        [
          (* "div" *)
          (nolabel, componentNameExpr);
          (* ReactDOMRe.props className:blabla foo::bar () *)
          (labelled "props", propsCall);
          (* [|moreCreateElementCallsHere|] *)
          (nolabel, childrenExpr)
        ] in
    Exp.apply
      ~loc
      (* throw away the [@JSX] attribute and keep the others, if any *)
      ~attrs
      (* ReactDOMRe.createDOMElement *)
      (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "createElement")})
      args in

  let jsxVersion = ref None in

  let structure =
    (fun mapper structure -> match structure with
      (*
        match against [@@@bs.config {foo, jsx: ...}] at the file-level. This
        indicates which version of JSX we're using. This code stays here because
        we used to have 2 versions of JSX PPX (and likely will again in the
        future when JSX PPX changes). So the architecture for switching between
        JSX behavior stayed here. To create a new JSX ppx, copy paste this
        entire file and change the relevant parts.

        Description of architecture: in bucklescript's bsconfig.json, you can
        specify a project-wide JSX version. You can also specify a file-level
        JSX version. This degree of freedom allows a person to convert a project
        one file at time onto the new JSX, when it was released. It also enabled
        a project to depend on a third-party which is still using an old version
        of JSX
      *)
      | {
            pstr_loc;
            pstr_desc = Pstr_attribute (
              ({txt = "bs.config"} as bsConfigLabel),
              PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_record (recordFields, b)} as innerConfigRecord, a)} as configRecord]
            )
          }::restOfStructure -> begin
            let (jsxField, recordFieldsWithoutJsx) = recordFields |> List.partition (fun ({txt}, _) -> txt = Lident "jsx") in
            match (jsxField, recordFieldsWithoutJsx) with
            (* no file-level jsx config found *)
            | ([], _) -> default_mapper.structure mapper structure
            (* {jsx: 2 | 3} *)
(* #if defined BS_NO_COMPILER_PATCH then *)
            | ((_, {pexp_desc = Pexp_constant (Pconst_integer (version, _))})::rest, recordFieldsWithoutJsx) -> begin
                (match version with
                | "2" -> jsxVersion := Some 2
                | "3" -> jsxVersion := Some 3
                | _ -> raise (Invalid_argument "JSX: the file-level bs.config's jsx version must be either 2 or 3"));
(* #else *)
            (* | ((_, {pexp_desc = Pexp_constant (Const_int version)})::rest, recordFieldsWithoutJsx) -> begin *)
                (* (match version with *)
                (* | 2 -> jsxVersion := Some 2 *)
                (* | 3 -> jsxVersion := Some 3 *)
                (* | _ -> raise (Invalid_argument "JSX: the file-level bs.config's jsx version must be either 2 or 3")); *)
(* #end *)
                match recordFieldsWithoutJsx with
                (* record empty now, remove the whole bs.config attribute *)
                | [] -> default_mapper.structure mapper restOfStructure
                | fields -> default_mapper.structure mapper ({
                  pstr_loc;
                  pstr_desc = Pstr_attribute (
                    bsConfigLabel,
                    PStr [{configRecord with pstr_desc = Pstr_eval ({innerConfigRecord with pexp_desc = Pexp_record (fields, b)}, a)}]
                  )
                }::restOfStructure)
              end
          | (_, recordFieldsWithoutJsx) -> raise (Invalid_argument "JSX: the file-level bs.config's {jsx: ...} config accepts only a version number")
        end
      | _ -> default_mapper.structure mapper structure
    ) in

  let transformJsxCall mapper callExpression callArguments attrs =
    (match callExpression.pexp_desc with
     | Pexp_ident caller ->
       (match caller with
        | {txt = Lident "createElement"} ->
          raise (Invalid_argument "JSX: `createElement` should be preceeded by a module name.")
        (* Foo.createElement prop1::foo prop2:bar children::[] () *)
        | {loc; txt = Ldot (modulePath, ("createElement" | "make"))} ->
          let f = match !jsxVersion with
            | Some 2 -> jsxTransformV2 modulePath
            | Some 3 -> jsxTransformV3 modulePath
            | Some _ -> raise (Invalid_argument "JSX: the JSX version must be either 2 or 3")
            | None -> jsxTransformV3 modulePath
          in f mapper loc attrs callExpression callArguments
        (* div prop1::foo prop2:bar children::[bla] () *)
        (* turn that into ReactDOMRe.createElement props::(ReactDOMRe.props props1::foo props2::bar ()) [|bla|] *)
        | {loc; txt = Lident id} ->
          lowercaseCaller mapper loc attrs callArguments id
        | {txt = Ldot (_, anythingNotCreateElementOrMake)} ->
          raise (
            Invalid_argument
              ("JSX: the JSX attribute should be attached to a `YourModuleName.createElement` or `YourModuleName.make` call. We saw `"
               ^ anythingNotCreateElementOrMake
               ^ "` instead"
              )
          )
        | {txt = Lapply _} ->
          (* don't think there's ever a case where this is reached *)
          raise (
            Invalid_argument "JSX: encountered a weird case while processing the code. Please report this!"
          )
       )
     | anythingElseThanIdent ->
       raise (
         Invalid_argument "JSX: `createElement` should be preceeded by a simple, direct module name."
       )
    ) in

  let expr =
    (fun mapper expression -> match expression with
       (* Does the function application have the @JSX attribute? *)
       |
         {
           pexp_desc = Pexp_apply (callExpression, callArguments);
           pexp_attributes
         } ->
         let (jsxAttribute, nonJSXAttributes) = List.partition (fun (attribute, _) -> attribute.txt = "JSX") pexp_attributes in
         (match (jsxAttribute, nonJSXAttributes) with
         (* no JSX attribute *)
         | ([], _) -> default_mapper.expr mapper expression
         | (_, nonJSXAttributes) -> transformJsxCall mapper callExpression callArguments nonJSXAttributes)
       (* Delegate to the default mapper, a deep identity traversal *)
       | e -> default_mapper.expr mapper e) in

(* #if defined BS_NO_COMPILER_PATCH then *)
  { default_mapper with structure; expr }
(* #else *)
  (* { default_mapper with structure; expr } *)
(* #end *)

(* #if defined BS_NO_COMPILER_PATCH then *)
let () =
  Driver.register ~name:"JSX"
    Versions.ocaml_404 (fun _config _cookies -> jsxMapper ())
(* #else *)
(* let () = Ast_mapper.register "JSX" (fun _argv -> jsxMapper ()) *)
(* #end *)
