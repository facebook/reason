let is_punned_labelled_expression e lbl =
  let open Ast_404.Parsetree in
  match e.pexp_desc with
  | Pexp_ident { txt; _ }
  | Pexp_constraint ({pexp_desc = Pexp_ident { txt; _ }; _}, _)
  | Pexp_coerce ({pexp_desc = Pexp_ident { txt; _ }; _}, _, _)
    -> txt = Longident.parse lbl
  | _ -> false

(* We manually check the length of `Thing.map(foo, bar, baz`,
 * in `Thing.map(foo, bar, baz, (a) => doStuff(a))`
 * because Easyformat doesn't have a hook to change printing when a list breaks
 *
 * we check if all arguments aside from the final one are either strings or identifiers,
 * where the sum of the string contents and identifier names are less than the print width
 *)
let funAppCallbackExceedsWidth ~printWidth ~args ~funExpr () =
  let open Ast_404.Parsetree in
  let open Ast_404.Asttypes in
  let funLen = begin match funExpr.pexp_desc with
    | Pexp_ident ident ->
       let identList = Longident.flatten ident.txt in
       let lengthOfDots = List.length identList - 1 in
       let len = List.fold_left (fun acc curr ->
         acc + (String.length curr)) lengthOfDots identList in
       len
    | _ -> -1
  end in
  (* eats an argument & substract its length from the printWidth
   * as soon as the print width reaches a sub-zero value,
   * we know the print width is exceeded & returns *)
  let rec aux len = function
    | _ when len < 0 -> true
    | [] -> false
    | arg::args ->
      begin match arg with
      | (label, ({ pexp_desc = Pexp_ident ident } as e)) ->
        let identLen = List.fold_left (fun acc curr ->
          acc + (String.length curr)
        ) len (Longident.flatten ident.txt) in
        begin match label with
        | Nolabel -> aux (len - identLen) args
        | Labelled s when is_punned_labelled_expression e s ->
            aux (len - (identLen + 1)) args
        | Labelled s ->
            aux (len - (identLen + 2 + String.length s)) args
        | Optional s ->
            aux (len - (identLen + 3 + String.length s)) args
        end
      | (label, {pexp_desc = Pexp_constant (Pconst_string (str, _))}) ->
        let strLen = String.length str in
        begin match label with
        | Nolabel -> aux (len - strLen) args
        | Labelled s ->
            aux (len - (strLen + 2 + String.length s)) args
        | Optional s ->
            aux (len - (strLen + 3 + String.length s)) args
        end
      | _ ->
        (* if we encounter a non-string or non-identifier argument exit *)
          true
    end
  in
  aux (printWidth - funLen) args

(* Determines if the start & end position of Location are on the same line *)
let staysOnOneLine ({loc_start; loc_end} : Location.t) =
  loc_start.pos_lnum == loc_end.pos_lnum
