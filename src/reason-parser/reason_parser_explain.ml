(* See the comments in menhir_error_processor.ml *)

open Reason_string

module Parser = Reason_parser
module Interp = Parser.MenhirInterpreter
module Raw = Reason_parser_explain_raw

let identlike_keywords =
  let reverse_table = lazy (
    let table = Hashtbl.create 7 in
    let flip_add k v = Hashtbl.add table v k in
    Hashtbl.iter flip_add Reason_declarative_lexer.keyword_table;
    table
  ) in
  function
  | Parser.SIG    -> Some "sig"
  | Parser.MODULE -> Some "module"
  | Parser.BEGIN  -> Some "begin"
  | Parser.END    -> Some "end"
  | Parser.OBJECT -> Some "object"
  | Parser.SWITCH -> Some "switch"
  | Parser.TO     -> Some "to"
  | Parser.THEN   -> Some "then"
  | Parser.TYPE   -> Some "type"
  | token ->
    match Hashtbl.find (Lazy.force reverse_table) token with
    | name -> Some name
    | exception Not_found -> None

let keyword_confused_with_ident state token =
  match identlike_keywords token with
  | Some name when Raw.transitions_on_lident state
                || Raw.transitions_on_uident state ->
    (name ^ " is a reserved keyword, it cannot be used as an identifier. Try `" ^ name ^ "_` or `_" ^ name ^ "` instead")
  | _ -> raise Not_found

let uppercased_instead_of_lowercased state token =
  match token with
  | Parser.UIDENT name when Raw.transitions_on_lident state ->
    let name = String.uncapitalize_ascii name in
    if Hashtbl.mem Reason_declarative_lexer.keyword_table name then
      "variables and labels should be lowercased"
    else
      Printf.sprintf "variables and labels should be lowercased. Try `%s'" name
  | _ -> raise Not_found

let semicolon_might_be_missing state _token =
  (*let state = Interp.current_state_number env in*)
  if Raw.transitions_on_semi state then
    "syntax error, consider adding a `;' before"
  else
    raise Not_found

let token_specific_message = function
  | Parser.UNDERSCORE ->
    "underscore is not a valid identifier. Use _ only in pattern matching and partial function application"
  | _ ->
    raise Not_found

let unclosed_parenthesis is_opening_symbol closing_symbol check_function env =
  let state = Interp.current_state_number env in
  if check_function state then
    let rec find_opening_location = function
      | None -> None
      | Some env ->
        let found =
          match Interp.top env with
          | Some (Interp.Element (state, _, startp, endp))
            when (is_opening_symbol (Interp.X (Interp.incoming_symbol state))) ->
            Some (startp, endp)
          | Some (Interp.Element (state, _, _, _))
            when (Interp.X (Interp.incoming_symbol state) = closing_symbol) ->
            raise Not_found
          | _ -> None
        in
        match found with
        | Some _ -> found
        | _ -> find_opening_location (Interp.pop env)
    in
    try find_opening_location (Some env)
    with Not_found -> None
  else
    None

let check_unclosed env =
  let check (message, open_msg, opening_symbols, closing_symbol, check_function) =
    match
      unclosed_parenthesis (fun x -> List.mem x opening_symbols)
        closing_symbol check_function env
    with
    | None -> None
    | Some (loc_start, _) ->
      Some (Format.asprintf "Unclosed %S (%s on line %d, column %d)"
              message open_msg loc_start.pos_lnum
              (loc_start.pos_cnum - loc_start.pos_bol))
  in
  let rec check_list = function
    | [] -> raise Not_found
    | x :: xs ->
      match check x with
      | None -> check_list xs
      | Some result -> result
  in
  check_list [
    ("${", "for string region beginning ", Interp.[X (T T_STRING_TEMPLATE_SEGMENT_LBRACE)],
     Interp.X (T T_RBRACE),
     Raw.transitions_on_rbrace);
    ("(", "opened", Interp.[X (T T_LPAREN)],
     Interp.X (T T_RPAREN),
     Raw.transitions_on_rparen);
    ("{", "opened", Interp.[X (T T_LBRACE); X (T T_LBRACELESS)],
     Interp.X (T T_RBRACE),
     Raw.transitions_on_rbrace);
    ("[", "opened", Interp.[ X (T T_LBRACKET); X (T T_LBRACKETAT);
                   X (T T_LBRACKETBAR); X (T T_LBRACKETGREATER);
                   X (T T_LBRACKETLESS); X (T T_LBRACKETPERCENT);
                   X (T T_LBRACKETPERCENTPERCENT); ],
     Interp.X (T T_RBRACKET),
     Raw.transitions_on_rbracket);
  ]

let message env (token, _, _) =
  let state = Interp.current_state_number env in
  (* Identify a keyword used as an identifier *)
  try keyword_confused_with_ident state token
  with Not_found ->
  try check_unclosed env
  with Not_found ->
  (* Identify an uppercased identifier in a lowercase place *)
  try uppercased_instead_of_lowercased state token
  with Not_found ->
  try semicolon_might_be_missing state token
  with Not_found ->
  try token_specific_message token
  with Not_found ->
  (* Is there a message for this specific state ? *)
    (* TODO: we don't know what to say *)
    "Syntax error"
