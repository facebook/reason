open Reason_parser

type 'a positioned = 'a * Lexing.position * Lexing.position

type t = {
  declarative_lexer_state: Reason_declarative_lexer.state;
  lexbuf: Lexing.lexbuf;
  mutable comments: (string * Location.t) list;
  mutable queued_tokens: token positioned list;
  mutable queued_exn: exn option;
  mutable last_cnum: int;
  mutable completion_ident_offset: int;
  completion_ident_pos: Lexing.position
}

let init ?insert_completion_ident lexbuf =
  let declarative_lexer_state = Reason_declarative_lexer.make () in
  let completion_ident_offset, completion_ident_pos =
    match insert_completion_ident with
    | None -> (min_int, Lexing.dummy_pos)
    | Some pos -> (pos.Lexing.pos_cnum, pos)
  in
  { declarative_lexer_state; lexbuf;
    comments = [];
    queued_tokens = [];
    queued_exn = None;
    last_cnum = -1;
    completion_ident_offset;
    completion_ident_pos;
  }

let lexbuf state = state.lexbuf


let rec comment_capturing_tokenizer tokenizer =
  fun state ->
  match tokenizer state.declarative_lexer_state state.lexbuf
  with
  | COMMENT (s, comment_loc) ->
     state.comments <- (s, comment_loc) :: state.comments;
     comment_capturing_tokenizer tokenizer state
  | tok -> tok

let token a = (comment_capturing_tokenizer Reason_declarative_lexer.token) a

let token_after_interpolation_region state =
  Reason_declarative_lexer.token_in_template_string_region
    state.declarative_lexer_state
    state.lexbuf

(* Routines for manipulating lexer state *)

let save_triple lexbuf tok =
  (tok, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

let fake_triple t (_, pos, _) =
  (t, pos, pos)

(* insert ES6_FUN *)

exception Lex_balanced_failed of token positioned list * exn option

let closing_of = function
  | LPAREN -> RPAREN
  | LBRACE -> RBRACE
  | LBRACKET
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETAT
  | LBRACKETPERCENT
  | LBRACKETPERCENTPERCENT -> RBRACKET
  | STRING_TEMPLATE_SEGMENT_LBRACE _ -> RBRACE
  | _ -> assert false

let continuer_after_closing = function
  | LBRACKET
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETAT
  | LBRACKETPERCENT
  | LBRACKETPERCENTPERCENT
  | LPAREN
  | LBRACE -> fun a -> token a
  | STRING_TEMPLATE_SEGMENT_LBRACE _ -> fun a-> token_after_interpolation_region a
  | _ -> assert false

let inject_es6_fun = function
  | tok :: acc ->
    tok :: fake_triple ES6_FUN tok :: acc
  | _ -> assert false

let is_triggering_token = function
  | EQUALGREATER | COLON -> true
  | _ -> false

let rec lex_balanced_step state closing acc tok =
  let lexbuf = state.lexbuf in
  let acc = save_triple lexbuf tok :: acc in
  match tok, closing with
  | (RPAREN, RPAREN) | (RBRACE, RBRACE) | (RBRACKET, RBRACKET) ->
    acc
  | ((RPAREN | RBRACE | RBRACKET | EOF), _) ->
    raise (Lex_balanced_failed (acc, None))
  | (( LBRACKET | LBRACKETLESS | LBRACKETGREATER
     | LBRACKETAT
     | LBRACKETPERCENT | LBRACKETPERCENTPERCENT ) as l, _) ->
    lex_balanced state closing  (lex_balanced state (closing_of l) acc)
  | ((LPAREN | LBRACE | STRING_TEMPLATE_SEGMENT_LBRACE _), _) ->
    let rparen =
      try lex_balanced state (closing_of tok) []
      with (Lex_balanced_failed (rparen, None)) ->
        raise (Lex_balanced_failed (rparen @ acc, None))
    in
    begin match (continuer_after_closing tok) state with
    | exception exn -> raise (Lex_balanced_failed (rparen @ acc, Some exn))
    | tok' ->
      let acc = if is_triggering_token tok' then inject_es6_fun acc else acc in
      lex_balanced_step state closing (rparen @ acc) tok'
    end
  | ((LIDENT _ | UNDERSCORE), _) ->
    begin match token state with
    | exception exn ->
      raise (Lex_balanced_failed (acc, Some exn))
    | tok' ->
      let acc = if is_triggering_token tok' then inject_es6_fun acc else acc in
      lex_balanced_step state closing acc tok'
    end
  (* `...` with a closing `}` indicates that we're definitely not in an es6_fun
   * Image the following:
   *    true ? (Update({...a, b: 1}), None) : x;
   *    true ? ({...a, b: 1}) : a;
   *    true ? (a, {...a, b: 1}) : a;
   * The lookahead_esfun is triggered initiating the lex_balanced procedure.
   * Since we now "over"-parse spread operators in pattern position (for
   * better errors), the ... pattern in ({...a, b: 1}) is now a valid path.
   * This means that the above expression `({...a, b: 1}) :` is seen as a pattern.
   * I.e. the arguments of an es6 function: (pattern) :type => expr
   * We exit here, to indicate that an expression needs to be parsed instead
   * of a pattern.
   *)
  | (DOTDOTDOT, RBRACE) -> acc
  | _ -> lex_balanced state closing acc

and lex_balanced state closing acc =
  match token state with
  | exception exn -> raise (Lex_balanced_failed (acc, Some exn))
  | tok -> lex_balanced_step state closing acc tok

let lookahead_esfun state (tok, _, _ as lparen) =
  match lex_balanced state (closing_of tok) [] with
  | exception (Lex_balanced_failed (tokens, exn)) ->
     state.queued_tokens <- List.rev tokens;
     state.queued_exn <- exn;
     lparen
  | tokens ->
     begin match token state with
     | exception exn ->
        state.queued_tokens <- List.rev tokens;
        state.queued_exn <- Some exn;
        lparen
     | token ->
        let tokens = save_triple state.lexbuf token :: tokens in
        if is_triggering_token token then (
          state.queued_tokens <- lparen :: List.rev tokens;
          fake_triple ES6_FUN lparen
        ) else (
          state.queued_tokens <- List.rev tokens;
          lparen
        )
     end

let rec lookahead_in_template_string_interpolation state (tok, _, _ as lparen) =
  match lex_balanced state (closing_of tok) [] with
  | exception (Lex_balanced_failed (tokens, exn)) ->
     state.queued_tokens <- List.rev tokens;
     state.queued_exn <- exn;
     lparen
  | tokens ->
     (* tokens' head will be the RBRACE *)
     (* Change the below to parse "remaining template string" entrypoint *)
     begin match token_after_interpolation_region state with
     | exception exn ->
        state.queued_tokens <- List.rev tokens;
        state.queued_exn <- Some exn;
        lparen
     | token ->
        state.queued_tokens <- List.rev (save_triple state.lexbuf token :: tokens);
        lparen
     end

let token state =
  let lexbuf = state.lexbuf in
  match state.queued_tokens, state.queued_exn with
  | [], Some exn ->
    state.queued_exn <- None;
    raise exn
  | [(LPAREN, _, _) as lparen], None ->
    lookahead_esfun state lparen
  | [(LBRACE, _, _) as lparen], None ->
    lookahead_esfun state lparen
  | [(STRING_TEMPLATE_SEGMENT_LBRACE s, _, _) as template_seg], None ->
    lookahead_in_template_string_interpolation state template_seg
  | [], None ->
    begin match token state with
    | (STRING_TEMPLATE_SEGMENT_LBRACE _) as tok ->
        lookahead_in_template_string_interpolation state (save_triple state.lexbuf tok)
    | LPAREN | LBRACE as tok ->
        lookahead_esfun state (save_triple state.lexbuf tok)
    | (LIDENT _ | UNDERSCORE) as tok ->
        let tok = save_triple lexbuf tok in
        begin match token state with
        | exception exn ->
           state.queued_exn <- Some exn;
           tok
        | tok' ->
          (* On finding an identifier or underscore in expression position, if
           * the next token is "triggering" =>/:, then only return a
           * ficticious (fake_triple) ES6_FUN marker, but queue up both the
           * identifier tok as well as whatever was "triggering" into
           * queued_tokens *)
          if is_triggering_token tok' then (
            state.queued_tokens <- [tok; save_triple lexbuf tok'];
            fake_triple ES6_FUN tok
          ) else (
            (* Otherwise return the identifier token, but queue up the token
             * that didn't "trigger" *)
            state.queued_tokens <- [save_triple lexbuf tok'];
            tok
          )
        end
    | token -> save_triple lexbuf token
    end
  | x :: xs, _ ->
    state.queued_tokens <- xs; x

(* Tokenize with support for IDE completion *)
let token state =
  (* The _last_ token's end position *)
  let space_start = state.last_cnum in
  let (token', start_p, curr_p) as token = token state in
  let token_start = start_p.Lexing.pos_cnum in
  let token_stop = curr_p.Lexing.pos_cnum in
  state.last_cnum <- token_stop;
  if state.completion_ident_offset > min_int &&
     space_start <= state.completion_ident_offset &&
     token_stop >= state.completion_ident_offset then (
    match token' with
    | LIDENT _ | UIDENT _
      when token_start <= state.completion_ident_offset ->
      state.completion_ident_offset <- min_int;
      token
    | _ ->
      state.queued_tokens <- token :: state.queued_tokens;
      state.completion_ident_offset <- min_int;
      (LIDENT "_", state.completion_ident_pos, state.completion_ident_pos)
  ) else
    token

type comment = string * Location.t
type invalid_docstrings = comment list

let empty_invalid_docstrings = []

let add_invalid_docstring text loc_start loc_end invalid_docstrings =
  let loc = {Location. loc_start; loc_end; loc_ghost = false} in
  ((text, loc) :: invalid_docstrings)

let get_comments state invalid_docstrings =
  let cnum (_, loc) = loc.Location.loc_start.Lexing.pos_cnum in
  let rec merge_comments acc = function
    | [], xs | xs, [] -> List.rev_append xs acc
    | ((x :: _) as xs), (y :: ys) when cnum x >= cnum y ->
      merge_comments (y :: acc) (xs, ys)
    | x :: xs, ys ->
      merge_comments (x :: acc) (xs, ys)
  in
  merge_comments [] (state.comments, invalid_docstrings)
