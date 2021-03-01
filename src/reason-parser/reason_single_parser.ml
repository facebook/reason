module I = Reason_parser.MenhirInterpreter

type token = Reason_parser.token
type invalid_docstrings = Reason_lexer.invalid_docstrings

module Step : sig
  type 'a parser
  type 'a step =
    | Intermediate of 'a parser
    | Success of 'a * invalid_docstrings
    | Error

  val initialize : 'a I.checkpoint -> 'a step
  val offer : 'a parser -> token Reason_lexer.positioned -> 'a step
  val add_docstring :
    string -> Lexing.position -> Lexing.position -> 'a parser -> 'a parser

  val recover : 'a I.checkpoint -> invalid_docstrings -> 'a parser
  val recovery_env : 'a parser -> 'a I.env * invalid_docstrings
end = struct

  type 'a postfix_state = {
    checkpoint: 'a I.checkpoint;
    docstrings: invalid_docstrings;
    fallback: 'a I.checkpoint;
    postfix_ops: int;
    postfix_pos: Lexing.position;
  }

  type 'a parser =
    | Normal of 'a I.checkpoint * invalid_docstrings
    | After_potential_postfix of 'a postfix_state

  type 'a step =
    | Intermediate of 'a parser
    | Success of 'a * invalid_docstrings
    | Error

  let mark_potential_postfix token fallback =
    let string_forall f s =
      let i = ref 0 in
      let len = String.length s in
      let valid = ref true in
      while !i < len && !valid do
        valid := f s.[!i];
        incr i;
      done;
      !valid
    in
    match token with
    | (Reason_parser.INFIXOP1 s, pos, _)
      when string_forall ((=) '^') s ->
      (fun checkpoint docstrings ->
         After_potential_postfix {
           checkpoint; fallback; docstrings;
           postfix_ops = String.length s;
           postfix_pos = pos;
         })
    | _ ->
      (fun checkpoint docstrings ->
         Normal (checkpoint, docstrings))

  let rec offer_postfix count pos = function
    | I.Shifting _ | I.AboutToReduce _ as checkpoint ->
      offer_postfix count pos (I.resume checkpoint)
    | I.InputNeeded _ as checkpoint ->
      if count <= 0 then checkpoint else (
        let pos_cnum = pos.Lexing.pos_cnum in
        let pos' = {pos with Lexing.pos_cnum = pos_cnum + 1} in
        offer_postfix (count - 1) pos'
          (I.offer checkpoint (Reason_parser.POSTFIXOP "^", pos, pos'))
      )
    | other -> other

  let rec step mark_potential_postfix safepoint docstrings = function
    | I.Shifting _ | I.AboutToReduce _ as checkpoint ->
       step mark_potential_postfix safepoint docstrings (I.resume checkpoint)
    | I.InputNeeded _ as checkpoint ->
      Intermediate (mark_potential_postfix checkpoint docstrings)
    | I.Accepted x -> Success (x, docstrings)
    | I.Rejected | I.HandlingError _ -> Error

  let offer parser token =
    match parser with
    | Normal (checkpoint, docstrings) ->
      step (mark_potential_postfix token checkpoint) checkpoint
        docstrings (I.offer checkpoint token)
    | After_potential_postfix r ->
      match step (mark_potential_postfix token r.checkpoint) r.checkpoint
              r.docstrings (I.offer r.checkpoint token)
      with
      | Error ->
        begin (* Try applying postfix operators on fallback parser *)
          match offer_postfix r.postfix_ops r.postfix_pos r.fallback with
          | I.InputNeeded _ as checkpoint ->
            step (mark_potential_postfix token checkpoint) checkpoint
              r.docstrings (I.offer checkpoint token)
          | _ -> Error
        end
      | result -> result

  let add_docstring text startp endp parser =
    match parser with
    | Normal (checkpoint, docstrings) ->
      let docstrings =
        Reason_lexer.add_invalid_docstring text startp endp docstrings
      in
      Normal (checkpoint, docstrings)
    | After_potential_postfix r ->
      let docstrings =
        Reason_lexer.add_invalid_docstring text startp endp r.docstrings
      in
      After_potential_postfix {r with docstrings}

  let initialize checkpoint =
    step (fun parser ds -> Normal (parser, ds)) checkpoint
      Reason_lexer.empty_invalid_docstrings checkpoint

  let recover cp ds =
    begin match cp with
      | I.InputNeeded _ -> ()
      | _ -> assert false
    end;
    Normal (cp, ds)

  let recovery_env parser =
    let cp, ds = match parser with
      | Normal (cp, ds) -> (cp, ds)
      | After_potential_postfix r -> (r.checkpoint, r.docstrings)
    in
    match cp with
    | I.InputNeeded env -> (env, ds)
    | _ -> assert false
end

type 'a parser = 'a Step.parser
type 'a step = 'a Step.step =
  | Intermediate of 'a parser
  | Success of 'a * invalid_docstrings
  | Error

let initial entry position =
  match Step.initialize (entry position) with
  | Step.Intermediate parser -> parser
  | _ -> assert false

let rec offer_many parser = function
  | [] -> Step.Intermediate parser
  | [token] -> Step.offer parser token
  | token :: tokens ->
     match Step.offer parser token with
     | Step.Intermediate parser -> offer_many parser tokens
     | other -> other

(* Logic for inserting ';' *)

let try_insert_semi_on = function
  | Reason_parser.LET | Reason_parser.TYPE | Reason_parser.MODULE
  | Reason_parser.OPEN | Reason_parser.EXCEPTION
  | Reason_parser.INCLUDE | Reason_parser.DOCSTRING _
  | Reason_parser.LIDENT _ | Reason_parser.UIDENT _
  | Reason_parser.IF | Reason_parser.WHILE
  | Reason_parser.FOR | Reason_parser.SWITCH
  | Reason_parser.TRY | Reason_parser.ASSERT
  | Reason_parser.EXTERNAL | Reason_parser.LAZY
  | Reason_parser.LBRACKETAT -> true
  | _ -> false

(* Logic for splitting '=?...' operators into '=' '?' '...' *)

let advance p n = {p with Lexing.pos_cnum = p.Lexing.pos_cnum + n}

let rec split_greaters acc pcur = function
  | '>' :: tl ->
    let pnext = (advance pcur 1) in
    split_greaters ((Reason_parser.GREATER, pcur, pnext) :: acc) pnext tl
  | nonGts -> (List.rev acc), nonGts, pcur

let common_remaining_infix_token pcur =
  let pnext = advance pcur 1 in
  function
  | ['-'] -> Some(Reason_parser.MINUS, pcur, pnext)
  | ['-'; '.'] -> Some(Reason_parser.MINUSDOT, pcur, advance pnext 1)
  | ['+'] -> Some(Reason_parser.PLUS, pcur, pnext)
  | ['+'; '.'] -> Some(Reason_parser.PLUSDOT, pcur, advance pnext 1)
  | ['!'] -> Some(Reason_parser.BANG, pcur, pnext)
  | ['>'] -> Some(Reason_parser.GREATER, pcur, pnext)
  | ['<'] -> Some(Reason_parser.LESS, pcur, pnext)
  | ['#'] -> Some(Reason_parser.SHARP, pcur, pnext)
  | [':'] -> Some(Reason_parser.COLON, pcur, pnext)
  | _ -> None

let rec decompose_token pos0 split =
  let pcur = advance pos0 1 in
  let pnext = advance pos0 2 in
  match split with
  (* Empty token is a valid decomposition *)
  | '=' :: tl ->
    let eq = (Reason_parser.EQUAL, pcur, pnext) in
    let (revFirstTwo, tl, pcur, _pnext) = match tl with
    | '?' :: tlTl ->
      [(Reason_parser.QUESTION, pcur, pnext); eq], tlTl, pnext, (advance pnext 1)
    | _ -> [eq], tl, pcur, pnext
    in
    if tl == [] then Some(List.rev revFirstTwo)
    else
      (match common_remaining_infix_token pcur tl with
      | None -> None
      | Some(r) -> Some(List.rev (r :: revFirstTwo)))
  (* For type parameters  type t<+'a> = .. and t<#classNameOrPolyVariantKind>*)
  | '<' :: tl ->
      let less = [Reason_parser.LESS, pcur, pnext] in
      if tl == [] then Some less
      else
        (match common_remaining_infix_token pcur tl with
        | None -> None (* Couldn't parse the non-empty tail - invalidates whole thing *)
        | Some(r) -> Some(List.rev (r :: less)))
  | '>' :: _tl ->
      (* Recurse to take advantage of all the logic in case the remaining
       * begins with an equal sign.
       * This also handles:
       *
       *    class foo<'a, 'b>: ...
       *
       * Where >: is initially lexed as an infix.
       *)
      let gt_tokens, rest_split, prest = split_greaters [] pcur split in
      if rest_split == [] then
        Some gt_tokens
      else
        (match decompose_token prest rest_split with
        | None -> None (* Couldn't parse the non-empty tail - invalidates whole thing *)
        | Some(r) -> Some(List.rev gt_tokens @ r))
  | [_] | [_; _] ->
    (match common_remaining_infix_token pcur split with
     | None -> None
     | Some a -> Some [a])
  | [] -> None
  | _ -> None


let rec init_tailrec_aux acc i n f =
  if i >= n then acc
  else init_tailrec_aux (f i :: acc) (i+1) n f

let list_init len f = List.rev (init_tailrec_aux [] 0 len f)

let explode s = list_init (String.length s) (String.get s)

let try_split_label (tok_kind, pos0, _posn) =
  match tok_kind with
  | Reason_parser.INFIXOP0 s ->
    (match decompose_token pos0 (explode s) with
    | None -> []
    | Some(l) -> l)
  | _ -> []

(* Logic for attempting to consume a token
   and try alternatives on failure *)

let step parser token =
  match Step.offer parser token with
  | (Success _ | Intermediate _) as step -> step
  | Error ->
     let try_alternative_tokens = function
       | [] -> Error
       | tokens ->
          match offer_many parser tokens with
          | (Step.Intermediate _ | Step.Success _) as result -> result
          (* Alternative failed... Return original failure *)
          | Step.Error -> Error
     in
     let alternative =
       match token with
       | tok_kind, pos, _ when try_insert_semi_on tok_kind ->
         try_alternative_tokens [(Reason_parser.SEMI, pos, pos); token]
       | _ -> try_alternative_tokens (try_split_label token)
     in
     match alternative, token with
     | Error, (Reason_parser.DOCSTRING text, startp, endp) ->
       Intermediate (Step.add_docstring text startp endp parser)
     | _ -> alternative

(* Interface for recovery *)

let recover = Step.recover
let recovery_env = Step.recovery_env
