open MenhirSdk

module G = Cmly_read.Read(struct let filename = Sys.argv.(1) end)
open G

let print fmt = Printf.ksprintf print_endline fmt

(* We want to detect any state where an identifier is admissible.
   That way, we can assume that if a keyword is used and rejceted, the user was
   intending to put an identifier. *)
let states_transitioning_on pred =
  let keep_state lr1 =
    (* There are two kind of transitions (leading to SHIFT or REDUCE), detect
       those who accept identifiers *)
    List.exists (fun (term, _) -> pred (T term)) (Lr1.reductions lr1) ||
    List.exists (fun (sym, _) -> pred sym) (Lr1.transitions lr1)
  in
  (* Now we filter the list of all states and keep the interesting ones *)
  G.Lr1.fold (fun lr1 acc -> if keep_state lr1 then lr1 :: acc else acc) []

let print_transitions_on name pred =
  (* Produce a function that will be linked into the reason parser to recognize
     states at runtime.
     TODO: a more compact encoding could be used, for now we don't care and
     just pattern matches on states.
  *)
  print "let transitions_on_%s = function" name;
  begin match states_transitioning_on pred with
    | [] -> prerr_endline ("no states matches " ^ name ^ " predicate");
    | states ->
      List.iter (fun lr1 -> print "  | %d" (Lr1.to_int lr1)) states;
      print "      -> true"
  end;
  print "  | _ -> false"

let terminal_find name =
  match
    Terminal.fold
      (fun t default -> if Terminal.name t = name then Some t else default)
      None
  with
  | Some term -> term
  | None -> failwith ("Unkown terminal " ^ name)

let () = (
  let lident_term = terminal_find "LIDENT" in
  print_transitions_on "lident" (fun t -> t = T lident_term);
  let uident_term = terminal_find "UIDENT" in
  print_transitions_on "uident" (fun t -> t = T uident_term);
  let semi_term = terminal_find "SEMI" in
  print_transitions_on "semi" (fun t -> t = T semi_term);
)
