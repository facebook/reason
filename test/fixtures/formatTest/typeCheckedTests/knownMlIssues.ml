(* [x] fixed *)
type t2 =
  int * int (* attributed to entire type not binding *)

type color =
  | Red of int (* After red *)
  | Black of int (* After black *)
  | Green of int (* Does not remain here *)


let blahCurriedX x =
  function
  | Red 10
  | Black 20
  | Green 10 -> 1 (* After or pattern green *)
  | Red x -> 0 (* After red *)
  | Black x -> 0 (* After black *)
  | Green x -> 0 (* After second green *)
(* On next line after blahCurriedX def *)


(* EOL comments wrap because other elements break first (in this example
   "mutable" causes breaks. We either need:
   1. To prevent wrapping of anything inside of eol comments attachments.
   2. Losslessly wrap eol comments.
*)
(* This example illustrates the above issue, but isn't een idempotent due to the issue. *)
(* type cfg = { *)
(*   node_id : int ref; *)
(*   node_list : int list ref; *)
(*   name_pdesc_tbl : (int, (int, int) Hashtbl.t) Hashtbl.t;  (** Map proc name to procdesc *) *)
(*   mutable priority_set : (int, int) Hashtbl.t (** set of function names to be analyzed first *) *)
(* } *)
(*  *)
(*  *)
