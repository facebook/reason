(* **** comment *)
(*** comment *)
(** docstring *)
(* comment *)
(** docstring *)
(*** comment *)
(**** comment *)
(***** comment *)

(*** *)
(**** *)

(***)
(****)



(* (** comment *) *)
(* (*** comment *) *)
(* *(*** comment *) *)

(* comment **)
(* comment ***)
(* comment ****)
(* comment *****)

let testingNotQuiteEndOfLineComments = [
  "Item 1"(* Comment For First Item *);
  "Item 2" (* Comment For Second Item *);
  "Item 3" (* Comment For Third Item *) ;
  "Item 4" (* Comment For Fourth Item - but no semi *)
  (* Comment after last item in list. *)
] (* Comment after list bracket *)

let testingEndOfLineComments = [
  "Item 1";(* Comment For First Item *)
  "Item 2"; (* Comment For Second Item *)
  "Item 3";  (* Comment For Third Item *)
  "Item 4" (* Comment For Fourth Item - but before semi *);
  (* Comment after last item in list. *)
] (* Comment after list bracket *)

(* This time no space between bracket and comment *)
let testingEndOfLineComments = [
](* Comment after list bracket *)


type t = int * int (* End of line on t *)

type t22 = (* End of t22 line on type t22 = *)
  int * int


type variant =
  (* Comment above X *)
  | X of int  (* End of line on X *)
  (* Comment above Y *)
  | Y of int  (* End of line on Y *)
(* Comment on entire type def for variant *)


type x = { (* not attached *above* x *)
  fieldOne : int
} (* Attached end of line after x *)
and y = { (* not attached *above* y *)
  fieldTwo : int
} (* Attached end of line after y *)


let result = match X 3 with
  | X x -> (* Where does this comment go? *)
    let tmp = x in
    x + tmp
  | Y x ->
    (* How about this one *)
    let tmp = x in
    x + tmp

let result = match None with
  | Some {fieldOne = 20} -> (* Where does this comment go? *)
    let tmp = 0 in
    2 + tmp
  | Some {fieldOne = n} ->
    (* How about this one *)
    let tmp = n in
    n + tmp
  | None -> 20

type pointWithManyKindsOfComments = {
  (* Line before x *)
  x: string; (* x field *)
  (* Line before y *)
  y: string; (* y field *)
  (* Final row of record *)
}

type 'a typeParamPointWithComments = {
  (* Line before x *)
  x: 'a; (* x field *)
  (* Line before y *)
  y: 'a (* y field *)
  (* Final row of record *)
}


let name_equal x y = x = y

let equal i1 i2 =
  i1.contents == i2.contents && true (* most unlikely first *)

let equal i1 i2 =
  compare (compare 0 0) (compare 1 1) (* END OF LINE HERE *)



module Temp = struct
  let v = true
  let logIt str () = print_string str
end

let store_attributes arg =
  let attributes_file = "test" in
  let proc_name = attributes_file ^ ".proc" in
  let should_write = (* only overwrite defined procedures *)
    Temp.v ||
    not (Temp.v) in
  if should_write then
    Temp.logIt proc_name ()
