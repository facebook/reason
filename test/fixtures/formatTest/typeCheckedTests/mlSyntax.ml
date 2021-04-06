(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

(**
 * Testing pattern matching using ml syntax to exercise nesting of cases.
 *)


type xyz =
  | X
  | Y of int * int * int
  | Z of int * int
  | Q
  | R

let doubleBar = function
  | X | Y (_, _, _) | Z (_, _) | Q -> true
  | _ -> false

let doubleBarNested = function
  | X | Y (_, _, _) | (Z (_, _) | Q)  -> true
  | _ -> false


(* Liberal use of the Any pattern being compatible with multiple arguments  *)
let doubleBarAnyPatterns = function
  | X | Y  _ | Z  _ | Q -> true
  | _ -> false

let doubleBarNestedAnyPatterns = function
  | X | Y  _ | (Z  _ | Q)  -> true
  | _ -> false

type bcd = B | C | D | E
type a = A of bcd
let result = match B with
  | B
  | C
  | D
  | E -> ()

let nested_match = function | A (B | C | D | E) -> 3

let some = Some (1, 2, 3)

let (===) = (=)

(* Test regression for https://github.com/facebook/Reason/issues/222 *)
let _ = Pervasives.(=)

let structuralEquality = 1 = 1

let physicalInequality = 1 <> 2

let referentialEquality = 2 == 2

let referentialInequality = 2 != 2

let equalityInIf = if 1 = 1 then true else false

let equalityWithIdentifiers = structuralEquality = referentialEquality

let nestedSome = Some (1, 2, Some (1, 2, 3))

let nestedSomeSimple = Some (Some (1, 2, 3))

module EM = struct
  (** Exception *)
  exception E of int * int
end

exception Ealias = EM.E

let switc = "match"
let switch = "match"
let switch_ = "match"
let pub = "method"
let pub_ = "method"
let pri = "private"
let pri_ = "private"

external private_ : unit -> unit = ""
external pri : unit -> unit = ""

type pub = int
type pub_ = int

