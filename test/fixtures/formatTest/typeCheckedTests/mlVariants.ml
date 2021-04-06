(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

type polyVariantsInMl = [
  | `IntTuple of (int * int)
  | `StillAnIntTuple of (int * int)
]



let intTuple = `IntTuple (1, 2)
let stillAnIntTuple = `StillAnIntTuple (4, 5)
let sumThem = function
  | `IntTuple (x, y) -> x + y
  | `StillAnIntTuple (a, b) -> a + b

type nonrec t = A of int | B of bool

type s = [ `Poly ]

let x = (`Poly: s)

(* There's a bug in ocaml 4.06 resulting in an extra Pexp_constraint on the `Poly,
 * duplicating the core_type.
 * https://caml.inria.fr/mantis/view.php?id=7758
 * https://caml.inria.fr/mantis/view.php?id=7344 *)
let x : s = `Poly
