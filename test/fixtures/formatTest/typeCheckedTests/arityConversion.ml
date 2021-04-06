Some (1, 2, 3)

type bcd = TupleConstructor of (int * int) | MultiArgumentsConstructor of int * int

let a = TupleConstructor(1, 2)
let b = MultiArgumentsConstructor(1, 2)

module Test = struct
  type a = | And of (int * int) | Or of (int * int)
end;;

let _ = Test.And (1, 2)
let _ = Test.Or (1, 2)
let _ = Some 1;;

Test.And (1, 2);;
Test.Or (1, 2);;
Some 1;;

module M = struct
  type t = TupleConstructorInModule of (int * int)
  type t2 = TupleConstructor2 of (int * int)
  type t3 = TupleConstructor3 of (int * int)
end

type t2 = TupleConstructor2 of (int * int)
type t3 = TupleConstructor3 of (int * int)

let _ = M.TupleConstructorInModule (1,2)

let _ = M.TupleConstructor2 (1,2)
let _ = TupleConstructor2 (1,2)

let _ = M.TupleConstructor3 (1,2)
let _ = TupleConstructor3 (1,2);;

M.TupleConstructorInModule (1,2);;

M.TupleConstructor2 (1,2);;
TupleConstructor2 (1,2);;

M.TupleConstructor3 (1,2);;
TupleConstructor3 (1,2);;
