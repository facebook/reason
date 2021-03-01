[@reason.version 3.7];
Some((1, 2, 3));

type bcd =
  | TupleConstructor((int, int))
  | MultiArgumentsConstructor(int, int);

let a = TupleConstructor((1, 2));
let b =
  [@implicit_arity]
  MultiArgumentsConstructor(1, 2);

module Test = {
  type a =
    | And((int, int))
    | Or((int, int));
};

let _ = Test.And((1, 2));
let _ = Test.Or((1, 2));
let _ = Some(1);

Test.And((1, 2));
Test.Or((1, 2));
Some(1);

module M = {
  type t =
    | TupleConstructorInModule((int, int));
  type t2 =
    | TupleConstructor2((int, int));
  type t3 =
    | TupleConstructor3((int, int));
};

type t2 =
  | TupleConstructor2((int, int));
type t3 =
  | TupleConstructor3((int, int));

let _ = M.TupleConstructorInModule((1, 2));

let _ = M.TupleConstructor2((1, 2));
let _ = TupleConstructor2((1, 2));

let _ = M.TupleConstructor3((1, 2));
let _ = TupleConstructor3((1, 2));

M.TupleConstructorInModule((1, 2));

M.TupleConstructor2((1, 2));
TupleConstructor2((1, 2));

M.TupleConstructor3((1, 2));
TupleConstructor3((1, 2));
