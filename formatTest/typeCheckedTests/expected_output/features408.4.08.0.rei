[@reason.version 3.7];
module X: {type t;};

module M := X;

[@attr]
module N := X;

type y = int;

type z = int;

type t = int;

type x := y;

[@attr1]
type y := z
[@attr2]
and w := t;

type x' = |
and y' = |;
