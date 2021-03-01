let (|.) = (x, y) => x + y;

let a = 1;
let b = 2;
let c = 3;

/* parses as 10 < (a->b->c) */
let t1: bool = 10 < a->b->c;

type coordinate = {
  x: int,
  y: int,
};
let coord = { x: 1, y: 1 };

/* parses as (coord.x)->a->b->c */
let t2: int = coord.x->a->b->c;

let (|.) = (x, y) => x || y;

let a = true;
let b = false;
let c = true;

/* parses as !(a->b->c) */
let t3: bool = !a->b->c;

/* parse pipe first with  underscore application correct */
let doStuff = (a: int, b: int, c: int): int => {
  a + 2 * b + 3 * c;
};

let (|.) = (a, f) => f(a);

let t4: int = 5->doStuff(1, _, 7);
let t5: int =
  5->doStuff(1, _, 7)->doStuff(1, _, 7);

module Foo = {
  let createElement = (~children, ()) =>
    List.hd(children) ++ "test";

  let map = (xs, f) => List.map(f, xs);

  let plusOne = x => x + 1;

  let toString = lst =>
    List.fold_left(
      (acc, curr) =>
        acc ++ string_of_int(curr),
      "",
      lst,
    );
};

let items = [1, 2, 3];

let t6: string =
  <Foo>
    {items->Foo.map(Foo.plusOne)->Foo.toString}
  </Foo>;

type saveStatus =
  | Pristine
  | Saved
  | Saving
  | Unsaved;

let saveStatus = Pristine;

let t7: string =
  <Foo>
    {(
       switch (saveStatus) {
       | Pristine => [0]
       | Saved => [1]
       | Saving => [2]
       | Unsaved => [3]
       }
     )
     ->Foo.map(Foo.plusOne)
     ->Foo.toString}
  </Foo>;

let genItems = f => List.map(f, items);

let t8: string =
  <Foo>
    {genItems(Foo.plusOne)->Foo.toString}
  </Foo>;

let blocks = [1, 2, 3];

let t9: string =
  <Foo> blocks->(b => Foo.toString(b)) </Foo>;

let foo = xs => List.concat([xs, xs]);

let t10: string =
  <Foo>
    {blocks
     ->foo
     ->Foo.map(Foo.plusOne)
     ->Foo.toString}
  </Foo>;

let t11: string =
  <Foo>
    {blocks
     ->foo
     ->Foo.map(Foo.plusOne)
     ->Foo.map(Foo.plusOne)
     ->Foo.toString}
  </Foo>;

let title = "los pilares de la tierra";

let t12: string =
  <Foo>
    (title === "" ? [1, 2, 3] : blocks)
    ->Foo.toString
  </Foo>;

type change =
  | Change(list(int));

type this = {send: change => string};

let change = x => Change(x);

let self = {
  send: x =>
    switch (x) {
    | Change(xs) => Foo.toString(xs)
    },
};

let urlToRoute = x => [x, x, x];

let t13: string =
  urlToRoute(1)->change->(self.send);

module FooLabeled = {
  let createElement = (~children, ()) =>
    List.hd(children) ++ "test";

  let map = (xs, ~f) => List.map(f, xs);

  let plusOne = x => x + 1;

  let toString = lst =>
    List.fold_left(
      (acc, curr) =>
        acc ++ string_of_int(curr),
      "",
      lst,
    );
};

let t14: string =
  <FooLabeled>
    {items
     ->FooLabeled.map(~f=FooLabeled.plusOne)
     ->FooLabeled.toString}
  </FooLabeled>;

let c = (a, b) => a + b;
let a = 1;
let b = 2;
let t: int = a->(b->c);

module Div = {
  let createElement = (~children, ()) =>
    List.hd(children) ++ "test";
};

let url = "reason";
let suffix = ".com";

let parse = (a, b) => a ++ b;

let t15: string =
  <Div> {url->parse(suffix, _)} </Div>;
