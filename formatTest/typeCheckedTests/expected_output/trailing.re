let x = 0;
let y = 0;

[@warning "-8"]
let [|x, y|] = [|0, y|];
[@warning "-8"]
let [|x, y|] = [|0, y|];

[@warning "-8"]
let [|
  reallyLongIdentifier,
  reallyLongIdentifier2,
|] = [|
  0,
  1,
|];
[@warning "-8"]
let [|
  reallyLongIdentifier_,
  reallyLongIdentifier2_,
|] = [|
  0,
  2,
|];

let takesUnit = () => ();
let res = takesUnit();
let wat = 0;

type t = {
  x: int,
  y: int,
};
let p = { contents: 0 };
let { contents: c } = p;
let point = { x: 0, y: 0 };
let point2 = { ...point, y: 200 };

let myTuple = (0, 0);
let (i, j) = myTuple;

type foo_('a, 'b) = ('a, 'b);
type foo__ = foo_(int, int);
type foo('a, 'b) =
  | Foo('a, 'b);
type tupVariant('a, 'b) =
  | Tup(('a, 'b));

/* Won't wrap so removes trailing comma */
let noWrap = (a, b) => {
  let x = a;
  x + x;
};

let res = noWrap(0, 0);
let reallyLongIdentifierCausesWrap = 0;
let wrap = noWrap;
let res =
  wrap(
    reallyLongIdentifierCausesWrap,
    reallyLongIdentifierCausesWrap,
  );

/* Won't wrap so removes trailing comma */
let noWrap = (~a, ~b) => {
  let x = a;
  x + x;
};

/* Won't wrap so removes trailing comma */
let noWrap = (~a=0, ~b=0, ()) => {
  let x = a;
  x + x;
};

let res =
  noWrap(
    ~a=reallyLongIdentifierCausesWrap,
    ~b=reallyLongIdentifierCausesWrap,
    (),
  );

/* Won't wrap so removes trailing comma */
let noWrap = (~a=0, ~b: int=0, ()) => {
  let x = a;
  x + x;
};

let res =
  noWrap(
    ~a=reallyLongIdentifierCausesWrap,
    ~b=reallyLongIdentifierCausesWrap,
    (),
  );

/* Long enough to wrap the args and therefore remove trail */
let wrap = (long, enough, toWrap, args) => {
  let x = long;
  x + enough + toWrap + args;
};

let takesPattern = (d, Foo(x, y)) => {
  /* won't wrap */
  let _ = Foo(y, x);
  /* will wrap */
  let ret =
    Foo(
      y + y + y + y,
      x + x + x + x + x + x + x + x + x,
    );
  ret;
};

let takesPattern = (d, Tup((x, y))) => {
  /* won't wrap */
  let _ = Tup((y, x));
  /* will wrap */
  let ret =
    Tup((
      y + y + y + y,
      x + x + x + x + x + x + x + x + x,
    ));
  ret;
};

let takesPattern =
    (
      d,
      Tup((
        thisPatternIsSoLongThatThe,
        fooWillWrapItself,
      )),
    ) => {
  /* won't wrap */
  let _ = Tup((d, d));
  /* will wrap */
  let ret =
    Tup((
      d + d + d + d,
      d + d + d + d + d + d + d + d + d,
    ));
  ret;
};

let myFunc = (type t, ()) => ();

type funcType = (int, int) => int;
type v =
  | Func((int, int) => int);

type params('a, 'b) = ('a, 'b);

let myList = [2, 3];

let yourList = [5, 6, ...myList];

class virtual
      tupleStack
      (
        'reallyLongIdentifier,
        'anotherReallyLongIdentifier,
      )
      (init, init2) = {
  val mutable v:
    list(
      (
        'reallyLongIdentifier,
        'anotherReallyLongIdentifier,
      ),
    ) = [
    (init, init2),
  ];
  pub virtual implementMe:
    (int, int) => (int, int);
  initializer {
    print_string("initializing object");
  };
};

class extendedStack
      (
        'reallyLongIdentifier,
        'anotherReallyLongIdentifier,
      )
      (init, init2) = {
  inherit
    (
      class tupleStack(
        'reallyLongIdentifier,
        'anotherReallyLongIdentifier,
      )
    )(
      init,
      init2,
    );
  pub implementMe = (i, j) => (i, j);
};

module type HasType = {type t;};
module type HasType2 = {
  type t;
  type q;
};
module type ReallyReallyReallyLongIdentifierModuleType = {
  type t;
};
module type F = (HasType) => HasType2;
module FInstance = (HasType: HasType) => {
  type t = HasType.t;
  type q = HasType.t;
};
module ReallyReallyReallyLongIdentifierModuleName = {
  type t = int;
};
module FResult =
  FInstance(
    ReallyReallyReallyLongIdentifierModuleName,
  );

module Component = {
  let createElement = (~arg, ~children, ()) => [
    0,
  ];
};

let componentList = [<Component arg=1 />];
let componentList = [<Component arg="2" />];
let componentList = [<Component arg=1 />];
let componentList = [<Component arg="2" />];
let componentList = [
  <Component arg=1 />,
  <Component arg=0 />,
];
let componentList = [
  <Component arg="2" />,
  <Component arg=0 />,
];
let componentList = [
  <Component arg=1 />,
  <Component arg=0 />,
];
let componentList = [
  <Component arg="2" />,
  <Component arg=0 />,
];
let componentList = [
  <Component arg=1 />,
  <Component arg=0 />,
];
let componentList = [
  <Component arg="2" />,
  <Component arg=0 />,
];

let componentList = [|<Component arg=1 />|];
let componentList = [|<Component arg="2" />|];
let componentList = [|<Component arg=1 />|];
let componentList = [|<Component arg="2" />|];
let componentList = [|
  <Component arg=1 />,
  <Component arg=0 />,
|];
let componentList = [|
  <Component arg="2" />,
  <Component arg=0 />,
|];
let componentList = [|
  <Component arg=1 />,
  <Component arg=0 />,
|];
let componentList = [|
  <Component arg="2" />,
  <Component arg=0 />,
|];
let componentList = [|
  <Component arg=1 />,
  <Component arg=0 />,
|];
let componentList = [|
  <Component arg="2" />,
  <Component arg=0 />,
|];
