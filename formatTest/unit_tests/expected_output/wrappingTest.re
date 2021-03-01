[@reason.version 3.7];
/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

/* Run the formatting pretty printer with width 50 */

/*
 * Testing infix wrapping
 */
let reallyLongIdent = 100;
let andYetAnotherReallyLongIdent = 30;

let something =
  reallyLongIdent
  + andYetAnotherReallyLongIdent
  + reallyLongIdent;

let something =
  /* Hopefully */
  reallyLongIdent
  /* It will indent like this */
  + andYetAnotherReallyLongIdent
  /* And no further */
  + reallyLongIdent;

/* Comments can be written like this.
   No leading star is required on each line.
   Everything will line up just fine.
   In this form, include the final closing on the last line. */
let test = 10;

/*     You could begin the block bar out like this.
       And it still works correctly. */
let test = 10;

/** Include multiple opening stars if you like.
    And it will still work. */
let test = 10;

/** This comment will be corrected.
      when printed. */
let test = 10;

/**  Comments with text on line zero
 *   Still work well with comments that have stars on the left side.
 */
let test = 10;

/*
 * Even though the precedence of the operators are different, no
 * "simplification" grouping is needed.
 */
let testPrintingPrecedence =
  reallyLongIdent
  + reallyLongIdent
  * andYetAnotherReallyLongIdent
  + reallyLongIdent;

let testPrintingPrecedence =
  reallyLongIdent
  /*
   * In this case, grouping of the right expression is needed because the
   * right side of the infix operator is of *lower* precedence than STAR.
   */
  + reallyLongIdent
  * (
    reallyLongIdent
    + andYetAnotherReallyLongIdent
  )
  + reallyLongIdent
  * 10;

let testPrintingPrecedence =
  reallyLongIdent
  /*
   * In this case, grouping of the right expression is needed because the
   * right side of the infix operator is of *lower* precedence than STAR.
   */
  + reallyLongIdent
  * (
    reallyLongIdent
    + andYetAnotherReallyLongIdent
  )
  + reallyLongIdent;

let add = (x, y) => x + y;
let testPrintingPrecedence =
  reallyLongIdent
  /*
   * In this case, grouping of the right expression is needed because the
   * right side isn't even infix at all.
   */
  + reallyLongIdent
  * add(
      reallyLongIdent,
      andYetAnotherReallyLongIdent,
    )
  + reallyLongIdent;
/*
 * Test wrapping every form of named arguments where various parts are
 * commented.
 */
let a = 10;
let b = 20;
/*A*/
let named =
    /* a::a */
    (
      ~a,
      /* b::b */
      ~b,
    ) =>
  /* a + b */
  a + b;

/*B*/
let namedAlias =
    /* a::aa */
    (
      ~a as aa,
      /* b::bb */
      ~b as bb,
    ) =>
  /* aa + bb */
  aa + bb;

/*C*/
let namedAnnot =
    /* ~a a: option(int) */
    (
      ~a: option(int),
      /* ~b b: option(int) */
      ~b: option(int),
    ) =>
  /* 20 */
  20;

/*D*/
let namedAliasAnnot =
    /* a::(aa: option int) */
    (
      ~a as aa: option(int),
      /* b::(bb: option int) */
      ~b as bb: option(int),
    ) =>
  /* 20 */
  20;

/*E*/
let optional =
    /* a::a=? */
    (
      ~a=?,
      /* b::b=? */
      ~b=?,
      /* () */
      (),
    ) =>
  /* 10 */
  10;

/*F*/
let optionalAlias =
    /* a::aa */
    (
      ~a as aa=?,
      /* ?b:bb */
      ~b as bb=?,
      /* () */
      (),
    ) =>
  /* 10 */
  10;

/*G*/
let optionalAnnot =
    /* a::(a: option int)=? */
    (
      ~a: option(int)=?,
      /* ?b:(b: option int) */
      ~b: option(int)=?,
      /* () */
      (),
    ) =>
  /* 10 */
  10;

/*H*/
let optionalAliasAnnot =
    /* a::(aa: option int)=? */
    (
      ~a as aa: option(int)=?,
      /* b::(bb: option int)=? */
      ~b as bb: option(int)=?,
      /* () = */
      (),
    ) =>
  /* 10 */
  10;
/*I: This one is really annoying? Where's the visual label?*/
let defOptional =
    /* a::a=10 */
    (
      ~a=10,
      /* b::b=10 */
      ~b=10,
      /* () = */
      (),
    ) =>
  /* 10 */
  10;

/*J*/
let defOptionalAlias =
    /* a::aa=10 */
    (
      ~a as aa=10,
      /* b::bb=10 */
      ~b as bb=10,
      /* () = */
      (),
    ) =>
  /* 10; */
  10;

/*K*/
let defOptionalAnnot =
    /* a::(a:int)=10 */
    (
      ~a: int=10,
      /* b::(b:int)=10 */
      ~b: int=10,
      /* () = */
      (),
    ) =>
  /* 10; */
  10;

/*L*/
let defOptionalAliasAnnot =
    /* a::(aa:int)=10 */
    (
      ~a as aa: int=10,
      /* b::(bb:int)=10 */
      ~b as bb: int=10,
      /* () = */
      (),
    ) =>
  /* 10; */
  10;

/* Invoking them */
named(
  /* a::a */
  ~a,
  /* b::b; */
  ~b,
);

named(
  /* a::a */
  ~a,
  /* b::b; */
  ~b,
);

optional(
  /* a::a */
  ~a,
  /* b::b; */
  ~b,
);
optional(
  /* a::a */
  ~a,
  /* b::b; */
  ~b,
);
let explictlyPassed =
  /* optional */
  optional(
    /* a::? */
    /* None */
    ~a=?None,
    /* b::? */
    /* None; */
    ~b=?None,
  );

let a = None;
let explictlyPassed =
  /* optional */
  optional(
    /* a::? */
    ~a?,
    /* b::? */
    /* None; */
    ~b=?None,
  );

let complex_default =
    (~callback=(k, d) => 4, x) => 3;

let myList = /*CommentAfterEqualBeforeList */ [
  1,
  2,
  3,
];
let myList = [
  /*CommentAfterEqualBefore1 */ 1,
  2,
  3,
];
let myList = [
  1 /*CommentAfterOneBeforeCons */,
  2,
  3,
];
let myList = [
  1,
  2 /*CommentAfterTwoBeforeCons */,
  3,
];
let myList = [
  1,
  2,
  /*CommentAfterConsBeforeThree */ 3,
];
let myList = [
  1,
  2,
  3 /*CommentAfterThreeBeforeCons*/,
];

let myList = [
  1,
  2,
  3 /*same w space after three    */,
];
let myList = [
  1,
  2,
  3 /*same w space before rbracket*/,
];
let myList = [
  1,
  2,
  3 /*same w both                 */,
];

/* End of line comments */
let myList = [
  1,
  2,
  3 /*no space after three    */
];
let myList = [
  1,
  2,
  3 /*same w space after three    */
];
let myList = [
  1,
  2, /*no space after two comma    */
  3,
];
let myList = [
  1,
  2, /*same w space after two comma    */
  3,
];

/* End of line comments */
let myList = [
  1,
  2, /*no space after two comma    */
  3,
];
let myList = [
  1,
  2, /*same w space after two comma    */
  3,
];
let myRec = {
  x: 1,
  y: 2, /*no space after two    */
  z: 3,
};
let myRec = {
  x: 1,
  y: 2, /*same w space after two    */
  z: 3,
};

/* Ensure end of line comments force breaks */
let myList = [
  1,
  2,
  3 /* */
];
let myList = [1, 2, /**/ 3];

let myList = [
  1,
  2,
  3 /*CommentAfterConsBeforeAppendedTo */,
  ...myList,
];
let myList = [3, 4, 5];

let simpleListPattern = x =>
  switch (x) {
  | [1, 2, 3] => 0
  | _ => 0
  };

type blahType = string;

let x: blahType = "asdf";

type nameAge = {
  age: int,
  name: string,
};

type hasABunch = {
  /*
   * Field comment
   */
  fieldOne: int,
  fieldtwo: list(int),
  fieldThree: list(string),
  fieldFour: nameAge,
  /* Comment at bottom of record type def */
};

type functionsInARecord = {
  adder: int => int,
  minuser: int => int,
};

let myFunctionsInARecord = {
  adder: x => x,
  minuser: x => x,
};

let myFunctionsInARecordThatMustWrap = {
  /* Desired wrapping */
  adder: reallyLongArgument => reallyLongArgument,
  minuser: anotherReallyLongArgument => anotherReallyLongArgument,
  /* Comment at bottom of record */
};

type twoArgFunctionsInARecord = {
  adder: (int, int) => int,
  minuser: (int, int) => int,
};

let myFunctionsInARecordThatMustWrap = {
  /* Desired wrapping */
  adder:
    (
      reallyLongArgument,
      anotherReallyLongArgument,
    ) => reallyLongArgument,
  minuser:
    (
      reallyLongArgument,
      anotherReallyLongArgument,
    ) =>
    reallyLongArgument
    + anotherReallyLongArgument,
};

type threeArgFunctionsInARecord = {
  adder: (int, int, int) => int,
  minuser: (int, int, int) => int,
};

let myFunctionsInARecordThatMustWrap = {
  /* Desired wrapping */
  adder:
    /* Even if you have a comment before fun */
    (
      reallyLongArgument,
      /* Or before the first arg */
      anotherReallyLongArgument,
      yetAnotherReallyLongArgument,
    ) => reallyLongArgument,
  minuser:
    (
      reallyLongArgument,
      anotherReallyLongArgument,
      anotherReallyLongArgument,
    ) =>
    reallyLongArgument
    + anotherReallyLongArgument,
};

let oneArgShouldWrapToAlignWith =
    theFunctionNameBinding => theFunctionNameBinding;

let twoArgsShouldWrapToAlignWith =
    (firstArgHere, secondArgThere) => secondArgThere;

let rec oneArgShouldWrapToAlignWith =
        theFunctionNameBinding => theFunctionNameBinding;

let rec twoArgsShouldWrapToAlignWith =
        (firstArgHere, secondArgThere) => secondArgThere;

let secondArgShouldWrap =
    (pointLess, (a, b, c, d, e, f, g, h)) =>
  pointLess + a + b + c + d + e;

/* Now check that one and two args both indent the same when applying */
let reallyReallyLongVarName = "hello";
let result =
  oneArgShouldWrapToAlignWith(
    reallyReallyLongVarName,
  );
let result =
  twoArgsShouldWrapToAlignWith(
    reallyReallyLongVarName,
    reallyReallyLongVarName,
  );

let justReturn = x => x;

/* With default formatting settings: Two arguments are special cased in
   function application "justReturn hasABunch" */
let acceptsTwoThings =
    (nameAge: nameAge, hasABunch: hasABunch) =>
  justReturn(hasABunch);

/*
  Ideally, we'd allow "acceptsTwoThings {age, name}" on the first line, then
  wrapping the final argument across multiple, but that is difficult to tell
  the formatter "if the final argument cannot fit", but everything else can,
  then only wrap the final argument with open faced braces.  It's possible, but
  not a v1 feature of wrapping.
 */
let result =
  acceptsTwoThings(
    {age: 20, name: "a"},
    {
      fieldOne: 10,
      fieldtwo: [10, 20],
      fieldThree: ["one", "two"],
      fieldFour: {
        age: 20,
        name: "joe",
      },
    },
  );

let howDoesInfixOperatorsWrapWhenYouMustWrapQuestionMark =
    (x, y, z) =>
  x + y + z;
let howDoesInfixOperatorsWrapWhenYouMustWrapQuestionMark =
    (x, y) =>
  x + y;
let reallyHowDoesInfixOperatorsWrapWhenYouMustWrapQuestionMark =
    (x, y, z) =>
  x + y + z;
let reallyHowDoesInfixOperatorsWrapWhenYouMustWrapQuestionMark =
    (x, y) =>
  x + y;

let reallyLongFunctionNameThatJustConcats = a =>
  String.concat("-", a);

let seeHowLongValuesWrap = {
  age: 30,
  name:
    reallyLongFunctionNameThatJustConcats([
      "one",
      "two",
      "two",
      "two",
      "two",
      "two",
      "two",
    ]),
};

/*
 /--Everything up to the arrow is label left--\  /-The return is label right-\
                           /-append => to last-\
 /-----------------------\ /--------------------\ */
let onlyReturnWraps = ((a, b, c, d, e, f)) => (
  a,
  b,
  c,
  d,
  e,
  f,
);

let bothArgsWrapAndIndent =
    ((a, b, c, d, e, f), (h, i, j, k, l, m)) => (
  a,
  b,
  c,
  d,
  e,
  f,
);

let result =
  onlyReturnWraps((10, 11, 12, 13, 14, 15));

let result =
  bothArgsWrapAndIndent(
    (10, 11, 12, 13, 14, 15),
    (10, 11, 12, 13, 14, 15),
  );

type sixteenTuple = (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
);

/* Nothing annotated */
let echoTuple =
    (
      (
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h,
        i,
        j,
        k,
        l,
        m,
        n,
        o,
        p,
      ),
    ) => (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  l,
  m,
  n,
  o,
  p,
);

/* Nothing annotated fun */
let echoTuple =
    (
      (
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h,
        i,
        j,
        k,
        l,
        m,
        n,
        o,
        p,
      ),
    ) => (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  l,
  m,
  n,
  o,
  p,
);

let echoTheEchoer =
    (x: sixteenTuple => sixteenTuple)
    : (sixteenTuple => sixteenTuple) => x;

/* Nothing annotated fun, passed to func */
echoTheEchoer(
  (
    (
      a,
      b,
      c,
      d,
      e,
      f,
      g,
      h,
      i,
      j,
      k,
      l,
      m,
      n,
      o,
      p,
    ),
  ) =>
  (
    a,
    b,
    c,
    d,
    e,
    f,
    g,
    h,
    i,
    j,
    k,
    l,
    m,
    n,
    o,
    p,
  )
);

/* Argument annotated */
let echoTuple =
    (
      (
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h,
        i,
        j,
        k,
        l,
        m,
        n,
        o,
        p,
      ): sixteenTuple,
    ) => (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  l,
  m,
  n,
  o,
  p,
);

/* Argument annotated fun */
let echoTuple =
    (
      (
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h,
        i,
        j,
        k,
        l,
        m,
        n,
        o,
        p,
      ): sixteenTuple,
    ) => (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  l,
  m,
  n,
  o,
  p,
);

/* Argument annotated, return type annotated */
let echoTuple =
    (
      (
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h,
        i,
        j,
        k,
        l,
        m,
        n,
        o,
        p,
      ): sixteenTuple,
    )
    : sixteenTuple => (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  l,
  m,
  n,
  o,
  p,
);

/* Desired formatting if first line fits within margin */
let makeTuple =
    (
      a,
      b,
      c,
      d,
      e,
      f,
      g,
      h,
      i,
      j,
      k,
      l,
      m,
      n,
      o,
      p,
    ) => (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  l,
  m,
  n,
  o,
  p,
);

/* Desired formatting if first line fits within margin (70) */
let (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  l,
  m,
  n,
  o,
  p,
) =
  makeTuple(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  );
/* Annotated version */
let (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  l,
  m,
  n,
  o,
  p,
): sixteenTuple =
  makeTuple(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  );
/* Annotated inline */
let x: (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) =
  makeTuple(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  );

let (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  l,
  m,
  n,
  o,
  p,
) =
  echoTuple((
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  ));
/* Annotated version */
let (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  l,
  m,
  n,
  o,
  p,
): sixteenTuple =
  echoTuple((
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  ));
/* Annotated inline */
let x: (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) =
  echoTuple((
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  ));

/* Desired formatting if pattern does not fit, arguments do (margin 70) */

/* Destructured */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
) =
  makeTuple(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  );
/* Annotated */
/* Destructured */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
): sixteenTuple =
  makeTuple(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  );

/* Annotated */
/* Destructured */
/* Inline */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
): (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) =
  makeTuple(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  );

/* Not-Destructured */
let someResult =
  makeTuple(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  );
/* Annotated */
/* Not-Destructured */
let someResult: sixteenTuple =
  makeTuple(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  );
/* Annotated */
/* Not-Destructured */
/* Inline */
let someResult: (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) =
  makeTuple(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  );

/* Destructured */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
) =
  echoTuple((
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  ));
/* Annotated */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
): sixteenTuple =
  echoTuple((
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  ));
/* Annotated Inline */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
): (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) =
  echoTuple((
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  ));
/* Not-Destructured */
let someResult =
  echoTuple((
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  ));
/* Annotated */
/* Not-Destructured */
let someResult: sixteenTuple =
  echoTuple((
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  ));
/* Annotated Inline */
/* Not-Destructured */
let someResult: (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) =
  echoTuple((
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  ));

/* Desired formatting if neither fit on one line (margin 70) */
/* Destructured */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
) =
  makeTuple(
    axx,
    bxx,
    cxx,
    dxx,
    exx,
    fxx,
    gxx,
    hxx,
    ixx,
    jxx,
    kxx,
    lxx,
    mxx,
    nxx,
    oxx,
    pxx,
  );
/* Annoted */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
): sixteenTuple =
  makeTuple(
    axx,
    bxx,
    cxx,
    dxx,
    exx,
    fxx,
    gxx,
    hxx,
    ixx,
    jxx,
    kxx,
    lxx,
    mxx,
    nxx,
    oxx,
    pxx,
  );
/* Annoted inline */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
): (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) =
  makeTuple(
    axx,
    bxx,
    cxx,
    dxx,
    exx,
    fxx,
    gxx,
    hxx,
    ixx,
    jxx,
    kxx,
    lxx,
    mxx,
    nxx,
    oxx,
    pxx,
  );
/* Not-Destructured */
let someResult =
  makeTuple(
    axx,
    bxx,
    cxx,
    dxx,
    exx,
    fxx,
    gxx,
    hxx,
    ixx,
    jxx,
    kxx,
    lxx,
    mxx,
    nxx,
    oxx,
    pxx,
  );
/* Not-Destructured */
/* Annoted */
let someResult: sixteenTuple =
  makeTuple(
    axx,
    bxx,
    cxx,
    dxx,
    exx,
    fxx,
    gxx,
    hxx,
    ixx,
    jxx,
    kxx,
    lxx,
    mxx,
    nxx,
    oxx,
    pxx,
  );
/* Not-Destructured */
/* Annoted inline */
let someResult: (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) =
  makeTuple(
    axx,
    bxx,
    cxx,
    dxx,
    exx,
    fxx,
    gxx,
    hxx,
    ixx,
    jxx,
    kxx,
    lxx,
    mxx,
    nxx,
    oxx,
    pxx,
  );

/* Desired formatting if neither fit on one line (margin 70) */
/* Destructured */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
) =
  echoTuple((
    1000,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
  ));
/* Annoted */
/* Destructured */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
): sixteenTuple =
  echoTuple((
    1000,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
  ));
/* Annoted Inline */
/* Destructured */
let (
  axx,
  bxx,
  cxx,
  dxx,
  exx,
  fxx,
  gxx,
  hxx,
  ixx,
  jxx,
  kxx,
  lxx,
  mxx,
  nxx,
  oxx,
  pxx,
): (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) =
  echoTuple((
    1000,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
  ));

/* Desired formatting if neither fit on one line (margin 70) */
/* Not-Destructured */
let someResult =
  echoTuple((
    1000,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
  ));
/* Annoted */
/* Not-Destructured */
let someResult: sixteenTuple =
  echoTuple((
    1000,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
  ));
/* Annoted Inline */
/* Not-Destructured */
let someResult: (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) =
  echoTuple((
    1000,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
  ));

/* The rhs of = shouldn't be broken onto its own newline: @see ensureSingleTokenSticksToLabel */
let someResult: (
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
  int,
) = someResult;

type sevenStrings = (
  string,
  string,
  string,
  string,
  string,
  string,
  string,
);
let (only, the, type_, should, have, to_, wrap) = (
  "only",
  "the",
  "type",
  "should",
  "have",
  "to",
  "wrap",
);

let (only, the, type_, should, have, to_, wrap): sevenStrings = (
  "only",
  "the",
  "type",
  "should",
  "have",
  "to",
  "wrap",
);

let ifTheNameIsReallyLongTheTypeAndValueShouldBothWrap: (
  string,
  string,
  string,
  string,
  string,
  string,
  string,
) = (
  "only",
  "the",
  "type",
  "should",
  "have",
  "to",
  "wrap",
);
let (
  the,
  type_,
  and_,
  value,
  should,
  both,
  wrap,
): (
  string,
  string,
  string,
  string,
  string,
  string,
  string,
) = (
  "but",
  "the",
  "destructured",
  "assignment",
  "should",
  "not",
  "wrap",
);

let myPolyFunc: 'a. 'a => 'a = o => o;
let myNonPolyFunc: 'a => 'a = o => o;

let locallyAbstractFunc = (type a, input: a) => input;
let locallyAbstractFuncNotSugared =
    (type a, input: a) => input;
let locallyAbstractFuncAnnotated: type a. a => a =
  (type a, input: a) => input;

/*
  Examples of how long versions of these should be wrapped: df stands for
  "desired formatting" when the function binding itself must wrap.
 */
let df_myPolyFunc: 'a. 'a => 'a = o => o;
let df_myNonPolyFunc: 'a => 'a = o => o;

type nameBlahType = {nameBlah: int};

let myFunc = (~firstArg, ~another, ~fl) => {
  nameBlah: 10,
};

type inputEchoRecord('a) = {inputIs: 'a};
let df_locallyAbstractFunc =
    (type a, type b, input: a) => {
  inputIs: input,
}; /* With setting ReturnValOnSameLine */

let df_locallyAbstractFuncNotSugared =
    (type a, type b, input: a) => {
  inputIs: input,
};

/**
 * The following is automatically expanded at the parser level into:
 *
 *   let df_locallyAbstractFuncAnnotated:
 *     'a .
 *     'a => 'a => inputEchoRecord 'a
 *    =
 *     fun (type a) => (
 *       fun (input: a) (input: a) => {inputIs: input}:
 *         a => a => inputEchoRecord a
 *     );
 *
 */
let df_locallyAbstractFuncAnnotated:
  type a. (a, a) => inputEchoRecord(a) =
  (input: a, input: a) => {inputIs: input};

/**
 * The following is automatically expanded at the parser level into:
 *
 *   let df_locallyAbstractFuncAnnotated:
 *     'a .
 *     'a => 'a => inputEchoRecord 'a
 *    =
 *     fun (type a) => (
 *       df_locallyAbstractFuncAnnotated:
 *         a => a => inputEchoRecord a
 *     );
 *
 */
let df_locallyAbstractFuncAnnotatedRef:
  type a. (a, a) => inputEchoRecord(a) = df_locallyAbstractFuncAnnotated;

/**
 * Doesn't do what you want:
 *
 *  let df_locallyAbstractFuncAnnotatedExtra: type a. a => a => inputEchoRecord a =
 *    fun (type a)
 *        (input:a)
 *        (input:a) => {
 *      inputIs: input
 *    };
 */

/**
 * The following is automatically expanded at the parser level into:
 *
 *   let df_locallyAbstractFuncAnnotatedTwo:
 *     'a 'b .
 *     'a => 'b => (inputEchoRecord 'a, inputEchoRecord 'b)
 *    =
 *     fun (type a) (type b) => (
 *       fun (input: a) (input2: b) => ({inputIs: input}, {inputIs:input2}):
 *         a => b => (inputEchoRecord a, inputEchoRecord b)
 *     );
 *
 */
let df_locallyAbstractFuncAnnotated:
  type a b.
    (a, b) =>
    (inputEchoRecord(a), inputEchoRecord(b)) =
  (input: a, input2: b) => (
    {inputIs: input},
    {inputIs: input2},
  );

/**
 * This case shows why inferring what was originally sugar type a b . blahblah
 * is not so trivial. We have to take the last Pexp_constraint type, varify the
 * constructors, then check if the result is equal to the first
 * Ppat_constraint. In this case, they're not equal!
 */
let df_locallyAbstractFuncAnnotated: 'figureMeOut =
  (type a, type b) => (
    (input: a, input2: b) => (
      {inputIs: input},
      {inputIs: input2},
    ):
      (a, b) =>
      (inputEchoRecord(a), inputEchoRecord(b))
  );

let createTuple_thisFuncShouldWrapCorrectlyNow:
  'a.
  ('a, 'a, 'a) => ('a, 'a, 'a)
 =
  (someVar, someVar2, someVar3) => (
    someVar,
    someVar2,
    someVar3,
  );

let theTupleTypeAnnotationShouldWrap: (
  string,
  string,
  string,
  string,
) = (
  "now these tuple values should wrap",
  "now these tuple values should wrap",
  "now these tuple values should wrap",
  "now these tuple values should wrap",
);

let rec mutuallyRecursiveOne = x =>
  mutuallyRecursiveTwo(x + x)
and mutuallyRecursiveTwo = y => print_int(y);

/* The only downside to this is that now you can't redeclare a binding. */
/* let newMutualRecursionSyntax x => newMutuallyRecursiveTwo (x + x); */
/* let newMutuallyRecursiveTwo y => print_int y; */
/*  */

type x = pri int;
type y = x = ..;
type myType('a, 'b, 'c) = pri ('a, 'b, 'c);

type privateVariant =
  pri | BigSize(int) | SmallSize(int);

type doubleEqualsDoublePrivateVariant =
  privateVariant =
    pri | BigSize(int) | SmallSize(int);

type myRecordWithReallyLongName = {
  xx: int,
  yy: int,
};
type doubleEqualsRecord =
  myRecordWithReallyLongName = {
    xx: int,
    yy: int,
  };
type doubleEqualsDoublePrivateRecord =
  myRecordWithReallyLongName =
    pri {
      xx: int,
      yy: int,
    };

type someConstructor =
  | SomeConstructorHi(int, int);
type someRecord = {
  firstFieldInRecord: int,
  secondField: int,
};

/*
   With settings.functionBindingStyle = AttachFirstTermToLabelIffTwoTotalTerms,
   the binding name becomes part of the label when there are only two total
   terms in the binding/argument pattern list (the name, followed by one
   pattern).
 */
let funcOnSomeConstructorHi =
    (SomeConstructorHi(x, y)) =>
  x + y;

let funcOnSomeConstructorHi =
    (SomeConstructorHi(x, y), secondArg) =>
  x + y;

/* With two args */
let funcOnSomeRecord =
    ({firstFieldInRecord, secondField}) =>
  firstFieldInRecord + secondField;

let funcOnSomeRecord =
    (
      {firstFieldInRecord, secondField},
      secondArg,
    ) =>
  firstFieldInRecord + secondField;

/*
   With settings.functionBindingStyle = DontAttachFirstTermToLabel,
   the binding name becomes part of the label when there are only two total
   terms in the binding/argument pattern list (the name, followed by one
   pattern).
 */
let funcOnSomeConstructorHi =
    (SomeConstructorHi(x, y)) =>
  x + y;

let funcOnSomeRecord =
    ({firstFieldInRecord, secondField}) =>
  firstFieldInRecord + secondField;

/* With two args */
let funcOnSomeConstructorHi =
    (SomeConstructorHi(x, y), secondArg) =>
  x + y;

let funcOnSomeRecord =
    (
      {firstFieldInRecord, secondField},
      secondArg,
    ) =>
  firstFieldInRecord + secondField;

type simpleTupleVariant =
  | SimpleActuallyATuple((int, int));

let returnTheSimpleTupleVariant = i =>
  SimpleActuallyATuple(i, i);

let shouldWrapLike = whenLongArg =>
  SimpleActuallyATuple(
    whenLongArg,
    whenLongArg,
  );

type recordWithLong = {
  someField: int,
  anotherField: string,
};

/*
 * Commenting first of two mutualy recursive types.
 */
type recursiveType =
  /* First variant of first mutually recursive */
  | Blah
  /* Second variant of first mutually recursive */
  | Another(option(anotherRecursiveType))

/*
 * Commenting second of two mutually recursive types.
 */
and anotherRecursiveType =
  /* Second variant of second mutually recursive */
  | Baz
  /* Second variant of second mutually recursive */
  | Recursive(option(recursiveType));

/**
 * Commented GADT definition.
 */
type term(_) =
  /* First variant leaf of GADT */
  | Int /*first var arg */(int)
    : /* First GADT res */ term(int)
  /* Second variant leaf of GADT */
  | Float /*second var arg */(int)
    : /* Second GADT res */ term(int)
  /* Third variant leaf of GADT */
  | Bool /*third var arg */(int)
    : /* Third GADT res */ term(int);

/* Commented colors */
type commentedTypeDef =
  /*
   * Commenting first variant member.
   */
  | First(
      (
        /* First field of tuple in first variant member */
        int,
        /* Second field of tuple in first variant member */
        int,
      ),
    )
  /*
   * Commenting second variant member.
   */
  | Second(int)
  /*
   * Commenting third variant member.
   */
  | Third(
      list
        /* Commenting deep in type def */
        (list(int)),
    );

type colors =
  | Red(int)
  | Black(int)
  | Green(int);

let blah = arg =>
  switch (arg) {
  /* Comment before Bar */
  | /* Comment between bar/pattern */ Red(_) => 1
  /* Comment Before non-first bar */
  | /* Comment betwen bar/pattern */ Black(_) => 0
  | Green(_) => 0
  };

let blah =
  fun
  | Red(_) => 1
  | Black(_) => 0
  | Green(_) => 1;

let blahCurriedX = x =>
  fun
  /* Comment before first bar */
  /* Comment between first bar and OR pattern */
  | Red(x)
  | Black(x)
  | Green(x) => 1
  /* Comment before second bar */
  | Black(x) => 0
  | Green(x) => 0;

type reallyLongVariantNames =
  | ReallyLongVariantName(recordWithLong)
  | AnotherReallyLongVariantName(int, int, int)
  | AnotherReallyLongVariantName2(
      int,
      int,
      int,
    );

let howDoLongMultiBarPatternsWrap = x =>
  switch (x) {
  | AnotherReallyLongVariantName(_, _, _) => 0
  | AnotherReallyLongVariantName2(_, _, _) => 0
  | ReallyLongVariantName({
      someField,
      anotherField,
    }) => 0
  };

let letsCombineTwoLongPatternsIntoOneCase = x =>
  switch (x) {
  | AnotherReallyLongVariantName(_, _, _)
  | AnotherReallyLongVariantName2(_, _, _) => 0
  | ReallyLongVariantName({
      someField,
      anotherField,
    }) => 0
  };

let letsPutAWhereClauseOnTheFirstTwo = x =>
  switch (x) {
  | AnotherReallyLongVariantName(_, _, _)
  | AnotherReallyLongVariantName2(_, _, _)
      when true => 0
  | ReallyLongVariantName({
      someField,
      anotherField,
    }) => 0
  };

let letsPutAWhereClauseOnTheLast = x =>
  switch (x) {
  | AnotherReallyLongVariantName(_, _, _)
  | AnotherReallyLongVariantName2(_, _, _) => 0
  | ReallyLongVariantName({
      someField,
      anotherField,
    })
      when true => 0
  };

type wrappingGadt(_) =
  | ThisIsLongSoTypeWillWrap(int)
    : wrappingGadt(int)
  | Add: wrappingGadt((int, int) => int)
  | App(
      wrappingGadt('b => 'a),
      wrappingGadt('b),
    )
    : wrappingGadt('a);

type withThreeFields = {
  name: string,
  age: int,
  occupation: string,
};

let testRecord = {
  name: "joe",
  age: 20,
  occupation: "engineer",
};
let anotherRecord = {
  ...testRecord,
  name: "joe++",
  age: testRecord.age + 10,
};

type polymorphicCommentedType
  /* Commenting the first type variable */
  (
    'a,
    /* Commenting the second type variable */
    'b,
  ) =
  list('a, 'b);

/**
 * Commenting the entire record definition.
 */
type withThreeFieldsCommented = {
  /* Commenting the first field */
  nameCommented: string,
  /* Commenting the second field */
  ageCommented: int,
  /* Commenting the third field */
  occupationCommented: string,
};

/**
 * Commenting the entire record.
 */
let testRecordCommented = {
  /* Commenting the first field */
  nameCommented: "joe",
  /* Commenting the second field */
  ageCommented: 20,
  /* Commenting the last field */
  occupationCommented: "engineer",
};

/*
 * Test comments near the arguments.
 */
let callMeWithComments =
    /* Comment before first arg "a" */
    (
      a: int,
      /* Comment before second arg "b" */
      b: int,
    )
    /* Comment before return type annotation "int" */
    : int =>
  /* Comment above return value a + b + c */
  a + b + c;

let callMeWithComments2 =
    /* Comment before the only argument */
    (
      (
        a: int,
        /* Comment before second arg "b" */
        b: int,
      ),
    )
    /* Comment before return type annotation "int" */
    : int =>
  /* Comment above return value a + b + c */
  a + b + c;

let result =
  /* Comment before function to invoke */
  callMeWithComments(
    /* Comment before first argument expression */
    1 + 2 + 3 + 3,
    /* Comment before second argument expression */
    1 + 2 + 3 + 3,
  );

module type ASig = {let a: int;};
module type BSig = {let b: int;};
module AMod = {
  let a = 10;
};
module BMod = {
  let b = 10;
};
module CurriedSugar =
       /* Commenting before First curried functor arg */
       /* If these comments aren't formatted correctly
        * see how functor args' locations aren't set
        * correclty due to the fold_left.
        */
       (
         A: ASig,
         /* Commenting before Second curried functor arg */
         B: BSig,
       ) => {
  let result = A.a + B.b;
  /* Comment at bottom of module expression */
};

module CurriedSugarFunctorResult =
  /* Commenting before functor name*/
  CurriedSugar
    /* Commenting before functor arg 1 in app */
    (
      AMod,
      /* Commenting before functor arg 2 in app */
      BMod,
    );

module CurriedSugarFunctorResultInline =
  /* Commenting before functor name*/
  CurriedSugar
    /* Commenting before functor arg 1 in app */
    (
      {
        let a = 10;
      },
      {
        /* Commenting before functor arg 2 in app */
        let b = 10;
      },
    );

module type FunctorType =
  (ASig, ASig, BSig) => BSig;
/*
 * Commenting locations
 */
let commentingBeforeEqual /*beforeEqual*/ = {
  name: "hello",
  age: 20,
  occupation: "programmer",
};
let commentingAfterEqual = /*afterEqual*/ {
  name: "hello",
  age: 20,
  occupation: "programmer",
};
let commentingBeforeEqualBeforeType /*beforeEqualBeforeType*/: withThreeFields = {
  name: "hello",
  age: 20,
  occupation: "programmer",
};
let commentingBeforeEqualAfterType:
  withThreeFields /*beforeEqualAfterType*/ = {
  name: "hello",
  age: 20,
  occupation: "programmer",
};
let commentingAfterEqualAfterType: withThreeFields = /*afterEqual*/ {
  name: "hello",
  age: 20,
  occupation: "programmer",
};
let /*beforePattern*/ commentingBeforePattern: withThreeFields = {
  name: "hello",
  age: 20,
  occupation: "programmer",
};
/*beforePattern*/
let /*beforePattern2 */ commentingBeforePattern2: withThreeFields = {
  name: "hello",
  age: 20,
  occupation: "programmer",
};

/*beforePattern*/
let /*beforePattern2 */ commentingBeforePatternSpecial: withThreeFields = {
  name: "hello",
  age: 20,
  occupation: "programmer",
};

let produceRecord /*commentBeforeArg*/ = x => {
  name: "hello",
  age: 20,
  occupation: "programmer",
};

let produceRecord = x => /*commentAfterArg*/ {
  name: "hello",
  age: 20,
  occupation: "programmer",
};

let myPolyFuncCommentBeforeColon /*beforeColon */:
  'a.
  'a => 'a
 =
  o => o;
let myPolyFuncCommentAfterColon: 'a. 'a => 'a =
  /*afterColon */
  o => o;
let myPolyFuncCommentBeforeArrow: 'a. 'a => 'a =
  /*beforeArrow */
  o => o;
let myPolyFuncCommentAfterArrow:
  'a.
  'a => /*afterArrow */ 'a
 =
  o => o;
/* THIS IS THE ONLY TEST THAT IS FAILING DUE TO BEING NON-IDEMPOTENT */
/* let myPolyFuncCommentBeforeEqual : 'a . ('a) => 'a /*beforeEqual */  = fun(o) => o; */
let myPolyFuncCommentAfterEqual: 'a. 'a => 'a =
  /*afterEqual */ o => o;

let myNonPolyFuncCommentBeforeColon /*BeforeColon */:
  'a => 'a =
  o => o;
let myNonPolyFuncCommentAfterColon:
  /*AfterColon */ 'a => 'a =
  o => o;
let myNonPolyFuncCommentBeforeArrow:
  'a /*BeforeArrow */ => 'a =
  o => o;
let myNonPolyFuncCommentAfterArrow:
  'a => /*AfterArrow */ 'a =
  o => o;
let myNonPolyFuncCommentBeforeEqual:
  'a => 'a /*BeforeEqual */ =
  o => o;
let myNonPolyFuncCommentAfterEqual: 'a => 'a =
  /*AfterEqual */ o => o;

let lATCurrySugarCommentBeforeType /*BeforeType */ =
    (type a, input: a) => input;
let lATCurrySugarCommentAfterType /*AfterType */ =
    (type a, input: a) => input;
let lATCurrySugarCommentBeforeArg =
    (type a, /*BeforeArg */ input: a) => input;
let lATCurrySugarCommentAfterArg =
    (type a, input: a) =>
  /*AfterArg */
  input;
let lATCurrySugarCommentAfterArrow =
    (type a, input: a) => /*AfterArrow */ input;

let lATNotSugaredCommentBeforeEqual /*BeforeEqual*/ =
    (type a, input: a) => input;
let lATNotSugaredCommentAfterEqual = /*AfterEqual*/
    (type a, input: a) => input;
let lATNotSugaredCommentBeforeType = /*BeforeType*/
    (type a, input: a) => input;
let lATNotSugaredCommentAfterType = /*AfterType*/
    (type a, input: a) => input;
let lATNotSugaredCommentBeforeArg =
    (type a, /*BeforeArg*/ input: a) => input;
let lATNotSugaredCommentAfterArg =
    (type a, input: a) =>
  /*AfterArg*/
  input;
let lATNotSugaredCommentAfterArrow =
    (type a, input: a) => /*AfterArrow*/ input;

let lAtFuncAnnotatedCommentBeforeColon /*BeforeColon*/:
  type a. a => a =
  (type a, input: a) => input;
let lAtFuncAnnotatedCommentAfterColon /*AfterColon*/:
  type a. a => a =
  (type a, input: a) => input;
let lAtFuncAnnotatedCommentBeforeTypeVar /*BeforeTypeVar*/:
  type a. a => a =
  (type a, input: a) => input;
let lAtFuncAnnotatedCommentAfterTypeVar /*AfterTypeVar*/:
  type a. a => a =
  (type a, input: a) => input;
let lAtFuncAnnotatedBeforeEqual:
  type a. a => a /*BeforeEqual*/ =
  (type a, input: a) => input;
let lAtFuncAnnotatedAfterEqual: type a. a => a =
  /*AfterEqual*/ (type a, input: a) => input;

/* Ternary wrapping comments */
let ternaryResult =
  /* Before Test */
  something
    /* Before ifTrue */
    ? callThisFunction(withThisArg)
    /* Before ifFalse */
    : thatResult;

let ternaryResult =
  /* Before Test */
  something
    /* Before ifTrue */
    ? callThisFunction(withThisArg)
    /* Before ifFalse */
    : trailingTest
        ? /* before nested ifTrue */ true
        : /* before nested ifFalse */ false;

let returningATernary = (x, y) =>
  x > y ? "hi" : "by";

/** Testing some special comment alignment features */

/* Comments can be written like this.
   No leading star is required on each line.
   Everything will line up just fine.
   In this form, include the final closing on the last line. */
let test = 10;
let test =
  /* And if the entire block needs to be re-indented
     such as this case, everything will still look okay. */
  10;

/*     You could begin the block bar out like this.
       And it still works correctly. */
let test = 10;

/** Include multiple opening stars if you like.
    And it will still work. */
let test = 10;

/** This comment will be corrected.
      when printed. */
let test = 10;

/**  Comments with text on line zero
 *   Still work well with comments that have stars on the left side.
 */
let test = 10;

let test =
  /* This kind of comment doesn't exactly render well though.
       Not many people write comments like this.
     */
  10;

let x =
  calWith(
    reallyLongName,
    reallyReallyLongName,
    reallyReallyLongName,
    reallyReallyLongName,
    reallyReallyLongName,
    reallyReallyLongName,
    a,
    a,
    a,
    alskdjfalskdjfalsdf,
  )
  + reallyReallyLongName;

let onlyDoingThisTopLevelLetToBypassTopLevelSequence = {
  let x = {
    print_int(1);
    print_int(20); /* Missing trailing SEMI */
  };

  let x = {
    print_int(1);
    print_int(20); /* Ensure missing middle SEMI reported well */
    print_int(20);
  };

  let x = {
    print_int(1);
    print_int(20);
    10;
  }; /* Missing final SEMI */

  let x = {
    print_int(1);
    print_int(20);
    10;
  };
  x + x; /* Final item */
};

/* With this unification, anywhere eyou see `= fun` you can just ommit it */
let blah = a => a; /* Done */
let blah = a => a; /* Done (almost) */

let blah = (a, b) => a; /* Done */
let blah = (a, b) => a; /* Done (almost) */

let tryingTheSameInLocalScope = {
  let blah = a => a; /* Done */
  let blah = a => a; /* Done (almost) */

  let blah = (a, b) => a; /* Done */
  let blah = (a, b) => a;
  (); /* Done (almost) */
};

reallyLongFunctionNameWithArrayThatBreaks([|
  "one",
  "two",
  "two",
  "two",
  "two",
  "two",
  "two",
|]);

reallyLongFunctionNameWithRecordStringKeys({
  "one": 2345,
  "two": 2345678,
  "three": 45678,
  "four": 45678,
});

fooSpreadES6List([
  "sldkjfklsjdflskjdflksjok",
  "more tests",
  ...x,
]);
