[@reason.version 3.7];
3; /* - */
3; /*-*/

3; /*-*/

3 /*-*/;
/* **** comment */
/*** comment */
/** docstring */
/* comment */
/** docstring */
/*** comment */
/**** comment */
/***** comment */
/** */
/*** */
/**** */
/**/
/***/
/****/
/** (** comment *) */
/** (*** comment *) */
/* (** comment *) */
/* (*** comment *) */
/* *(*** comment *) */
/* comment **/
/* comment ***/
/* comment ****/
/* comment *****/
/**
 * Multiline
 */
/** Multiline
 *
 */
/**
 **
 */
module JustString = {
  include Map.Make(Int32); /* Comment eol include */
};

let testingEndOfLineComments = [
  "Item 1" /* Comment For First Item */,
  "Item 2" /* Comment For Second Item */,
  "Item 3" /* Comment For Third Item */,
  "Item 4" /* Comment For Fourth Item - but before trailing comma */,
  /* Comment after last item in list. */
] /* Comment after rbracket */;

/* But if you place them after the comma at eol, they're preserved as such */
let testingEndOfLineComments = [
  "Item 1", /* Comment For First Item */
  "Item 2", /* Comment For Second Item */
  "Item 3", /* Comment For Third Item */
  "Item 4" /* Comment For Fourth Item - but before trailing comma */,
  /* Comment after last item in list. */
] /* Comment after rbracket */;

/* The space between ; and comment shoudn't matter */
let testPlacementOfTrailingComment = [
  "Item 0" /* */
  /* Comment after last item in list. */
]; /* Comment after semi */

/* The space between ; and comment shoudn't matter */
let testPlacementOfTrailingComment = [
  "Item 0" /* */
  /* Comment after last item in list. */
]; /* Comment after semi */

/* Try again but without other things in the list */
let testPlacementOfTrailingComment = [
  "Item 0" /* */
]; /* Comment after semi */

/* The space between ; and comment shoudn't matter */
let testPlacementOfTrailingComment = [
  "Item 0" /* */
  /* Comment after last item in list. */
]; /* Comment after semi */

let testingEndOfLineComments = []; /* Comment after entire let binding */

/* The following is not yet idempotent */
/* let myFunction */
/*     withFirstArg  /* First arg */ */
/*     andSecondArg  => { /* Second Arg */ */
/*   withFirstArg + andSecondArg /* before semi */ ; */
/* }; */

let myFunction = /* First arg */
    (
      withFirstArg,
      /* Second Arg */
      andSecondArg,
    ) => {
  withFirstArg + andSecondArg;
}; /* After Semi */

type point = {
  x: string, /* x field */
  y: string /* y field */
};

type pointWithManyKindsOfComments = {
  /* Line before x */
  x: string, /* x field */
  /* Line before y */
  y: string /* y field */
  /* Final row of record */
};

type typeParamPointWithComments('a) = {
  /* Line before x */
  x: 'a, /* x field */
  /* Line before y */
  y: 'a /* y field */
  /* Final row of record */
};

/* Now, interleaving comments in type params */
/* Type name */
type typeParamPointWithComments2
  /* The a type param */
  (
    'a,
    /* The b type apram */
    'b,
  ) = {
  /* Line before x */
  x: 'a, /* x field */
  /* Line before y */
  y: 'a /* y field */
  /* Final row of record */
};

/* The way the last row comment is formatted is suboptimal becuase
 * record type definitions do not include enough location information */
type anotherpoint = {
  x: string, /* x field */
  y: string /* y field */
  /* comment as last row of record */
};

type t = (int, int); /* End of line on t */
type t2 = (int, int); /* End of line on (int, int) */

type t3 = (int, int); /* End of line on (int, int) */

type variant =
  | X(int, int) /* End of line on X */
  | Y(int, int); /* End of line on Y */ /* Comment on entire type def for variant */

/* Before let */
let res =
  /* Before switch */
  switch (X(2, 3)) {
  /* Above X line */
  | X(_) => "result of X" /* End of arrow and X line */
  /* Above Y line */
  | Y(_) => "result of Y" /* End of arrow and Y line */
  }; /* After final semi in switch */

let res =
  switch (X(2, 3)) {
  | X(0, 0) =>
    /* After X arrow */
    "result of X" /* End of X body line */
  | X(1, 0) /* Before X's arrow */ => "result of X" /* End of X body line */
  | X(_) =>
    /* After X _ arrow */
    "result of X" /* End of X body line */
  /* Above Y line */
  | Y(_) =>
    /* Comment above Y body */
    "result of Y"
  };

type variant2 =
  /* Comment above X */
  | X(int, int) /* End of line on X */
  /* Comment above Y */
  | Y(int, int);

type variant3 =
  /* Comment above X */
  | X(int, int) /* End of line on X */
  /* Comment above Y */
  | Y(int, int); /* End of line on Y  */

type x = {
  /* not attached *above* x */
  fieldOne: int,
  fieldA: int,
} /* Attached end of line after x */
and y = {
  /* not attached *above* y */
  fieldTwo: int,
}; /* Attached end of line after y */

type x2 = {
  /* not attached *above* x2 */
  fieldOne: int,
  fieldA: int,
} /* Attached end of line after x2 */
and y2 = {
  /* not attached *above* y2 */
  fieldTwo: int,
};

let result =
  switch (None) {
  | Some({fieldOne: 20, fieldA: a}) =>
    /* Where does this comment go? */
    let tmp = 0;
    2 + tmp;
  | Some({fieldOne: n, fieldA: a}) =>
    /* How about this one */
    let tmp = n;
    n + tmp;
  | None => 20
  };

let res =
  /* Before switch */
  switch (X(2, 3)) {
  /* Above X line */
  | X(_) => "result of X" /* End of arrow and X line */
  /* Above Y line */
  | Y(_) => "result of Y" /* End of arrow and Y line */
  };

/*
 * Now these end of line comments *should* be retained.
 */
let result =
  switch (None) {
  | Some({
      fieldOne: 20, /* end of line */
      fieldA: a /* end of line */
    }) =>
    let tmp = 0;
    2 + tmp;
  | Some({
      fieldOne: n, /* end of line */
      fieldA: a /* end of line */
    }) =>
    let tmp = n;
    n + tmp;
  | None => 20
  };

/*
 * These end of line comments *should* be retained.
 * To get the simple expression eol comment to be retained, we just need to
 * implement label breaking eol behavior much like we did with sequences.
 * Otherwise, right now they are not idempotent.
 */
let res =
  switch (
    /* Retain this */
    X(2, 3)
  ) {
  /* Above X line */
  | X(
      _, /* retain this */
      _ /* retain this */
    ) => "result of X"

  /* Above Y line */
  | Y(_) => "result of Y" /* End of arrow and Y line */
  };

type optionalTuple =
  | OptTup(
      option(
        (
          int, /* First int */
          int /* Second int */
        ),
      ),
    );

type optionTuple =
  option(
    (
      int, /* First int */
      int /* Second int */
    ),
  );

type intPair = (
  int, /* First int */
  int /* Second int */
);
type intPair2 = (
  /* First int */
  int,
  /* Second int */
  int,
);

let result =
  /**/
  {
    2 + 3;
  };

/* This is not yet idempotent */
/* { */
/*   /**/ */
/*   (+) 2 3 */
/* }; */

let a = ();
for (i in 0 to 10) {
  /* bla  */
  a;
};

if (true) {
  /* hello */
  ()
};

type color =
  | Red(int) /* After red end of line */
  | Black(int) /* After black end of line */
  | Green(int); /* After green end of line */ /* On next line after color type def */

let blahCurriedX = x =>
  fun
  | Red(10)
  | Black(20)
  | Green(10) => 1 /* After or pattern green */
  | Red(x) => 0 /* After red */
  | Black(x) => 0 /* After black */
  | Green(x) => 0; /* After second green */ /* On next line after blahCurriedX def */

let name_equal = (x, y) => {
  x == y;
};

let equal = (i1, i2) =>
  i1.contents === i2.contents && true; /* most unlikely first */

let equal = (i1, i2) =>
  compare(compare(0, 0), compare(1, 1)); /* END OF LINE HERE */

let tuple_equal = ((i1, i2)) => i1 == i2;

let tuple_equal = ((csu, mgd)) =>
  /* Some really long comments, see https://github.com/facebook/reason/issues/811 */
  tuple_equal((csu, mgd));

/** Comments inside empty function bodies
 * See https://github.com/facebook/reason/issues/860
 */
let fun_def_comment_inline = () => {/* */};

let fun_def_comment_newline = () => {/* */};

let fun_def_comment_long = () => {
  /* longer comment inside empty function body */
};

let trueThing = true;

for (i in 0 to 1) {
  /* comment */
  print_newline();
};

while (trueThing) {
  /* comment */
  print_newline();
};

if (trueThing) {
  /* comment */
  print_newline();
};

/* Comment before if test */
if (trueThing) {
  /* Comment before print */
  print_newline();
  /* Comment before print */
  print_newline();
  /* Comment after final print */
};

/* Comment before if test */
if (trueThing) {
  /* Comment before print */
  print_newline();
               /* Comment after final print */
};

/* Comment before if test */
if (trueThing) {
  /* Comment before print */
  print_newline();
  /* Comment before print */
  print_newline();
  /* Comment after final print */
} else {
  /* Comment before print */
  print_newline();
  /* Comment before print */
  print_newline();
  /* Comment after final print */
};

/* Comment before if test */
if (trueThing) {
  /* Comment before print */
  print_newline();
               /* Comment after final print */
} else {
  /* Comment before print */
  print_newline();
               /* Comment after final print */
};

/* Comment before while test */
while (trueThing) {
  /* Comment before print */
  print_newline();
  /* Comment before print */
  print_newline();
  /* Comment after final print */
};

/* Comment before while test */
while (trueThing) {
  /* Comment before print */
  print_newline();
               /* Comment after final print */
};

/* Comment before for test */
for (i in 0 to 100) {
  /* Comment before print */
  print_newline();
  /* Comment before print */
  print_newline();
  /* Comment after final print */
};

/* Comment before for test */
for (i in 0 to 100) {
  /* Comment before print */
  print_newline();
               /* Comment after final print */
};

if (trueThing) {
  /* Comment before print */
  print_newline(); /* eol print */
  /* Comment before print */
  print_newline(); /* eol print */
  /* Comment after print */
};

/* Comment before if test */
if (trueThing) {
  /* Comment before print */
  print_newline(); /* eol print */
               /* Comment after print */
};

/* Comment before if test */
if (trueThing) {
  /* Comment before print */
  print_newline(); /* eol print */
  /* Comment before print */
  print_newline(); /* eol print */
  /* Comment after print */
} else {
  /* Comment before print */
  print_newline(); /* eol print */
  /* Comment before print */
  print_newline(); /* eol print */
  /* Comment after print */
};

/* Comment before if test */
if (trueThing) {
  /* Comment before print */
  print_newline(); /* eol print */
               /* Comment before print */
} else {
  /* Comment before print */
  print_newline(); /* eol print */
  /* Comment before print */
  print_newline(); /* eol print */
  /* Comment after print */
};

/* Comment before while test */
while (trueThing) {
  /* Comment before print */
  print_newline(); /* eol */
  /* Comment before print */
  print_newline(); /* eol */
  /* Comment after final print */
};

/* Comment before while test */
while (trueThing) {
  /* Comment before print */
  print_newline(); /* eol */
               /* Comment after final print */
};

/* Comment before for test */
for (i in 0 to 100) {
  /* Comment before print */
  print_newline(); /* eol */
  /* Comment before print */
  print_newline(); /* eol */
  /* Comment after final print */
};

/* Comment before for test */
for (i in 0 to 100) {
  /* Comment before print */
  print_newline(); /* eol */
               /* Comment after final print */
};

let f = (a, b, c, d) => a + b + c + d;

while (trueThing) {
  f(
    /* a */
    1,
    /* b */
    2,
    /* c */
    3,
    /* d */
    4,
    /* does work */
  );
};
while (trueThing) {
  f(
    /* a */
    1,
    /* b */
    2,
    /* c */
    3,
    /* d */
    4 /* does work */
  );
};

ignore(
  (
    _really,
    _long,
    _printWidth,
    _exceeded,
    _here,
  ) => {
  /* First comment */
  let x = 0;
  x + x;
  /* Closing comment */
});

ignore((_xxx, _yyy) => {
  /* First comment */
  let x = 0;
  x + x;
  /* Closing comment */
});

type tester('a, 'b) =
  | TwoArgsConstructor('a, 'b)
  | OneTupleArgConstructor(('a, 'b));
let callFunctionTwoArgs = (a, b) => ();
let callFunctionOneTuple = tuple => ();

let y =
  TwoArgsConstructor(
    1, /*eol1*/
    2 /* eol2 */
  );

let y =
  callFunctionTwoArgs(
    1, /*eol1*/
    2 /* eol2 */
  );

let y =
  OneTupleArgConstructor((
    1, /*eol1*/
    2 /* eol2 */
  ));

let y =
  callFunctionOneTuple((
    1, /*eol1*/
    2 /* eol2 */
  ));

type polyRecord('a, 'b) = {
  fieldOne: 'a,
  fieldTwo: 'b,
};

let r = {
  fieldOne: 1, /*eol1*/
  fieldTwo: 2 /* eol2 */
};

let r = {
  fieldOne: 1, /*eol1*/
  fieldTwo: 2 /* eol2 with trailing comma */
};

let y =
  TwoArgsConstructor(
    "1", /*eol1*/
    "2" /* eol2 */
  );

let y =
  callFunctionTwoArgs(
    "1", /*eol1*/
    "2" /* eol2 */
  );

let y =
  OneTupleArgConstructor((
    "1", /*eol1*/
    "2" /* eol2 */
  ));

let y =
  callFunctionOneTuple((
    "1", /*eol1*/
    "2" /* eol2 */
  ));

let r = {
  fieldOne: "1", /*eol1*/
  fieldTwo: "2" /* eol2 */
};

let r = {
  fieldOne: "1", /*eol1*/
  fieldTwo: "2" /* eol2 with trailing comma */
};

let identifier = "hello";

let y =
  TwoArgsConstructor(
    identifier, /*eol1*/
    identifier /* eol2 */
  );

let y =
  callFunctionTwoArgs(
    identifier, /*eol1*/
    identifier /* eol2 */
  );

let y =
  OneTupleArgConstructor((
    identifier, /*eol1*/
    identifier /* eol2 */
  ));

let y =
  callFunctionOneTuple((
    identifier, /*eol1*/
    identifier /* eol2 */
  ));

let r = {
  fieldOne: identifier, /*eol1*/
  fieldTwo: identifier /* eol2 */
};

let r = {
  fieldOne: identifier, /*eol1*/
  fieldTwo: identifier /* eol2 with trailing comma */
};

let y =
  TwoArgsConstructor(
    identifier: string, /*eol1*/
    identifier: string /* eol2 */
  );

let y =
  callFunctionTwoArgs(
    identifier: string, /*eol1*/
    identifier: string /* eol2 */
  );

let y =
  OneTupleArgConstructor((
    identifier: string, /*eol1*/
    identifier: string /* eol2 */
  ));

let y =
  callFunctionOneTuple((
    identifier: string, /*eol1*/
    identifier: string /* eol2 */
  ));

let r = {
  fieldOne: (identifier: string), /*eol1*/
  fieldTwo: (identifier: string) /* eol2 */
};

let r = {
  fieldOne: (identifier: string), /*eol1*/
  fieldTwo: (identifier: string) /* eol2 with trailing comma */
};

/** doc comment */
[@bs.send]
external url: t => string;

/**
 * Short multiline doc comment
 */
[@bs.send]
external url: t => string;

/** Longer doc comment before an attribute on an external. */
[@bs.send]
external url: t => string;

/* normal comment */
[@bs.send] external url: t => string;

/** doc type */
type q = {
  a: int,
  b: string,
};

/** doc let */
let letter: q = {a: 42, b: "answer"};
