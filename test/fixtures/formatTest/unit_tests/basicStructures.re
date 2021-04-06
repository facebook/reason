/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

let run = fun () => {TestUtils.printSection("Basic Structures");};

while (something) {
  print_string ("You're in a while loop");
  print_newline ();
};

for (i in 0 to 5) {
  print_int (i);
  print_newline ();
  for (i in 10 downto 0) {
    print_string ("Counting in reverse direction");
    print_newline ();
  };
};

for (i in 0 to endOfRangeMustBeSimple(expr,soWrap)) {
  print_int (i);
  print_newline ();
  for (i in theSame(isTrue,ofThe,startOfRange) downto 0) {
    print_string ("Counting in reverse direction");
    print_newline ();
  };
};

let x = (foo^)^.bar^;

let x = foo.bar^;

let x = foo#bar^;

let x = foo^.bar^;

let x = (foo^)#bar^;

/* Prefix operators:
 * ! followed by zero or more appropriate_operator_suffix_chars (see the
 * lexer).
 * ? or ~ followed by at least one appropriate_operator_suffix_chars.
 */
let x = !(! !foo).bar;

let x = !foo.bar;

let x = !foo#bar;

let x = !(!foo).bar;

let x = !(!foo)#bar;

let x = ! !foo.bar;

let x = ?! (!foo.bar);

let x = ! ?!foo.bar;

let x = ~! (!foo.bar);

let x = ! ~!foo.bar;

let x = ~! ~!foo.bar;

let x = !!foo.bar;

let x = !!foo#bar;

let x = !~foo.bar;

let x = !~foo#bar;

let noParensNeeded = !blah.foo.bar;

let parensNeededAroundFirst = (!blah).foo.bar;

let parensNeededAroundSecond = (!blah.foo).bar;

let noParensNeeded = !blah#foo#bar;

let parensNeededAroundFirst = (!blah)#foo#bar;

let parensNeededAroundSecond = (!blah#foo)#bar;


let parensWithSpaceNeededAroundFirst = (! !blah)#foo#bar;

let parensWithSpaceNeededAroundSecond = (! !blah#foo)#bar;

let parensWithSpaceNeededAroundFirst = (?! ~+blah)#foo#bar;

let parensWithSpaceNeededAroundSecond = (?! ~+blah#foo)#bar;

let x = !(!foo.bar);

let x = !(!foo#bar);

let x = -10;

let x = -5.0;

let x = Some(-10);

let x = Some(-5.0);

let lazy x = 10;
let lazy (x : int) = 10;
let lazy [] = 10;
let lazy true = 10;
let lazy #x = 10;
let lazy `Variant = 10;
let lazy `variant = 10;
let lazy ('0' .. '9') = 10;
let lazy (lazy true) = 10;
let lazy [%extend] = 10;

/* Test precedence on access sugar */
let x = arr^[0];

let x = Array.get(arr^,0);

let x = str^.[0];

let x = String.get(str^,0);

let x = Array.set(arr^,0,1);

let x = arr^[0] = 1;

/* Comments */
/*Below is an empty comment*/
/**/
/**                            IF
 *============================================================================
 */;

let (/++) = (+); /* // indicates the start of a comment, not an infix op */

let something = if (self.ext.logSuccess) {
                            print_string("Did tap");
                            print_newline ();
                          };

let logTapSuccess = fun(self) => if (self.ext.logSuccess) {
                            print_string("Did tap");
                            print_newline ();
                          } else {
                            ();
                          };

let logTapSuccess(self) = if (self.ext.logSuccess) {
                            print_string("Did tap");
                            print_newline ();
                          };

(!data).field = true;
(!data).field1.field2 = true;
(!data.field1).field2 = true;
((!data).field1).field2 = true;
(!(data.field1)).field2 = true;

let loop(appTime,frameTime) = {
  if (hasSetup.contents) {
    setupScene ();
    renderIntoTop ();
    hasSetup.contents = true;
  };
  process(appTime,frameTime);
};

/* These parens should be kept around the entire last if/then/else */
if (something) {
  if (somethingElse) {
    ();
  } else {
    "blah";
  };
};

/* These parens should be kept around just the last if/then*/
if (something) {if (somethingElse) {();} else {"blah";};};

/* Parens should be generated to wrap the entire final if then else.
 * To test that it's being parsed correclty, should print "one". */
if (true) {
  if (true) {
    print_string("one");
  } else {
    print_string("two");
  };
};

/* Should print two */
if (true) {
  if (false) {
    print_string("one");
  } else {
    print_string("two");
  };
};

/* Should not print */
if (false) {
  if (true) {
    print_string("one");
  } else {
    print_string("two");
  };
};



/* Should wrap (if a > b then a else b).
 * printer(
 */
let printIfFirstArgGreater = true;
let result =
  if (printIfFirstArgGreater) {
    fun(a,b) => if (a > b) {print_string("a > b");} else {print_string("b >= a");};
  } else if ({
    fun(a,b) => if (a > b) {print_string("b < a");} else {print_string("a <= b");};
  }) {
    print_string ("That could never possibly type check");
    print_newline ();
  };


let myRecord = {
  nestedRecord: {
    anotherNestedRecord: fun(instaComp,displayRect) =>
      if (Graphics.cgRectIntersectsWithSlop
          (defaultCompositeTimerRectSlop,instaComp.relativeRect,displayRect)) {
        IoEligible;
      } else {
        IoInelibleButTryComposition;
      }
  }
};


if (printIfFirstArgGreater) {
  fun(a,b) => if (a > b) {print_string("a > b");};
} else {fun(a,b) => if (a > b) {print_string("b < a");};};
/* Should Be Parsed As: Cleary a type error, but at least the parsing makes that clear */
if (printIfFirstArgGreater) {
  fun(a,b) =>
    if (a > b) {
      print_string("a > b");
    } else {
      fun(a,b) => if (a > b) {print_string("b < a");};
    };
};

fun(a,b) => if (a > b) {print_string("a > b");};

/* What you probably wanted was: */
if (printIfFirstArgGreater) {
  fun(a,b) => (if (a > b) {print_string("a > b");});
} else {
  fun(a,b) => (if (a > b) {print_string("b < a");});
};

/* Mutative if statement: Not used to evaluate to something. */
if (10 < 100) {
  let msg = "If there was any doubt, 10 is in fact less than 100.";
  print_string (msg);
} else {
  let msg = "All bets are off.";
  print_string (msg);
};

if (10 < 100) {
  print_string ("If there was any doubt, 10 is in fact less than 100.");
} else {
  print_string ("All bets are off.");
};


/**                            TYPE CONSTRAINTS
 *============================================================================
 */;
let x = (10:int);
let x:int = 10;
let (x:int) = 10;
let (x:int) = (10:int);
/* let (x:int) = (10:string); */
/* let (x:string) = ("hello":int); */

/**                            TUPLES
 *============================================================================
 */;

/* In Reason, types look like the data they model! Tuples are no exception. */
type pairOfInts = (int, int);
let (letBindingWithTypeConstraint:int) = 10;
let ((tupleItem:int), (withTypeConstraint:int)) = (10, 20);

/* To make sure that tuple field annotations are annotating the entire field */
let _dummyFunc(x) = 10;
let annotatingFuncApplication = (_dummyFunc("a"):int, _dummyFunc("a"):int);

/* Pretty printer might stick the [int] at the label. */
let annotatingSingleFuncApplication = (_dummyFunc("a"):int);

/* So lets try a place where it won't */
let annotatingSingleFuncApplication = {
  /* Commenting a let binding. */
  let a = 100;
  /* Commenting another let binding. */
  let int = 200;
  /*
   * This demonstrates why named arguments cannot simply have the form (func
   * arg:val) - it is indistinguishable from a type constraint.
   */
  2 + (_dummyFunc(a):int);
};

let (tupleItem:int, constrainedWithoutGrouping:int) = (10, 20);
let (tupleItem, withOutsideTypeConstraint):(int,int) = (10, 20);

/* Trailing commas */
let trailingCommaAccepted = (1, 2,);
let moreTrailing = (1, 2, 3, 4, 5, 7, );

/**                        Immutable Lists
 * ============================================================================
 */;

/* Anatomy:        -Head-      --------- Tail---------  nil: You can't see nil */
let x: list(int) = [    1,      2, 3, 4, 5, 6, 7, 8, 9];
let hd = "appendedToHead";
let tl = ["listTo", "append", "to"];

/* To push *one* and only *one* item to the front of a list - use [hd, ...tl] */
let result: list(string) = [hd, ...tl];

/* Is the same as writing */
let result: list(string) = ["appendedToHead", "listTo", "append", "to"];

/* To operate on lists, use pattern matching */
let rec size = fun
  | [] => 0
  | [hd, ...tl] => 1 + size(tl);

/* Optimize for tail recursion */
let rec size = fun(soFar,lst) => switch (lst) {
  | [] => 0
  | [hd, ...tl] => size(soFar + 1,tl)
};

let nestedMatch(lstLst) = switch (lstLst) {
  | [hd, ...tl] when false => 10
  | [hd, ...tl] => switch (tl) {
      | [] => 0 + 0
      | [tlHd, ...tlTl] => 0 + 1
    }
  | [] => 0
};

let nestedMatchWithWhen(lstLst) = switch (lstLst) {
  | [hd, ...tl] when false => 10
  | [hd, ...tl] when true => switch (tl) {
      | [] when false => 0 + 0
      | [] when true => 0 + 0
      | [tlHd, ...tlTl] => 0 + 1
    }
  | [] => 0
};


/**
 * Aliasing with "as" during matches.
 */;
type mine = MyThing(int) | YourThing(int);
/*
 * Reason parses "as" aliases differently than OCaml.
 */
let ppp = switch (MyThing(20)) {
  | MyThing(x) as ppp
  | YourThing(x) as ppp => ppp
};

let MyThing(_) as ppp | YourThing(_) as ppp = ppp;

/*
 * in order to achieve the previous example in ocaml, you would have to group
 * as:
 */
let ppp = switch (MyThing(20)) {
  | (MyThing(x) as ppp)
  | (YourThing(x) as ppp) => ppp
};

let (MyThing(_) as ppp) |(YourThing(_) as ppp) = ppp;
/*
 * But this isn't needed in Reason because OR patterns have much lower
 * precedence - they should be pretty printed in the same way.
 */



/* TODO: */
/* let rec nestedMatch lstLst => match lstLst with { */
/*   hd::tl: match tl with { */
/*     []: 0 + 0, */
/*     tlHd::tlTl: 0 + 1, */
/*   }, */
/*   []: 0 */
/* }; */
/*  */


/**                               ARRAYS
 * ============================================================================
 * Arrays are weird looking. Usually you want lists because they support pattern
 * matching - that's why they have nicer syntax - to entice you. But if you want
 * random access and better control over memory layout, use arrays.
 */;
let emptyArray = [||];
let arrayWithOne = [|10|];
let arrayWithTwo = [|10, 10|];
let secondItem = Array.get(arrayWithTwo,1);

/* Getting And Setting: Yeah, we should really change this */
/* Get an array item at index 1 */
let secondItem = arrayWithTwo[1];
/* Set an array item at index 1 */
arrayWithTwo[1] = 300;


/**
 *                                STRINGS
 *  ============================================================================
 *  The language supports mutating strings, but that should not be depended upon.
 */;
let myString = "asdf";
myString.[2] = '9'; /* Replacing a character: I could do without this sugar */




/*                           FUNCTIONS
 *=============================================================================
 */



/*                           TYPE ANNOTATIONS
 * =============================================================================
 */



let one = 900;
let two = 10000;
/* Tuple expressions can be annotated without additional paren wrapping */
let myTuple = (one:int, two:int);
type myTupleType = (int, int);
let myTuple = (myTuple:myTupleType);

/* Anything *outside* of a tuple, must still be annotated within parens. */
let myTuple = ((one:int, two:int):myTupleType);

/* Now functions that accept a single argument being a tuple look familiar */
let addValues = fun (a:int, b:int) => {
  a + b;
};

let addValues = fun (a:int, b:int) => {
  a + b;
};

let myFunction = fun (a : int, b : int) : int => a + b;

let functionReturnValueType (i:int, s:string): (int) => int = fun(x) => x + 1;

let curriedFormOne (i:int, s:string) = s ++ string_of_int(i);

let curriedFormTwo (i:int, x:int) :(int, int) = (i, x);
/* let nonCurriedFormTwo = fun (i:int, x:int) (:(int, int)) => (i, x); */

let curriedFormThree (i:int, (a:int, b:int):(int, int)) :(int, int, int) = (i, a, b);

/* let nonCurriedFormThree = fun (i:int, (a:int, b:int):(int, int)) (:(int, int, int)) => (i, a, b);  */


/** TODO: But this, however doesn't work.
 *  let (myCurriedFunc: int => int) a => a;
 *  Note: This is likely because only "simple patterns" are accepted as constraints
 *  in let bindings - that may be easy to change.
 */;

type myFuncType = (int, int) => int;

let myFunc: myFuncType = fun (a,b) => a + b;

let funcWithTypeLocallyAbstractTypes (type atype, type btype, a, b, c: (atype, btype) => unit) = c(a,b);

/* Checks that function types aren't unnecessary wrapped */
type a = ((unit => unit));

type b =
  | Foo((unit => unit))
  | Bar((unit => unit), (unit => unit), ((a, b) => c))
  | Baz(unit => unit, unit => unit, (a, b) => c);

type c =
  | Foo((a, b) => unit)
  | Bar(((a, b) => unit));

type d = [> | `Foo((unit => unit))];


/**
 * Records:
 *=============================================================================
 */;

type withThreeFields = {
  name: string,
  age: int,
  occupation: string
};

let testRecord = {
  name: "joe",
  age: 20,
  occupation: "engineer"
};
let anotherRecord = {
  ...testRecord,
  name: "joe++",
  age: testRecord.age + 10
};

let makeRecordBase () {name: "Joe", age: 30, occupation: "Engineer"};
let anotherRecord = {
  /* These parens should be evaporated. */
  ...(makeRecordBase ()),
  name: "joe++",
  age: testRecord.age + 10
};

let anotherRecord = {
  /* Comments should be correctly placed before ... expression */
  ...makeRecordBase(),
  /* Comment after record extension */
  name: "joe++",
  age: testRecord.age + 10
};

let anotherRecord = {
  /* Currently, type annotations must be wrapped in parens - that's easy to improve */
  ...(makeRecordBase () : withThreeFields),
  name: "joe++",
  age: testRecord.age + 10
};


let anotherRecord = {
  /* This is meaningless, sure */
  ...someArray.[0] = 20,
  name: "joe++",
  age: testRecord.age + 10
};

let anotherRecord = {
  ...SomeReally.longFunctionCall {
    passingRecordField: 0,
    andThisOtherRecordField: 10
  },
  name: "joe++",
  age: testRecord.age + 10
};

let anotherRecord = {
  ...SomeReally.longFunctionCall(withArguments, thatWrap:bool),
  name: "joe++",
  age: testRecord.age + 10
};

let anotherRecord = {
  ...SomeReally.longFunctionCall
   (withArg,
    ["and", "final", "list", "that", "should", "break"]),
  name: "joe++",
  age: testRecord.age + 10
};

/* Record type punning */
type props = {title: string};

type state = unit;

type component = {props};

type component2 = {props, state, updater: unit,};

type component3 = {props: M.props, state};

type mutableComponent = {mutable props};

type mutabeleComponent2 = {mutable props, mutable state, style: int,};

/* Don't pun parameterized types */
type description('props) = {
  element: string,
  tag: tag('props)
};

/* Don't pun types from other modules */
module Foo = {
  type bar = {foo: Baz.foo};
};

/* Don't pun field names that aren't "simple" */
type foo = {
  bar: Baz.bar,
  qux: qux,
  fooo: Fooo.fooo
};

let moreFoo = {
  bar: Baz.bar,
  qux: qux,
  fooo: Fooo.fooo
};

/* record value punning */

let props = {title: "hi"};
/* no punning available for a single field. Can't tell the difference with a scope + expression */
let componentA = {props: props};
/* pun for real */
let componentB = {props: props, state: ()};
/* pun fields with module prefix too */
let foo = {Foo.foo: foo};
let bar = {Foo.foo: foo, bar: 1};
let bar = {bar: 1, Foo.foo: foo};
let bar = {Foo.foo: foo, Bar.bar: bar};

fun ({M.x: x, y: y}) => 1;

switch (foo) {
| {y: 1, M.x: x} => 2
};

/* Requested in #566 */
let break_after_equal = no_break_from_here(some_call(to_here));

/* Pexp_letexception */
let () = {
  exception E;
  raise(E)
};

/* # 1587: don't print fun keyword when printing Pexp_fun in a record expression  */
{contents: fun () => ((): unit)};

/* #1556: Always break nested record/obj */
let z = {a: {b: c, d: e}, f: g};

let z = {a: {"b": c, "d": e}, f: g};

let z = {a: {pub b = c; pub d = e}, f: g};

let z = {"a": {"b": c, "d": e}, "f": g};

let z = {"a": {b: c, d: e}, "f": g};

let z = {"a": {pub b = c; pub d = e}, "f": g};


/**
 * Unnecessary parens should be removed.
 */
let unitLambda = (()) => ();
let identifierLambda = (a) => ();
let underscoreLambda = (_) => ();
it("should remove parens", (a) => {
  print_string("did it work?");
  print_string("did it work?");
});

/* https://github.com/facebook/reason/issues/1554 */
(curNode^)##childNodes;

foo(preserveBraces => {
  inCallback
});

foo(preserveBraces => {
  inFirstPos
}, secondArg);

foo(oneArg, preserveBraces => {
  inFirstPos
}, secondArg);
