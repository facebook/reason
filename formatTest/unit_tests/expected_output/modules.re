[@reason.version 3.7];
/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

let run = () => {
  TestUtils.printSection("Modules");
};

/**
 * Modules:
 * ----------------------------------------------------------------------------
 * Modules accomplish the following:
 * - Organization of code and data.
 * - Basis for separate compilation and fast compile times.
 * - Enabled higher order type abstractions that are capable of modelling very
 * sophisticated typing relationships between various parts of your code.
 *
 * Modules are much like records, but much more powerful. Much of modules'
 * powers are derived from the fact that they are primarily built up and
 * organized at compilation time, instead of runtime. It is this constraint
 * that allows modules to be more powerful than simple records.
 *
 * There are some important ways that modules differ from records:
 *
 * - Fields are lexically scoped: In the following record: `{x:1, y: x + x}`,
 * the reference to `x` does not refer to the record field `x`. This is because
 * records fields do not form lexical scopes for the evaluation of other record
 * values. Modules, on the other hand do allow fields to reference other
 * fields. Appropriately, each field is introduced via the keyword `let`.
 */
module MyFirstModule = {
  let x = 0;
  let y = x + x;
};

let result = MyFirstModule.x + MyFirstModule.y;

/**
 * - A module is introduced with the `module` phrase.
 * - A module *must* have a capital letter as its first character.
 * - The exported fields of a module must be listed within `{}` braces and each
 * exported value binding is specified via a `let` keyword.
 */
/**
 * Another way that modules are more powerful than records, is that they may
 * also export types.
 */
module MySecondModule = {
  type someType = int;
  let x = 0;
  let y = x + x;
};

let myInt: MySecondModule.someType = 100;

/** Module signatures:
 * ----------------------------------------------------------------------------
 * Not only may modules export types, but modules *themselves* can be described
 * by types via the `module type` phrase. We call these module types
 * "signatures". For example, `MySecondModule` has the following `module type`
 * signature:
 */
module type MySecondModuleType = {
  type someType = int;
  let x: int;
  let y: int;
};

/**
 * Much like how you can ensure that a value is compatible with a specific
 * type:

 let myTestVal: int = 10;

 * You can also perform the same type of annotation to ensure that you have
 * written code that matches your understanding. For example, `MySecondModule`
 * could have been written as:

 module MySecondModule: MySecondModuleType = {
 type someType = int;
 let x = 0;
 let y = x + x;
 };
 */

/**
 * - Modules may be artificially "constrained" so that users of a module see
 * fewer details than are actually present.
 * - Modules may be combined, merged, and transformed at compile time in ways
 * that
 * - Because they are more powerful, they may not be passed around at runtime
 * as easily as records.
 *
 * Some additioal benefits to using modules:
 * - Modules are a very elegant way to organize large packages of code.
 * - Modules are the unit of compilation. Minimal recompilation changes
 * - Modules can help you achieve higher degrees of polymorphism than the core
 * language.
 */

let opensAModuleLocally = {
  module MyLocalModule = {
    type i = int;
    let x: i = 10;
  };
  /* Notice how local modules names may be used twice and are shadowed */
  module MyLocalModule: MySecondModuleType = {
    type someType = int;
    let x: someType = 10;
    let y: someType = 20;
  };
  let tmp = MyLocalModule.x + 22;
  tmp + 30;
};

module type HasTT = {type tt;};

module SubModule: HasTT = {
  type tt = int;
};

module type HasEmbeddedHasTT = {
  module SubModuleThatHasTT = SubModule;
};

module type HasPolyType = {type t('a);};

module type HasDestructivelySubstitutedPolyType =
  HasPolyType with type t('a) := list('a);

module type HasDestructivelySubstitutedSubPolyModule = {
  /* Cannot perform destructive substitution on submodules! */
  /* module X: HasPolyType with type t := list (int, int); */
  module X: HasDestructivelySubstitutedPolyType;
};
module type HasSubPolyModule = {
  /* Cannot perform destructive substitution on submodules! */
  /* module X: HasPolyType with type t := list (int, int); */
  module X: HasPolyType;
};

module EmbedsSubPolyModule: HasSubPolyModule = {
  module X = {
    type t('a) = list('a);
  };
};

module EmbedsDestructivelySubstitutedPolyModule:
  HasDestructivelySubstitutedSubPolyModule = {
  module X = {
    type t = list(int, int);
  };
};

module type HasMultiPolyType = {
  type substituteThis('a, 'b);
  type substituteThat('a, 'b);
};

module type HasDestructivelySubstitutedMultiPolyType =
  HasMultiPolyType with
    type substituteThis('a, 'b) :=
      Hashtbl.t('a, 'b) and
    type substituteThat('a, 'b) :=
      Hashtbl.t('a, 'b);

module InliningSig: {
  let x: int;
  let y: int;
} = {
  /*
   * Comment inside of signature.
   */
  let x = 10;
  /* Inline comment inside signature. */
  let y = 20;
};

module MyFunctor = (M: HasTT) => {
  type reexportedTT = M.tt;
  /* Inline comment inside module. */
  /** Following special comment inside module. */
  let someValue = 1000;
};

/* Notice how
   - Functors no longer require parens around argument.
   - A final semicolon is required for module structures.
   - We should eliminate both those requirements. See action items 13-14 at the
   bottom of this file. [Actually, forgiving the trailing SEMI might not be
   such a great idea].
   */
module MyFunctorResult =
  MyFunctor({
    type tt = string;
  });

module LookNoParensNeeded =
  MyFunctor({
    type tt = string;
  });

module type SigResult = {let result: int;};

module type ASig = {let a: int;};
module type BSig = {let b: int;};
module AMod = {
  let a = 10;
};
module BMod = {
  let b = 10;
};

module CurriedSugar = (A: ASig, B: BSig) => {
  let result = A.a + B.b;
};

/* Right now [CurriedSuperSugar] is parsed as being indistinguishable from
   the above.

   module CurriedSuperSugar (A:ASig) (B:BSig): SigResult => ({
   let result = A.a + B.b;
   }: SigResult);

   /* Not supported in OCaml OR Reason (Edit: now supported in OCaml for functions) */
   let x = fun (a:foo) :bar => baz;
   module x = fun (A:Foo) :Bar => Baz;

   /* Supported in both OCaml and Reason */
   let x (a:foo) :bar => baz;
   module x (A:Foo) :Bar => Baz;

   */
module CurriedSugarWithReturnType =
       (A: ASig, B: BSig)
       : SigResult => {
  let result = A.a + B.b;
};

/* This is parsed as being equivalent to the above example */
module CurriedSugarWithAnnotatedReturnVal =
       (A: ASig, B: BSig)
       : SigResult => {
  let result = A.a + B.b;
};

module CurriedNoSugar = (A: ASig, B: BSig) => {
  let result = A.a + B.b;
};

let letsTryThatSyntaxInLocalModuleBindings = () => {
  module CurriedSugarWithReturnType =
         (A: ASig, B: BSig)
         : SigResult => {
    let result = A.a + B.b;
  };
  module CurriedSugarWithAnnotatedReturnVal =
         (A: ASig, B: BSig)
         : SigResult => {
    let result = A.a + B.b;
  };

  module CurriedNoSugar = (A: ASig, B: BSig) => {
    let result = A.a + B.b;
  };

  /*
   * The following doesn't work in OCaml (LocalModule (struct end)).x isn't even
   * parsed!
   *
   * let thisDoesntWorkInOCaml () =
   * module LocalModule(A:sig end) = struct let x = 10 end in
   * module Out = (LocalModule (struct end)) in
   * let outVal = (LocalModule (struct end)).x in
   * let res = Out.x in
   * res;;
   */

  module TempModule =
    CurriedNoSugar(AMod, BMod);
  module TempModule2 =
    CurriedSugarWithAnnotatedReturnVal(
      AMod,
      BMod,
    );
  TempModule.result + TempModule2.result;
};

module type EmptySig = {};
module MakeAModule = (X: EmptySig) => {
  let a = 10;
};
module CurriedSugarFunctorResult =
  CurriedSugar(AMod, BMod);
module CurriedSugarFunctorResultInline =
  CurriedSugar(
    {
      let a = 10;
    },
    {
      let b = 10;
    },
  );
module CurriedNoSugarFunctorResult =
  CurriedNoSugar(AMod, BMod);
module CurriedNoSugarFunctorResultInline =
  CurriedNoSugar(
    {
      let a = 10;
    },
    {
      let b = 10;
    },
  );

module ResultFromNonSimpleFunctorArg =
  CurriedNoSugar(
    (
      MakeAModule({})
    ),
    BMod,
  );

/* TODO: Functor type signatures should more resemble value signatures */
let curriedFunc: (int, int) => int =
  (a, b) => a + b;
module type FunctorType =
  (ASig, BSig) => SigResult;
/* Which is sugar for:*/
module type FunctorType2 =
  (ASig, BSig) => SigResult;

/* Just for compability with existing OCaml ASTs you can put something other
 * than an underscore */
module type FunctorType3 =
  (Blah: ASig, ThisIsIgnored: BSig) => SigResult;

/* The actual functors themselves now have curried sugar (which the pretty
 * printer will enforce as well */
/* The following: */
module CurriedSugarWithAnnotation2:
  (ASig, BSig) => SigResult =
  (A: ASig, B: BSig) => {
    let result = A.a + B.b;
  };

/* Becomes: */
module CurriedSugarWithAnnotation:
  (ASig, BSig) => SigResult =
  (A: ASig, B: BSig) => {
    let result = A.a + B.b;
  };

/* "functors" that are not in sugar curried form cannot annotate a return type
 * for now, so we settle for: */
module CurriedSugarWithAnnotationAndReturnAnnotated:
  (ASig, BSig) => SigResult =
  (A: ASig, B: BSig) => (
    {
      let result = A.a + B.b;
    }:
      SigResult
  );

module ReturnsAFunctor =
       (A: ASig, B: BSig)
       : ((ASig, BSig) => SigResult) =>
  (A: ASig, B: BSig) => {
    let result = 10;
  };

module ReturnsSigResult =
       (A: ASig, B: BSig)
       : SigResult => {
  let result = 10;
};

module ReturnsAFunctor2 =
       (A: ASig, B: BSig)
       : ((ASig, BSig) => SigResult) =>
  (A: ASig, B: BSig) => {
    let result = 10;
  };

/*
 * Recursive modules.
 * TODO: Test [Psig_recmodule]
 */
module rec A: {
  type t =
    | Leaf(string)
    | Node(ASet.t);
  let compare: (t, t) => int;
} = {
  type t =
    | Leaf(string)
    | Node(ASet.t);
  let compare = (t1, t2) =>
    switch (t1, t2) {
    | (Leaf(s1), Leaf(s2)) =>
      Pervasives.compare(s1, s2)
    | (Leaf(_), Node(_)) => 1
    | (Node(_), Leaf(_)) => (-1)
    | (Node(n1), Node(n2)) =>
      ASet.compare(n1, n2)
    };
}
and ASet: Set.S with type elt = A.t =
  Set.Make(A);

/*
 * How recursive modules appear in signatures.
 */
module type HasRecursiveModules = {
  module rec A: {
    type t =
      | Leaf(string)
      | Node(ASet.t);
    let compare: (t, t) => int;
  }
  and ASet: Set.S with type elt = A.t;
};

/* From http://stackoverflow.com/questions/1986374/higher-order-type-constructors-and-functors-in-ocaml */
module type Type = {type t;};
module Char = {
  type t = char;
};
module List = (X: Type) => {
  type t = list(X.t);
};
module Maybe = (X: Type) => {
  type t = option(X.t);
};
module Id = (X: Type) => X;
module Compose =
       (
         F: (Type) => Type,
         G: (Type) => Type,
         X: Type,
       ) =>
  F((G(X)));
let l: Compose(List)(Maybe)(Char).t = [
  Some('a'),
];
module Example2 = (F: (Type) => Type, X: Type) => {
  /**
   * Note: This is the one remaining syntactic issue where
   * modules/functions do not have syntax unified with values.
   * It should be:
   *
   *   let iso (a:(Compose Id F X).t): (F X).t => a;
   *
   */
  let iso = (a: Compose(Id)(F)(X).t): F(X).t => a;
};

Printf.printf(
  "\nModules And Functors: %n\n",
  CurriedNoSugarFunctorResultInline.result,
);

/* We would have: */
/* module CurriedSugarWithAnnotation: ASig => BSig => SigResult =
   fun (A:ASig) (B:BSig) => {let result = A.a + B.b;; */

/*
 module Typeahead = React.Create {
 type props = {initialCount: int};
 type state = {count: int};
 let getInitialState props => {count: 10};
 let render {props, state} =>
 <div>
 <span><BigBox></span>
 </div>;
 };
 */

include YourLib.CreateComponent({
  type thing = blahblahblah;
  type state = unit;
  let getInitialState = _ => ();
  let myValue = {recordField: "hello"};
});

module type HasInt = {let x: int;};

module MyModule = {
  let x = 10;
};

let myFirstClass: module HasInt =
  (module MyModule);

let myFirstClassWillBeFormattedAs: module HasInt =
  (module MyModule);

let acceptsAndUnpacksFirstClass =
    (module M: HasInt) =>
  M.x + M.x;

let acceptsAndUnpacksFirstClass =
    (module M: HasInt) =>
  M.x + M.x;

module SecondClass = (val myFirstClass);

module SecondClass2 = (
  val (module MyModule): HasInt
);

let p = SecondClass.x;

/* Opening Modules */
module M = {
  module Inner = {};
};

module N = {
  open M;
  let z = {
    M.(34);
  };
  let z = {
    open M;
    34;
    35;
  };
  let z = {
    M.(34, 35);
  };
  let z = M.(34, 35);
  let z = M.(34, 35);
  let z = {
    M.{};
  };
  let z = M.{};
  let z = M.{};
  let z = {
    M.{x: 10};
  };
  let z = {
    M.[foo, bar];
  };
  let z = {
    M.[foo, bar];
  };
  let z = {
    M.{x: 10, y: 20};
  };
  let z = {
    M.(M2.(value));
  };
  let z = {
    M.(M2.value);
  };
  let z = {
    open! M;
    34;
  };
  let z = {
    open! M;
    34;
    35;
  };
  let z = {
    open! M;
    {};
  };
  let z = {
    open! M;
    {x: 10};
  };
  let z = {
    open! M;
    [foo, bar];
  };
  let z = {
    open! M;
    [foo, bar];
  };
  let z = {
    open! M;
    {x: 10, y: 20};
  };
  let z = {
    open! M;
    open! M2;
    value;
  };
  let z = {
    open! M;
    M2.value;
  };
  let y = 44;
};

open M;
open M.Inner;
open M;

module OldModuleSyntax = {
  module InnerOldModule = {};
};

module type SigWithModuleTypeOf = {
  module type ModuleType;
  include (module type of String);
  include (module type of Array);
};

module type T = t with type t = a => a;
module type T = t with type t = a => a;
module type T = (t with type t = a) => a;

module X = [%test extension];
module type T = [%test extension];

let foo =
    (type a, module X: X_t with type t = a) => X.a;

let f =
    (module M: M with type x = x and type y = y) => M.x;

let foo =
    (
      module X:
        X_t with
          type t = a and
          type s = a and
          type z = a,
      module Y: Y_t with type t = a,
      module Z: Z_t with type t = a,
    ) => X.a;

/* https://github.com/facebook/reason/issues/2028 */
M.[];

module type Event = (module type of {
  include ReactEventRe;
});

include (Version2: (module type of Version2));

/* https://github.com/facebook/reason/issues/2608 */
module Functor =
       (())
       : (module type of {}) => {};
