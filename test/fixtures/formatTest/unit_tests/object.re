/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

type t = {.};

type t = {
  .
  u: int,
  v: int
};

type t = {.. u: int};

type t = {.. u: int};

type t = {
  ..
};

type t = {..};

let (<..>)(a,b) = a + b;
let five = 2 <..> 3;

type closedObjSugar = Js.t({. foo: bar, baz: int});

type openObjSugar = Js.t({.. x: int, y: int});

type x = Js.t({.});

type y = Js.t({..});

/* #1595: always break object rows (>= 2) for readability */
type o = {
  .
  a: int,
  b: int
};

type o2 = {
  ..
  a: int,
  b: int
};

let obj = {as _; [@foo] val a = 1};

/* Oinherit (https://github.com/ocaml/ocaml/pull/1118) */
type t1 = {
  .
  n: string,
  ...t,
};

type t1 = {
  ..
  n: string,
  ...t,
};

type g1 = {
  .
  n: string,
  ...t,
  ...y,
};

type g2 = {
  .
  n: string,
  ...t,
  ...y,
};

type m1 = {
  .
  ...M.t,
};
type m2('a) = {
  .
  n: string,
  ...M.t('a),
};
