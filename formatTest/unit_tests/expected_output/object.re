/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
type t = {.};

type t = {
  .
  u: int,
  v: int
};

type t = {.. u: int};

type t = {.. u: int};

type t = {..};

type t = {..};

let (<..>) = (a, b) => a + b;

let five = 2 <..> 3;

type closedObjSugar = {. "foo": bar, "baz": int};

type openObjSugar = {.. "x": int, "y": int};

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
