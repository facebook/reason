/* should not break */
type point = {x: int, y: int};

type point = {. "x": int, "y": int};

type point = {.. "x": int, "y": int};

let p = {x: 1, y: 1};

let p = {"x": 1, "y": 1};

/* should break */
type point = {x: int, y: int
};

type point = {. "x": int, "y": int
};

let p = {x: 1, y: 1
};

let p = {"x": 1, "y": 1
};

type point = {
  x: int, y: int};

type point = {.
  "x": int, "y": int};

let p = {
  x: 1, y: 1};

let p = {
  "x": 1, "y": 1};

type point = {x: int,
  y: int};

type point = {. "x": int,
  "y": int};

let p = {x: 1,
  y: 1};

let p = {"x": 1,
  "y": 1};

type point = {
  x: int,
  y: int
};

type point = {
  .
  "x": int,
  "y": int
};

let p = {
  x: 1,
  y: 1
};

let p = {
  "x": 1,
  "y": 1
};
