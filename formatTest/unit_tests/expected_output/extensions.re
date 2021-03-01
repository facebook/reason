[@reason.version 3.7];

/* Extension sugar */

%extend
open M;

%extend
module M = {};

%extend
module type M = {};

type a = [%extend int];

let%extend x = "hi";

let x = {
  let%extend x = ();
  ignore();
  %extend
  ignore();
  let%extend x = ();
  %extend
  return("hi");
};

let x = {
  if%extend (true) {1} else {2};
  switch%extend (None) {
  | Some(x) => assert(false)
  | None => ()
  };
  try%extend(raise(Not_found)) {
  | Not_found => ()
  | Invalid_argument(msg) => prerr_endline(msg)
  };
};

let x = {
  if%extend (true) {1} else {2};
};

let x = {
  switch%extend (None) {
  | Some(x) => assert(false)
  | None => ()
  };
};

let x = {
  try%extend(raise(Not_found)) {
  | Not_found => ()
  | Invalid_argument(msg) => prerr_endline(msg)
  };
};

/* At structure level */

try%extend() {
| _ => ()
};

switch%extend () {
| _ => ()
};

if%extend (true) {1} else {2};

for%extend (i in 1 to 10) {
  ();
};

while%extend (false) {
  ();
};

[%extend () => ()];

fun%extend
| None => ()
| Some(1) => ();

/* In a top-level binding */

let x =
  try%extend() {
  | _ => ()
  };

let x =
  switch%extend () {
  | _ => ()
  };

let x = if%extend (true) {1} else {2};

let x =
  for%extend (i in 1 to 10) {
    ();
  };

let x =
  while%extend (false) {
    ();
  };

let x = [%extend () => ()];

let x =
  fun%extend
  | None => ()
  | Some(1) => ();

/* With two extensions, alone */

let x = {
  %extend1
  try%extend2() {
  | _ => ()
  };
};

let x = {
  %extend1
  switch%extend2 () {
  | _ => ()
  };
};

let x = {
  %extend1
  if%extend2 (true) {1} else {2};
};

let x = {
  %extend1
  for%extend2 (i in 1 to 10) {
    ();
  };
};

let x = {
  %extend1
  while%extend2 (false) {
    ();
  };
};

let x = {
  %extend1
  [%extend2 () => ()];
};

let x = {
  %extend1
  fun%extend2
  | None => ()
  | Some(1) => ();
};

/* With two extensions, first in sequence */

let x = {
  %extend1
  try%extend2() {
  | _ => ()
  };
  ignore();
};

let x = {
  ignore();
  %extend1
  switch%extend2 () {
  | _ => ()
  };
  ignore();
};

let x = {
  ignore();
  %extend1
  if%extend2 (true) {1} else {2};
  ignore();
};

let x = {
  ignore();
  %extend1
  for%extend2 (i in 1 to 10) {
    ();
  };
  ignore();
};

let x = {
  ignore();
  %extend1
  while%extend2 (false) {
    ();
  };
  ignore();
};

let x = {
  ignore();
  %extend1
  [%extend2 () => ()];
  ignore();
};

let x = {
  ignore();
  %extend1
  fun%extend2
  | None => ()
  | Some(1) => ();
};

/* With two extensions, in sequence */

let x = {
  ignore();
  %extend1
  try%extend2() {
  | _ => ()
  };
  ignore();
};

let x = {
  ignore();
  %extend1
  switch%extend2 () {
  | _ => ()
  };
  ignore();
};

let x = {
  ignore();
  %extend1
  if%extend2 (true) {1} else {2};
  ignore();
};

let x = {
  ignore();
  %extend1
  for%extend2 (i in 1 to 10) {
    ();
  };
  ignore();
};

let x = {
  ignore();
  %extend1
  while%extend2 (false) {
    ();
  };
  ignore();
};

let x = {
  ignore();
  %extend1
  [%extend2 () => ()];
  ignore();
};

let x = {
  ignore();
  %extend1
  fun%extend2
  | None => ()
  | Some(1) => ();
  ignore();
};

/* With two extensions, second in sequence */

let x = {
  ignore();
  %extend1
  try%extend2() {
  | _ => ()
  };
};

let x = {
  ignore();
  %extend1
  switch%extend2 () {
  | _ => ()
  };
};

let x = {
  ignore();
  %extend1
  if%extend2 (true) {1} else {2};
};

let x = {
  ignore();
  %extend1
  for%extend2 (i in 1 to 10) {
    ();
  };
};

let x = {
  ignore();
  %extend1
  while%extend2 (false) {
    ();
  };
};

let x = {
  ignore();
  %extend1
  [%extend2 () => ()];
};

let x = {
  ignore();
  %extend1
  fun%extend2
  | None => ()
  | Some(1) => ();
};

let _ =
  switch%ext (expr) {
  | A =>
    /* Comment under A */
    ()
  | B => ()
  };

/* comments in presence of extension point syntax #1938 */
let () = {
  /* 1. comment attached to extension */
  %defer
  /* 2. comment attached to expr in extension */
  cleanup();
  /* 3. comment attached to next expr */
  something_else();
};

/* comments in presence of extension point syntax #1938 */
let () = {
  /* random let binding */
  let x = 1;
  /* 1. comment attached to extension */
  %defer
  /* 2. comment attached to expr in extension */
  cleanup();
  /* 3. comment attached to next expr */
  something_else();
};

[%bs.raw x => x];

let work = () => {
  open Syntax;
  let%bind name = x;
  name;
};

/** header */
%raw
"console.log(42)";

/* https://github.com/facebook/reason/issues/2032 */
let predicate =
  predicate === Functions.alwaysTrue1
    ? defaultPredicate
    : fun%extend
      | None => false
      | Some(exn) => predicate(exn);
