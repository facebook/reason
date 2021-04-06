/** Interleave whitespace intelligently in signatures */

/* a */
let a: int;

/** Hi I'm a doc comment for some_definition */
let some_definition:
  (
    ~what_a_long_label: M.its_a_type,
    ~just_need_to_make_this: long_enough_to_wrap
  ) =>
  unit;

/** Hi I'm a doc comment for another_definition */
let another_definition: unit => unit;

/* b */
let b: int;
/* trailing */

/* kkk */
/* ---> */

/* amazing */
let f: int => int;

/** stick to x */
type x = string;

/** one newline below */

type x = string;


/** one newline below with comment */

/* comment */
type x = string;

/** doc comment attached */
type payload =
|	PStr(structure)
|	PSig(signature)
|	PTyp(core_type)
|	PPat(pattern, option(expression));

/** doc comment with whitespace */

type payload =
|	PStr(structure)
|	PSig(signature)
|	PTyp(core_type)
|	PPat(pattern, option(expression));

/** doc comment with whitespace and comment below */

/* comment here */
type payload =
|	PStr(structure)
|	PSig(signature)
|	PTyp(core_type)
|	PPat(pattern, option(expression));

/** doc attached */
type core_type = {
  ptyp_desc: core_type_desc,
  ptyp_loc: Location.t,
  ptyp_attributes: attributes,
};

/** doc attached with newline */

type core_type = {
  ptyp_desc: core_type_desc,
  ptyp_loc: Location.t,
  ptyp_attributes: attributes,
};

/** doc attached with newline and comments */

/* comment1 */

/* comment2 */
type core_type = {
  ptyp_desc: core_type_desc,
  ptyp_loc: Location.t,
  ptyp_attributes: attributes,
};

/** doc attached */
type water +=
  | H20
  | Spritzer;

/** doc with newline */

type water +=
  | H20
  | Spritzer;

/** doc with newline and comment */

/* test */
type water +=
  | H20
  | Spritzer;

/** doc attached */
exception Key_not_found(string);

/** doc attached with newline  */

exception Key_not_found(string);

/** doc attached with newline and comment  */

/* comment */
exception Key_not_found(string);

/** doc on open */
open Belt;

/** doc on open with whitespace */

open Belt;

/** doc on open with whitespace and comment */

/* test */
open Belt;

/** doc attached */
include Shared;

/** doc attached with whitespace */
include Shared;

/** doc attached with whitespace and comment */

/* test */
include Shared;

/** doc attached */
module type X = {let x: int};

/** doc attached with whitespace */

module type X = {let x: int};

/** doc attached with whitespace and comment */

/* test */
module type X = {let x: int};

/** doc attached */
module X : MX;

/** doc attached with whitespace */

module X : MX;

/** doc attached with whitespace and comment */

/* test */
module X : MX;

/** doc attached */
module X = MX;

/** doc attached with whitespace */

module X = MX;

/** doc attached with whitespace and comment */

/* test */
module X = MX;

/** doc attr attached */
class type x = {
  pub height: int
};

/** doc attr with newline */

class type x = {
  pub height: int
};

/** doc attr with newline and comment */

/* test */
class type x = {
  pub height: int
};

/** doc attr attached */
[@id];

/** doc attr with newline */

[@id];

/** doc attr with newline and comment */

/* test */
[@id];

/** doc attr attached */
[%%obj {a: 1}];

/** doc attr attached with newline */

[%%obj {a: 1}];

/** doc attr attached with newline and comment */

/* test */
[%%obj {a: 1}];

/** doc attached */
class reason : ocaml;

/** doc attached with whitespace */

class reason : ocaml;

/** doc attached with whitespace and comment */

/* test */
class reason : ocaml;

/** doc attached */
module rec X1: Y1
and X2: Y2;

/** doc attached with whitespace */

module rec X1: Y1
and X2: Y2;

/** doc attached with whitespace and comment */

/* comment */
module rec X1: Y1
and X2: Y2;


/** rec modules with whitespace */

/* -> */

module rec X1: Y1

and X2: Y2;

/** rec modules with whitespace and attrs */

/* -> */

[@foo]
module rec X1: Y1

/** another one below */

/* random comment */

[@bar]
and X2: Y2;

/* notice the whitespace after the last signature item */

/* this one has whitespace interleaved */
/* stick together */

/* :) */
