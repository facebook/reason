[@reason.version 3.7];
/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

let named: (~a: int, ~b: int) => int;

let namedAlias: (~a: int, ~b: int) => int;

let namedAnnot:
  (~a: option(int), ~b: option(int)) => int;

let namedAliasAnnot:
  (~a: option(int), ~b: option(int)) => int;

let optional: (~a: 'a=?, ~b: 'b=?, unit) => int;

let optionalAlias:
  (~a: 'a=?, ~b: 'b=?, unit) => int;

let optionalAnnot:
  (~a: int=?, ~b: int=?, unit) => int;

let optionalAliasAnnot:
  (~a: int=?, ~b: int=?, unit) => int;

let defOptional:
  (~a: int=?, ~b: int=?, unit) => int;

let defOptionalAlias:
  (~a: int=?, ~b: int=?, unit) => int;

let defOptionalAnnot:
  (~a: int=?, ~b: int=?, unit) => int;

let defOptionalAliasAnnot:
  (~a: int=?, ~b: int=?, unit) => int;

let fun_option_int:
  (option(int), option(int)) => int;

/* Comments can be written like this.
   No leading star is required on each line.
   Everything will line up just fine.
   In this form, include the final closing on the last line. */
let test: int;
let test:
  /* And if the entire block needs to be re-indented
     such as this case, everything will still look okay. */
  int;

/*     You could begin the block bar out like this.
       And it still works correctly. */
let test: int;

/** Include multiple opening stars if you like.
    And it will still work. */
let test: int;

/** This comment will be corrected.
      when printed. */
let test: int;

/**  Comments with text on line zero
 *   Still work well with comments that have stars on the left side.
 */
let test: int;

let test:
  /* This kind of comment doesn't exactly render well though.
       Not many people write comments like this.
     */
  int;
