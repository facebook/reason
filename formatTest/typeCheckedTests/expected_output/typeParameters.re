/**
 * Testing type parameters.
 */;
[@reason.version 3.7];

type threeThings('t) = ('t, 't, 't);
type listOf('t) = list('t);

type underscoreParam(_) =
  | Underscored;
type underscoreParamCovariance(+_) =
  | Underscored;
type underscoreParamContravariance(-_) =
  | Underscored;

type tickParamCovariance(+'a) =
  | Underscored;
type tickParamContravariance(-'a) =
  | Underscored;

let x: option(list('a)) = None;
type myFunctionType('a) = (
  list(('a, 'a)),
  int => option(list('a)),
);
let funcAnnoted = (~a: list(int)=[0, 1], ()) => a;

/**
 * Syntax that would be likely to conflict with lexing parsing of < > syntax.
 */

let zero = 0;
let isGreaterThanNegFive = zero > (-5);
let isGreaterThanNegFive2 = zero > (-5);
let isGreaterThanNegFive3 = zero > (-5);

let isGreaterThanEqNegFive = zero >= (-5);
let isGreaterThanEqNegFive2 = zero >= (-5);
let isGreaterThanEqNegFive3 = zero >= (-5);

let (>>=) = (a, b) => a >= b;

let isSuperGreaterThanEqNegFive = zero >>= (-5);
let isSuperGreaterThanEqNegFive2 = zero >>= (-5);
let isSuperGreaterThanEqNegFive3 = zero >>= (-5);

let jsx = (~children, ()) => 0;

type t('a) = 'a;
let optionArg = (~arg: option(t(int))=?, ()) => arg;
let optionArgList =
    (~arg: option(list(list(int)))=?, ()) => arg;
let defaultJsxArg = (~arg: t(int)=<jsx />, ()) => arg;
let defaultFalse = (~arg: t(bool)=!true, ()) => arg;
/* Doesn't work on master either let defaultTrue = (~arg:t<bool>= !!true) => arg; */

/**
 * Things likely to conflict or impact precedence.
 */
let neg = (-1);
let tru = !false;
let x =
  "arbitrary" === "example"
  && "how long" >= "can you get"
  && "seriously" <= "what is the line length";

let z = 0;
module Conss = {
  let (>-) = (a, b) => a + b;
  let four = 3 >- 1;
  let two = 3 >- (-1);
  let four' = 3 >- 1;

  let tr = 3 > (-1);
  let tr' = 3 > 1;
  let tr'' = 3 > (-1);
};

module Idents = {
  let (>-) = (a, b) => a + b;
  let four = z >- z;
  let two = z >- - z;
  let four' = z >- - (- z);

  let tr = z > - z;
  let tr' = z > - (- z);
  let tr'' = z > - (- (- z));
};
