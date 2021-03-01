/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

[@reason.version 3.8];

class type _module ('provider_impl) = {

};
type t;
class type bzz = {
  inherit _module(t)
};

class type t = { as 'a;
  constraint 'a = #s
};

/* https://github.com/facebook/reason/issues/2037 */
class type xt = { as 'a };

class x = {
  as self
};

class type classWithNoArgType {
  pub x : int;
  pub y : int
};

class classWithNoArg {
  pub x = 0;
  pub y = 0
};

class type t = {
  open M;
  as 'a;
};

class type t = {
  open M;
};
