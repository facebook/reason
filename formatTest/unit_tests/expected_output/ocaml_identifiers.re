[@reason.version 3.7];
/* Type names (supported with PR#2342) */
module T = {
  type pub_ = unit;
};

/* Value names (already supported) */
module V = {
  let method = ();
};

/* Record fields */
module R = {
  type r = {mutable method_: int};

  let foo = {method_: 4};

  let x = foo.method_;

  let () = foo.method_ = 42;

  let y =
    switch (foo) {
    | {method_: method} => method
    };

  let z =
    switch (foo) {
    | {method_: 12} => 21
    };
};

/* Class names and instance variables */
module C = {
  class pub_ = {
    as _;
  };

  class c = {
    as _;
    val pub_ = 0;
    pub method = () => ();
  };

  class c' = {
    as _;
    inherit class method;
    val! pub_ = 1;
  };
};

/* Class types */
module Ct = {
  class type method = {
    val method: unit => unit;
  };
};

/* Virtual */
module Cv = {
  class virtual method = {
    as _;
  };
};

/* Object methods */
module O = {
  let o = {as _; pub method = ()};
};

/* Function parameter labels */
module L = {
  let f = (~method_ as method) =>
    ignore(method);
};

/* Module types */
module type method = {};

/* Polymorphic variants (probably ok as-is?) */
module P = {
  type t = [ | `pub_ | `method];

  let x = `method;

  let () = (`method) => 34;
};

type method = string;

[@some_attr: type_]
[@other_attr: method]
type foo = {method_: method};

let f = (~method_ as method) => Js.log(method);

let x = f(~method_="GET");

type marshalFields = {. "switch": string};

let testMarshalFields: marshalFields = {
  "switch": "switch",
};

/* Not an identifier test, but this is testing OCaml -> RE */
let x =
  List.map(y => {
    ();
    y;
  });

let newType = (type method, ()) => ();
