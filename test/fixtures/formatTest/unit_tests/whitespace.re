module Test = {
  open Belt;
  open React;

  type a = int;
  type b = string;

  let x = 12;
  let y = 34;

};

/** recursive let bindings */

/* see below */

let foo = "abc"
and bar = "def"
and baz = "ghi";

/* with whitespace */

let foo = "abc"

and bar = "def"

and baz = "ghi";

/** with whitespace and attrs */

/* -> */

[@foo]
let foo = "abc"

[@bar]
and bar = "def"

[@baz]
and baz = "ghi";

module Comments = {

  let z = 1;
  /* comment *without* whitespace interleaved*/
  let ab = 2;

  let add = (a, b) => a + b;


  /* comment *with* multiple newlines above */
  let min = (a, b) => a - b;


  let a = 1; /* trailing comment ok */
  let b = 2;

  /* comment on top */
  let x = 1; /* this comment sits at the end of the line */
  /* wow another one below too */

  let add = Test.x;


  /* this
     is
     a multiline
     comment */
  let minus = (a, b) => a - b;

  /* look
     another
     multi
     line
     comment */
  let vermenigvuldig = (a, b) => a * b;
  /* attach another comment below
     it spreads
     over
     multiple
     line
  */

  type x = {a: int /* comment1*/, b: string /* comment2 */};
};

module FloatingComments = {
  let a = 1;
  /* a */

  /* b */

  /* c */
  let b = 1;

  /* d */

  let c = 1;

  /* e */
  /* f */

  let d = 1;
  /* g */
  /* h */

  /* i */
  /* j */

  /* k */
  /* l */
  let e = 1;
};

module FloatingMultiLineComments = {
  let a = 1;
  /* 1
     2 */

  /* ok
     another one */

  /* wow
     here */
  let b = 1;

  /* float
     -ing */
  /* here
     on the second */

  let c = 1;

  /* one
     two */
  /* three
     four */

  /* extreme
     comment */
  /* here
      on two lines */

  /* another
     one */
  /* chocolate
     is
     good */


  let d = 2;
};

module type TestModuleType = {
  type a = int;
  type b = string;



  let x: a;
  let y: b;

};

let main = () => {
  let%lwt tc = tcGetAddr(stdin);

  let a = 1;

  let%lwt () = tcsetattr(stdin, TCSANOW, tc);

  let%lwt _i = write_string(stdout, s, 0, len);
  ();
};

module PatternMatching = {
  let x = switch(color) {
  | Black => ()

  | Red => ()

  | White => ()
  };

  /* with comments */
  let color = switch (color) {
  /* c1 */

  /* c2 */
  | Black =>
      "black"

  /* c3 */

  /* c4 */
  /* c5 */

  /* c6 */
  | Green => "green"

  /* multi
     line
     comment */

  | Blue => "blue"
  };
};

/** recursive modules without whitespace */
module rec A: {type t; let a_fn: t => B.t; let of_float: float => t;} = {
  type t = int;
  let a_fn = x => B.of_int(x);
  let of_float = x => int_of_float(x);
}
/* no whitespace */
and B: {type t; let another_fn: t => A.t; let of_int: int => t;} = {
  type t = float;
  let another_fn = x => A.of_float(x);
  let of_int = x => float_of_int(x);
};

/** recursive modules with whitespace */

/* -> below */

module rec A: {type t; let a_fn: t => B.t; let of_float: float => t;} = {
  type t = int;
  let a_fn = x => B.of_int(x);
  let of_float = x => int_of_float(x);
}

/** okok */

/* lala */

and B: {type t; let another_fn: t => A.t; let of_int: int => t;} = {
  type t = float;
  let another_fn = x => A.of_float(x);
  let of_int = x => float_of_int(x);
};

/** recursive modules with attrs */

/* -> below */

[@foo1]
module rec A: {type t; let a_fn: t => B.t; let of_float: float => t;} = {
  type t = int;
  let a_fn = x => B.of_int(x);
  let of_float = x => int_of_float(x);
}

/** okok */

/* lala */

[@foo2]
and B: {type t; let another_fn: t => A.t; let of_int: int => t;} = {
  type t = float;
  let another_fn = x => A.of_float(x);
  let of_int = x => float_of_int(x);
};

module EdgeCase = {
  let x = 1; /* a */

  /* b */

  /* c */

  let x = 1;
};

/** Record-like expressions */
let r = {
  a: 1,

  b: 2,
  c: 3,
};

/* with punning */
let r = {
  a,

  b,
  c,
};

/* with spread */
let r = {
  ...x,

  a: 1,

  b: 2,
  c: 3,
};

/* comments */
let r = {
  ...x,

  /* a */
  a: 1,

  /* b */
  /* c */

  /* d */
  b: 2,
  /* e */

  c: 3,

  /* f */
  d,

  e,
};

/* string keys */
let x = {
  "a": 1,

  "b": 2,
  "c": 3,
};

/* string keys punning */
let x = {
  "a",

  "b",
  "c"
};

/* string keys with spread */
let x = {
  ...x,

  "a": 1,

  "b": 2,
  "c": 3,
};

/* string keys with comments */
let x = {
  ...x,

  /* a */
  "a": 1,

  /* b */
  /* c */

  /* d */
  "b": 2,
  /* e */

  "c": 3,

  /* f */
  "d",

  "e",
};

let make = _children => {
  ...component,

  initialState: () => {
    posts: [],
    activeRoute: urlToRoute(ReasonReact.Router.dangerouslyGetInitialUrl()),
  },

  didMount: self => {
    let watcherID =
      ReasonReact.Router.watchUrl(url =>
        self.send(ChangeRoute(urlToRoute(url)))
      );
    self.onUnmount(() => ReasonReact.Router.unwatchUrl(watcherID));
  },

  reducer: (action, state) =>
    switch (action) {
    | ChangeRoute(activeRoute) =>
      ReasonReact.Update({...state, activeRoute})
    | FetchCats => ReasonReact.NoUpdate
    },

  render: ({state: {posts, activeRoute}}) =>
    <div>
      <h1> <a href="/"> {ReasonReact.string("Instagram")} </a> </h1>
      {
        switch (activeRoute) {
        | Default => <Grid posts />
        | Detail(postId) => <Single posts postId />
        }
      }
    </div>,
};

// Recursive types
// Also create another form for splicing in nodes into otherwise fixed length sets.
type elem('t) =
  | Empty: elem(empty)
constraint 't = ('st, 'a) => 'subtree
and subtree('t) =
  | EmptyInstance: subtree(empty);

// Also create another form for splicing in nodes into otherwise fixed length sets.
type elem('t) =
  | Empty: elem(empty)
constraint 't = ('st, 'a) => 'subtree

and subtree('t) =
  | EmptyInstance: subtree(empty);

type elem('t) =
  | Empty: elem(empty)
constraint 't = ('st, 'a) => 'subtree
// trailing comment

// leading comment
and subtree('t) =
  | EmptyInstance: subtree(empty);

type elem('t) =
  | Empty: elem(empty)
constraint 't = ('st, 'a) => 'subtree
// trailing comment

// in between

// leading comment
and subtree('t) =
  | EmptyInstance: subtree(empty);

// with attrs
type elem('t) =
  | Empty: elem(empty)
constraint 't = ('st, 'a) => 'subtree
[@attr]
and subtree('t) =
  | EmptyInstance: subtree(empty);

// with attrs
type elem('t) =
  | Empty: elem(empty)
constraint 't = ('st, 'a) => 'subtree

[@attr]
and subtree('t) =
  | EmptyInstance: subtree(empty);

let f = (a, b) => a + b;
/* this comment sticks at the end */

/* another one below the structure */
/* this one should stick */

/* :) */
