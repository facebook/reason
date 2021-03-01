module Foo = {
  type t = {name: string};
};

let foo = (Foo.{ name }) => ();

let f =
  fun
  | Foo.{ name } => ()
  | _ => ();

let x = { Foo.name: "Reason" };
let Foo.{ name } = x;

let (Foo.{ name }, _) = (x, ());
