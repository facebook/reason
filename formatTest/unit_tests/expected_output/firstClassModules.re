[@reason.version 3.7];
module Modifier = (
  val Db.Hashtbl.create():
    Db.Sig with type t = Mods.t
);
module Modifier = (
  val Db.Hashtbl.create():
    Db.Sig with type t = Mods.t
);
module Modifier = (
  val Db.Hashtbl.create():
    Db.Sig with type t = Mods.t
);
module Modifier = (
  val Db.Hashtbl.create():
    Db.Sig with type t = Mods.t
);
module Modifier = (val Db.Hashtbl.create());
module Modifier = (
  val Db.Hashtbl.create():
    Db.Sig with
      type t = Mods.t and
      type s = Mods.s and
      type z = Mods.z
);

module Lowercase = (val stuff: lowercase);
module Lowercase = (
  val stuff: Foo.Bar.lowercase
);
module Lowercase = (
  val stuff:
    Foo.Bar.lowercase with type t = Mods.t
);

module T = (
  val (module FirstClass): myLowercaseModule
);

module Three = (val three: X_int);

let thing: module Thing = (module MyModule);
let thing: module Foo.Bar.Thing =
  (module MyModule);

let smallThing: module lowercase = (module Mod);
let smallThing: module lowercase = (module Mod);
let smallThing: module Foo.Bar.lowercase =
  (module Mod);
let smallThing: module Foo.Bar.lowercase =
  (module Mod);

let f = (module Add: S.Z, x) => Add.add(x);

let join_iter =
    (
      type ta,
      type tb,
      module A: Sig with type t = ta,
      module B: Sig with type t = tb,
      module C: Sig with type t = tb,
      module D:
        Sig with
          type t = tb and
          type s = tc and
          type x = td and
          type z = te,
      fn,
    ) =>
  fn(A.value + B.value);

type t = ref(module Console);
type firstClassConsole = (module Console);

type crossPlatform =
  Platform.t(
    module Windows,
    module Mac,
    module Linux,
  );

type t = (
  module FirstClass,
  module SecondClass,
);

type withAttr = ref([@bar] (module Console));
type withAttrPlatform =
  Platform.t(
    [@bar] (module Iphone),
    [@foo] (module Ipad),
  );
type tWithAttr = (
  [@foo] (module FirstClass),
  [@bar] (module SecondClass),
);

type t = {m: (module M)};

/* https://github.com/facebook/reason/issues/2150 */
type t('a) = (module Test with type a = 'a);
