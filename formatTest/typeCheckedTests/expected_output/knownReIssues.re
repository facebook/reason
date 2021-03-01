[@reason.version 3.7];
/**
   Issue 940: https://github.com/facebook/reason/issues/940
   The parens in the exception match case with an alias,
   are required for correct parsing:
   i.e. (Sys_error _ as exc) instead of Sys_error _ as exc
   The latter doesn't type-check with Error: Unbound value exc.
   Warning 11 (unused match case) is also triggered.
 */
let f = () => raise(Sys_error("error"));

switch (f()) {
| x => ()
| exception (Sys_error(_) as exc) => raise(exc)
};

exception Foo(string);

let g = () => raise(Foo("bar errors"));

switch (g()) {
| x => ()
| exception (Foo(f)) => raise(Foo(f))
};
