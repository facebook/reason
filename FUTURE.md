
## 3.0.0

- Even better infix operators formatting for `==`, `&&`, `>` and the rest (#1380, #1386, etc.)
- Deprecated `--add-printers` option from refmt; we'll have a better strategy soon.
- Removed `--use-stdin` and `--is-interface-pp` option from refmt; they've been deprecated for a long time now
- Remove unused binaries: `reup`, etc.
- Remove the old `reactjs_jsx_ppx.ml`. You've all been on `reactjs_jsx_ppx_2.ml` for a long time now.
- New syntax based on (but not entirely) #1299
- BuckleScript's `Js.t {. foo: bar}` now formats to `{. "foo": bar}`, just like its value counterpart (`[%bs.obj {foo: 1}]` to `{"foo": bar}`.
- [@foo], [@@foo] and [@@@foo] are now unified into [@foo] and placed in front instead of in the back.
- `!` is now the logical negation. It was `not` previously.
- dereference was `!`. Now it's ...
- string concat is now `++` instead of the old `^`
- labeled argument with type now has punning!
- Works on ocaml 4.05 and the latest topkg (#1438)
- Record field punning for module field prefix now prints well too: `{M.x, y}` is `{M.x: x, y: y}`
- JSX needs {} like in JS
- Fix reason-specific keywords printing in interface files (e.g. `==`, `match`, `method`)
- Fix LOTS of bugs regarding parsing & formatting,
