## 3.0.6 (soon to be released)

- **New pipe sugar for function call argument in arbitrary position**: `foo |> map(_, addOne) |> filter(_, isEven)` ([#1804](https://github.com/facebook/reason/pull/1804)).
- **BuckleScript [@bs] uncurry sugar**: `[@bs] foo(bar, baz)` is now `foo(. bar, baz)`. Same for declaration ([#1803](https://github.com/facebook/reason/pull/1803), [#1832](https://github.com/facebook/reason/pull/1832)).
- **Trailing commas** for record, list, array, and everything else ([#1775](https://github.com/facebook/reason/pull/1775), [#1821](https://github.com/facebook/reason/pull/1821))!
- Better comments interleaving ([#1769](https://github.com/facebook/reason/pull/1769), [#1770](https://github.com/facebook/reason/pull/1770), [#1817](https://github.com/facebook/reason/pull/1817))
- Better JSX printing: `<Foo bar=<Baz />>`, `<div><span></span></div>` ([#1745](https://github.com/facebook/reason/pull/1745), [#1762](https://github.com/facebook/reason/pull/1762)).
- **switch** now mandates parentheses around the value. Non-breaking, as we currently support parentheses-less syntax but print parens ([#1720](https://github.com/facebook/reason/pull/1720), [#1733](https://github.com/facebook/reason/pull/1733)).
- Better OCaml 4.06 support ([#1709](https://github.com/facebook/reason/pull/1709)).
- Extension points sugar: `let%foo a = 1` ([#1703](https://github.com/facebook/reason/pull/1703))!
- Final expression in a function body now also has semicolon. Easier to add new expressions afterward now ([#1693](https://github.com/facebook/reason/pull/1693))!
- Better editor printing (outcome printer) of Js.t object types, @bs types, unary variants and infix operators ([#1688](https://github.com/facebook/reason/pull/1688), [#1784](https://github.com/facebook/reason/pull/1784), [#1831](https://github.com/facebook/reason/pull/1831)).
- Parser doesn't throw Location.Error anymore; easier exception handling when refmt is used programmatically ([#1695](https://github.com/facebook/reason/pull/1695)).

## 3.0.4

- **Default print width is now changed from 100 to 80** ([#1675](https://github.com/facebook/reason/pull/1675)).
- Much better callback formatting ([#1664](https://github.com/facebook/reason/pull/1664))!
- Single argument function doesn't require wrapping the argument with parentheses anymore ([#1692](https://github.com/facebook/reason/pull/1692)).
- Printer more lenient when user writes `[%bs.obj {"foo": bar}]`. Probably a confusion with just `{"foo": bar}` ([#1659](https://github.com/facebook/reason/pull/1659)).
- Better formatting for variants constructors with attributes ([#1668](https://github.com/facebook/reason/pull/1668), [#1677](https://github.com/facebook/reason/pull/1677)).
- Fix exponentiation operator printing associativity ([#1678](https://github.com/facebook/reason/pull/1678)).

## 3.0.2

- **JSX**: fix most of the parsing errors (#856 #904 [#1181](https://github.com/facebook/reason/pull/1181) [#1263](https://github.com/facebook/reason/pull/1263) [#1292](https://github.com/facebook/reason/pull/1292))!! Thanks @IwanKaramazow!
- In-editor syntax error messages are now fixed! They should be as good as the terminal ones ([#1654](https://github.com/facebook/reason/pull/1654)).
- Polymorphic variants can now parse and print \`foo(()) as \`foo() ([#1560](https://github.com/facebook/reason/pull/1560)).
- Variant values with annotations like `Some((x: string))` can now be `Some(x: string)` ([#1576](https://github.com/facebook/reason/pull/1576)).
- Remove few places remaining that accidentally print `fun` for functions ([#1588](https://github.com/facebook/reason/pull/1588)).
- Better record & object printing ([#1593](https://github.com/facebook/reason/pull/1593), [#1596](https://github.com/facebook/reason/pull/1596)).
- Fewer unnecessary wrappings in type declarations and negative constants ([#1616](https://github.com/facebook/reason/pull/1616), [#1634](https://github.com/facebook/reason/pull/1634)).
- Parse and print attributes on object type rows ([#1637](https://github.com/facebook/reason/pull/1637)).
- Better printing of externals with attributes ([#1640](https://github.com/facebook/reason/pull/1640)).
- Better printing for multiple type equations in a module type in a function argument ([#1641](https://github.com/facebook/reason/pull/1641)).
- Better printing for unary -. in labeled argument ([#1642](https://github.com/facebook/reason/pull/1642)).

## 3.0.0

Our biggest release! **Please see our blog post** on https://reasonml.github.io/blog/2017/10/27/reason3.html.

Summary: this is, practically speaking, a **non-breaking** change. You can mix and match two projects with different syntax versions in BuckleScript 2 (which just got release too! Go check), and they'll Just Work (tm).

To upgrade your own project, we've released a script, https://github.com/reasonml/upgradeSyntaxFrom2To3

Improvements:

- Much better printing for most common idioms.
- Even better infix operators formatting for `==`, `&&`, `>` and the rest ([#1380](https://github.com/facebook/reason/pull/1380), [#1386](https://github.com/facebook/reason/pull/1386), etc.).
- More predictable keyword swapping behavior ([#1539](https://github.com/facebook/reason/pull/1539)).
- BuckleScript's `Js.t {. foo: bar}` now formats to `{. "foo": bar}`, just like its value counterpart (`[%bs.obj {foo: 1}]` to `{"foo": bar}`.
- `[@foo]`, `[@@foo]` and `[@@@foo]` are now unified into `[@foo]` and placed in front instead of at the back.
- `!` is now the logical negation. It was `not` previously.
- Dereference was `!`. Now it's a postfix `^`.
- Labeled argument with type now has punning!
- String concat is now `++` instead of the old `^`.
- For native, Reason now works on OCaml 4.05 and the latest topkg ([#1438](https://github.com/facebook/reason/pull/1438)).
- Record field punning for module field prefix now prints well too: `{M.x, y}` is `{M.x: x, y: y}`.
- JSX needs `{}` like in JS.
- Fix reason-specific keywords printing in interface files (e.g. `==`, `match`, `method`).
- Record punning with renaming ([#1517](https://github.com/facebook/reason/pull/1517)).
- The combination of function label renaming + type annotation + punning is now supported!
- Label is now changed from `::foo` back to `~foo`, just like for OCaml.
- Fix LOTS of bugs regarding parsing & formatting (closing around 100 improvement-related issues!).
- Official `refmt.js`, with public API. See `README.md`.
- Official `refmt` native public API too.
- **New JS application/abstraction syntax**. Yes yes, we know. Despite the 100+ fixes, this one's all you cared about. Modern software engineering ¯\\\_(ツ)\_/¯. Please do read the blog post though.

Breaking Changes:

- Remove `--use-stdin` and `--is-interface-pp` option from refmt; they've been deprecated for a long time now
- Remove unused binaries: `reup`, etc.
- Remove the old `reactjs_jsx_ppx.ml`. You've all been on `reactjs_jsx_ppx_2.ml` for a long time now.
- Reserved keywords can no longer be used as an `external` declaration's labels.

Deprecated:

- Deprecate `--add-printers` option from refmt; we'll have a better strategy soon.

## 1.13.7

- Much better infix operators (e.g. |>) formatting! ([#1259](https://github.com/facebook/reason/pull/1259))
- Official `refmt.js`, with public API. See `README.md`. We've back-ported this into the 1.13.7 release =)

## 1.13.6

- Changelog got sent into a black hole
