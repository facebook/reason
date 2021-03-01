## Future

- Add support for (limited) interop between letop + OCaml upstream (@anmonteiro) [#2624](https://github.com/facebook/reason/pull/2624)
- Add support for OCaml 4.12 (@kit-ty-kate) [#2635](https://github.com/facebook/reason/pull/2635)
- Remove support for OCaml 4.02.3 (@anmonteiro) [#2638](https://github.com/facebook/reason/pull/2638)

## 3.6.2

**New Feature, Non Breaking:**
- Reason Syntax v4 [NEW-FEATURE-NON-BREAKING]: Angle Brackets Type Parameters (PARSING) (@jordwalke)[#2604][https://github.com/facebook/reason/pull/2604]

**Bug Fixes:**
- Fix printing of externals that happen to have newlines/quotes in them (@jordwalke)[#2593](https://github.com/facebook/reason/pull/2593)
- Fix parsing/printing of attributes on patterns (@jordwalke)[#2592](https://github.com/facebook/reason/pull/2592)
- Fix Windows CI (@ManasJayanth) [#2611](https://github.com/facebook/reason/pull/2611)
- Fix uncurry attribute on function application(@anmonteiro) [#2566](https://github.com/facebook/reason/pull/2566)
- Support OCaml 4.11 (@anmonteiro) [#2582](https://github.com/facebook/reason/pull/2582)
- Vendor ocaml-migrate-parsetree for greater compatibility (@jordwalke) [#2623](https://github.com/facebook/reason/pull/2623)

**Docs:**
- README Reason logo (@iamdarshshah)[#2609][https://github.com/facebook/reason/pull/2609]

**Changes:**
- Updated README.md by adding Logo at the top & aligned header elements to the center (@iamdarshshah)[#2609](https://github.com/facebook/reason/pull/2609)

## 3.6.0

**New Feature, Non Breaking:**
- External syntax: make the `external ... = ""` part optional (@romanschejbal)[#2464](https://github.com/facebook/reason/pull/2464)
  - `external myFn: (string) => unit;` is now equivalent to `external myFn: (string) => unit = "";`

**Bug Fixes:**
- Fixes issues where `method` and similar keywords will be transformed to `method_` (@cristianoc) [#2530](https://github.com/facebook/reason/pull/2530)

## 3.5.4

Fixes:
- Fix regression where keywords were not renamed correctly (@cristianoc) [#2520](https://github.com/facebook/reason/pull/2520)
- Fix regression where quoted object attributes / labeled arguments weren't renamed correctly (@anmonteiro) [#2509](https://github.com/facebook/reason/pull/2509)
- Fix issue where JSX braces break into multiple lines (@anmonteiro) [#2503](https://github.com/facebook/reason/pull/2503)

Others:
- Improve bspacks process for 4.06 and add esy workflow for building refmt.js


## 3.5.3
- 🎉 MUCH better parsing error locations - more reliable autocomplete 🎉 (@let-def)[https://github.com/let-def] ([#2439](https://github.com/facebook/reason/pull/2439))
- Rebased the better error recovery diff onto 4.09 OCaml [@anmonteiro](https://github.com/anmonteiro) ([#2480](https://github.com/facebook/reason/pull/2480))
- Fix printing of fragments inside JSX props [@anmonteiro](https://github.com/anmonteiro) ([#2463](https://github.com/facebook/reason/pull/2463))
- Modernize CI based on latest hello-reason CI [@jordwalke](https://github.com/jordwalke) ([#2479](https://github.com/facebook/reason/pull/2479))
- Fix bug that caused necessary braces to be removed [@anmonteiro](https://github.com/anmonteiro) ([#2481](https://github.com/facebook/reason/pull/2481))
- Make prepublish script auto-generate opam files [@jordwalke](https://github.com/jordwalke) ([#2468](https://github.com/facebook/reason/pull/2468))
- Fix brace removal with pipe-first in JSX attributes [@bloodyowl](https://github.com/bloodyowl) ([#2474](https://github.com/facebook/reason/pull/2474))
- CI Improvements [@ulrikstrid](https://github.com/ulrikstrid) ([#2459](https://github.com/facebook/reason/pull/2459))
- Make sure you can still include rtop from inside utop [@sync ](https://github.com/sync ) ([#2466](https://github.com/facebook/reason/pull/2466))

## 3.5.2
- Support OCaml 4.09 ([2450](https://github.com/facebook/reason/pull/2450)).
- Improve opam packaging config ([2431](https://github.com/facebook/reason/pull/2431)).
- Improve repo to support esy resolutions to master branch ([31225fc0](https://github.co(https://github.com/facebook/reason/commit/31225fc066731075b6fa695e555f65ffcc172bcf))

## 3.5.0
Improvements:
- Support OCaml 4.08 ([2426](https://github.com/facebook/reason/pull/2426)).

Fixes:
- Print attributes in class fields [2414](https://github.com/facebook/reason/pull/2414).
- Preserve function body braces when Pexp_fun is an argument to a function [commit](https://github.com/facebook/reason/commit/f8eb7b1c1f3bc93883b663bb6b7fc0552e7b1791)
- Prettify try to hug braces [2378](https://github.com/facebook/reason/pull/2378)
- Fix operator swap for type declarations [commit](https://github.com/facebook/reason/commit/d4516beaceb1fa1fa53b9d1c30565c7e7cacd39b)
- Fix JSX removing semicolons [commit](https://github.com/facebook/reason/commit/ab4bf53ab1a76d7ead7e634489a2a1fcbb7cf817)
- Better formatting of Pexp_lazy [commit](https://github.com/facebook/reason/commit/46bffd1590a4f19a72a9c6e8d754bb47fb63fa4b)

## 3.4.2

Not released to @esy-ocaml/reason - would have required a major version bump.
These features will be released in 3.5.0.

Improvements:
- Parse and print parentheses around inline record declarations ([2363](https://github.com/facebook/reason/pull/2363))
- Proper outcome printing (for editor and build) of inline records ([2336](https://github.com/facebook/reason/pull/2336))
- Proper outcome printing of types with inline records (parentheses) ([2370](https://github.com/facebook/reason/pull/2370))

## 3.4.1

Fixes:
- Don't remove semis in blocks inside ternary expressions as jsx children ([2352](https://github.com/facebook/reason/pull/2352)).
- Handle single line comments ending with end-of-file ([2353](https://github.com/facebook/reason/pull/2353)).

## 3.4.0

Fixes:
- Don't pun record types if they contain attributes ([2316](https://github.com/facebook/reason/pull/2316)).

Improvements:

- `// line comments`! ([2268](https://github.com/facebook/reason/pull/2146)). Make sure that your constraints on `refmt` versions for native projects. Specify a version >= `3.4.0` if you use `//` comments in your Reason code. Specifiy ranges like `3.4.0-3.5.0`.
- Better whitespace interleaving ([1990](https://github.com/facebook/reason/pull/1990)).
- Allow Reason to be used with Merlin Natively on Windows ([2256](https://github.com/facebook/reason/pull/2256)).
- Improved Ternary Formatting ([2294](https://github.com/facebook/reason/pull/2294)).

## 3.3.4

Fixes:

- Pipe first braces ([2133](https://github.com/facebook/reason/pull/2133), [2148](https://github.com/facebook/reason/pull/2148)).
- Better rtop `use` directives ([2146](https://github.com/facebook/reason/pull/2146), [2147](https://github.com/facebook/reason/pull/2147)).
- `foo(~Foo.a)` becoming `foo(~Fooa=Foo.a)` ([2136](https://github.com/facebook/reason/pull/2136)).
- Parse `<div> ...c</div>` correctly ([2137](https://github.com/facebook/reason/pull/2137)).
- Invalid formatting of first-class module with type constraint ([2151](https://github.com/facebook/reason/pull/2151)).
- Precedence printing of pipe first with underscore sugar as JSX child ([2159](https://github.com/facebook/reason/pull/2159)).
- Correct location for JSX name & extension expression ([2166](https://github.com/facebook/reason/pull/2166), [2162](https://github.com/facebook/reason/pull/2162)).
- Lack of space after `module type of` ([2175](https://github.com/facebook/reason/pull/2175)).
- Outcome printer (editor & interface generator) function signature ([2185](https://github.com/facebook/reason/pull/2185)).
- Precedence issue with unary operators and labeled arguments ([2201](https://github.com/facebook/reason/pull/2201)).
- Type printing of polymorphic variants row fields ([2191](https://github.com/facebook/reason/pull/2191)).
- Pattern parsing inside ternary expressions ([2188](https://github.com/facebook/reason/pull/2188)).

Improvements:

- Preserve empty lines in records and bs objects ([2152](https://github.com/facebook/reason/pull/2152)).
- Make `let not = blabla` work (not is a keyword) ([2197](https://github.com/facebook/reason/pull/2197)).
- Format doc comments on variant leafs with consistency ([2194](https://github.com/facebook/reason/pull/2194))
- Single pipe first printing layout ([2193](https://github.com/facebook/reason/pull/2193)).
- Performance. One case where the printer took exponential time ([2195](https://github.com/facebook/reason/pull/2195)).

## 3.3.3

- More fixes for pipe first ([2120](https://github.com/facebook/reason/pull/2120), [2119](https://github.com/facebook/reason/pull/2119), [2111](https://github.com/facebook/reason/pull/2111)).
- Fix regressed printing of first-class module ([2124](https://github.com/facebook/reason/pull/2124)).
- Fix local open printing for `bs.obj` ([2123](https://github.com/facebook/reason/pull/2123)).
- fix printing of `foo[(bar + 1)]` to `foo[bar + 1]` ([2110](https://github.com/facebook/reason/pull/2110)).
- Only wrap `fun` in parentheses when necessary ([2033](https://github.com/facebook/reason/pull/2033)).
- Change all precedence printing to braces for consistency inside JSX ([2106](https://github.com/facebook/reason/pull/2106)).
- Format docblock comments above std attrs on record rows ([2105](https://github.com/facebook/reason/pull/2105)).

## 3.3.2

Big release! No breaking change. Big shout out to [@anmonteiro](https://twitter.com/anmonteiro90) and [@iwanKaramazow](https://twitter.com/_iwan_refmt)!

Major:
- Pipe first `|.` now got a Reason sugar, `->`, with better precedence support than the former ([1999](https://github.com/facebook/reason/pull/1999), [2078](https://github.com/facebook/reason/pull/2078), [2092](https://github.com/facebook/reason/pull/2092), [2082](https://github.com/facebook/reason/pull/2082), [2087](https://github.com/facebook/reason/pull/2087), [2055](https://github.com/facebook/reason/pull/2055)).
- ReasonReact JSX PPX DOM children spread ([2095](https://github.com/facebook/reason/pull/2095)).
- ReasonReact JSX PPX fragment ([2091](https://github.com/facebook/reason/pull/2091)).
- Other ReasonReact JSX PPX fixes ([2088](https://github.com/facebook/reason/pull/2088), [2060](https://github.com/facebook/reason/pull/2060), [2027](https://github.com/facebook/reason/pull/2027), [2024](https://github.com/facebook/reason/pull/2024), [2007](https://github.com/facebook/reason/pull/2007), [2021](https://github.com/facebook/reason/pull/2021), [1963](https://github.com/facebook/reason/pull/1963)).
- Semicolon relaxation & reporting improvements ([2040](https://github.com/facebook/reason/pull/2040), [2012](https://github.com/facebook/reason/pull/2012), [1968](https://github.com/facebook/reason/pull/1968)).
- Module parsing & formatting improvements ([2061](https://github.com/facebook/reason/pull/2061), [2059](https://github.com/facebook/reason/pull/2059), [1984](https://github.com/facebook/reason/pull/1984), [1949](https://github.com/facebook/reason/pull/1949), [1946](https://github.com/facebook/reason/pull/1946), [2062](https://github.com/facebook/reason/pull/2062)).
- Remove extra space in some places after formatting ([2047](https://github.com/facebook/reason/pull/2047), [2041](https://github.com/facebook/reason/pull/2041), [1969](https://github.com/facebook/reason/pull/1969), [1966](https://github.com/facebook/reason/pull/1966), [2097](https://github.com/facebook/reason/pull/2097)).
- Much better `...` spread errors for everything ([1973](https://github.com/facebook/reason/pull/1973)).
- Fix `foo##bar[baz]`, `foo->bar^##baz` and other precedences ([2050](https://github.com/facebook/reason/pull/2050), [2055](https://github.com/facebook/reason/pull/2055), [2044](https://github.com/facebook/reason/pull/2044), [2044](https://github.com/facebook/reason/pull/2044)).
- Milder "unknown syntax error" message ([1962](https://github.com/facebook/reason/pull/1962)).

Others:
- Parentheses hugging for multi-line `Js.t({foo: bar})` ([2074](https://github.com/facebook/reason/pull/2074)).
- Correctly parse prefix ops in labeled parameters ([2071](https://github.com/facebook/reason/pull/2071)).
- Attach doc attributes before extension sugar ([2069](https://github.com/facebook/reason/pull/2069)).
- Support non-parenthesized label colon type equal optional in type declarations ([2058](https://github.com/facebook/reason/pull/2058)).
- Printf uncurried application when last argument is a callback ([2064](https://github.com/facebook/reason/pull/2064)).
- OCaml rtop syntax printing( [2031](https://github.com/facebook/reason/pull/2031)).
- Fix Bigarray syntax ([2045](https://github.com/facebook/reason/pull/2045)).
- Parse `M.[]` ([2043](https://github.com/facebook/reason/pull/2043)).
- Fix printing of polymorphic variant with annotation ([2019](https://github.com/facebook/reason/pull/2019)).
- Format GADT type variants better ([2016](https://github.com/facebook/reason/pull/2016)).
- Better autocomplete for Merlin ([1998](https://github.com/facebook/reason/pull/1998)).
- Print newline after doc comments before attributes ([1869](https://github.com/facebook/reason/pull/1869)).
- Fix inconsistent printing of opening extension expressions ([1979](https://github.com/facebook/reason/pull/1979)).
- Fix error when parsing `let x=-.1;` and others ([1945](https://github.com/facebook/reason/pull/1945)).
- Arguments no longer accidentally punned when they carry attributes ([1955](https://github.com/facebook/reason/pull/1955)).

## 3.2.0

See the blog post [here](https://reasonml.github.io/blog/2018/05/25/reason-3.2.0.html).

- **WHITESPACES IMPROVEMENTS ARE HERE**: empty lines between most things will now be preserved when you format your code! Multiple lines still collapse to one line in most cases ([1921](https://github.com/facebook/reason/pull/1921), [1919](https://github.com/facebook/reason/pull/1919), [1876](https://github.com/facebook/reason/pull/1876)).
- **Semicolon relaxation**: see blog post ([1887](https://github.com/facebook/reason/pull/1887)).
- Fix parsing & printing of es6 function syntax inside attributes ([1943](https://github.com/facebook/reason/pull/1943)).
- List spread now has better error ([1925](https://github.com/facebook/reason/pull/1925)).
- Functor in JSX tags ([1927](https://github.com/facebook/reason/pull/1927)).
- Better comment printing ([1940](https://github.com/facebook/reason/pull/1940), [1934](https://github.com/facebook/reason/pull/1934)).
- Various other printer improvements.

## 3.1.0

- **New pipe sugar for function call argument in arbitrary position**: `foo |> map(_, addOne) |> filter(_, isEven)` ([1804](https://github.com/facebook/reason/pull/1804)).
- **BuckleScript [@bs] uncurry sugar**: `[@bs] foo(bar, baz)` is now `foo(. bar, baz)`. Same for declaration ([1803](https://github.com/facebook/reason/pull/1803), [1832](https://github.com/facebook/reason/pull/1832)).
- **Trailing commas** for record, list, array, and everything else ([1775](https://github.com/facebook/reason/pull/1775), [1821](https://github.com/facebook/reason/pull/1821))!
- Better comments interleaving ([1769](https://github.com/facebook/reason/pull/1769), [1770](https://github.com/facebook/reason/pull/1770), [1817](https://github.com/facebook/reason/pull/1817))
- Better JSX printing: `<Foo bar=<Baz />>`, `<div><span></span></div>` ([1745](https://github.com/facebook/reason/pull/1745), [1762](https://github.com/facebook/reason/pull/1762)).
- **switch** now mandates parentheses around the value. Non-breaking, as we currently support parentheses-less syntax but print parens ([1720](https://github.com/facebook/reason/pull/1720), [1733](https://github.com/facebook/reason/pull/1733)).
- Attributes on open expressions ([1833](https://github.com/facebook/reason/pull/1833)).
- Better OCaml 4.06 support ([1709](https://github.com/facebook/reason/pull/1709)).
- Extension points sugar: `let%foo a = 1` ([1703](https://github.com/facebook/reason/pull/1703))!
- Final expression in a function body now also has semicolon. Easier to add new expressions afterward now ([1693](https://github.com/facebook/reason/pull/1693))!
- Better editor printing (outcome printer) of Js.t object types, @bs types, unary variants and infix operators ([1688](https://github.com/facebook/reason/pull/1688), [1784](https://github.com/facebook/reason/pull/1784), [1831](https://github.com/facebook/reason/pull/1831)).
- Parser doesn't throw Location.Error anymore; easier exception handling when refmt is used programmatically ([1695](https://github.com/facebook/reason/pull/1695)).

## 3.0.4

- **Default print width is now changed from 100 to 80** ([1675](https://github.com/facebook/reason/pull/1675)).
- Much better callback formatting ([1664](https://github.com/facebook/reason/pull/1664))!
- Single argument function doesn't require wrapping the argument with parentheses anymore ([1692](https://github.com/facebook/reason/pull/1692)).
- Printer more lenient when user writes `[%bs.obj {"foo": bar}]`. Probably a confusion with just `{"foo": bar}` ([1659](https://github.com/facebook/reason/pull/1659)).
- Better formatting for variants constructors with attributes ([1668](https://github.com/facebook/reason/pull/1668), [1677](https://github.com/facebook/reason/pull/1677)).
- Fix exponentiation operator printing associativity ([1678](https://github.com/facebook/reason/pull/1678)).

## 3.0.2

- **JSX**: fix most of the parsing errors (#856 #904 [1181](https://github.com/facebook/reason/pull/1181) [1263](https://github.com/facebook/reason/pull/1263) [1292](https://github.com/facebook/reason/pull/1292))!! Thanks @IwanKaramazow!
- In-editor syntax error messages are now fixed! They should be as good as the terminal ones ([1654](https://github.com/facebook/reason/pull/1654)).
- Polymorphic variants can now parse and print \`foo(()) as \`foo() ([1560](https://github.com/facebook/reason/pull/1560)).
- Variant values with annotations like `Some((x: string))` can now be `Some(x: string)` ([1576](https://github.com/facebook/reason/pull/1576)).
- Remove few places remaining that accidentally print `fun` for functions ([1588](https://github.com/facebook/reason/pull/1588)).
- Better record & object printing ([1593](https://github.com/facebook/reason/pull/1593), [1596](https://github.com/facebook/reason/pull/1596)).
- Fewer unnecessary wrappings in type declarations and negative constants ([1616](https://github.com/facebook/reason/pull/1616), [1634](https://github.com/facebook/reason/pull/1634)).
- Parse and print attributes on object type rows ([1637](https://github.com/facebook/reason/pull/1637)).
- Better printing of externals with attributes ([1640](https://github.com/facebook/reason/pull/1640)).
- Better printing for multiple type equations in a module type in a function argument ([1641](https://github.com/facebook/reason/pull/1641)).
- Better printing for unary -. in labeled argument ([1642](https://github.com/facebook/reason/pull/1642)).

## 3.0.0

Our biggest release! **Please see our blog post** on https://reasonml.github.io/blog/2017/10/27/reason3.html.

Summary: this is, practically speaking, a **non-breaking** change. You can mix and match two projects with different syntax versions in BuckleScript 2 (which just got release too! Go check), and they'll Just Work (tm).

To upgrade your own project, we've released a script, https://github.com/reasonml/upgradeSyntaxFrom2To3

Improvements:

- Much better printing for most common idioms.
- Even better infix operators formatting for `==`, `&&`, `>` and the rest ([1380](https://github.com/facebook/reason/pull/1380), [1386](https://github.com/facebook/reason/pull/1386), etc.).
- More predictable keyword swapping behavior ([1539](https://github.com/facebook/reason/pull/1539)).
- BuckleScript's `Js.t {. foo: bar}` now formats to `{. "foo": bar}`, just like its value counterpart (`[%bs.obj {foo: 1}]` to `{"foo": bar}`.
- `[@foo]`, `[@@foo]` and `[@@@foo]` are now unified into `[@foo]` and placed in front instead of at the back.
- `!` is now the logical negation. It was `not` previously.
- Dereference was `!`. Now it's a postfix `^`.
- Labeled argument with type now has punning!
- String concat is now `++` instead of the old `^`.
- For native, Reason now works on OCaml 4.05 and the latest topkg ([1438](https://github.com/facebook/reason/pull/1438)).
- Record field punning for module field prefix now prints well too: `{M.x, y}` is `{M.x: x, y: y}`.
- JSX needs `{}` like in JS.
- Fix reason-specific keywords printing in interface files (e.g. `==`, `match`, `method`).
- Record punning with renaming ([1517](https://github.com/facebook/reason/pull/1517)).
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

- Much better infix operators (e.g. |>) formatting! ([1259](https://github.com/facebook/reason/pull/1259))
- Official `refmt.js`, with public API. See `README.md`. We've back-ported this into the 1.13.7 release =)

## 1.13.6

- Changelog got sent into a black hole
