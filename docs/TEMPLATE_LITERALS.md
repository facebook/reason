
Contributors: Lexing and Parsing String Templates:
===================================================
Supporting string templates requires coordination between the lexer, parser and
printer. The lexer (as always) creates a token stream, but when it encounters a
backtick, it begins a special parsing mode that collects the (mostly) raw text,
until either hitting a closing backtick, or a `${`. If it encounters the `${`
(called an "interpolation region"), it will temporarily resume the "regular"
lexing approach, instead of collecting the raw text - until it hits a balanced
`}`, upon which it will enter the "raw text" mode again until it hits the
closing backtick.

- Parsing of raw text regions and regular tokenizing: Handled by
  `reason_declarative_lexer.ml`.
- Token balancing: Handled by `reason_lexer.ml`.

The output of lexing becomes tokens streamed into the parser, and the parser
`reason_parser.mly` turns those tokens into AST expressions.

## Lexing:

String templates are opened by:
- A backtick.
- Followed by any whitespace character (newline, or space/tab).

- Any whitespace character (newline, or space/tab).
- Followed by a backtick

```reason
let x = ` hi this is my string template `
let x = `
The newline counts as a whitespace character both for opening and closing.
`

```

Within the string template literal, there may be regions of non-string
"interpolation" where expressions are lexed/parsed.

```reason
let x = ` hi this is my ${expressionHere() ++ "!"} template `
```

Template strings are lexed into tokens, some of those tokens contain a string
"payload" with portions of the string content.
The opening backtick, closing backtick, and `${` characters do not become a
token that is fed to the parser, and are not included in the text payload of
any token. The Right Brace `}` closing an interpolation region `${` _does_
become a token that is fed to the parser. There are three tokens that are
produced when lexing string templates.

- `STRING_TEMPLATE_TERMINATED(string)`: A string region that is terminated with
  closing backtick. It may be the entire string template contents if there are
  no interpolation regions `${}`, or it may be the final string segment after
  an interpolation region `${}`, as long as it is the closing of the entire
  template.
- `STRING_TEMPLATE_SEGMENT_LBRACE(string)`: A string region occuring _before_
  an interpolation region `${`. The `string` payload of this token is the
  contents up until (but not including) the next `${`.
- `RBRACE`: A `}` character that terminates an interpolation region that
  started with `${`.

Simple example:

     STRING_TEMPLATE_TERMINATED
     |                         |
   ` lorem ipsum lorem ipsum bla `
    ^                          ^
    |                          |
    |                The closing backtick also doesn't show up in the token
    |                stream, but the last white space is part of the lexed
    |                STRING_TEMPLATE_TERMINATED token
    |                (it is used to compute indentation, but is stripped from
    |                the string constant, or re-inserted in refmting if not present)
    |
    The backtick doesn't show up anywhere in the token stream.  The first
    single white space after backtick is also not part of the lexed tokens.

Multiline example:

    All of this leading line whitespace remains parts of the tokens' payloads
    but it is is normalized and stripped when the parser converts the tokens
    into string expressions.
    |
    |   This newline not part of any token
    |   |
    |   v
    |  `
    +->   lorem ipsum lorem
          ipsum bla
       `
      ^
      |
      All of this white space on final line is part of the token as well.


For interpolation, the token `STRING_TEMPLATE_SEGMENT_LBRACE` represents the
string contents (minus any single/first white space after backtick), up to the
`${`. As with non-interpolated string templates, the opening and closing
backtick does not show up in the token stream, the first white space character
after opening backtick is not included in the lexed string contents, the final
white space character before closing backtick *is* part of the lexed string
token (to compute indentation), but that final white space character, along
with leading line whitespace is stripped from the string expression when the
parsing stage converts from lexed tokens to AST string expressions.

   ` lorem ipsum lorem ipsum bla${expression}lorem ipsum lorem ip lorem`
     |                         |            ||                        |
     STRING_TEMPLATE_TERMINATED             |STRING_TEMPLATE_TERMINATED
                                      RBRACE
## Parsing:

The string template tokens are turned into normal AST expressions.
`STRING_TEMPLATE_SEGMENT_LBRACE` and `STRING_TEMPLATE_TERMINATED` lexed tokens
contains all of the string contents, plus leading line whitespace for each
line, including the final whitespace before the closing backtick. These are
normalized in the parser by stripping that leading whitespace including two
additional spaces for nice indentation, before turning them into some
combination of string contants with a special attribute on the AST, or string
concats with a special attribute on the concat AST node.

```reason

// This:
let x = `
  Hello there
`;
// Becomes:
let x = [@reason.template] "Hello there";

// This:
let x = `
  ${expr} Hello there
`;
// Becomes:
let x = [@reason.template] (expr ++ [@reason.template] "Hello there");

```

User Documentation:
===================
> This section is the user documentation for string template literals, which
> will be published to the [official Reason Syntax
> documentation](https://reasonml.github.io/) when 

TODO
