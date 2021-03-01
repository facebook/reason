[@reason.version 3.7];
/**
 * Tests external formatting.
 */
external foo: type_ = "%caml_something_or_other";

external multilineStringExtern: int => int =
  {|
 Did you know you can put whatver you want inside
 of an extern? Good luck with the linker though!
|};

module Nested = {
  external multilineStringExtern: int => int =
    {|
   Did you know you can put whatver you want inside
   of an extern? Good luck with the linker though!
  |};
  external multilineStringExternWithTag:
    int => int =
    {|
   Did you know you can put whatver you want inside
   of an extern? Good luck with the linker though!
  |};
  external multilineStringExtern: int => int =
    {|
   And this has a newline in it, so will be formatted with { | | } style string|};
  external containsQuote: int => int =
    {|This has a quote in it " so will be formatted as { | | } style string|};
  external noIndentation: int => int =
    {|
Did you know you can put whatver you want inside
of an extern? Good luck with the linker though!
|};
};
