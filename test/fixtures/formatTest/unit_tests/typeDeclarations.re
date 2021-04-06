/* === test wrapping for arrows === */
type foo = option((int => int));
type foo = option((int => (int => int)));
type foo = option(((int => int) => int));
type foo = option(((int, int) => int));

/* tuple */
type foo = option(((int => int), (int => int)));
type foo = option(((int => int), string));
type foo = option((string, (int => int), string));
type foo = option((string, (int => int)));

/* other preceeding/trailing */
type foo = option((int => int), (int => int));
type foo = option((int => int), string);
type foo = option(string, (int => int), string);
type foo = option(string, (int => int));

/* preceeding/trailing, more args */
type foo = option((int => string => int), (int => string => int));
type foo = option((int => string => int), string);
type foo = option(string, (int => string => int), string);
type foo = option(string, (int => string => int));

/* others */
type foo = option(string, option(int => int), string);
type foo = option(string, option(option(option(int) => int)), string);
type foo = option(string, option([@foo]option(option(int) => int)), string);

/* with attributes */
type foo = option([@foo]([@bar] int => [@baz] int));
type foo = option([@foo]((([@bar] int) => [@baz] int)));
type foo = option((int => [@foo](int => int)));
type foo = option(([@foo](int => int) => int));
type foo = option(([@foo](int, int) => int));

/* tuple */
type foo = option([@foo]([@bar](int => int), [@baz](int => int)));
type foo = option([@foo]([@bar](int => int), [@baz] string));
type foo = option([@foo]([@bar]string, [@baz](int => int), [@qux]string));
type foo = option((string, [@foo](int => int)));

/* other preceeding/trailing */
type foo = option([@foo](int => int), [@bar](int => int));
type foo = option([@foo](int => int), [@bar]string);
type foo = option([@foo]string, [@bar](int => int), [@baz]string);
type foo = option([@foo]string, [@bar](int => int));

/* preceeding/trailing, more args */
type foo = option([@foo](int => string => int), [@bar](int => string => int));
type foo = option([@foo](int => string => int), [@bar]string);
type foo = option([@foo]string, [@bar](int => string => int), [@baz]string);
type foo = option([@foo]string, [@bar](int => string => int));

/* === end test wrapping for arrows === */

/* https://github.com/facebook/reason/issues/2073 */
type a = array({. "someStringKeyThatCausesLineToBreak": string });

type b = {
  punned: [@with_attribute] punned
};
