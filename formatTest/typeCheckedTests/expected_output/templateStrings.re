[@reason.version 3.7];
/**
 * Comments:
 */

let addTwo = (a, b) => string_of_int(a + b);
let singleLineConstant = `
  Single line template
`;
let singleLineInterpolate = `
  Single line ${addTwo(1, 2)}!
`;

let multiLineConstant = `
  Multi line template
  Multi %a{x, y}line template
  Multi line template
  Multi line template
`;

let printTwo = (a, b) => {
  print_string(a);
  print_string(b);
};

let templteWithAttribute =
  [@attrHere]
  `
    Passing line template
    Passing line template
    Passing line template
    Passing line template
  `;

let result =
  print_string(
    `
      Passing line template
      Passing line template
      Passing line template
      Passing line template
    `,
  );

let resultPrintTwo =
  printTwo(
    "short one",
    `
      Passing line template
      Passing line template
      Passing line template
      Passing line template
    `,
  );

let hasBackSlashes = `
  One not escaped: \
  Three not escaped: \ \ \
  Two not escaped: \\
  Two not escaped: \\\
  One not escaped slash, and one escaped tick: \\`
  Two not escaped slashes, and one escaped tick: \\\`
  Two not escaped slashes, and one escaped dollar-brace: \\\${
  One not escaped slash, then a close tick: \
`;

let singleLineInterpolateWithEscapeTick = `
  Single \`line ${addTwo(1, 2)}!
`;

let singleLineConstantWithEscapeDollar = `
  Single \${line template
`;

// The backslash here is a backslash literal.
let singleLineInterpolateWithBackslashThenDollar = `
  Single \$line ${addTwo(2, 3)}!
`;

let beforeExpressionCommentInNonLetty = `
  Before expression comment in non-letty interpolation:
  ${/* Comment */ string_of_int(1 + 2)}
`;

let beforeExpressionCommentInNonLetty2 = `
  Same thing but with comment on own line:
  ${
    /* Comment */
    string_of_int(10 + 8)
  }
`;
module StringIndentationWorksInModuleIndentation = {
  let beforeExpressionCommentInNonLetty2 = `
    Same thing but with comment on own line:
    ${
      /* Comment */
      string_of_int(10 + 8)
    }
  `;
};

let beforeExpressionCommentInNonLetty3 = `
  Same thing but with text after final brace on same line:
  ${
    /* Comment */
    string_of_int(20 + 1000)
  }TextAfterBrace
`;

let beforeExpressionCommentInNonLetty3 = `
  Same thing but with text after final brace on next line:
  ${
    /* Comment */
    string_of_int(100)
  }
  TextAfterBrace
`;

let x = 0;
let commentInLetSequence = `
  Comment in letty interpolation:
  ${
    /* Comment */
    let x = 200 + 49;
    string_of_int(x);
  }
`;

let commentInLetSequence2 = `
  Same but with text after final brace on same line:
  ${
    /* Comment */
    let x = 200 + 49;
    string_of_int(x);
  }TextAfterBrace
`;

let commentInLetSequence3 = `
  Same but with text after final brace on next line:
  ${
    /* Comment */
    let x = 200 + 49;
    string_of_int(x);
  }
  TextAfterBrace
`;

let reallyCompicatedNested = `
  Comment in non-letty interpolation:

  ${
    /* Comment on first line of interpolation region */

    let y = (a, b) => a + b;
    let x = 0 + y(0, 2);
    // Nested string templates
    let s = `
      asdf${addTwo(0, 0)}
      alskdjflakdsjf
    `;
    s ++ s;
  }same line as brace with one space
  and some more text at the footer no newline
`;

let reallyLongIdent = "!";
let backToBackInterpolations = `
  Two interpolations side by side:
  ${addTwo(0, 0)}${addTwo(0, 0)}
  Two interpolations side by side with leading and trailing:
  Before${addTwo(0, 0)}${addTwo(0, 0)}After

  Two interpolations side by side second one should break:
  Before${addTwo(0, 0)}${
    reallyLongIdent
    ++ reallyLongIdent
    ++ reallyLongIdent
    ++ reallyLongIdent
  }After

  Three interpolations side by side:
  Before${addTwo(0, 0)}${
    reallyLongIdent
    ++ reallyLongIdent
    ++ reallyLongIdent
    ++ reallyLongIdent
  }${
    ""
  }After
`;
