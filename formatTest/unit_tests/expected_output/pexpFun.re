let x =
  switch (x) {
  | Bar =>
    ReasonReact.UpdateWithSideEffects(
      { ...state, click: click + 1 },
      self => {
        let _ = 1;
        apply(bar);
      },
      "foo",
    )
  | Foo => ()
  };

let x =
  switch (x) {
  | Bar =>
    ReasonReact.UpdateWithSideEffects(
      self => {
        let _ = 1;
        apply(bar);
      },
    )
  | Foo => ()
  };

Mod.Update(
  (acc, curr) => {
    let x = 1;
    string_of_int(curr);
  },
  "",
  lst,
);

Mod.Update(
  (acc, curr): string => {
    let x = 1;
    string_of_int(curr);
  },
  "",
  lst,
);

Mod.Update(
  [@foo] [@bar] (acc, curr) => {
    let x = 1;
    string_of_int(curr);
  },
  "",
  lst,
);

Mod.Update(
  [@foo] [@bar] curr => string_of_int(curr),
  "",
  lst,
);

Mod.Update(
  [@foo] [@bar] [@baz] [@something] [@do] curr =>
    string_of_int(curr),
  "",
  lst,
);

Mod.Update(
  (
    acc,
    curr,
    lkdjf,
    lskdfj,
    sdfljk,
    slkdjf,
    skdjf,
    sdlkfj,
  ): string => {
    let x = 1;
    string_of_int(curr);
  },
  "",
  lst,
);

Mod.Update(
  (acc, curr) => string_of_int(curr),
  "",
  lst,
);
