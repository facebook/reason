[@reason.version 3.7];
type elt =
  | Text(string)
  | Group(list(elt));

module X = {
  let createElement = (~children=[], ()) => {
    Text("x");
  };
};

module Y = {
  let createElement = (~children=[], ()) => {
    Text("y");
  };
};

module M =
       (
         X: (module type of X),
         Y: (module type of Y),
       ) => {
  let createElement =
      (~name="M", ~id=0, ~children=[], ()) => {
    Group(
      [
        Text(name),
        Text(string_of_int(id)),
        <X />,
        <Y />,
      ]
      @ children,
    );
  };
};

let _ =
  Group([
    <M(X, Y) />,
    <M(X, Y)> {Text("A")} </M>,
    <M(X, Y) name="Test" id=10 />,
  ]);
