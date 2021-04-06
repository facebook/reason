let (let.opt) = (x, f) => switch x { | None => None | Some(x) => f(x) };
let (let.&opt) = (x, f) => switch x { | None => None | Some(x) => Some(f(x)) };

let z = {
  let.opt a = Some(2);
  let.&opt b = Some(5);
  a + b
}

let (let./\/) = (x, f) => switch x { | None => None | Some(x) => f(x) };
let (let.&/\*) = (x, f) => switch x { | None => None | Some(x) => Some(f(x)) };

/* Test syntax that could potentially conflict with comments */
let z = {
  let./\/ a = Some(2);
  let.&/\* b = Some(5);
  a + b
}

let _ = {
  let.opt _ = Some("a");

  let.opt _ = Some("c");

  // hello

  None;
};
