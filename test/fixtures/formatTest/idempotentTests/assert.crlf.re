switch(true) {
  | true => ()
  | false => assert(false)
  | _ => assert false
};

let root = {
  let root = Doc.rootNode(doc);
  assert (root.type_ == "expression");
  assert (Node.namedChildCount(root) == 1);
  assert (Node.childCount(root) == 1);
  assert (Point.toString(root.startPoint) == "(Point.t {:row 0 :column 0})");
  assert (Point.toString(root.endPoint) == "(Point.t {:row 0 :column 9})");
  root;
};

assert(theTruth());
