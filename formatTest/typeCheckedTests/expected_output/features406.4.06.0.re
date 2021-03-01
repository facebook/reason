[@reason.version 3.7];
module EM = {
  /** Exception */

  exception E(int, int);
};

/* Pcl_open */
class x = {
  open EM;
  as self;
};

module OM = {
  type t;
};

class y = {
  open EM;
  open OM;
  as self;
};

module type S = {
  type t = pri ..;
  type t +=
    | Foo;
};
