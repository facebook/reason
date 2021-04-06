module EM = struct
  (** Exception *)
  exception E of int * int
end

(* Pcl_open *)
class x = let open EM in object (self) end

module OM = struct
  type t
end

class y = let open EM in let open OM in object (self) end

module type S = sig
  type t = private ..
  type t += Foo
end
