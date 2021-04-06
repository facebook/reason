module X : sig
  type t
end

module M := X

module N := X [@@attr]

type y = int

type z = int

type t = int

type x := y

type y := z [@@attr1]
and w := t [@@attr2]

type x' = | and y' = |
