(* Type names (supported with PR#2342) *)
module T = struct
  type pub = unit
end

(* Value names (already supported) *)
module V = struct
  let method_ = ()
end

(* Record fields *)
module R = struct
  type r = { mutable method_ : int }

  let foo = { method_ = 4 }

  let x = foo.method_

  let () = foo.method_ <- 42

  let y = match foo with {method_} -> method_

  let z = match foo with {method_=12} -> 21
end

(* Class names and instance variables *)
module C = struct
  class pub = object end

  class c = object
    val pub = 0
    method method_ () = ()
  end

  class c' = object
    inherit method_
    val! pub = 1
  end
end

(* Class types *)
module Ct = struct
  class type method_ = object
    val method_: unit -> unit
  end
end

(* Virtual *)
module Cv = struct
  class virtual method_ = object end
end

(* Object methods *)
module O = struct
  let o = object method method_ = () end
end

(* Function parameter labels *)
module L = struct
  let f ~method_ = ignore method_
end

(* Module types *)
module type method_ = sig
end

(* Polymorphic variants (probably ok as-is?) *)
module P = struct
  type t = [ `pub | `method_ ]

  let x = `method_

  let () = fun `method_ -> 34
end

type method_ = string

type foo = {method_: method_}
[@@some_attr: type_]
[@@other_attr: method_]

let f ~method_ = Js.log(method_)

let x = f ~method_:"GET"

type marshalFields = < switch: string   >  Js.t

let testMarshalFields = ([%bs.obj { switch = "switch" }] : marshalFields)

(* Not an identifier test, but this is testing OCaml -> RE *)
let x = List.map (fun y ->
  ();
  y)

let newType (type method_) () = ()