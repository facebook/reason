module Foo = struct
  type t = { name: string }
end

let foo Foo.{name} = ()

let f = function
  | Foo.{name} -> ()
  | _ -> ()

let x = { Foo.name = "Reason" }
let Foo.{name} = x

let Foo.{name}, _ = x, ()
