open struct
  type t = string
end

let (let+) x f = List.map f x

let (and+) = List.map2 (fun x y -> x,y)

let x =
  let+ x = [2]
  and+ y = [3]
  in
  x, y

let y =
  let+ x = [2] in
  x
