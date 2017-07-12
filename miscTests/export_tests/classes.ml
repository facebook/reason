class[@export] some =
let a = 2 in
object
  val x: int = a
end

class[@export] istack =
object
    val mutable v: int list = [0; 2]

    method pop: int option =
      match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None

    method push (hd: int): unit =
      v <- hd :: v
  end

class[@export] ['a] stack (init: 'a list) (binit: int) = object
    val mutable v : 'a list = init

    method pop: 'a option =
      match v with
      | hd :: tl ->
        v <- tl;
        Some hd
      | [] -> None

    method push (hd: 'a): unit =
      v <- hd :: v
  end

class[@export] square (w: int) = object(self)
  method width: int = w
  method area: int = (self#width * self#width)
  method larger (other: square): bool = self#area > other#area
end

class[@export] asquare (w: int) =
let a = 2 in
object(self)
  inherit some
  method width: int = w + a
  method area: int = (self#width * self#width)
  method larger (other: asquare): bool = self#area > other#area
end

class[@export] m: asquare = asquare 20

class[@export] bsquare = object
  inherit asquare 20
  initializer
    print_string "awesome"
end
