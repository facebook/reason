(* methods must be typed *)
class%export istack = object
    val mutable v: int list = [0; 2]

    method pop option =
      match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None

    method push (hd: int): unit =
      v <- hd :: v
  end


class ['a] stack init = object
    val mutable v : 'a list = init

    method pop =
      match v with
      | hd :: tl ->
        v <- tl;
        Some hd
      | [] -> None

    method push hd =
      v <- hd :: v
  end

class square w = object(self)
  method width = w
  method area = (self#width * self#width)
  method larger (other: square) = self#area > other#area
end

