let on_click start stop f = ()

let on_mousedown start stop f = ()

type mev = {mouse_x: int; mouse_y: int}

class[@export] virtual shape (x: int) (y: int) = object(self)
  method virtual private contains: int -> int -> bool

  val mutable x: int = x
  method x: int = x

  val mutable y: int = y
  method y: int = y

  method on_click (start:int) (stop:int) (f:int->int->unit): unit =
    on_click start stop
      (fun ev ->
         if self#contains ev.mouse_x ev.mouse_y then
           f ev.mouse_x ev.mouse_y)

  method on_mousedown (start:int) (stop:int) (f:int->int->unit): unit =
    on_mousedown start stop
      (fun ev ->
         if self#contains ev.mouse_x ev.mouse_y then
           f ev.mouse_x ev.mouse_y)
end

class[@export] square (w: int) (x: int) (y: int) = object
  inherit shape x y

  val mutable width: int = w
  method width: int = width

  method draw: unit = ()

  method private contains (x': int) (y': int): bool =
    x <= x' && x' <= x + width &&
    y <= y' && y' <= y + width
end

class[@export] circle (r: int) (x: int) (y: int) = object
  inherit shape x y

  val mutable radius: int = r
  method radius: int = radius

  method draw: unit = ()

  method private contains (x': int) (y': int): bool =
    let dx = abs (x' - x) in
    let dy = abs (y' - y) in
    let dist = sqrt (float_of_int ((dx * dx) + (dy * dy))) in
    dist <= (float_of_int radius)
end

class growing_circle r x y = object(self)
  inherit circle r x y

  initializer
    self#on_click 0 0 (fun _x _y -> radius <- radius * 2)
end
