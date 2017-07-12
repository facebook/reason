
let[@export] a (b:int) (c:char) : string = "boe"

let[@export] a ((): unit): int = 34

(* Allow unannotated "unit" *)
let[@export] a (): int = 34

let[@export] rec a (b: int): int = if b > 2 then a (b - 1) else 10;

