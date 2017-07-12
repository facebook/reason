let[@export] int_implicit = 1
let[@export] string_implicit = "str"
let[@export] char_implicit = 'c'
let[@export] float_implicit = 1.1

let[@export] int_explicit:int = 1
let[@export] string_explicit:string = "str"
let[@export] char_explicit:char = 'c'
let[@export] float_explicit:float = 1.1

let[@export: int] int_override = int_explicit
let[@export: val int_full: int] int_full = int_explicit

type[@export abstract] t = int
let[@export: t] int_abstract:int = 10

let[@export:int] a = 10
and[@export] b: int = 10
(* not exporting one of a group *)
and c = 20

