type[@export abstract] t = int
type[@export abstract] t2 = int list
type[@export] mylist = int list
type[@export: t list] mylist2 = int list
type[@export: t2] mylist3 = int list

type[@export] ('a, 'b) ttt = 'a list * 'b

let[@export: (t * t)] m:(int * int) = (2,3)
and[@export: t] n: int = 5
