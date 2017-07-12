module[@export] rec Even : sig
  type t = Zero | Succ of Odd.t
end = struct
  type t = Zero | Succ of Odd.t
end
and[@export] Odd : sig
  type t = Succ of Even.t
end = struct
  type t = Succ of Even.t
end

