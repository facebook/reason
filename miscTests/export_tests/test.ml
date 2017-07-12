type[@export] foo = | Pleh | Bleh

let[@export] x:foo = Bleh

type[@export] record = {
  a: string
}

let[@export] impl:record = { a = "2"}

type[@export] extensible = ..

type[@export] extensible += | SayWhat

exception[@export] SomeException of string

let[@export] a (b:int) (c:char) : string = "boe"

module[@export] Bar =
  struct
    let[@export] int_implicit = 1
    let[@export] string_implicit = "str"
    let[@export] char_implicit = 'c'
    let[@export] float_implicit = 1.1

    let[@export] int_explicit:int = 1
    let[@export] string_explicit:string = "str"
    let[@export] char_explicit:char = 'c'
    let[@export] float_explicit:float = 1.1

    type[@export] foo = | Pleh | Bleh

    let[@export] x:foo = Bleh

    type[@export] record = {
      a: string
    }

    let[@export] impl:record = { a = "2"}

    type[@export] extensible = ..

    type[@export] extensible += | SayWhat

    exception[@export] SomeException of string

    let[@export] a (b:int) (c:char) : string = "boe"
    module[@export] Deep =
      struct
        module[@export] Dive =
          struct
            let[@export] int_implicit = 1
            let[@export] string_implicit = "str"
            let[@export] char_implicit = 'c'
            let[@export] float_implicit = 1.1

            let[@export] int_explicit:int = 1
            let[@export] string_explicit:string = "str"
            let[@export] char_explicit:char = 'c'
            let[@export] float_explicit:float = 1.1

            type[@export] foo = | Pleh | Bleh

            let[@export] x:foo = Bleh

            type[@export] record = {
              a: string
            }

            let[@export] impl:record = { a = "2"}

            type[@export] extensible = ..

            type[@export] extensible += | SayWhat

            exception[@export] SomeException of string

          end
      end
  end
