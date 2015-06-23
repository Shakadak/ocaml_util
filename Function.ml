let (@.) f g x = f (g x)

let on c f = fun x y -> c (f x) (f y)

(* Infix operators, can be used like `compare /*on*/ fst`.
** Because of precedence concerne, it is recommended that you surround the
** composition with parentheses, like so: `(compare /*on*/ fst) x y` *)
let ( /* ) x y = y x
and ( */ ) x y = x y

let flip f x y = f y x
