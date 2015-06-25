let (@.) f g = fun x -> f (g x)

let on c f = fun x y -> c (f x) (f y)

let flip f = fun x y -> f y x

let curry f x y = f (x, y)

let uncurry f (x, y) = f x y

let comparing p = on compare p

let not f = fun x y -> not (f x y)

(* Infix operators, can be used like `compare /*on*/ fst`.
** Because of precedence concerne, it is recommended that you surround the
** composition with parentheses, like so: `(compare /*on*/ fst) x y` *)
let ( /* ) x f = f x and ( */ ) f y = f y

(* Another possibility, this need further testing *)
let (>-) x y = (x,y) and (-<) (x,f) y = f x y
