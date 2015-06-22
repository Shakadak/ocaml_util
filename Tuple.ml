let curry f x y = f (x, y)

let uncurry f (x, y) = f x y

let swap (x, y) = (y, x)

let fst (x, _) = x

let snd (_, y) = y
