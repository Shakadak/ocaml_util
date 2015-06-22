let (@.) f g x = f (g x)

let on c f = fun x y -> c (f x) (f y)

let flip f x y = f y x
