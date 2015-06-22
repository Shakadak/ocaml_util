let rec take n xs = match xs, n with
    | [], _     -> []
    | _, 0      -> []
    | x::xs, n  -> x::take (n - 1) xs

let rec take_while p xs = match xs with
    | []                -> []
    | x::xs when p x    -> x::take_while p xs
    | _                 -> []

let rec drop_while p xs = match xs with
    | []                -> []
    | x::xs when p x    -> drop_while p xs
    | xs                -> xs

let span p xs = (take_while p xs, drop_while p xs)

let rec group_by p xs = match xs with
    | []    -> []
    | x::xs -> let (ys, zs) = span (p x) xs
    in (x::ys)::group_by p zs

let last xs = List.hd @@ List.rev xs

let init xs = List.tl @@ List.rev xs

let rec append xs ys = match xs with
    | []    -> ys
    | x::xs -> x::append xs ys

let rec foldl f acc xs = match xs with
    | []    -> acc
    | x::xs -> foldl f (f acc x) xs

let rec map f xs = match xs with
    | []    -> []
    | x::xs -> f x::map f xs

let rec any p xs = match xs with
    | []                -> false
    | x::xs when p x    -> true
    | _::xs             -> any p xs

let elem x xs = any ((=) x) xs
