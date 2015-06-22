let rec take n xs = match xs, n with
    | [], _     -> []
    | _, 0      -> []
    | x::xs, n  -> x::take (n - 1) xs

let rec take_while f xs = match xs with
    | []                -> []
    | x::xs when f x    -> x::take_while f xs
    | _                 -> []

let rec drop_while f xs = match xs with
    | []                -> []
    | x::xs when f x    -> drop_while f xs
    | xs                -> xs

let span f xs = (take_while f xs, drop_while f xs)

let rec group_by f xs = match xs with
    | []    -> []
    | x::xs -> let (ys, zs) = span (f x) xs
    in (x::ys)::group_by f zs

let last xs = List.hd @@ List.rev xs

let init xs = List.tl @@ List.rev xs
