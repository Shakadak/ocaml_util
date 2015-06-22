let rec take n xs = match xs, n with
    | [], _     -> []
    | _, 0      -> []
    | x::xs, n  -> x::take (n - 1) xs

let rec take_while p = function
    | []                -> []
    | x::xs when p x    -> x::take_while p xs
    | _                 -> []

let rec drop_while p = function
    | []                -> []
    | x::xs when p x    -> drop_while p xs
    | xs                -> xs

let span p xs = (take_while p xs, drop_while p xs)

let rec group_by p = function
    | []    -> []
    | x::xs -> let (ys, zs) = span (p x) xs
    in (x::ys)::group_by p zs

let last xs = List.hd @@ List.rev xs

let init xs = List.tl @@ List.rev xs

(* Does not work. *)
let rev xs =
    let rec rev acc = function
        | []    -> acc
        | x::xs -> rev (x::acc) xs
    in rev [] xs

let rec append xs ys = match xs with
    | []    -> ys
    | x::xs -> x::append xs ys

let append' xs ys =
    let rec append' acc = function
        | []    -> acc
        | x::xs -> append' (x::acc) xs
    in append' ys (rev xs)

let rec foldl f acc = function
    | []    -> acc
    | x::xs -> foldl f (f acc x) xs

let rec map f = function
    | []    -> []
    | x::xs -> f x::map f xs

let rec any p = function
    | []                -> false
    | x::xs when p x    -> true
    | _::xs             -> any p xs

let elem x xs = any ((=) x) xs

let null = function
    | []    -> true
    | _     -> false

let length xs = foldl (fun acc _ -> acc + 1) 0 xs

let rec filter p = function
    | []                -> []
    | x::xs when p x    -> x::filter p xs
    | _::xs             -> filter p xs
