let rec take n xs = match xs, n with
    | [], _     -> []
    | _, 0      -> []
    | x::xs, n  -> x::take (n - 1) xs

let rec filter p = function
    | []                -> []
    | x::xs when p x    -> x::filter p xs
    | _::xs             -> filter p xs

let rec takeWhile p = function
    | []                -> []
    | x::xs when p x    -> x::takeWhile p xs
    | _                 -> []

let rec dropWhile p = function
    | []                -> []
    | x::xs when p x    -> dropWhile p xs
    | xs                -> xs

let span p xs = (takeWhile p xs, dropWhile p xs)

let partition p xs = (filter p xs, filter (Combinator.(@.) not p) xs)

let rec groupBy p = function
    | []    -> []
    | x::xs -> let (ys, zs) = span (p x) xs
    in (x::ys)::groupBy p zs

let head = function
    []      -> invalid_arg "head: empty list"
    | x::_  -> x

let tail = function
    []      -> invalid_arg "head: empty list"
    | _::xs -> xs

let rev xs =
    let rec rev acc = function
        | []    -> acc
        | x::xs -> rev (x::acc) xs
    in rev [] xs

let last xs = head @@ rev xs

let init xs = tail @@ rev xs

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

let rec foldr f acc = function
    | []    -> acc
    | x::xs -> f (foldr f acc xs) xs

let rec map f = function
    | []    -> []
    | x::xs -> f x::map f xs

let map' f l =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (f x::acc) xs
  in
  aux [] l |> rev

let rec any p = function
    | []                -> false
    | x::xs when p x    -> true
    | _::xs             -> any p xs

let elem x xs = any ((=) x) xs

let null = function
    | []    -> true
    | _     -> false

let length xs = foldl (fun acc _ -> acc + 1) 0 xs

let map'' f xs = rev @@ foldl (fun acc x -> (f x)::acc) [] xs
