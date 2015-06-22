let string_of_list string_of_a xs =
    let rec string_of_list string_of_a xs = match xs with
        | [] -> ""
        | x::y::zs -> string_of_a x ^ "; " ^ string_of_list string_of_a (y::zs)
        | x::ys -> string_of_a x ^ string_of_list string_of_a ys
    in "[" ^ string_of_list string_of_a xs ^ "]"

let string_of_duple string_of_left string_of_right (l, r) =
    "(" ^ string_of_left l ^ ", " ^ string_of_right r ^ ")"

let string_to_list s =
    let rec expl i acc =
        if i < 0
        then acc
        else expl (i - 1) (String.get s i :: acc)
    in expl (String.length s - 1) []
