module type BTREE =
sig
  type t
  val compare : t -> t -> int
  val print : t -> unit
end

module StringTree : BTREE =
struct
  type t = string
  let compare = compare
  let print = print_endline
end

module BinaryTree =
  functor (Tree : BTREE) ->
    struct
      type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

      let create () = Nil

      let rec draw = function
        | Nil -> ()
        | Node (v, l, r) -> (Tree.print v ; draw l ; draw r)

      let rec insert a = function
        | Nil -> Node (a, Nil, Nil)
        | Node (v, l, r) ->
          if (Tree.compare v a) > 0
          then Node (v, insert a l, r)
          else Node (v, l, insert a r)
    end

module MyStringTree = BinaryTree (StringTree)

let () =
  let tree = MyStringTree.create in
  let add = MyStringTree.insert in
  add "Coucou" tree ;
  add "Hello" tree ;
  MyStringTree.print tree
