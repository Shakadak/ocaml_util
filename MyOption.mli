val option : 'b -> ('a -> 'b) -> 'a option -> 'b
(** Take a default value, a function and an option.
 ** If the option is None, return the default value.
 ** Otherwise return the application of the function to contained value. *)

val isSome : 'a option -> bool
(** Return wether an option is Some or not *)

val isNone : 'a option -> bool
(** Return wether an option is None or not *)

val fromSome : 'a option -> 'a
(** Extract the value of an option, throwing an error in case of None *)

val fromOption : 'a -> 'a option -> 'a
(** Extract the value of an option, using the default value in case of None *)

val headToOption : 'a list -> 'a option
(** Extract the head of a list, returning None in case of empty list *)

val tailToOption : 'a list -> 'a list option
(* Extract the tail of a list, returning None in case of empty list *)

val lastToOption : 'a list -> 'a option
(** Extract the last element of a list, returning none in case of empty liste *)

val initToOption : 'a list -> 'a list option
(** Return a list without its last element, return None in case of empty list *)

val optionToList : 'a option -> 'a list
(** Extract an option to a singleton, returning an empty list in case of None *)

val catOptions : 'a option list -> 'a list
(** Construct a list discarding the None values *)

val mapOption : ('a -> 'b option) -> 'a list -> 'b list
(** Construct a list discarding the None values returned *)
