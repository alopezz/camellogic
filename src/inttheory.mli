module Types : sig
  type variable = string
  type term =
    | Sum of term list
    | Sub of term list
    | Constant of int
    | Mul of int * term
    | Var of variable
  type predicate =
    | Equal of term * term
    | GreaterThan of term * term
end

type t = Types.predicate

val render : t -> string

val precedence : int
