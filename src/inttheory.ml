
module Types = struct
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

open Types
  
type t = predicate

module TermRenderer = Pretty.MakeRenderer(
                               struct
                                 type t = term
                                 let arity = function
                                   | Sum ops -> Pretty.Variadic ops
                                   | Sub ops -> Pretty.Variadic ops
                                   | Constant _ -> Pretty.None
                                   | Mul (_, a) -> Unary a
                                   | Var _ -> Pretty.None
                                 let precedence = function
                                   | Sum _ -> 2
                                   | Sub _ -> 2
                                   | Constant _ -> 0
                                   | Mul (_, _) -> 1
                                   | Var _ -> 0
                                 let symbol = function
                                   | Sum _ -> "+"
                                   | Sub _ -> "-"
                                   | Constant a -> string_of_int a
                                   | Mul (a, _) -> string_of_int a
                                   | Var a -> a
                               end)


let render = function
  | Equal (a, b) -> Printf.sprintf "%s = %s"
                      (TermRenderer.render a)
                      (TermRenderer.render b)
  | GreaterThan (a, b) -> Printf.sprintf "%s > %s"
                            (TermRenderer.render a)
                            (TermRenderer.render b)
