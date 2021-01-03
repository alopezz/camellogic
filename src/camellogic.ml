type atom = string

type formula =
  | And of formula list
  | Or of formula list
  | Not of formula
  | Implies of formula * formula
  | Iff of formula * formula
  | True
  | False
  | Atom of atom


module Renderer = Pretty.MakeRenderer(
                      struct
                        type t = formula
                        let arity = function
                          | And ops | Or ops  -> Pretty.Variadic ops
                          | Not op -> Pretty.Unary op
                          | Implies (a, b) | Iff (a, b) -> Pretty.Binary (a, b)
                          | Atom _ | True | False -> Pretty.None

                        let precedence = function
                          | And _ -> 2
                          | Or _ -> 3
                          | Not _ -> 1
                          | Implies (_, _) -> 4
                          | Iff (_, _) -> 5
                          | True | False | Atom _ -> 0

                        let symbol = function
                          | And _ -> "∧"
                          | Or _ -> "∨"
                          | Not _ -> "¬"
                          | Implies (_, _) -> "→"
                          | Iff (_, _) -> "⬌"
                          | True -> "⊤"
                          | False -> "⊥"
                          | Atom a -> a
                      end)

let render = Renderer.render


(** Helper function to simplify And / Or that have a list of operands.
    trivial refers to the value that can be removed from the list;
    cancel refers to the value that the expression will simplify to if present *)
let simplify_op_list trivial cancel constructor operands =
  match List.filter ((<>) trivial) operands with
  | [] -> True
  | [only] -> only
  | ops -> if (List.exists ((=) cancel) ops)
           then cancel
           else constructor ops

(** Simplify (unnest) a list-based operator when all the children have the same
   operator as the parent. It receives two functions as arguments to
   do so*)
let unnest_op check_type unnest operands =
  if List.for_all check_type operands
  then List.concat (List.map unnest operands)
  else operands

(** Remove duplicates from a list of operands *)
let rec remove_duplicates operands =
  match operands with
  | [] -> []
  | [only] -> [only]
  | hd :: tl -> if List.mem hd tl then remove_duplicates tl
                else hd :: remove_duplicates tl

(** Simplify operands that appear negated within the same And or Or.
[result] is the value that it should simplify to *)
let rec simplify_negated_duplicates result operands =
  match operands with
  | [] -> []
  | [only] -> [only]
  | (Not hd) :: tl -> if List.mem hd tl then [result]
                else hd :: simplify_negated_duplicates result tl
  | hd :: tl -> if List.mem (Not hd) tl then [result]
                else hd :: simplify_negated_duplicates result tl


let rec simplify formula =
  match formula with
  | And operands ->
     List.map simplify operands
     |> unnest_op
          (function
           | And _ -> true
           | _ -> false)
          (function
           | And y -> y
           | _ -> assert false)
     |> remove_duplicates
     |> simplify_negated_duplicates False
     |> simplify_op_list True False (fun x -> And x)
  | Or operands ->
     List.map simplify operands
     |> unnest_op
          (function
           | Or _ -> true
           | _ -> false)
          (function
           | Or y -> y
           | _ -> assert false)
     |> remove_duplicates
     |> simplify_negated_duplicates True
     |> simplify_op_list False True (fun x -> Or x)
  | Iff (a, b) -> begin match simplify a, simplify b with
                  | False, a | a, False -> Not a
                  | True, a | a, True -> a
                  | a, b -> Iff (a, b)
                  end
  | Implies (a, b) -> begin match simplify a, simplify b with
                      | _, True -> True
                      | False, _ -> True
                      | True, b -> b
                      | a, False -> Not a
                      | a, b -> Implies (a, b)
                      end
  | Not True -> False
  | Not False -> True
  | Not (Not a) -> simplify a
  | Not a -> Not (simplify a)
  | True | False | Atom _ as v -> v


let negate_operands operands =
  List.map (fun op -> (Not op)) operands


let rec nnf_of_formula formula =
  simplify
    (match formula with
     | Not (And ops) -> Or (negate_operands ops |> List.map nnf_of_formula)
     | Not (Or ops) -> And (negate_operands ops |> List.map nnf_of_formula)
     | Implies (a, b) -> Or ([Not a; b] |> List.map nnf_of_formula)
     | Iff (a, b) -> let a, b = nnf_of_formula a, nnf_of_formula b in
                     And [Implies (a, b); Implies(a, b)]
     | f -> f)
