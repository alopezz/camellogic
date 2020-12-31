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


let precedence formula = match formula with
  | And _ -> 2
  | Or _ -> 3
  | Not _ -> 1
  | Implies (_, _) -> 4
  | Iff (_, _) -> 5
  | True | False | Atom _ -> 0


let rec render formula =
  (* Helper function to wrap child in parens if needed (according to
     rules of precedence) *)
  let render_child child =
    let repr = render child in
    if precedence formula < precedence child
    then Printf.sprintf "(%s)" repr
    else repr
  in
  match formula with
  | And operands ->
     String.concat " ∧ " (List.map render_child operands)
  | Or operands ->
     String.concat " ∨ " (List.map render_child operands)
  | Not operand -> "¬" ^ (render_child operand)
  | Implies (a, b) -> String.concat " → " (List.map render_child [a; b])
  | Iff (a, b) -> String.concat " ⬌ " (List.map render_child [a; b])
  | True -> "⊤"
  | False -> "⊥"
  | Atom a -> a

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
  | Not a -> Not (simplify a)
  | True | False | Atom _ as v -> v
