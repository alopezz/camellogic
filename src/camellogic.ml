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


let rec render_formula formula =
  (* Helper function to wrap child in parens if needed *)
  let render child =
    let repr = render_formula child in
    if precedence formula < precedence child
    then Printf.sprintf "(%s)" repr
    else repr
  in
  match formula with
  | And operands ->
     String.concat " ∧ " (List.map render operands)
  | Or operands ->
     String.concat " ∨ " (List.map render operands)
  | Not operand -> "¬" ^ (render operand)
  | Implies (a, b) -> String.concat " → " (List.map render [a; b])
  | Iff (a, b) -> String.concat " ⬌ " (List.map render [a; b])
  | True -> "⊤"
  | False -> "⊥"
  | Atom a -> a
