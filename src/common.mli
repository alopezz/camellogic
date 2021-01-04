type 'a formula =
  | And of 'a formula list
  | Or of 'a formula list
  | Not of 'a formula
  | Implies of 'a formula * 'a formula
  | Iff of 'a formula * 'a formula
  | True
  | False
  | Atom of 'a

val simplify : 'a formula -> 'a formula

val nnf_of_formula : 'a formula -> 'a formula
val dnf_of_formula : 'a formula -> 'a formula
val cnf_of_formula : 'a formula -> 'a formula
