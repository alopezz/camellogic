
type 'a formula =
  | And of 'a formula list
  | Or of 'a formula list
  | Not of 'a formula
  | Implies of 'a formula * 'a formula
  | Iff of 'a formula * 'a formula
  | True
  | False
  | Atom of 'a

(** Obtain textual representation of a formula as a string *)
val render : string formula -> string

(** General simplification of a PL formula, removing true / false
   values and joining operators where necessary *)
val simplify : 'a formula -> 'a formula

(** Convert formula to Negation normal form (NNF) *)
val nnf_of_formula : 'a formula -> 'a formula

val dnf_of_formula : 'a formula -> 'a formula

val cnf_of_formula : 'a formula -> 'a formula
