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

(** Obtain textual representation of a formula as a string *)
val render : formula -> string

(** General simplification of a PL formula, removing true / false
   values and joining operators where necessary *)
val simplify : formula -> formula

(** Convert formula to Negation normal form (NNF) *)
val nnf_of_formula : formula -> formula
