type 'a formula = 'a Common.formula =
  | And of 'a formula list
  | Or of 'a formula list
  | Not of 'a formula
  | Implies of 'a formula * 'a formula
  | Iff of 'a formula * 'a formula
  | True
  | False
  | Atom of 'a

let simplify = Common.simplify

let nnf_of_formula = Common.nnf_of_formula
let dnf_of_formula = Common.dnf_of_formula
let cnf_of_formula = Common.cnf_of_formula

module Propositional = Logic.MakeLogic(struct
                           type t = string
                           let render v = v
                         end)

let render = Propositional.render
