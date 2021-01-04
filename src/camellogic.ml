let simplify = Common.simplify
let nnf_of_formula = Common.nnf_of_formula
let dnf_of_formula = Common.dnf_of_formula
let cnf_of_formula = Common.cnf_of_formula

module Propositional = Logic.Make(struct
                           type t = string
                           let render v = v
                         end)

let render = Propositional.render

module IntTypes = struct
  type variable = Inttheory.variable
  type term = Inttheory.term
  type predicate = Inttheory.predicate
end
module IntTheoryLogic = Fol.IntTheoryLogic

