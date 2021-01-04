include Common

module Propositional = Logic.Make(struct
                           type t = string
                           let render v = v
                         end)

let render = Propositional.render

module IntLogic = struct
  include Logic.Make(Inttheory)
  module Types = Inttheory.Types
end
