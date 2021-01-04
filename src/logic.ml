open Common


module type PredicateM = sig
  type t
  val render : t -> string
end

module type Logic = sig
  type t
  val render : t -> string
end

module MakeLogic (M : PredicateM) : (Logic with type t := M.t formula) = struct
  module Renderer = Pretty.MakeRenderer(
                        struct
                          type t = M.t formula
                          open Common
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
                            | Atom a -> M.render a
                        end)
  let render = Renderer.render
end
