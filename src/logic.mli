
module type PredicateM = sig
  type t
  val render : t -> string
  val precedence : int
end


module type Logic = sig
  type t
  val render : t -> string
end

module Make (M : PredicateM) : (Logic with type t := M.t Common.formula)
