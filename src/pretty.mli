type 'a arity =
  | None
  | Unary of 'a
  | Binary of 'a * 'a
  | Variadic of 'a list


module type Representable = sig
  type t
  val arity : t -> t arity
  val precedence : t -> int
  val symbol : t -> string
end

module type PrettyRenderer = sig
  type t
  val render : t -> string
end

(** [MakeRenderer M] creates a module providing a [render] function for the given type *)
module MakeRenderer (M : Representable) : (PrettyRenderer with type t := M.t)
