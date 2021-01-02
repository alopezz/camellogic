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

let spaced sym = Printf.sprintf " %s " sym

module MakeRenderer (M : Representable) : PrettyRenderer with type t := M.t = struct
  let rec render formula =
    (* Helper function to wrap child in parens if needed (according to
     rules of precedence) *)
    let render_child child =
      let repr = render child in
      if M.precedence formula < M.precedence child
      then Printf.sprintf "(%s)" repr
      else repr
    in
    let sym = M.symbol formula in
    match M.arity formula with
    | None -> sym
    | Unary operand -> sym ^ (render_child operand)
    | Binary (a, b) -> String.concat (spaced sym) (List.map render_child [a; b])
    | Variadic operands -> String.concat (spaced sym) (List.map render_child operands)
end
