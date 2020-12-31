open Camellogic
open OUnit2


let make_render_formula_test formula repr =
  "Render " ^ repr >::
    fun _ -> assert_equal (render_formula formula) repr

let render_testsuite =
  "Test rendering">:::
    [
      make_render_formula_test
        (Implies (And [Atom "R"; And [Not (Atom "R"); Atom "P"]], Atom "P"))
        "R ∧ ¬R ∧ P → P";
      make_render_formula_test
        (Or [And [Atom "P1"; And [Not (Atom "P2"); True]]; And [Not (Atom "P1"); Atom "P2"]])
        "P1 ∧ ¬P2 ∧ ⊤ ∨ ¬P1 ∧ P2"
    ]

let () =
  run_test_tt_main render_testsuite
