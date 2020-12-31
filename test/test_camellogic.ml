open Camellogic
open OUnit2


let make_render_formula_test formula repr =
  "Render " ^ repr >::
    fun _ -> assert_equal (render formula) repr

let make_simplify_test formula expected =
  "Simplify " ^ (render formula) >::
    fun _ -> assert_equal (simplify formula) expected

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

let simplify_testsuite =
  "Test simplification">:::
    [
      make_simplify_test
        (Implies(And [Atom "P"; False], Or [Atom "P"; Not (Atom "Q")]))
        True;
      make_simplify_test
        (And [And [Atom "A"; Atom "B"]; And [Atom "C"; Atom "D"]])
        (And [Atom "A"; Atom "B"; Atom "C"; Atom "D"])
    ]

let () =
  run_test_tt_main render_testsuite;
  run_test_tt_main simplify_testsuite;
