open Camellogic
open OUnit2

let render = Camellogic.Propositional.render


let make_render_formula_test formula repr =
  "Render " ^ repr >::
    fun _ -> assert_equal (render formula) repr
               ~printer: Fun.id

let make_simplify_test formula expected =
  "Simplify " ^ (render formula) >::
    fun _ -> assert_equal (simplify formula) expected
               ~printer: render

let make_nnf_test formula expected =
  "Convert " ^ (render formula) ^ " to NNF" >::
    fun _ -> assert_equal (nnf_of_formula formula) expected
               ~printer: render

let make_dnf_test formula expected =
  "Convert " ^ (render formula) ^ " to DNF" >::
    fun _ -> assert_equal (dnf_of_formula formula) expected
               ~printer:render

let make_cnf_test formula expected =
  "Convert " ^ (render formula) ^ " to CNF" >::
    fun _ -> assert_equal (cnf_of_formula formula) expected
               ~printer: render

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
        (And [Atom "A"; Atom "B"; Atom "C"; Atom "D"]);
      make_simplify_test
        (And [Atom "A"; Atom "B"; Atom "B"; Atom "A"])
        (And [Atom "B"; Atom "A"]);
      make_simplify_test
        (Not True)
        False;
      make_simplify_test
        (Or [Atom "A"; Not False])
        True;
      make_simplify_test
        (Not (Not (Atom "A")))
        (Atom "A");
      make_simplify_test
        (And [Atom "A"; Atom "B"; Not (Atom "A")])
        False;
      make_simplify_test
        (Or [Atom "A"; Atom "B"; Not (Atom "A")])
        True;
    ]

let normal_form_testsuite =
  "Test conversion to normal forms">:::
    [
      make_nnf_test
        (Not (And [Atom "A"; Not (Atom "B")]))
        (Or [Not (Atom "A"); Atom "B"]);
      make_dnf_test
        (And [Or [Atom "F1"; Atom "F2"]; Atom "F3"])
        (Or [And [Atom "F1"; Atom "F3"]; And [Atom "F2"; Atom "F3"]]);
      make_cnf_test
        (Or [And [Atom "F1"; Atom "F2"]; Atom "F3"])
        (And [Or [Atom "F1"; Atom "F3"]; Or [Atom "F2"; Atom "F3"]]);
      make_cnf_test
        (Or [And [Atom "Q1"; Not (Not (Atom "Q2"))]; (Implies (Not (Atom "R1"), Atom "R2"))])
        (And [Or [Atom "Q1"; Atom "R1"; Atom "R2"]; Or [Atom "Q2"; Atom "R1"; Atom "R2"]])
    ]


let () =
  run_test_tt_main render_testsuite;
  run_test_tt_main simplify_testsuite;
  run_test_tt_main normal_form_testsuite;
