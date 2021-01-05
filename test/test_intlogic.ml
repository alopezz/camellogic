open Camellogic
open OUnit2

let render = Camellogic.IntLogic.render

let make_render_formula_test formula repr =
  "Render " ^ repr >::
    fun _ -> assert_equal (render formula) repr
               ~printer: Fun.id

let make_nnf_test formula expected =
  "Convert " ^ (render formula) ^ " to NNF" >::
    fun _ -> assert_equal (nnf_of_formula formula) expected
               ~printer: render

let render_testsuite =
  let open Camellogic.IntLogic.Types in
  "Test rendering">:::
    [
      make_render_formula_test
        (Implies (And [Atom (Equal (Var "a", Var "b"));
                       And [Not (Atom (GreaterThan (Var "c", Var "a"))); Atom (Equal (Var "d", Var "e"))]],
                  Atom (Equal ((Var "e"), (Var "z")))))
        "a = b ∧ ¬c > a ∧ d = e → e = z";
    ]

let normal_form_testsuite =
  "Test conversion to normal forms">:::
    [
      make_nnf_test
        (Not (And [Atom (Equal (Var "a", Var "b")); Not (Atom (Equal (Var "d", Var "e")))]))
        (Or [Not (Atom (Equal (Var "a", Var "b"))); Atom (Equal (Var "d", Var "e"))]);
    ]


let () =
  run_test_tt_main render_testsuite;
  run_test_tt_main normal_form_testsuite
