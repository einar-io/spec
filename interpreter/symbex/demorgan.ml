open Z3
open Z3.Solver
open Z3.Goal
open Z3.Expr
open Z3.Boolean



let prove_demorgan () =
    let cfg = [("model", "true"); ("proof", "false")] in
    let ctx = (Z3.mk_context cfg) in
    let bs = (Z3.Boolean.mk_sort ctx) in

    (* (declare-const a Bool) *)
    let a = (Z3.Expr.mk_const ctx (Z3.Symbol.mk_string ctx "a") bs) in

    (* (declare-const b Bool) *)
    let b = (Z3.Expr.mk_const ctx (Z3.Symbol.mk_string ctx "b") bs) in
(*
    (* (define-fun demorgan () Bool
        (= (and a b) (not (or (not a) (not b))))) *)
    let domain = [bs; bs] in
    let range = bs in
    let fname = (Symbol.mk_string ctx "demorgan") in
    let f = (FuncDecl.mk_func_decl ctx fname domain range) in
    (*let fapp = (mk_app ctx f [ (Expr.mk_const ctx a bs); (Expr.mk_const ctx b * bs)]) in *)

    let fapp = (mk_app ctx f [ a; b]) in
*)

    let law1 = (mk_eq ctx (mk_not ctx (mk_or  ctx [a; b])) (mk_and ctx [mk_not ctx a; mk_not ctx b])) in
    let law2 = (mk_eq ctx (mk_not ctx (mk_and ctx [a; b])) (mk_or  ctx [mk_not ctx a; mk_not ctx b])) in

    (* (assert (not demorgan)) *)
    let g = (Z3.Goal.mk_goal ctx true false false) in

    (* Z3.Goal.add g [Z3.Bool.mk_eq ctx a b]); *)
    Z3.Goal.add g [Z3.Boolean.mk_eq ctx law1 (Z3.Boolean.mk_false ctx)];
    Z3.Goal.add g [Z3.Boolean.mk_eq ctx law2 (Z3.Boolean.mk_false ctx)];

    let solver = (Z3.Solver.mk_solver ctx None) in

    (List.iter (fun a -> (Z3.Solver.add solver [ a ])) (Z3.Goal.get_formulas g)) ;

    (* (check-sat) *)
    let satisfiability = (Z3.Solver.check solver []) in
        (* Interpret results and print to user *)
        if satisfiability = SATISFIABLE then
            (* We have found a satisfying assignment to that makes the law false *)
            Printf.printf "De Morgan is invalid.\n"
        else
            Printf.printf "De Morgan valid.\n"


let () = Printf.printf "Starting main..\n"
let () = try prove_demorgan () with Error msg -> print_endline @@ "Z3 Exception thrown: " ^ msg
