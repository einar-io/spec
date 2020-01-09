open Z3
open Z3.Boolean


let prove_demorgan () =
    let cfg = [("model", "true"); ("proof", "false")] in
    let ctx = (Z3.mk_context cfg) in

    (* (declare-const a Bool) *)
    let a = mk_const ctx (Symbol.mk_string ctx "a") in

    (* (declare-const b Bool) *)
    let b = mk_const ctx (Symbol.mk_string ctx "b") in

    let law1 = mk_eq ctx
                (mk_not ctx (mk_or  ctx [a; b]))
                (mk_and ctx [mk_not ctx a; mk_not ctx b]) in

    let law2 = mk_eq ctx
                (mk_not ctx (mk_and ctx [a; b]))
                (mk_or  ctx [mk_not ctx a; mk_not ctx b]) in

    let g = Goal.mk_goal ctx true false false in

    (* (assert (not demorgan)) *)
    Goal.add g [mk_eq ctx law1 (mk_false ctx)
               ;mk_eq ctx law2 (mk_false ctx)];

    let solver = Solver.mk_solver ctx None in

    List.iter (fun a -> Solver.add solver [a]) (Goal.get_formulas g) ;

    (* (check-sat) *)
    match Solver.check solver [] with
    | SATISFIABLE             -> Printf.printf "De Morgan is invalid.\n"
    | UNSATISFIABLE | UNKNOWN -> Printf.printf "De Morgan is valid.\n"


let () = try Printf.printf "Test: De Morgan\n"; prove_demorgan ()
        with Error msg -> print_endline @@ "Z3 ML/OCaml exception: " ^ msg
