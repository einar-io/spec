open Z3
open Z3.Arithmetic
(*
open Z3.Goal
open Z3.Expr
open Z3.Solver
open Z3.Log
open Z3.Arithmetic.Integer
*)

(* Prove that `a < b, b < c, a > c` is not satisfiable in integers.
 * (but maybe is in bitvectors)
 *)
let transitivity ( ctx : context ) =

    (* (declare-const x Int) *)
    let a = Integer.mk_const_s ctx "a" in
    let b = Integer.mk_const_s ctx "b" in
    let c = Integer.mk_const_s ctx "c" in

    let sts = [mk_lt ctx a b; mk_lt ctx b c; mk_lt ctx a c] in

    (* (assert (sts)) *)
    let g = (Goal.mk_goal ctx true false false) in
    Goal.add g sts;

    let solver = (Solver.mk_solver ctx None) in

    (List.iter (fun a -> (Solver.add solver [ a ])) (Goal.get_formulas g)) ;

    (* (check-sat) *)
    let satisfiability = (Solver.check solver []) in
        (* Interpret results and print to user *)
    if satisfiability = SATISFIABLE
    then ( 
        print_endline "SAT:\n";
        (* We have found a satisfying assignment to that makes the law false *)
        let m_opt = Solver.get_model solver in
        match m_opt with
        | Some m -> print_endline @@ Model.to_string m
        | None   -> print_endline @@ "<no model produced>"
    )
    else
        Printf.printf "UNSAT\n"


let cyclic ( ctx : context ) =

    (* (declare-const x Int) *)
    let a = Integer.mk_const_s ctx "a" in
    let b = Integer.mk_const_s ctx "b" in
    let c = Integer.mk_const_s ctx "c" in

    let sts = [mk_lt ctx a b; mk_lt ctx b c; mk_gt ctx a c] in

    (* (assert (sts)) *)
    let g = (Goal.mk_goal ctx true false false) in
    Goal.add g sts;

    let solver = (Solver.mk_solver ctx None) in

    (List.iter (fun a -> (Solver.add solver [ a ])) (Goal.get_formulas g)) ;

    (* (check-sat) *)
    let satisfiability = (Solver.check solver []) in
        (* Interpret results and print to user *)
    if satisfiability = SATISFIABLE then
        (* We have found a satisfying assignment to that makes the law false *)
        let m_opt = Solver.get_model solver in
        Printf.printf "SAT:\n";
        match m_opt with
        | Some m -> print_endline @@ Model.to_string m
        | None   -> print_endline @@ "<no model produced>"
    else
        Printf.printf "UNSAT\n"


let main () =
    let with_log = Log.open_ "Z3.log" in

    if with_log
        then Log.append "Arithmetic test:\n"
        else ();

    let cfg = [("model", "true"); ("proof", "false")] in
    let ctx = (Z3.mk_context cfg) in

    let t = transitivity ctx in
    let c = cyclic ctx in

    if with_log 
        then (Log.append "Exiting `main ()`";
              Log.close ())
        else ()


let () = try Printf.printf "Test: arithmetic.z3.ml\n"; main () with Error msg -> print_endline @@ "Z3 Exception thrown: " ^ msg
