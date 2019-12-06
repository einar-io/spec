open Z3
open BitVector
(*
open Z3.Goal
open Z3.Expr
open Z3.Solver
open Z3.Log
open Z3.Arithmetic
open Z3.Arithmetic.Integer
*)
let vector_length = 32

(* Prove that `a < b, b < c, a > c` is not satisfiable in integers.
 * (but maybe is in bitvectors)
 *)
let transitivity ( ctx : context ) =

    (* (declare-const x Int) *)
    let a = BitVector.mk_const_s ctx "a" vector_length in
    let b = BitVector.mk_const_s ctx "b" vector_length in
    let c = BitVector.mk_const_s ctx "c" vector_length in

    let sts = [mk_slt ctx a b; mk_slt ctx b c; mk_slt ctx a c] in

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
    let a = BitVector.mk_const_s ctx "a" vector_length in
    let b = BitVector.mk_const_s ctx "b" vector_length in
    let c = BitVector.mk_const_s ctx "c" vector_length in

    let sts = [mk_slt ctx a b; mk_slt ctx b c; mk_sgt ctx a c] in

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
        then Log.append "Entering `main ()`"
        else ();

    let cfg = [("model", "true"); ("proof", "false")] in
    let ctx = (Z3.mk_context cfg) in

    let t = transitivity ctx in
    let c = cyclic ctx in

    if with_log 
        then (Log.append "Exiting `main ()`";
              Log.close ())
        else ()


let () = try Printf.printf "Entering main..\n"; main () with Error msg -> print_endline @@ "Z3 Exception thrown: " ^ msg
