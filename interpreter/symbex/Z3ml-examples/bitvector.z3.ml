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

let print_ith_assertion last_index (i : int) a =
    let offset = last_index - 1 - i in
    print_endline 
        @@ "Assertion[" 
        ^ string_of_int offset
        ^ "]:"
        ^ Expr.to_string a

(* Prove that `a < b, b < c, a > c` is not satisfiable in integers.
 * (but maybe is in bitvectors)
 *)
let transitivity ( ctx : context ) =

    (* (declare-const x Int) *)
    let a = BitVector.mk_const_s ctx "a" vector_length in
    let b = BitVector.mk_const_s ctx "b" vector_length in
    (*let c = BitVector.mk_const_s ctx "c" vector_length in*)
    let c = BitVector.mk_numeral ctx "1" vector_length in

    let sts = [mk_slt ctx a b;
               mk_slt ctx b c;
               mk_slt ctx a c] in

    List.iteri (print_ith_assertion @@ List.length sts) sts;
    (* (assert (sts)) *)
    let g = (Goal.mk_goal ctx true false false) in
    Goal.add g sts;
    print_endline @@ Goal.to_string g;

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

    let sts = [mk_slt ctx a b;
               mk_slt ctx b c;
               mk_sgt ctx a c] in

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


let overflow ( ctx : context ) =

    (* (declare-const x Int) *)
    let a = BitVector.mk_const_s ctx "a" vector_length in
    let b = BitVector.mk_const_s ctx "b" vector_length in
    let z = BitVector.mk_numeral ctx "0" vector_length in

    (* 0 < a, 0 < b, a + b < 0 *)
    let sts = [mk_slt ctx z a;
               mk_slt ctx z b;
               mk_sge ctx z (mk_add ctx a b)] in

    (* (assert (sts)) *)
    let () = print_endline @@ "Assertions: " in
    let () = List.iter (fun x -> print_endline @@ Expr.to_string x) sts in
    let g = (Goal.mk_goal ctx true false false) in
    let () = Goal.add g sts in
    let () = print_endline @@ Goal.to_string g in

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

    let _ = transitivity ctx in
    let _ = cyclic ctx in
    let _ = overflow ctx in

    if with_log 
        then (Log.append "Exiting `main ()`";
              Log.close ())
        else ()


let () = try Printf.printf "Test: BitVector\n"; main () with Error msg -> print_endline @@ "Z3 Exception thrown: " ^ msg
