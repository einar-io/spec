(* The option :print-success true is particularly useful when Z3 is being
 * controlled by another application using pipes. In this mode, commands, that
 * otherwise would not print any output, will print success.
 *)

open Utils
open Z3
open Z3.Boolean
open Z3.Expr
open Z3.Arithmetic
open Z3.Log
open Z3.Solver


let build_constraint_tree _ = []


let check_pc pc =
    (* PART 0: Initialisation *)
    print_endline @@ print_pc pc;
    print_endline "Invoking `main()`.";

    let with_log = Log.open_ "Z3.log" in
    if  with_log then Log.append "Entering `main ()`" else ();

    Printf.printf "Running Z3 version %s\n" Version.to_string ;
    Printf.printf "Z3 full version string: %s\n" Version.full_version ;

    (* timeout (unsigned) default timeout (in milliseconds) used for solvers *)
    let cfg = [("model", "true" );
               ("proof", "false")] in
    let ctx = (Z3.mk_context cfg)  in

    (* PART1: mk_const_s/mk_const_int *)
    (* (declare-const x Int) *)
    (*
    let a = Integer.mk_const_s ctx "a" in
    let b = Integer.mk_const_s ctx "b" in
    let c = Integer.mk_const_s ctx "c" in
    *)


    (* PART2: construct tree with above constants *)
    let sts = build_constraint_tree pc in

    (* goal (context &c, bool models=true, bool unsat_cores=false, bool proofs=false) 
     * https://z3prover.github.io/api/html/classz3_1_1goal.html *)
    let g = (Goal.mk_goal ctx true false false) in
    Goal.add g sts;


    let solver = Solver.mk_solver ctx None in

    (List.iter (fun a -> (Solver.add solver [ a ])) (Goal.get_formulas g)) ;

    let status = Solver.check solver [] in

    let stmsg ="Exiting `main ()` with status: " ^ Solver.string_of_status status in
    
    print_endline stmsg; 

    if with_log 
        then (Log.append stmsg; 
              Log.close ())
        else ();

    match status with
    | SATISFIABLE   -> true
    | UNKNOWN       -> true
    | UNSATISFIABLE -> false


let satisfiable pc =
    print_endline @@ print_pc pc;
    match pc with
    | [] -> let s = true in print_endline @@ string_of_bool s; s
    | pc -> try let s = check_pc pc in print_endline @@ string_of_bool s; s
            with Error msg -> let emsg = "Z3/ML Exception thrown with message: " ^ msg in Error msg; true
