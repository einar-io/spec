(* The option :print-success true is particularly useful when Z3 is being
 * controlled by another application using pipes. In this mode, commands, that
 * otherwise would not print any output, will print success.
 *)

(* open Z3 this shadows Set *)
open Z3
open Z3.Solver
open Z3.BitVector
(*open Z3.Boolean*)
open Utils
open Types

(* Our Set *)
module S = Types.Symwords

let build_constraint_tree _ = []

let rec gather_free pc =
    match pc with
    | SNot a        -> gather_free a
    | SAny a as elt -> S.singleton elt
    | SAdd (a, b)   -> S.union (gather_free a) (gather_free b)
    | SEq  (a, b)   -> S.union (gather_free a) (gather_free b)
    | SOr  (a, b)   -> S.union (gather_free a) (gather_free b)
    | SCon a        -> S.empty
    | SAnd (a, b)   -> S.union (gather_free a) (gather_free b)
    | SLt  (a, b)   -> S.union (gather_free a) (gather_free b)


let rec sym_to_smt ctx apc =
    let s2s = sym_to_smt ctx in
    match apc with
    | SNot a   -> mk_not ctx (s2s a)
    | SAny a   -> mk_const ctx (Z3.Symbol.mk_int ctx @@ Int32.to_int a) wordsize
    | SAdd (a, b) -> mk_add ctx (s2s a) (s2s b)
    | SEq  (a, b) -> Z3.Boolean.mk_eq ctx (s2s a) (s2s b)
    | SOr  (a, b) -> mk_or ctx (s2s a) (s2s b)
    | SCon a  -> mk_const ctx (Z3.Symbol.mk_int ctx @@ Int32.to_int a) wordsize
    | SAnd (a, b) -> mk_and ctx (s2s a) (s2s b)
    | SLt  (a, b) -> mk_slt ctx (s2s a) (s2s b)


let check_pc pc =
    (* PART 0: Initialisation *)
    print_endline @@ print_pc pc;
    print_endline "Invoking `check_pc ()`.";

    let with_log = Z3.Log.open_ "Z3.log" in
    if  with_log then Z3.Log.append "Entering `check_pc ()`" else ();

    Printf.printf "Running Z3 version %s\n" Version.to_string ;
    Printf.printf "Z3 full version string: %s\n" Version.full_version ;

    (* timeout (unsigned) default timeout (in milliseconds) used for solvers *)
    let cfg = [("model", "true" );
               ("proof", "false")] in
    let ctx = (Z3.mk_context cfg)  in

    (* PART1: mk_const_s/mk_const_int *)
    (* (declare-const x Int) *)
    (*
    let a = BitVector.mk_const_s ctx "a" vector_length in
    let b = BitVector.mk_const_s ctx "b" vector_length in
    let c = BitVector.mk_const_s ctx "c" vector_length in
    *)

    (*
    let sts = [mk_slt ctx a b;
               mk_slt ctx b c;
               mk_slt ctx a c] in
    *)
    (* PART2: construct list with above assertions *)
    (* let assertions = sym_to_smt ctx pc in *)
    let assertions = List.map (sym_to_smt ctx) pc in

    (* goal (context &c, bool models=true, bool unsat_cores=false, bool proofs=false) 
     * https://z3prover.github.io/api/html/classz3_1_1goal.html *)
    let g = (Goal.mk_goal ctx true false false) in
    Goal.add g assertions;


    let solver = Solver.mk_solver ctx None in

    (List.iter (fun a -> (Solver.add solver [ a ])) (Goal.get_formulas g)) ;

    let status = Solver.check solver [] in

    let stmsg ="Exiting `check_pc ()` with status: " ^ Solver.string_of_status status in

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


