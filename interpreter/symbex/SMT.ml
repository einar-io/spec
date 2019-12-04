(*
let context = Z3.mk_context []
let solver = Z3.Solver.mk_solver context None

let xsy = Z3.Symbol.mk_string context "x"
let x = Z3.Boolean.mk_const context xsy

let () = Z3.Solver.add solver [x]

let main () =
    match Z3.Solver.check solver [] with
    | UNSATISFIABLE -> Printf.printf "unsat\n" 
    | UNKNOWN -> Printf.printf "unknown"
    | SATISFIABLE ->
        match Z3.Solver.get_model solver with
        | None -> ()
        | Some model ->
            Printf.printf "%s\n"
                (Z3.Model.to_string model)

let () = main ()
*)

(* The option :print-success true is particularly useful when Z3 is being
 * controlled by another application using pipes. In this mode, commands, that
 * otherwise would not print any output, will print success.
 *)

open Utils
open Z3
open Z3.Boolean
open Z3.Symbol
open Z3.Expr

let satisfiable pc =
    print_endline @@ print_pc pc;
    if pc == [] then true else false
    (*let smtExpr = toSMT pc in
    let context = Z3.mk_context [] in
    let solver = Z3.Solver.mk_solver context None *)

(*
(declare-const a Bool)
(declare-const b Bool)
(define-fun demorgan () Bool
    (= (and a b) (not (or (not a) (not b)))))
(assert (not demorgan))
(check-sat)
let demorgan ctx =
    let demorgan ctx in
*)


let main () =
    print_endline "Invoking `main()`.";
    Printf.printf "Running Z3 version %s\n" Version.to_string ;
    Printf.printf "Z3 full version string: %s\n" Version.full_version ;

    (* timeout (unsigned) default timeout (in milliseconds) used for solvers *)
    let cfg = [("model", "true"); ("proof", "false")] in
    let ctx = (mk_context cfg) in
    let bs = (Boolean.mk_sort ctx) in

    let a = (mk_string ctx "a") in
    let b = (mk_string ctx "b") in
    let fname = (mk_string ctx "f") in
    let f = (FuncDecl.mk_func_decl ctx fname [ bs; bs ] bs) in
    let fapp = (mk_app ctx f [ (Expr.mk_const ctx a bs); (Expr.mk_const ctx b bs)]) in
    let fargs2 = [ (mk_fresh_const ctx "cp" bs) ] in
    


    (*  	goal (context &c, bool models=true, bool unsat_cores=false, bool
     *  	proofs=false) 
    * https://z3prover.github.io/api/html/classz3_1_1goal.html
    let g 
    *  	*)

    
    (*
    printf "bool sort: %s\n" (Sort.to_string bs);
    printf "bool sort: %s\n" (Sort.to_string bs);
    *)

    (*
    let f = (
    

        let solver = (mk_solver ctx None) in (Solver.add solver [c]) ;
        check solver [];

       (* 
        match check solver [] with
        | SATISFIABLE -> ()
        | UNSATISFIABLE -> ()
        | UNKNOWN ->

        *)
       *)





    print_endline "`main()` about to return.";
    exit 0






let () = main ()
