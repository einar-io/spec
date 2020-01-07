(* The option :print-success true is particularly useful when Z3 is being
 * controlled by another application using pipes. In this mode, commands, that
 * otherwise would not print any output, will print success.
 *)

(* open Z3 this shadows Set *)
open Z3
open Z3.Solver
open Z3.BitVector
open Utils
open Types

(* Our Set *)
module S = Types.Symwords


(* Create the set of unassigned variables *)
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


(* in: [SAny 1; SAny 2; .. SAny N]
 * out: [(1, SAny 1); (2, SAny 2); .. (N, SAny N)]
 *)
    (* PART1: mk_const_s/mk_const_int *)
    (* (declare-const x Int) *)
    (*
let createSym ctx pcs =
    let create_pair free_var =
    match free_var with
    | (SAny i) ->
        let expr_id = Symbol.mk_int ctx (Int32.to_int i) in
        let z3_expr = BitVector.mk_const ctx expr_id wordsize in
        (i, z3_expr)
    | _   -> raise @@ Error "Constructor was not SAny."
    in List.map create_pair pcs
    *)

let sym_to_z3 ctx = function
    | (SAny i) ->
        let expr_id = Symbol.mk_int ctx (Int32.to_int i) in
        let z3_expr = BitVector.mk_const ctx expr_id wordsize in
        (i, z3_expr)
    | _   -> raise @@ Error "Constructor was not SAny."


let combine_assertions pcs =
    List.fold_left (fun x y -> SAnd (x, y)) (SCon 1l) pcs


let rec tree_mapper ctx symvar_map pc =
    let s2s = tree_mapper ctx symvar_map in
    match pc with
    | SNot a      -> BitVector.mk_not ctx (s2s a)
  (*| SAny a      -> List.find (fun (i, _) -> i = a) symvar_map |> snd *)
    | SAny a      -> SMTmap.find a symvar_map
    | SAdd (a, b) -> BitVector.mk_add ctx (s2s a) (s2s b)
    | SEq  (a, b) -> Boolean.mk_eq ctx (s2s a) (s2s b)
    | SOr  (a, b) -> BitVector.mk_or ctx (s2s a) (s2s b)
    | SCon a      -> BitVector.mk_numeral ctx (Int32.to_string a) wordsize
    | SAnd (a, b) -> BitVector.mk_and ctx (s2s a) (s2s b)
    | SLt  (a, b) -> BitVector.mk_slt ctx (s2s a) (s2s b)


let sym_to_smt ctx symvar_map pcs = List.map (tree_mapper ctx symvar_map) pcs


let print_ith_assertion last_index (i : int) a =
    let offset = last_index - 1 - i in
    print_endline 
        @@ "Assertion[" 
        ^ string_of_int offset
        ^ "]:"
        ^ Expr.to_string a


let to_assertions ctx pcs =
    let combined_pc = combine_assertions pcs in
    let set_of_free_vars = gather_free combined_pc in
    let list_of_free_vars = S.elements set_of_free_vars in
    (* let symMap = createSym ctx list_of_free_vars in *)
    let symvar_map = list_of_free_vars
                    |> List.map (sym_to_z3 ctx)
                    |> List.to_seq
                    |> SMTmap.of_seq
    in
    let z3assertions = sym_to_smt ctx symvar_map pcs in
    z3assertions


let check_pc pcs =
    (* PART 0: Initialisation *)
    print_endline @@ print_pc pcs;
    print_endline "---------------- Invoking `check_pc ()`.";
    Z3.toggle_warning_messages true;

    let with_log = Z3.Log.open_ "Z3.log" in
    if  with_log then (
        Z3.Log.append @@ "Running Z3 version: " ^ Version.to_string ^ "\n";
        Z3.Log.append @@ "Z3 full version string: " ^ Version.full_version ^ "\n";
        Z3.Log.append "Entering `check_pc ()`.."
    ) else ();

    let cfg = [("model", "true" );
               ("proof", "false");
            (* ("timeout", "10000"); (*10k miliseconds *) *)
               ("trace", "true");
               ("trace_file_name", "smt-trace.log");
               ("unsat_core", "false");
               ] in
    let ctx = (Z3.mk_context cfg)  in

    (* PART2: construct list with above assertions *)
    (* This should be a Z3.Expr.expr list *)
    let assertions = to_assertions ctx pcs in
    print_endline @@ "Assertions: ";
    List.iteri (print_ith_assertion @@ List.length assertions) assertions;
    (* List.iter (fun x -> print_endline @@ Expr.to_string x) assertions; *)

    (* goal (context &c, bool models=true, bool unsat_cores=false, bool proofs=false) 
     * https://z3prover.github.io/api/html/classz3_1_1goal.html *)
    let g = Goal.mk_goal ctx true false false in
    print_endline "DEBUG start";
    Goal.add g assertions;
    print_endline "DEBUG stop";
    print_endline @@ Goal.to_string g;

    let solver = Solver.mk_solver ctx None in

    print_endline "DEBUG start";
    List.iter (fun a -> (Solver.add solver [ a ])) (Goal.get_formulas g);
    print_endline "DEBUG stop";

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


let satisfiable = function
    (* print_endline @@ print_pc pc; *)
    (* match pc with *)
    | [] -> let s = true in (* print_endline @@ string_of_bool s;*) s
    | pc -> try let s = check_pc pc in print_endline @@ string_of_bool s; s
            with Error msg -> let emsg = "Z3 ML/OCaml exception thrown with message: "
            ^ msg in print_endline emsg; false


