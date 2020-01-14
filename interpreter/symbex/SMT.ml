open Z3 (* shadows Set *)
open Utils
open Types


(* Create the set of unassigned variables *)
let rec gather_free pc =
    match pc with
    | SNot a      -> gather_free a
    | SCon a      -> Symset.empty
    | SAny a as e -> Symset.singleton e
    | SAnd (a, b) -> Symset.union (gather_free a) (gather_free b)
    | SAdd (a, b) -> Symset.union (gather_free a) (gather_free b)
    | SEq  (a, b) -> Symset.union (gather_free a) (gather_free b)
    | SOr  (a, b) -> Symset.union (gather_free a) (gather_free b)
    | SLt  (a, b) -> Symset.union (gather_free a) (gather_free b)


let combine_assertions pcs =
    let acc = SCon 1l in
    List.fold_left (fun x y -> SAnd (x, y)) acc pcs


let sym_to_z3 ctx = function
    | SAny i when 0l <= i && i < 1073741824l ->
        let expr_id = Symbol.mk_int ctx (Int32.to_int i) in
        let z3_expr = BitVector.mk_const ctx expr_id wordsize in
        (i, z3_expr)
    | _   -> raise @@ Error "Constructor was not `SAny` or too many symbolic variables."


let is_false ctx bv =
    Boolean.mk_eq ctx bv @@ BitVector.mk_numeral ctx "0" wordsize


let is_true ctx bv =
    Boolean.mk_not ctx @@ is_false ctx bv


let sbooltosbv ctx bv =
    Boolean.mk_ite ctx bv
        (BitVector.mk_numeral ctx "1" wordsize)
        (BitVector.mk_numeral ctx "0" wordsize)


let rec tree_mapper ctx symvar_map pc =
    let s2s = tree_mapper ctx symvar_map in
    match pc with
    | SNot a      -> sbooltosbv ctx @@ is_false ctx (s2s a)
    | SAny a      -> SMTmap.find a symvar_map
    | SCon a      -> BitVector.mk_numeral ctx (Int32.to_string a) wordsize
    | SEq  (a, b) -> Boolean.mk_eq ctx (s2s a) (s2s b)
    | SAdd (a, b) -> BitVector.mk_add ctx (s2s a) (s2s b)
    | SOr  (a, b) -> BitVector.mk_or ctx (s2s a) (s2s b)
    | SAnd (a, b) -> BitVector.mk_and ctx (s2s a) (s2s b)
    | SLt  (a, b) -> BitVector.mk_slt ctx (s2s a) (s2s b)


let sym_to_smt ctx symvar_map pcs = List.map (tree_mapper ctx symvar_map) pcs





let bv_mk_and ctx bv1 bv2 =
    Boolean.mk_and ctx [is_true ctx bv1; is_true ctx bv2]


let conjoin ctx pc =
    match pc with
    | [] -> Boolean.mk_true ctx
    | pc -> Boolean.mk_and ctx @@ List.map (is_true ctx) pc


let conjoinfold ctx pc =
    let ne = BitVector.mk_numeral ctx "1" wordsize in
    List.fold_left (bv_mk_and ctx) ne pc


let to_assertions ctx pcs =
    let combined_pc = combine_assertions pcs in
    let set_of_free_vars = gather_free combined_pc in
    let list_of_free_vars = Symset.elements set_of_free_vars in
    let symvar_map = list_of_free_vars
                    |> List.map (sym_to_z3 ctx)
                    |> List.to_seq
                    |> SMTmap.of_seq
    in
    let z3assertions = sym_to_smt ctx symvar_map pcs in
    z3assertions


let check_pc pcs =
    (* PART 0: Initialisation *)
    print_endline "--- Invoking `check_pc ()`.";
    Z3.toggle_warning_messages true;

    let with_log = Z3.Log.open_ "Z3.log" in
    if  with_log then (
        Z3.Log.append @@ "Running Z3 version: " ^ Version.to_string ^ "\n";
        Z3.Log.append @@ "Z3 full version string: " ^ Version.full_version ^ "\n";
        Z3.Log.append "Entering `check_pc ()`.."
    ) else ();

    let cfg = [("model", "false");
               ("proof", "false");
               ("timeout", "10000"); (* miliseconds *)
               ("trace", "true");
               ("trace_file_name", "smt-trace.log");
               ("unsat_core", "false");]
    in
    let ctx = Z3.mk_context cfg in

    (* PART2: construct list with above assertions *)
    (* This should be a Z3.Expr.expr list *)
    let assertions = to_assertions ctx pcs in
    (* print_endline @@ "Assertions: "; *)
    List.iteri (print_ith_assertion @@ List.length assertions) assertions;
    (* List.iter (fun x -> print_endline @@ Expr.to_string x) assertions; *)

    let conjoined = conjoin ctx assertions in (* print_ith_assertion 2 0 conjoined; *)

    (* goal (context &c, bool models=true, bool unsat_cores=false, bool proofs=false) 
     * https://z3prover.github.io/api/html/classz3_1_1goal.html *)
    let g = Goal.mk_goal ctx true false false in
    Goal.add g [conjoined];
    print_endline @@ Goal.to_string g;

    let solver = Solver.mk_solver ctx None in

    List.iter (fun a -> Solver.add solver [a]) (Goal.get_formulas g);

    let status = Solver.check solver [] in
    
    let colored_status = Utils.color status in
    
    (* Logging and printing *)
    let stmsg = "Returns: " ^ colored_status in

    print_endline stmsg; 

    if with_log then (Log.append stmsg; Log.close ()) else ();

    (* Return *)
    match status with
    | UNKNOWN
    | SATISFIABLE   -> true
    | UNSATISFIABLE -> false


let satisfiable pc =
    match pc with
    | [] -> true (* No constraints *)
    | pc -> try check_pc pc
            with Error msg -> let emsg = "Z3 ML/OCaml exception: " ^ msg 
                              in print_endline emsg;
                              false


