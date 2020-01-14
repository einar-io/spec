open Types
open Utils


let wr addr value mem = Store.add addr value mem


let ld addr mem =
    match Store.find_opt addr mem with
    | Some value -> value
    | None       -> SCon 0l


let sym_step ((ip, next, symmem, symstack, pc) as symstate) instr : symstate list =
    let step_possible = SMT.satisfiable pc in
    if not step_possible then [] else
    match instr, symstack with

    | Add, l::r::tl               -> [(ip+1l, next+1l, symmem, SAdd(l, r)::tl, pc)]

    | JmpIf, cond::SCon addr::tl  -> [(ip+1l, next, symmem, symstack, SNot cond::pc); (* false branch *)
                                      (addr,  next, symmem, symstack,      cond::pc)] (* true branch *)
    | JmpIf, _::_::tl             -> [(ip+1l, next, symmem, symstack, pc)]            (* not SCon value *)

    | Dup, hd::tl                 -> [(ip+1l, next, symmem, hd::hd::tl, pc)]

    | Eq, l::r::tl when l=r       -> [(ip+1l, next, symmem, SCon 1l::tl, pc)]
    | Eq, l::r::tl                -> [(ip+1l, next, symmem, SCon 0l::tl, pc)]

    | Not, hd::tl when hd=SCon 0l -> [(ip+1l, next, symmem, SCon 1l::tl, pc)]
    | Not, hd::tl                 -> [(ip+1l, next, symmem, SCon 0l::tl, pc)]

    | Over, a::b::c::tl           -> [(ip+1l, next, symmem, a::b::c::a::tl, pc)]

    | Pop, hd::tl                 -> [(ip+1l, next, symmem, tl, pc)]
    | Push hd, stack              -> [(ip+1l, next, symmem, SCon hd::stack, pc)]

    | Store, SCon addr::value::tl -> [(ip+1l, next, (wr addr value symmem), tl, pc)]
    | Store,             _::_::tl -> [(ip+1l, next, symmem, tl, pc)]
    | Load, SCon addr::tl         -> [(ip+1l, next, symmem, (ld addr symmem)::tl, pc)]

    | Swap, l::r::tl              -> [(ip+1l, next, symmem, r::l::tl, pc)]

    | Trace, stack                -> print_symstate symstate; [(ip+1l, next, symmem, stack, pc)]

    | Local sv, _                -> [(ip+1l, next+1l, symmem, SAny next::symstack, pc)]

    (* Illegal instruction-stack pair *)
    | _, _                        -> []


let rec sym_run max_depth prg ((ip, next, symmem, symstack, pc) as symstate) : symstate tree =
    match List.nth_opt prg @@ Int32.to_int ip with
    | Some Done  -> Node (symstate, [])
    | Some instr when max_depth > 0l ->
                        let child_states = sym_step symstate instr in
                        let children = List.map (sym_run (max_depth + -1l) prg) child_states in
                        Node (symstate, children)
    | Some instr ->
                        print_endline @@ "Killed program after reaching `max_depth`.";
                        Node (symstate, [])

    | None       -> print_endline
                    @@ "Error: Non instruction at" ^ Int32.to_string ip;
                    Empty


let print_sym_res ?(max_depth = 8l) title prg state =
        print_endline @@ "Running program: " ^ title;
        match sym_run max_depth prg state with
        | Empty     -> print_endline "Nothing to print: symstate tree is <Empty>."
        | tree      -> draw_tree tree;
                       print_halt ()
