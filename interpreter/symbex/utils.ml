open Types


let (+) a b = Int32.add a b


module I32 = struct
    open Int32
    let (+), (-) = add, sub
end


(* Symbolic.hs/renderSym *)
let rec render_sym (s : symword) : string =
    match s with
    | SAdd (s, t) -> "(" ^ render_sym s ^ " + " ^ render_sym t ^ ")"
    | SEq  (s, t) -> "(" ^ render_sym s ^ " = " ^ render_sym t ^ ")"
    | SAnd (s, t) -> "(" ^ render_sym s ^ " && " ^ render_sym t ^ ")"
    | SOr  (s, t) -> "(" ^ render_sym s ^ " || " ^ render_sym t ^ ")"
    | SLt  (s, t) -> "(" ^ render_sym s ^ " < " ^ render_sym t ^ ")"
    | SNot c      -> "~(" ^ render_sym c ^ ")"
    | SCon w      -> "(i32.const " ^ Int32.to_string w ^ ")"
    | SAny v      -> "(local $" ^ (v
                                   |> (+) 97l
                                   |> Int32.to_int
                                   |> Char.chr
                                   |> Char.escaped)
                                   ^ ")"


let print_ip ip = print_endline @@ "IP: " ^ Int32.to_string ip


let print_next next = print_endline @@ "Next: " ^ Int32.to_string next


let print_symstack stackold =
    let rec print_elems stack =
        match stack with
        | hd::[] -> render_sym hd
        | hd::tl -> render_sym hd ^ "|" ^ print_elems tl
        | []     -> "<Empty>"
    in
    let stack = List.rev stackold in
    "[" ^ print_elems stack ^  "]"


let print_symmem_entry key value : string =
    "(" ^ Int32.to_string key ^ "->" ^ render_sym value ^ ")"


let print_symmem symmem =
    if Store.is_empty symmem then "<Empty>" else
    Store.fold (fun k v a -> print_symmem_entry k v ^ a) symmem ""


let print_pc = function
    | [] -> "Trivial"
    | pc -> List.fold_left (fun y x -> x ^ "; " ^ y) "" (List.map render_sym pc)


let print_solved_values sv = "Not implemented."


let print_symstate (ip, next, symmem, symstack, pc) =
    print_endline "-------- TRACE ---------";
    print_ip ip;
    print_next next;
    print_endline @@ "SymStack: " ^ print_symstack symstack;
    print_endline @@ "SymMem: " ^ print_symmem symmem;
    print_endline @@ "Path Constraints: " ^ print_pc pc;
    print_endline "--------  END  ---------"


let rec draw_tree ?(indent = 0l) (tree : symstate tree) =
    let indentation = String.make (Int32.to_int indent) ' ' in
    let print_indented st = print_endline @@ indentation ^ st in
    match tree with
    | Empty        -> print_endline "Symstate tree is Empty."
    | Node (s, ss) -> let (ip, next, symmem, symstack, pc) = s in
                      print_indented @@ "IP: " ^ Int32.to_string ip;
                      print_indented @@ "Next: " ^ Int32.to_string next;
                      print_indented @@ "SymMemory: " ^ print_symmem symmem;
                      print_indented @@ "SymStack: " ^ print_symstack symstack;
                      print_indented @@ "Path Constraints: " ^ print_pc pc;
                      (* print_indented @@ "Solved Values: " ^ * print_solved_values ""; *)
                      (* print_indented @@ "Indentation level: " ^ * Int32.to_string indent; *)
                      print_indented @@ "|";
                      print_indented @@ "`-";
                      List.iter (draw_tree ~indent:(I32.(indent+1l))) ss


let print_halt () = Printf.printf "\n[HALT]\n"


let print_ith_assertion last_index (i : int) a =
    let offset = string_of_int @@ last_index - 1 - i in
    Printf.printf "Assertion[%s]: %s\n" offset (Z3.Expr.to_string a)
