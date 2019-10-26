(*# require "batteries" *)
open Batteries.Option

open Types

(* operators cannot be overloaded
 * https://stackoverflow.com/a/53131220 *)
let (+) a b = Int32.add a b

(* replace with Module Option.Monad_infix
 * https://ocaml.janestreet.com/ocaml-core/latest/doc/base/Base/Option/Monad_infix/index.html
 * *)
(* let (>>=) = bind (* Batteries.Option.bind *) *)


(* Symbolic.hs/renderSym *)
let rec render_sym (s : symword) : string =
    match s with
    | SAdd (s, t)  -> "(" ^ render_sym s ^ " + " ^ render_sym t ^ ")"
    | SEq  (s, t)  -> "(" ^ render_sym s ^ " = " ^ render_sym t ^ ")"
    | SAnd (s, t)  -> "(" ^ render_sym s ^ " = " ^ render_sym t ^ ")"
    | SOr  (s, t)  -> "(" ^ render_sym s ^ " = " ^ render_sym t ^ ")"
    | SLt  (s, t)  -> "(" ^ render_sym s ^ " = " ^ render_sym t ^ ")"
    | SNot c       -> "~(" ^ render_sym c ^ ")"
    | SCon w       -> Int32.to_string w
    | SAny v       -> Int32.to_string v


let print_symstack stackold =
    let rec print_elems stack =
        match stack with
        | hd::[] -> render_sym hd
        | hd::tl -> render_sym hd ^ "|" ^ print_elems tl
        | []     -> "<empty>"
    in
    let stack = List.rev stackold in
    (* print_endline @@ "SymStack: [" ^  print_elems stack ^  "]" *)
    "[" ^ print_elems stack ^  "]"



let rec draw_tree ?(indent = 0l) (tree : symstate tree) =
    let indentation = String.make (Int32.to_int indent) ' ' in
    let print_indented st = print_endline @@ indentation ^ st in
    match tree with
    | Empty        -> print_endline "Symstate tree is Empty."
    | Node (s, ss) -> let (ip, next, symmem, symstack, pc) = s in
                      print_indented @@ "IP: " ^ Int32.to_string ip;
                      print_indented @@ "Next: " ^ Int32.to_string next;
                      print_indented @@ "SymStack: " ^ print_symstack symstack;
                      (*
                      print_indented @@ "SymMemory: " ^ symmem;
                      print_indented @@ "Path Constraints: " ^ pc
                      *)
                      (* print_indented @@ "Solved Values" ^ ""*)
                      print_indented @@ "|";
                      print_indented @@ "`-";
                      List.iter (draw_tree ~indent:(indent+1l)) ss




let print_ip ip = print_endline @@ "IP: " ^ Int32.to_string ip


let print_halt () =
        print_endline "";
        print_endline "[HALT]";
        print_endline "";
