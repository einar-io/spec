# require "batteries"
open Batteries.Option
open String
open List
open Int32

(* operators cannot be overloaded
 * https://stackoverflow.com/a/53131220 *)
let (+) a b = Int32.add a b

(* key : int32 -> value : int32 *)
module Store = Map.Make(Int32)

type word = int32

type ip = word
type mem = word Store.t
type stack = word list
(* Program state: (Instruction pointer, Store, Stack) *)
type state = ip * mem * stack

type instr =
    | Add
    | And
    | Or
    | Not
    | Lt
    | Eq
    | Push of word
    | Pop
    | Swap
    | Dup
    | Over
    | Print
    | RotL
    | Read
    | Trace
    | Show
    | JmpIf
    | Store
    | Load
    | Done

let maxpc = 16l
let init_state : state = (0l, Store.empty, [])

let (>>=) = bind (* Batteries.Option.bind *)

let print_ip ip = print_endline @@ "IP: " ^ Int32.to_string ip


let print_mem_pair key value = print_endline
                            @@ (Int32.to_string key)
                            ^  ": "
                            ^  (Int32.to_string value)

let print_mem mem = print_endline "";
                    print_endline "Memory Map";
                    print_endline "----------";
                    Store.iter print_mem_pair mem;
                    print_endline ""


let print_stack stackold =
    let rec print_elems stack =
        match stack with
        | hd::[] -> Int32.to_string hd
        | hd::tl -> Int32.to_string hd ^ "|" ^ print_elems tl
        | []     -> "<empty>"
    in
    let stack = List.rev stackold in
    print_endline @@ "Stack: [" ^  print_elems stack ^  "]"

let print_state (ip, mem, stack) =
    print_ip      ip;
    print_mem     mem;
    print_stack   stack


let wr addr value mem = Store.add addr value mem


let ld addr mem =
    match Store.find_opt addr mem with
    | Some value -> value
    | None       -> 0l


let substitute
?ip ?mem ?stack (oldip, oldmem, oldstack) : state option =
    let nextip = oldip + 1l in
    match ip, mem, stack with
    | None       , None        , None          -> Some (nextip , oldmem , oldstack)

    | Some newip , None        , None          -> Some (newip  , oldmem , oldstack)
    | None       , Some newmem , None          -> Some (nextip , newmem , oldstack)
    | None       , None        , Some newstack -> Some (nextip , oldmem , newstack)

    | None       , Some newmem , Some newstack -> Some (nextip , newmem , newstack)
    | Some newip , None        , Some newstack -> Some (newip  , oldmem , newstack)
    | Some newip , Some newmem , None          -> Some (newip  , newmem , oldstack)

    | Some newip , Some newmem , Some newstack -> Some (newip  , newmem , newstack)



let step ((ip, mem, stack) as state) instr : state option =
    match instr, stack with

    | Add, l::r::tl                            -> substitute ~stack:(l+r::tl) state

    | Dup, hd::tl                              -> substitute ~stack:(hd::hd::tl) state

    | Eq, l::r::tl when l=r                    -> substitute ~stack:(1l::tl) state
    | Eq, l::r::tl                             -> substitute ~stack:(0l::tl) state

    | JmpIf, cond::addr::tl when cond!=0l      -> substitute ~ip:addr ~stack:(tl) state
    | JmpIf, cond::addr::tl                    -> substitute          ~stack:(tl) state

    | Not, hd::tl when hd=0l                   -> substitute ~stack:(1l::tl) state
    | Not, hd::tl                              -> substitute ~stack:(0l::tl) state

    | Over, a::b::c::tl                        -> substitute ~stack:(a::b::c::a::tl) state

    | Pop, a::tl                               -> substitute ~stack:(tl) state
    | Push a, stack                            -> substitute ~stack:(a::stack) state

    | Store, addr::value::tl                   -> substitute ~mem:(wr addr value mem) ~stack:(tl) state
    | Load,  addr::tl                          -> print_mem mem; substitute ~stack:((ld addr mem)::tl) state

    | Swap, l::r::tl                           -> substitute ~stack:(r::l::tl) state

    | Trace, stack                             -> print_state state; substitute state

    (* Not implemented *)
    | Show, _ | Read, _                        -> None

    (* Illegal instruction-stack pair *)
    | _, _                                     -> None


let rec run prg ((ip, mem, stack) as state) : stack option =
    print_ip ip;
    (* if ip > maxpc then None else *)
    match List.nth_opt prg @@ Int32.to_int ip with
    | Some Done  -> Some stack
    | Some instr -> step state instr >>= run prg
    | None       -> print_endline
                 @@ "Error: Non instruction at" ^ Int32.to_string ip
                 ; None

let print_res title prg state =
        print_endline "";
        print_endline "-R -E -S -E -T-";
        print_endline "";
        print_endline title;
        match run prg state with
        | Some stack -> print_stack stack
        | None       -> print_endline "An error occured."


(* example program *)
(* let addTwoNumbers = [Read; Read; Add; Trace; Done] *)
let addTwoNumbers = [Add; Trace; Done]
let pushAdd = [Push 5l; Push 29l; Add; Done]
let storeLoadAddPrint = [ Push 4l
                        ; Push 2l
                        ; Push 3l
                        ; Push 1l
                        ; Trace
                        ; Store
                        ; Store
                        ; Push 1l
                        ; Load
                        ; Push 2l
                        ; Load
                        ; Trace
                        ; Add
                        ; Done]

let prgLoop = [Push 0l; Push 1l; Trace; JmpIf; Trace]

(*
let () = print_res "addTwoNumbers" addTwoNumbers (0l, Store.empty, [3l;5l])
let () = print_res "pushAdd" pushAdd init_state
let () = print_res "prgLoop" prgLoop init_state (*infinite loop*)
let () = print_res "storeLoadAddPrint" storeLoadAddPrint init_state
*)



type symword =
    | SAdd of symword * symword
    | SEq  of symword * symword
    | SNot of symword
    | SOr  of symword * symword
    | SCon of word
    | SAnd of symword * symword
    | SLt  of symword * symword
    | SAny of word (* Beal uses Int *)


(* key : int32 -> value : symword *)
module Store  = Map.Make(Int32)

type ip       = word
type next     = word
type symmem   = symword Store.t
type symstack = symword list
type pc       = symword list
type symstate = ip * next * symmem * symstack * pc
type 'a tree  = Empty | Node of 'a * 'a tree list




let rec draw_tree ?(indent = 0l) (tree : symstate tree) =
    let indentation = String.make (Int32.to_int indent) ' ' in
    let print_indented st = print_endline @@ indentation ^ st in
    match tree with
    | Empty        -> print_endline "Symstate tree is Empty."
    | Node (s, ss) -> let (ip, next, symmem, symstack, pc) = s in
                      print_indented @@ "IP: " ^ Int32.to_string ip;
                      print_indented @@ "Next: " ^ Int32.to_string next;
                      print_indented @@ "|";
                      print_indented @@ "`-";
                      List.iter (draw_tree ~indent:(indent+1l)) ss
                      (*
                      print_endline @@ "SymMemory: " ^ symmem;
                      print_endline @@ "SymStack: " ^ symstack;
                      print_endline @@ "Path Constraints: " ^ pc
                      *)
                      (* print_endline @@ "Solved Values" ^ ""*)







let ld addr mem =
    match Store.find_opt addr mem with
    | Some value -> value
    | None       -> SCon 0l

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



let symStep ((ip, next, symmem, symstack, pc) as symstate) instr : symstate list =
    match instr, symstack with

    | Add, l::r::tl               -> [(ip+1l, next+1l, symmem, SAdd(l, r)::tl, pc)]


    | JmpIf, cond::SCon addr::tl  -> [(ip+1l, next, symmem, symstack, SNot cond::pc); (* false branch *)
                                      (addr,  next, symmem, symstack,      cond::pc)] (* true branch *)

    | JmpIf, _::_::tl             -> [(ip+1l, next, symmem, symstack, pc)] (* false branch *)

    | Dup, hd::tl                 -> [(ip+1l, next, symmem, hd::hd::tl, pc)]

    | Eq, l::r::tl when l=r       -> [(ip+1l, next, symmem, SCon 1l::tl, pc)]
    | Eq, l::r::tl                -> [(ip+1l, next, symmem, SCon 0l::tl, pc)]

    | Not, hd::tl when hd=SCon 0l -> [(ip+1l, next, symmem, SCon 1l::tl, pc)]
    | Not, hd::tl                 -> [(ip+1l, next, symmem, SCon 0l::tl, pc)]

    | Over, a::b::c::tl           -> [(ip+1l, next, symmem, a::b::c::a::tl, pc)]

    | Pop, a::tl                  -> [(ip+1l, next, symmem, tl, pc)]
    | Push a, stack               -> [(ip+1l, next, symmem, SCon a::stack, pc)]

    | Store, SCon addr::value::tl -> [(ip+1l, next, (wr addr value symmem), tl, pc)]
    | Store,             _::_::tl -> [(ip+1l, next, symmem, tl, pc)]
    (* print_mem symmem *)
    | Load, SCon addr::tl         -> [(ip+1l, next, symmem, (ld addr symmem)::tl, pc)]

    | Swap, l::r::tl              -> [(ip+1l, next, symmem, r::l::tl, pc)]

    | Trace, stack                -> (* print_state symstate;*) [(ip+1l, next, symmem, stack, pc)]

    (* Not implemented *)
    | Show, _ | Read, _           -> []

    (* Illegal instruction-stack pair *)
    | _, _                        -> []



let rec symRun maxDepth prg ((ip, next, symmem, symstack, pc) as symstate) : symstate tree =
    print_ip ip;
    (* if ip > maxpc then None else *)
    match List.nth_opt prg @@ Int32.to_int ip with
    | Some Done  -> Node (symstate, [])
    | Some instr -> if maxDepth > 0l then
                        let childStates = symStep symstate instr in
                        let children = List.map (symRun (maxDepth + -1l) prg) childStates in
                        Node (symstate, children)
                    else
                        Node (symstate, [])

    | None       -> print_endline
                 @@ "Error: Non instruction at" ^ Int32.to_string ip
                 ; Empty






let print_sym_res title prg state =
        print_endline "";
        print_endline "-R -E -S -E -T-";
        print_endline "";
        print_endline title;
        let maxDepth = 16l in
        match symRun maxDepth prg state with
        | Empty     -> print_endline "Symstate tree is Empty."
        | tree      -> draw_tree tree


let init_symstate : symstate = (0l, 0l, Store.empty, [], [])

let () = print_sym_res "addTwoNumbers" addTwoNumbers (0l, 0l, Store.empty, [SCon 3l; SCon 5l], [])
let () = print_sym_res "pushAdd" pushAdd init_symstate
(* let () = print_sym_res "prgLoop" prgLoop init_symstate (*infinite loop*) *)
let () = print_sym_res "storeLoadAddPrint" storeLoadAddPrint init_symstate



