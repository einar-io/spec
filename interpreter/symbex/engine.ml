# require "batteries"
open Batteries.Option
open String
open List
open Int32

(* operators cannot be overloaded
 * https://stackoverflow.com/a/53131220 *)
let (+) a b = Int32.add a b

module Store = Map.Make(Int32)

type word = int32

type pc = word
type mem = word Store.t
type stack = word list
(* Program state: (Instruction pointer, Store, Stack) *)
type state = pc * mem * stack

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
    | RotL
    | Read
    | Print
    | JmpIf
    | Load
    | Store
    | Done

let maxpc = 16l
let init_state : state = (0l, Store.empty, [])

let (>>=) = bind (* Batteries.Option.bind *)

let print_pc pc = print_endline @@ "PC: " ^ Int32.to_string pc


let print_mem_pair key value = print_endline
                            @@ (Int32.to_string key)
                            ^  ": "
                            ^  (Int32.to_string value)

let print_mem mem = print_endline "";
                    print_endline "Memory Map";
                    print_endline "----------";
                    Store.iter print_mem_pair mem;
                    print_endline ""

let print_stackold stack =
    print_endline
    @@ "Stack content is: ["
    ^ let prefix =
        match stack with
        | hd::[] -> Int32.to_string hd
        | hd::tl -> Int32.to_string hd
                    ^  List.fold_left (^) ";"
                    @@ List.map Int32.to_string tl
        | []     -> "(empty)"
    in prefix ^ "]"

let print_stack stackold =
    let stack = List.rev stackold in
    let rec print_elems stack =
        match stack with
        | hd::[] -> Int32.to_string hd
        | hd::tl -> Int32.to_string hd ^ "|" ^ print_elems tl
        | []     -> "<empty>"
    in print_endline
    @@ "Stack: [" ^  print_elems stack ^  "]"

let print_state (pc, mem, stack) =
    print_pc      pc;
    print_mem     mem;
    print_stack   stack


let wr addr value mem = Store.add addr value mem


let ld addr mem =
    match Store.find_opt addr mem with
    | Some value -> value
    | None       -> 0l


let substitute
?pc ?mem ?stack (oldpc, oldmem, oldstack) : state option =
    let nextpc = oldpc + 1l in
    match pc, mem, stack with
    | None       , None        , None          -> Some (nextpc , oldmem , oldstack)

    | Some newpc , None        , None          -> Some (newpc  , oldmem , oldstack)
    | None       , Some newmem , None          -> Some (nextpc , newmem , oldstack)
    | None       , None        , Some newstack -> Some (nextpc , oldmem , newstack)

    | None       , Some newmem , Some newstack -> Some (nextpc , newmem , newstack)
    | Some newpc , None        , Some newstack -> Some (newpc  , oldmem , newstack)
    | Some newpc , Some newmem , None          -> Some (newpc  , newmem , oldstack)

    | Some newpc , Some newmem , Some newstack -> Some (newpc  , newmem , newstack)



let step ((pc, mem, stack) as state) instr : state option =
    match instr, stack with

    | Add, l::r::tl                            -> substitute ~stack:(l+r::tl) state

    | Dup, hd::tl                              -> substitute ~stack:(hd::hd::tl) state

    | Eq, l::r::tl when l=r                    -> substitute ~stack:(1l::tl) state
    | Eq, l::r::tl                             -> substitute ~stack:(0l::tl) state

    | JmpIf, cond::addr::tl when cond!=0l      -> substitute ~pc:addr ~stack:(tl) state
    | JmpIf, cond::addr::tl                    -> substitute          ~stack:(tl) state

    | Not, hd::tl when hd=0l                   -> substitute ~stack:(1l::tl) state
    | Not, hd::tl                              -> substitute ~stack:(0l::tl) state

    | Over, a::b::c::tl                        -> substitute ~stack:(a::b::c::a::tl) state

    | Pop, a::tl                               -> substitute ~stack:(tl) state

    | Print, stack                             -> print_stack stack; substitute state

    | Push a, stack                            -> substitute ~stack:(a::stack) state

    | Read, _                                  -> None

    | Store, addr::value::tl                   -> substitute ~mem:(wr addr value mem) ~stack:(tl) state
    | Load,  addr::tl                          -> print_mem mem; substitute           ~stack:((ld addr mem)::tl) state

    | Swap, l::r::tl                           -> substitute ~stack:(r::l::tl) state

    (* Illegal instruction-stack pair *)
    | _, _                                     -> None


let rec run prg ((pc, mem, stack) as state) : stack option =
    if pc > maxpc then None else
    match List.nth_opt prg @@ Int32.to_int pc with
    | Some Done  -> Some stack
    | Some instr -> step state instr >>= run prg
    | None       -> print_endline
                 @@ "Error: Non instruction at" ^ Int32.to_string pc
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
(* let addTwoNumbers = [Read; Read; Add; Print; Done] *)
let addTwoNumbers = [Add; Print; Done]
let pushAdd = [Push 5l; Push 29l; Add; Done]
let storeLoadAddPrint = [ Push 4l
                        ; Push 2l
                        ; Push 3l
                        ; Push 1l
                        ; Print
                        ; Store
                        ; Store
                        ; Print
                        ; Push 1l
                        ; Load
                        ; Print
                        ; Push 2l
                        ; Load
                        ; Print
                        ; Add
                        ; Done]

(*
*)
let () = print_res "addTwoNumbers" addTwoNumbers (0l, Store.empty, [3l;5l])
let () = print_res "pushAdd" pushAdd init_state
let () = print_res "storeLoadAddPrint" storeLoadAddPrint init_state

