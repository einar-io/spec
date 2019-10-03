# require "batteries"
open Batteries.Option
open String
open List
open Int32

(* https://stackoverflow.com/a/53131220 *)
let (+) a b = Int32.add a b

(* module Store = Map.Make(BatInt) *)
(* module Store = Map.Make(struct type t = int let compare = compare end) *)
module Store = Map.Make(Int32)

type word = int32
type pc = word
type stack = word list
type mem = word Store.t


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

let (>>=) = bind (* Batteries.Option.bind *)

(* maybe use `update` *)
let wr addr value mem = Store.add addr value mem

let ld addr mem =
    match Store.find_opt addr mem with
    | Some value -> value
    | None       -> 0l

let maxpc = 16l


(*
let update ((oldpc, oldmem, oldstack) as oldstate)
?(pc=oldpc+1) ?(mem=oldmem) ?(stack=oldstack) () : state option =
    Some (pc, mem, stack)
*)


(*
let rec update2
(*?(pc=oldpc+1) ?(mem=oldmem) ?(stack=oldstack)  *)
?pc ?mem ?stack
((oldpc, oldmem, oldstack) as oldstate) : state option =
    match pc, mem, stack with
    | None, newmem, newstack -> update2 ~pc:(oldpc+1) ~mem:newmem ~stack:newstack oldstate
    | newpc, None, newstack  -> update2 ~pc:newpc ~mem:oldmem ~stack:newstack     oldstate
    | newpc, newmem, None    -> update2 ~pc:newpc ~mem:newmem ~stack:oldstack     oldstate
    | newstate                      ->  Some newstate
*)

let substitute
?pc ?mem ?stack (oldpc, oldmem, oldstack) : state option =
    let nextpc = oldpc + 1l in
    match pc, mem, stack with
    | None       , None        , None          -> Some (nextpc , oldmem , oldstack)

    | Some newpc , None        , None          -> Some (newpc , oldmem , oldstack)
    | None       , Some newmem , None          -> Some (nextpc , newmem , oldstack)
    | None       , None        , Some newstack -> Some (nextpc , oldmem , newstack)

    | None       , Some newmem , Some newstack -> Some (nextpc , newmem , newstack)
    | Some newpc , None        , Some newstack -> Some (newpc , oldmem , newstack)
    | Some newpc , Some newmem , None          -> Some (newpc , newmem , oldstack)

    | Some newpc , Some newmem , Some newstack -> Some (newpc , newmem , newstack)



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

    | Print, stack                             -> print_endline
            @@ String.concat ""
            @@ List.map Int32.to_string stack;
            substitute state

    | Read, _                                  -> None

    | Store, addr::value::tl                   -> substitute ~mem:(wr addr value mem) ~stack:(tl) state
    | Load,  addr::tl                          -> substitute           ~stack:((ld addr mem)::tl) state

    | Swap, l::r::tl                           -> substitute ~stack:(r::l::tl) state

    (* Illegal instruction-stack pair *)
    | _, _                                     -> None


let rec run prg ((pc, mem, stack) as state) : stack option =
    if pc > maxpc then None else
    match List.nth_opt prg @@ Int32.to_int pc with
    | Some Done   -> Some stack
    | Some instr  -> step state instr >>= run prg
    | None      -> print_endline
                @@ "Error: Non instruction at" ^ Int32.to_string pc
                ; None

(* example program *)
let initState : state = (0l, Store.empty, [2l; 2l])
(* let addTwoNumbers = [Read; Read; Add; Print; Done] *)
let addTwoNumbers = [ Add; Print; Done]

let () = match run addTwoNumbers initState with
        | Some stack -> print_endline
                     @@ List.fold_left (^) ""
                     @@ List.map Int32.to_string stack
        | None       -> print_endline "Error occured"
