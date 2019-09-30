# require "batteries"
open Batteries.Option
open Batteries.Result
open String
open List

module Store = Map.Make(String)

let (>>=) = bind
let (@@) f g = f (g)
let (<>) str1 str2 = String.concat "" [str1; str2]

let bool_to_int = if true then 1 else 0


type instr =
    | Add
    | And
    | Or
    | Not
    | Lt
    | Eq
    | Push of int
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

type stack = int list
type pc = int
let  maxpc = 16

type mem = string Store.t

(* Program state: (Instruction pointer, Store, Stack) *)
type state = pc * mem * stack

let step ((pc, mem, stack) as state) instr : state option =
    match instr, stack with

    | Add, l::r::tl -> Some (pc + 1, mem, l + r::tl)
    | Add, _        -> None

    | Dup, hd::tl   -> Some (pc + 1, mem, hd::hd::tl)
    | Dup, _        -> None

    | Swap, l::r::tl -> Some (pc + 1, mem, r::l::tl)
    | Swap, _        -> None

    | Eq, l::r::tl when l=r  -> Some (pc + 1, mem, 1::tl)
    | Eq, l::r::tl           -> Some (pc + 1, mem, 0::tl)
    | Eq, _                  -> None

    | Not, hd::tl when hd=0  -> Some (pc + 1, mem, 1::tl)
    | Not, hd::tl            -> Some (pc + 1, mem, 0::tl)
    | Not, _                 -> None

    | Over, a::b::c::tl  -> Some (pc+1, mem, a::b::c::a::tl)
    | Over, _         -> None

    | Pop, a::tl       -> Some (pc+1, mem, tl)
    | Pop, _           -> None

    | Print, stack  -> print_endline
            @@ String.concat ""
            @@ List.map string_of_int stack;
            Some (pc+1, mem, stack)

    | Read, _       -> None

    | _, _          -> None


let rec run prg ((pc, mem, stack) as state) : stack option =
    if pc > maxpc then None else
    match List.nth_opt prg pc with
    | Some Done   -> Some stack
    | Some instr  -> step (pc + 1, mem, stack) instr >>= run prg
    (*| None      -> Error @@ "Error: Non instruction at" <> pc *)
    | None        -> print_endline
                    ("Error: Non instruction at" <> string_of_int pc );
                    None

(* example program *)
let initState : state = (0, Store.empty, [2; 2])
(* let addTwoNumbers = [Read; Read; Add; Print; Done] *)
let addTwoNumbers = [ Add; Print; Done]

let () = match run addTwoNumbers initState with
        (* | Some stack -> print_endline (String.concat "" (List.map * string_of_int stack)) *)
        | Some stack -> print_endline 
                     @@ String.concat ""
                     @@ List.map string_of_int stack
        | None       -> print_endline "Error occured"
