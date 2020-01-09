(* key : int32 -> (value : stack-elm-type) *)
module Store = Map.Make(Int32)

(* DS to hold z3 assertions *)
module SMTmap = Map.Make(Int32)

type word = int32
let wordsize = 32

type conip = word
(* key : int32 -> value : int32 *)
type conmem = word Store.t
type constack = word list
(* Program state: (Instruction pointer, Store, Stack) *)
type state = conip * conmem * constack

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
    | Unreachable
    | Local of word

let init_state : state = (0l, Store.empty, [])

type symword =
    | SAdd of symword * symword
    | SEq  of symword * symword
    | SNot of symword
    | SOr  of symword * symword
    | SCon of word
    | SAnd of symword * symword
    | SLt  of symword * symword
    | SAny of word

module Symword = struct
    type t = symword
    let compare = compare
end

module Symset = Set.Make(Symword)

type ip       = word
type next     = word
(* key : int32 -> value : symword *)
type symmem   = symword Store.t
type symstack = symword list
type pc       = symword list
type symstate = ip * next * symmem * symstack * pc
type 'a tree  = Empty | Node of 'a * 'a tree list

let init_symstate : symstate = (0l, 0l, Store.empty, [], [])
