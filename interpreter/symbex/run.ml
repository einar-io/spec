open Types
open Programs
open Symbolic
let run = Symbolic.print_sym_res

(*
let () = print_sym_res "emptyPrg" emptyPrg init_symstate
let () = print_sym_res "donePrg" donePrg init_symstate

let twoNumbersStack = (0l, 0l, Store.empty, [SCon 3l; SCon 5l], [])
let () = print_sym_res "addTwoNumbers" addTwoNumbers twoNumbersStack

let () = print_sym_res "pushAdd" pushAdd init_symstate
let () = print_sym_res "storeLoadAddPrint" storeLoadAddPrint init_symstate
*)

(*infinite loop*)
(* let () = print_sym_res "prgLoop" prgLoop init_symstate *)


let () = run "prunablePrg" prunablePrg init_symstate
