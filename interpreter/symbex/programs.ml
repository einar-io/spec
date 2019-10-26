open Types

(* example program texts *)
let emptyPrg = []


let donePrg = [Done]


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


let simpleSplit = []
