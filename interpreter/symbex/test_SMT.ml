open Types
open SMT
open Utils


let () = (SOr ((SAnd ((SAny 3l), (SAny 1l))), (SAny 3l)))
    |> gather_free
    |> SMT.S.iter (fun x -> x
                            |> render_sym
                            |> print_endline)

     (* |> SMT.S.iter (fun x -> print_endline (render_sym x)) *)
