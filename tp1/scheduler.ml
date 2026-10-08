open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp eq = 
  let list_single_arg a =
      match a with
      | Avar (i) -> [i]
      | Aconst (_) -> []
  in

  let search_exp (exp : Netlist_ast.exp)  =
    match exp with
    | Earg (a) -> list_single_arg a
    | Ereg (i) -> []
    | Enot (a) -> list_single_arg a
    | Ebinop (b, a1, a2) -> let l = [list_single_arg a1 ; list_single_arg a2] in List.concat l 
    | Emux (a1, a2,  a3) -> let l = [list_single_arg a1 ; list_single_arg a2 ; list_single_arg a3] in List.concat l 
    | Erom (_, _, a) -> list_single_arg a
    | Eram (_, _, a1, a2, a3, a4) -> let l = [list_single_arg a1 ; list_single_arg a2 ; list_single_arg a3; list_single_arg a4] in List.concat l
    | Econcat (a1, a2) -> let l = [list_single_arg a1 ; list_single_arg a2] in List.concat l
    | Eslice (_, _, a) -> list_single_arg a
    | Eselect (_, a) -> list_single_arg a
    in
  
  let (id, exp) = eq in id :: search_exp exp

let schedule p = failwith "Scheduler.schedule: Non implementé"

