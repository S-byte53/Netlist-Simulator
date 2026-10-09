open Netlist_ast
open Graph

let print_only = ref false
let number_steps = ref (-1)

let lec_et_ajout_input x ty_val curr_env =
  (*en fonction du type de la variable x, on va lire 1 ou plusieurs chiffres*)
  ()
let simulator program number_steps = 

  (*parcours variables de program et définition valeur par défaut*)
  let initial_env = Env.fold (fun x _ curr_env -> Env.add x 0 curr_env) program.p_vars Env.empty in

  (*fonction qui va simuler le programme au pas i*)
  let rec simulation i curr_env=
    if i = 0 then 
      ()
    else 
      (*on effectuera un Env.fold sur program.p_vars*)
      ()
      
  in

  simulation number_steps initial_env


let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
