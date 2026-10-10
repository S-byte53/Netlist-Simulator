open Netlist_ast
open Graph
open Bool_string_tools

module Tools = Bool_string_tools

exception IncorrectLength
exception IncorrectChar

let print_only = ref false
let number_steps = ref (-1)

(*retourne le type de la var x*)
let find_type (x : ident) (curr_env : ty Env.t) =  Env.find x curr_env

let find_length (x : ident) (curr_env : ty Env.t) = 
  let t = find_type x curr_env in
  match t with
  | TBit -> 1
  | TBitArray (n) -> n

let verify_correct_char c = 
  if c != '0' && c != '1' then raise IncorrectChar

let read_n_digits n =  
  let () = print_endline (String.cat "Introduisez " (String.cat (Int.to_string n) " bits:")) in

  let s = read_line () in
  (*string bien lu*)

  if (String.length s) != n then 
    raise IncorrectLength
  else
    (*on a bien lu n valeurs, mais il faut vérifier que les 
    caractères lus sont bien '0' ou '1'*)
    String.iter verify_correct_char s ; 
    Tools.value_of_const s

let lec_et_ajout_input x ty_val curr_env =
  (*en fonction du type de la variable x, on va lire 1 ou plusieurs chiffres*)
  match ty_val with
  | TBit -> Env.add x (read_n_digits 1) curr_env 
  | TBitArray (length) -> Env.add x (read_n_digits length) curr_env 


let get_value_arg a (nouv_env : value Env.t) = 
  match a with
  | Avar(x) -> Env.find x nouv_env (*on cherche la valeur de x et on la renvoie*)   
  | Aconst(v) -> v

let eval_exp (exp : exp) (prev_env : value Env.t) (nouv_env : value Env.t) =
  match exp with
  | Earg (a) -> get_value_arg a nouv_env  
  | Ereg (x) -> Env.find x prev_env 
  | Enot (a) -> 
      let val_a = get_value_arg a nouv_env in
      begin match val_a with
      | VBit (b) -> VBit (not b)
      | VBitArray (l) -> VBitArray ( Array.map (fun x -> not x) l )
      end
  | Ebinop (b, a1, a2) -> 
     begin match b with
     | Or -> ()
     | Xor -> ()
     | And -> ()
     | Nand -> ()
     end
  | Emux (a1, a2,  a3) -> ()
  | Erom (_, _, a) -> ()
  | Eram (_, _, a1, a2, a3, a4) -> ()
  | Econcat (a1, a2) -> ()
  | Eslice (_, _, a) -> ()
  | Eselect (_, a) -> ()


let simulator program number_steps = 

  (*parcours variables de program et définition valeur par défaut*)
  (*pour chaque variable, on cherche son nb de bits et on définit sa valeur par défaut
  comme un string de cette longueur*)
  let initial_env = 
    Env.fold (fun x _ curr_env -> 
              Env.add x 
                      (String.init (find_length x program.p_vars) (fun i -> '0')) 
                      curr_env) 
              program.p_vars 
              Env.empty 
  in
  
  (*fonction qui va simuler le programme au pas i*)
  let rec simulation i curr_env=
    if i = 0 then 
      ()
    else 
      (*lecture des inputs dans un nouvel environement*)
      let nouv_env = 
        List.fold_left (fun n_env x -> lec_et_ajout_input x (find_type x program.p_vars) n_env) 
                       Env.empty  
                       program.p_outputs
      in
      
      (*parcours des equations, calcul des variables et actualisation environement*)
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
