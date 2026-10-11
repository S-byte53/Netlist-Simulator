open Netlist_ast
open Graph
open Bool_string_tools
open Netlist_printer

module Tools = Bool_string_tools

exception IncorrectLength
exception IncorrectChar
exception IncompatibleOperands
exception NotImplemented of string 

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
  (*affichage initial*)
  let () = print_endline (String.cat "Lecture de la variable " (String.cat x " :")) in

  match ty_val with
  | TBit -> Env.add x (read_n_digits 1) curr_env 
  | TBitArray (length) -> Env.add x (read_n_digits length) curr_env 


let get_value_arg a (nouv_env : value Env.t) = 
  match a with
  | Avar(x) -> Env.find x nouv_env (*on cherche la valeur de x et on la renvoie*)   
  | Aconst(v) -> v


(*fonction auxiliaire pour eval_exp*)
let xor a b = (a || b) && not (a && b)

(*fonction qui vérifie que les arguments du multiplexeur sont cohérents au niveau des longueurs*)
let verify_correct_mux (v1 : value) (v2 : value) (v3 : value) =
   match (v1, v2, v3) with
   | (VBitArray (l1), VBitArray (l2), VBit (b)) -> 
      if Array.length l1 != Array.length l2 then false else true
   | (VBit(b1), VBit(b2), VBit(l3)) -> true
   | (_,_,_) -> false

let verify_correct_slice i1 i2 v = 
  match v with
  | VBit (_) -> false
  | VBitArray (l) -> (0 <= i1) && (i1 <= i2) && (i2 < Array.length l)

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
    let v1 = get_value_arg a1 nouv_env in
    let v2 = get_value_arg a2 nouv_env in
     begin match b with
     | Or -> 
        begin match (v1, v2) with
        | (VBit(b1), VBit(b2)) -> VBit (xor b1 b2)
        | (VBitArray(l1), VBitArray(l2)) ->
            if Array.length l1 != Array.length l2 then
              raise IncompatibleOperands
            else
              VBitArray (Array.map2 (fun x y -> xor x y) l1 l2)
        | (_, _) -> raise IncompatibleOperands
        end
     | Xor -> 
        begin match (v1, v2) with
        | (VBit(b1), VBit(b2)) -> VBit (b1 || b2)
        | (VBitArray(l1), VBitArray(l2)) ->
            if Array.length l1 != Array.length l2 then
              raise IncompatibleOperands
            else
              VBitArray (Array.map2 (fun x y -> x || y) l1 l2)
        | (_, _) -> raise IncompatibleOperands
        end
     | And -> 
        begin match (v1, v2) with
        | (VBit(b1), VBit(b2)) -> VBit (b1 && b2)
        | (VBitArray(l1), VBitArray(l2)) ->
            if Array.length l1 != Array.length l2 then
              raise IncompatibleOperands
            else
              VBitArray (Array.map2 (fun x y -> x && y) l1 l2)
        | (_, _) -> raise IncompatibleOperands
        end
     | Nand -> 
        begin match (v1, v2) with
        | (VBit(b1), VBit(b2)) -> VBit (not (b1 && b2))
        | (VBitArray(l1), VBitArray(l2)) ->
            if Array.length l1 != Array.length l2 then
              raise IncompatibleOperands
            else
              VBitArray (Array.map2 (fun x y -> not (x && y)) l1 l2)
        | (_, _) -> raise IncompatibleOperands
        end
     end
  | Emux (a1, a2,  a3) -> 
    let v1 = get_value_arg a1 nouv_env in
    let v2 = get_value_arg a2 nouv_env in
    let v3 = get_value_arg a3 nouv_env in
     if not (verify_correct_mux v1 v2 v3) then 
      raise IncompatibleOperands
    else
      begin match v3 with
      | VBit(true) -> v1
      | VBit(false) -> v2
      | _ -> raise IncompatibleOperands
      end
    
  | Erom (_, _, a) -> raise (NotImplemented "ROM non implémenté")
  | Eram (_, _, a1, a2, a3, a4) -> raise (NotImplemented "RAM non implémenté")


  | Econcat (a1, a2) -> 
      let v1 = get_value_arg a1 nouv_env in
      let v2 = get_value_arg a2 nouv_env in
      begin match (v1, v2) with
      | (VBit (b1), VBit (b2)) -> VBitArray ([|b1; b2|])
      | (VBit (b1), VBitArray (l2)) -> VBitArray (Array.append [|b1|] l2)
      | (VBitArray (l1), VBit(b2)) -> VBitArray (Array.append l1 [|b2|])
      | (VBitArray (l1), VBitArray (l2)) -> VBitArray (Array.append l1 l2)
      end

  (*la façon dont on vérifie la compatibilité des arguments ne permet pas de faire 
  des slice depuis une valeur VBit(_)*)
  | Eslice (i1, i2, a) -> 
      let v = get_value_arg a nouv_env in
      if not (verify_correct_slice i1 i2 v) then
        raise IncompatibleOperands
      else
        begin match v with
        | VBitArray (l) -> 
            if i2 = i1 then 
              VBit(l.(i1))  (*la slice qu'on veut faire donne 1 seul bit en sortie*)
            else
              VBitArray (Array.sub l i1 (i2 - i1 + 1)) (*slice qui produit au moins 2 bits*)
        | _ -> raise IncompatibleOperands
      end

  (*la façon dont on vérifie la compatibilité des arguments ne permet pas de selectionner 
  1 seul bit depuis une valeur VBit(_)*)
  | Eselect (i, a) -> 
      let v = get_value_arg a nouv_env in
      if not (verify_correct_slice i i v) then
        raise IncompatibleOperands
      else
        match v with
        | VBit (_) -> raise IncompatibleOperands
        | VBitArray (l) -> VBit (l.(i))

(*traite une equation: calcule la valeur de l'expression,
associe cette valeur à x dans nouv_env et renvoie nouv_env actualisé*)
let eval_equation (eq : equation) (prec_env : value Env.t) (nouv_env : value Env.t) =
  let (x, exp) = eq in
  let v = eval_exp exp prec_env nouv_env in
  Env.add x v nouv_env

let print_outputs (program : Netlist_ast.program) (nouv_env : value Env.t) = 
  List.iter (fun x -> print_endline (String.cat x " = " );
  Netlist_printer.print_value Format.std_formatter (Env.find x nouv_env)) program.p_outputs

let initial_value x program = 
  let t = find_type x program.p_vars in
  match t with
  | TBit -> VBit(false)
  | TBitArray(n) -> VBitArray (Array.make n false)

let simulator program number_steps = 

  (*parcours variables de program et définition valeur par défaut*)
  (*pour chaque variable, on cherche son nb de bits et on définit sa valeur par défaut
  comme un string de cette longueur*)
  let initial_env = 
    Env.fold (fun x _ curr_env -> 
              Env.add x 
                      (initial_value x program) 
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
      let nouv_env = List.fold_left (fun n_env eq -> eval_equation eq curr_env n_env) nouv_env program.p_eqs in

      (*affichage des outputs*)
      let () = print_outputs program nouv_env in

      (*nouv_env contient les valeurs des variables après ce pas de la simulation*)
      simulation (i - 1) nouv_env 

      
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
