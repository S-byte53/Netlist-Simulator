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


let traitement_equation (eq : equation) (g : 'a graph) = 
  let (id, _) = eq in
  
  (*on lit les variables de l'equation --> par exemple: x = AND a b
    -> si besoin, on les ajoute dans le graphe de la netlist
    -> une fois qu'elles sont dans le netlist, on ajoute les aretes du graphe --> par ex: a -> x et b -> x
  *)
  let rec ajout_var (list_v : ident list) (i : int)= 
    match list_v with
    | [] -> ()
    | x :: list_v_aux -> 
        (*on cherche la variable x dans le graphe g*)
        let () =  begin try
                    let _ = node_of_label g x in ()
                    (*noeud x déjà trouvé --> on a pas besoin de l'ajouter*)
                  with 
                    Not_found -> let () = add_node g x in ()
                    (*noeud x non trouvé, on l'ajoute dans le graphe*)
                  end
        in
        (*variable x est actuellement dans le graphe*)
        if i > 0 then add_edge g x id ; ajout_var list_v_aux (i + 1) in

  ajout_var (read_exp eq) 0
  
(*vérifie si le terme de gauche de l'equation eq est la variable x*)
let match_eq (eq : equation) (x : ident) = 
  let (id, _) = eq in id = x

let find_eq (l_eq : equation list) (x : ident) = 
  List.find (fun eq -> match_eq eq x) l_eq

let schedule p =
  (*création du graphe de la netlist*)
  let g = mk_graph () in

  (*fonction pour parcours equations, ajout des variables et des aretes entre les variables*)
  let rec parcours_equations (l : equation list) =
    match l with
    | [] -> ()
    | eq :: l_aux ->  
       traitement_equation eq g ; 
       parcours_equations l_aux in
  
  (*création effective du graphe de la netlist*)
  let () = parcours_equations p.p_eqs in

  (*tri topologique du graphe de la netlist*)
  try 
    let list_noeuds = topological g in

    (*fonction pour le parcours du tri topologique et 
    la construction de la nouvelle liste d'equations*)
    let rec construct_liste_eq (topo : ident list) = 
      match topo with
      | [] -> [] (*parcours topo fini, aucune equation à ajouter*)
      | n :: topo_aux -> (*on a un ident dans le parcours*)
          (*on vérifie si le noeud correspond à une equation qu'il faut placer*)
          begin try 
            let eq = find_eq p.p_eqs n in eq :: construct_liste_eq topo_aux
          with
            Not_found -> construct_liste_eq topo_aux
          end in
    
    {p with p_eqs = construct_liste_eq list_noeuds}

  with Cycle -> raise Combinational_cycle
