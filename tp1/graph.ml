exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n :: g.g_nodes

let node_of_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to   <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found -> Format.eprintf "Tried to add an edge between non-existing nodes"; raise Not_found

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g =
  let () = clear_marks g in
  let rec dfs g n = 
    if n.n_mark = InProgress then true
    else if n.n_mark = Visited then false
    else 
      let () = n.n_mark <- InProgress in
      let res = List.exists (fun n -> dfs g n) n.n_link_to in 
      let () = n.n_mark <- Visited in res in
  List.exists (fun n -> if n.n_mark = NotVisited then (dfs g n) else false) g.g_nodes 

let topological g = 
  if has_cycle g = true then raise Cycle else 

  let () = clear_marks g in
  let rec visit (n : 'a node) (tri_topo : 'a node list) =
    if n.n_mark = Visited then tri_topo
    else
      let () = n.n_mark <- InProgress in
      let l = List.fold_left (fun tri_topo n -> visit n tri_topo) tri_topo n.n_link_to in
      let () = n.n_mark <- Visited in  
      n :: l
    in let t = List.fold_left (fun tri_topo n -> visit n tri_topo) [] g.g_nodes 
  in List.map (fun x -> x.n_label) t
