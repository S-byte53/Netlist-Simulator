open Netlist_ast

type t
  
val init_zero : int -> int-> t (*initialise une ROM avec les dimensions demandées, initialisée avec des 0 partout*)
val none : unit -> t (*initialise une ROM vide -- utilisée lorsque la netlist n'utilise pas de ROM*)
val is_none : t -> bool (*vérifie si la ROM est vide*)
val get_word : t -> int -> value (*lit un mot dans une case de la RAM*)
val set_write_args : t -> arg -> arg -> arg -> unit (*identifie les arguments de l'écriture: WE, WD, WA et les positionne dans la RAM*)
val write_if_needed : t -> value Env.t -> unit (*trouve les args de l'écriture dans l'environnement courrant et effectue l'écriture si nécessaire*)
val scan_program : program -> (int * int) (*renvoie les dimensions de la RAM nécessaire pour la netlist*)

(*Précisions:

write_word doit recevoir des données correctes --> une valeur de la bonne taille,
une adresse valide (la fonction ne fait pas de vérification interne de cela)

get_word se comporte pareil

*)