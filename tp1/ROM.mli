open Netlist_ast

type t
  
val empty : int -> int-> t
val random : int -> int -> t 
val none : unit -> t
val is_none : t -> bool
val get_word : t -> int -> value
val scan_program : program -> (int * int)

