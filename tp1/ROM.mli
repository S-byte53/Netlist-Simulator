open Netlist_ast

type t
  
val init_zero : int -> int-> t
val init_one : int -> int -> t 
val read_rom_from_file : string -> int -> int -> t
val none : unit -> t
val is_none : t -> bool
val get_word : t -> int -> value
val scan_program : program -> (int * int)


