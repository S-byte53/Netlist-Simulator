open Netlist_ast

type t = {
  word_size : int;
  addr_size : int;
  data : value array
}

let none () = {data = Array.make 0 (VBitArray(Array.make 0 _)); word_size = 0; addr_size = 0}

let is_none ram = ram.word_size = 0

let init_zero (ad_size : int) (w_size : int) = 
  if w_size > 0 then {data = Array.make (1 lsl ad_size) (VBitArray(Array.make w_size false));
                                            word_size = w_size;
                                            addr_size = ad_size}

  else none ()

let get_word ram read_addr = ram.data.(read_addr)

let write_word ram write_addr v = ram.data.(write_addr) <- v

let scan_program p = 
  try
    let eq = List.find (fun (_, exp) -> match exp with
                                          | Eram _ -> true
                                          | _ -> false) p.p_eqs in

    let (_, exp) = eq in
    begin match exp with 
    | Erom(ad, w, _, _, _, _) -> (ad, w)
    | _ -> (-1, -1) 
    end
  with Not_found -> (-1, -1)