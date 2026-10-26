open Netlist_ast

type t = {
  word_size : int;
  addr_size : int;
  data : value array
}

let empty (w_size : int) (ad_size : int) = 
  if w_size > 0 then {data = Array.make (1 lsl ad_size) (VBitArray(Array.make w_size false));
                                            word_size = w_size;
                                            addr_size = ad_size}

  else {data = Array.make 0 (VBitArray(Array.make 0 false)); word_size = 0; addr_size = 0}

let random (w_size : int) (ad_size : int) = 
  if w_size > 0 then {data = Array.make (1 lsl ad_size) (VBitArray(Array.make w_size true));
                                            word_size = w_size;
                                            addr_size = ad_size}
                                            
  else {data = Array.make 0 (VBitArray(Array.make 0 false)); word_size = 0; addr_size = 0}

let none () = {data = Array.make 0 (VBitArray(Array.make 0 false)); word_size = 0; addr_size = 0}

let is_none rom = (rom.word_size = 0) && (rom.addr_size = 0)

let get_word rom read_addr = rom.data.(read_addr)

let scan_program p = 
  let eq = List.find (fun (_, exp) -> match exp with
                                        | Erom (_, _, _) -> true
                                        | _ -> false) p.p_eqs in
  let (_, exp) = eq in
  match exp with 
  | Erom(ad, w, _) -> (ad, w)
  | _ -> (-1, -1)