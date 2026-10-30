open Netlist_ast

exception IncorrectLengthWE
exception IncorrectWriteAddress
exception IncorrectWriteDataLength

type t = {
  word_size : int;
  addr_size : int;
  data : value array;
  mutable write_enable : arg;
  mutable write_addr : arg;
  mutable write_data : arg
}

let get_value_arg a (nouv_env : value Env.t) = 
  begin match a with
  | Avar(x) -> Env.find x nouv_env (*on cherche la valeur de x et on la renvoie*)   
  | Aconst(v) -> v
  end

let to_int (v : value) = 
  begin match v with
  | VBit(false) -> 0
  | VBit(true) -> 1
  | VBitArray (a) -> Array.fold_right (fun b sum -> if b = false then sum * 2 else sum * 2 + 1)
                                      a 0 
  end


let get_length_value v = 
  begin match v with
  | VBit _ -> 1
  | VBitArray l -> Array.length l
  end

let none () = {data = Array.make 0 (VBitArray(Array.make 0 false)); word_size = 0; addr_size = 0; write_enable = Aconst (VBit false); write_addr = Aconst (VBit false); write_data = Aconst (VBit false)}

let is_none ram = ram.word_size = 0

let init_zero (ad_size : int) (w_size : int) = 
  if w_size > 0 then {data = Array.make (1 lsl ad_size) (VBitArray(Array.make w_size false));
                                            word_size = w_size;
                                            addr_size = ad_size;
                                            write_enable = Aconst (VBit false);
                                            write_addr = Aconst (VBit false);
                                            write_data = Aconst (VBit false)}

  else none ()

let get_word ram read_addr = ram.data.(read_addr)

let set_write_args ram write_enable write_addr write_data =
  ram.write_enable <- write_enable;
  ram.write_addr <- write_addr;
  ram.write_data <- write_data

let write_if_needed ram curr_env =
  let we_v = get_value_arg ram.write_enable curr_env in
  
  match we_v with
  | VBit true ->
      let wa_v = get_value_arg ram.write_addr curr_env in
      let wd_v = get_value_arg ram.write_data curr_env in
      let l = get_length_value wd_v in
      if l != ram.word_size then raise IncorrectWriteDataLength;
      let wa_int = to_int wa_v in
      if wa_int < (1 lsl ram.addr_size) then
        ram.data.(wa_int) <- wd_v
      else raise IncorrectWriteAddress
  | VBit false -> ()
  | _ -> raise IncorrectLengthWE

let scan_program p = 
  try
    let eq = List.find (fun (_, exp) -> match exp with
                                          | Eram _ -> true
                                          | _ -> false) p.p_eqs in

    let (_, exp) = eq in
    begin match exp with 
    | Eram(ad, w, _, _, _, _) -> (ad, w)
    | _ -> (-1, -1) 
    end
  with Not_found -> (-1, -1)