open Netlist_ast
open Bool_string_tools

module Tools = Bool_string_tools

exception IncorrectFormat of string
exception ListTooShort

type t = {
  word_size : int;
  addr_size : int;
  data : value array
}

let take l i = 
  let rec take_rec l n = 
    if n = 0 then []
    else 
      begin match l with
      | x :: l' -> x :: take_rec l' (n-1)
      | _ -> raise ListTooShort
    end
  in
  take_rec l i


let verify_correct_char c = 
  if c != '0' && c != '1' then raise (IncorrectFormat "Le format des mots n'est pas respecte")

let verify_correct_line s w_size=
  if String.length s != w_size then raise (IncorrectFormat "La taille des mots n'est pas respectee");
  String.iter (fun c -> verify_correct_char c) s;
  Tools.value_of_const s

(*lit tous les contenus d'un fichier et les mets dans un string*)
let read_file file = 
  In_channel.with_open_bin file In_channel.input_all

let parse file expected_as expected_ws = 
  let s = read_file file in
  let l = String.split_on_char '\n' s in
  match l with
  | hd :: words -> 
      let args_rom = String.split_on_char ' ' hd in
      begin match args_rom with
      | addr_size :: word_size :: [] -> 
          let ad_size = Stdlib.int_of_string addr_size in
          let w_size = Stdlib.int_of_string word_size in
          if ad_size != expected_as || w_size != expected_ws then raise (IncorrectFormat "Les dimensions de la ROM ne sont pas conformes au circuit");
          (*List.iter (fun x -> Format.fprintf Format.std_formatter "%s  %d @." x (String.length x)) words;*)
          if List.length words < (1 lsl ad_size) then raise (IncorrectFormat "La taille de la ROM n'est pas respectee");

          let actual_words = take words (1 lsl ad_size) in
          (ad_size, w_size, List.map (fun line -> verify_correct_line line w_size) actual_words)
      | _ -> raise (IncorrectFormat "1ere ligne du fichier ne contient pas que 2 elements")
      end
  | _ -> failwith "Error"
  
  
let read_rom_from_file file ad_size word_size = 
  let (addr_size, word_size, data) = parse file ad_size word_size in
  {addr_size = addr_size; word_size = word_size; data = Array.of_list data}

let init_zero (ad_size : int) (w_size : int) = 
  if w_size > 0 then {data = Array.make (1 lsl ad_size) (VBitArray(Array.make w_size false));
                                            word_size = w_size;
                                            addr_size = ad_size}

  else {data = Array.make 0 (VBitArray(Array.make 0 false)); word_size = 0; addr_size = 0}

let init_one (ad_size : int) (w_size : int) = 
  if w_size > 0 then {data = Array.make (1 lsl ad_size) (VBitArray(Array.make w_size true));
                                            word_size = w_size;
                                            addr_size = ad_size}
                                            
  else {data = Array.make 0 (VBitArray(Array.make 0 false)); word_size = 0; addr_size = 0}

let none () = {data = Array.make 0 (VBitArray(Array.make 0 false)); word_size = 0; addr_size = 0}

let is_none rom = (rom.word_size = 0) && (rom.addr_size = 0)

let get_word rom read_addr = rom.data.(read_addr)

let scan_program p = 
  begin try
    let eq = List.find (fun (_, exp) -> match exp with
                                          | Erom (_, _, _) -> true
                                          | _ -> false) p.p_eqs in
    let (_, exp) = eq in
    match exp with 
    | Erom(ad, w, _) -> (ad, w)
    | _ -> (-1, -1)
  with Not_found -> (-1, -1)
end