open Ast

(** [RepeatingLabel] is raised when the declaration of a label is repeated in the program *)
exception RepeatingLabel of string

(** [MissingStrAddt] is raised when hashtable lookup for an AInst StrAddr fails *)
exception MissingStrAddr of string


(*
        Handling C Instructions

        A C Instruction looks like this:
        111acccc ccdddjjj
        a       selects between A or M registers
        cccccc  selects the operation to perform (related to Ast.comp_ident)
        ddd     selects the destination register (related to Ast.dest_ident)
        jjj     selects the jump type to perform (related to Ast.jump_ident)
*)

(** [res_jump_ident jmp] maps [jmp] of type Ast.jump_ident to its value as jjj *)
let res_jump_ident (jmp : jump_ident) : int =
        match jmp with
        | JNL -> 000
        | JGT -> 001
        | JEQ -> 002
        | JGE -> 003
        | JLT -> 004
        | JNE -> 005
        | JLE -> 006
        | JMP -> 007


(** [res_dest_ident jmp] maps [dest] of type Ast.dest_ident to its value as ddd *)
let res_dest_ident (dest : dest_ident) : int = 
        match dest with
        | N   -> 000
        | M   -> 001
        | D   -> 002
        | MD  -> 003
        | A   -> 004
        | AM  -> 005
        | AD  -> 006
        | AMD -> 007


(** [res_comp_ident comp] maps [comp] of type Ast.comp_ident to its value as acccccc *)
let res_comp_ident (comp : comp_ident) : int = 
        match comp with
        | Comp_Zero     -> 042 (* 0101010 *)
        | Comp_One      -> 063 (* 0111111 *)
        | Comp_MinusOne -> 058 (* 0111010 *)
        | Comp_D        -> 012 (* 0001100 *)
        | Comp_A        -> 048 (* 0110000 *)
        | Comp_M        -> 112 (* 1110000 *)
        | Comp_NotD     -> 013 (* 0001101 *)
        | Comp_NotA     -> 049 (* 0110001 *)
        | Comp_NotM     -> 113 (* 1110001 *)
        | Comp_MinusD   -> 015 (* 0001111 *)
        | Comp_MinusA   -> 051 (* 0110011 *)
        | Comp_MinusM   -> 115 (* 1110011 *)
        | Comp_DPlusOne -> 031 (* 0011111 *)
        | Comp_APlusOne -> 055 (* 0110111 *)
        | Comp_MPlusOne -> 119 (* 1110111 *)
        | Comp_DMinusOne-> 014 (* 0001110 *)
        | Comp_AMinusOne-> 050 (* 0110010 *)
        | Comp_MMinusOne-> 114 (* 1110010 *)
        | Comp_DPlusA   -> 002 (* 0000010 *)
        | Comp_DPlusM   -> 066 (* 1000010 *)
        | Comp_DMinusA  -> 019 (* 0010011 *)
        | Comp_DMinusM  -> 083 (* 1010011 *)
        | Comp_AMinusD  -> 007 (* 0000111 *)
        | Comp_MMinusD  -> 071 (* 1000111 *)
        | Comp_DAndA    -> 000 (* 0000000 *)
        | Comp_DAndM    -> 064 (* 1000000 *)
        | Comp_DOrA     -> 021 (* 0010101 *)
        | Comp_DOrM     -> 085 (* 1010101 *)
        
        
(** [new_inst] returns a Bytes.t type which can hold 2 bytes *)
let new_inst : bytes = Bytes.create 2

(** [printHash hashtbl] prints the contents of [hashtbl] onto stdout *)
let printHash (hashtbl : (string, int) Hashtbl.t) : unit = Hashtbl.iter (fun x y -> Printf.printf "%s\t%d\n" x y) hashtbl

(** [int_to_inst x] converts [x]:int to Bytes.t *)
let int_to_inst (x : int) : bytes = 
        let n = Bytes.create 2 in
        Bytes.set_uint16_be n 0 x;
        n


(** [int_to_binary n] converts [n]:int into its binary representation *)
let rec int_to_binary_int (n : int) : int = 
        if (n < 0) then raise (Failure "can not convert negative numbers")
        else
                match n with
                | 0 -> 0
                | _ -> (n mod 2) + (10 * (int_to_binary_int (n/2)))


(** [int_to_binary_string n] converts [n]:int into its binary representation in string *)
let int_to_binary_string (n : int) : string = string_of_int (int_to_binary_int n)


(** [zfill s length] returns [s] with leading zeroes until it is [length] long
and returns [s] if [length] <= [String.length s] *)
let rec zfill (s : string) (length : int) : string = 
        let string_length = String.length s in
        if (string_length >= length) then s
        else
                "0" ^ (zfill s (length-1))
        

(** [int_to_binary_inst n] takes [n]:int and converts into a 16 bit binary string representation of it *)
let int_to_binary_inst (n : int) : string = 
        zfill (int_to_binary_string n) 16


(** [first i] returns the first element in the truple [i] *)
let first i = match i with (a, _, _) -> a

(** [second i] returns the second element in the truple [i] *)
let second i = match i with (_, b, _) -> b

(** [third i] returns the third element in the truple [i] *)
let third i = match i with (_, _, c) -> c


(** [c_inst_to_int c] converts [c]:Ast.c_inst into its binary encoding as an integer *)
let c_inst_to_int (c : c_inst) : int = 
        Int.logor (res_jump_ident (third c) + (64 * (res_comp_ident (second c))) + (8 * (res_dest_ident (first c)))) 0xe000 


(*
        Handling A Instructions 

        An A Instruction looks like this
        1bbbbbbbbbbbbbbbb
        The trailing 15 bits denote the address in binary
        If the address happens to be longer than 15 bits, the 
        tailing 15 bits are selected

        eg. 110000000001101100 (18 bits) --> 1000000001101100
            ^^^ discarded bits
*)

(* 
        Handling Labels
*)

(** [extract_label s] retrieves the string part of a label [s], otherwise returns [""] *)
let extract_label (s : statement) : string = 
        match s with
        | Label a -> a
        | AInst _ -> ""
        | CInst _ -> ""


(** [get_label_name sl] returns a list of labels in the given list of statements [sl] *)
let rec get_label_name (sl : statement list) : string list = 
        match sl with
        | [] -> []
        | (head :: tail) -> let head_label = (extract_label head) in 
        if (head_label <> "") then [head_label] @ (get_label_name tail)
        else get_label_name tail


(** [get_label_num ?line_offset sl] retrieves the line number associated with the labels in [sl]
assuming that the indexing of program begins at [line_offset], which defaults to 0 *)
let rec get_label_num ?(line_offset=0) (sl : statement list) : int list = 
        match sl with
        | [] -> []
        | head :: tail -> 
        begin
        match head with
        | Label _ -> [line_offset] @ (get_label_num ~line_offset:(line_offset) tail)
        | _ -> get_label_num ~line_offset:(1+line_offset) tail
        end


(** [edit_hash_label hashtbl lbl_name_num] takes a hashtbl [hashtbl] and a list of tuples (label_name, label_number)
and stores in [hashtbl], label_name and label_number
raises [RepeatingLabel] if a label is repeated *)

let rec edit_hash_label (hashtbl : (string, int) Hashtbl.t) (lbl_name_num : (string * int) list) : unit = 
        match lbl_name_num with
        | [] -> () (* can add hashtbl.clear here *)
        | head :: tail ->
                        let lab_name = (fst head) in 
                        let lab_num = (snd head) in 
                        if (Hashtbl.mem hashtbl lab_name) then raise (RepeatingLabel ("Label repeated: " ^ lab_name)) 
                        else 
                                begin 
                                        Hashtbl.add hashtbl lab_name lab_num;
                                        edit_hash_label hashtbl tail
                                end


(** [make_hash_label sl] returns a hashtbl of labels from the given statement list [sl] *)
let make_hash_label (sl : statement list) : (string, int) Hashtbl.t = 
        let lbl_name_num = List.combine (get_label_name sl) (get_label_num sl) in
        let label_tbl = Hashtbl.create 10 in
        let _ = edit_hash_label label_tbl lbl_name_num in
        label_tbl


(* 
        Handling Symbols
*)

(** [extract_str_addr s] retrieves the string from an AInst StrAddr [s], otherwise returns [""] *)
let extract_str_addr (s : statement) : string = 
        match s with
        | AInst StrAddr x -> x
        | AInst IntAddr _ -> ""
        | CInst _ -> ""
        | Label _ -> ""


(** [get_str_addr sl] returns a list of all StrAddr addresses in the statement list [sl] *)
let rec get_str_addr (sl : statement list) : string list = 
        match sl with
        | [] -> []
        | head :: tail -> 
                begin
                        match head with
                        | AInst StrAddr _ -> [extract_str_addr head] @ get_str_addr tail
                        | _ -> get_str_addr tail
                end


(** [add_predefined_registers ?start_reg ?end_reg hashtbl] adds registers R{start_reg}...R{end_reg} with 
values {start_reg}...{end_red} into the hashtbl
[start_reg] defaults to 0 and [end_reg] defaults to 15 *)
let rec add_predefined_registers ?(start_reg=0) ?(end_reg=15) (hashtbl : (string, int) Hashtbl.t) : unit =
        if (start_reg <= end_reg) then 
                begin
                        Hashtbl.add hashtbl ("R" ^ (string_of_int start_reg)) start_reg;
                        add_predefined_registers ~start_reg:(1+start_reg) hashtbl
                end
        else ()


(** [add_predefined_symbols hashtbl] adds some predefined miscellaneous symbols into the hashtbl *)
let add_predefined_symbols (hashtbl : (string, int) Hashtbl.t) : unit = 
        begin
                Hashtbl.add hashtbl "SCREEN" 16384;
                Hashtbl.add hashtbl "KBD" 24756;
                Hashtbl.add hashtbl "SP" 0;
                Hashtbl.add hashtbl "LCL" 1;
                Hashtbl.add hashtbl "ARG" 2;
                Hashtbl.add hashtbl "THIS" 3;
                Hashtbl.add hashtbl "THAT" 4;
        end


(** [edit_hash_symbol ?start_offset hashtbl lbl_table str_addr_list] adds each entry of [str_addr_list] into [hashtbl]
only if that entry is not present in [lbl_table]
values assigned to entries in hashtbl will begin with [start_offset], which defaults to 16 *)

let rec edit_hash_symbol ?(start_offset=16) (hashtbl : (string, int) Hashtbl.t) (lbl_table : (string, int) Hashtbl.t) (str_addr_lst : string list) : unit = 
        match str_addr_lst with
        | [] -> ()
        | head :: tail -> 
                if (Hashtbl.mem lbl_table head) then 
                        edit_hash_symbol ~start_offset:start_offset hashtbl lbl_table tail
                else if (Hashtbl.mem hashtbl head) then
                        edit_hash_symbol ~start_offset:start_offset hashtbl lbl_table tail
                else
                        (
                                Hashtbl.add hashtbl head start_offset;
                                edit_hash_symbol ~start_offset:(start_offset+1) hashtbl lbl_table tail
                        )

        
(** [make_hash_symbol sl] returns a hashtbl containing symbols and their assigned values from the program [sl] *)
let make_hash_symbol (sl : statement list) : (string, int) Hashtbl.t = 
        let str_addr_lst = get_str_addr sl in
        let symbol_tbl = Hashtbl.create 10 in
        let label_tbl = make_hash_label sl in
        let _ = add_predefined_registers symbol_tbl in
        let _ = add_predefined_symbols symbol_tbl in
        let _ = edit_hash_symbol symbol_tbl label_tbl str_addr_lst in
        symbol_tbl

(** [a_inst_to_int ~sym_tbl ~lbl_tbl a] converts Ast.a_inst [a] into an integer 
by looking it up in [sym_tbl] symbol table and [lbl_tbl] label table *)
let a_inst_to_int ~sym_tbl ~lbl_tbl (a : a_inst) : int = 
        match a with
        | IntAddr x ->  (Int.logand x 0x7fff)
        | StrAddr y ->  
                        if (Hashtbl.mem sym_tbl y)
                                then (Hashtbl.find sym_tbl y)
                        else if (Hashtbl.mem lbl_tbl y)
                                then (Hashtbl.find lbl_tbl y)
                        else raise (MissingStrAddr ("Missing String Address: " ^ y))


(** [resolve_statements_int sym_tbl lbl_tbl sl] resolves the program and returns a list of integers *)
let rec resolve_statements_int (sym_tbl : (string, int) Hashtbl.t) (lbl_tbl : (string, int) Hashtbl.t) (sl : statement list) : int list =
        match sl with
        | [] -> []
        | head :: tail ->
                begin
                        match head with
                        | AInst a -> (a_inst_to_int ~sym_tbl:sym_tbl ~lbl_tbl:lbl_tbl a) :: (resolve_statements_int sym_tbl lbl_tbl tail)
                        | CInst c -> (c_inst_to_int c) :: (resolve_statements_int sym_tbl lbl_tbl tail)
                        | Label _ -> resolve_statements_int sym_tbl lbl_tbl tail
                end


(** [resolve_statements_int sym_tbl lbl_tbl sl] resolves the program and returns a list of bytes *)
let resolve_statements_bytes (sym_tbl : (string, int) Hashtbl.t) (lbl_tbl : (string, int) Hashtbl.t) (sl : statement list) : bytes list =
        List.map int_to_inst (resolve_statements_int sym_tbl lbl_tbl sl)


(** [resolve_statements_int sym_tbl lbl_tbl sl] resolves the program and returns a list of strings *)
let resolve_statements_string (sym_tbl : (string, int) Hashtbl.t) (lbl_tbl : (string, int) Hashtbl.t) (sl : statement list) : string list = 
        List.map int_to_binary_inst (resolve_statements_int sym_tbl lbl_tbl sl)

