exception UnexpectedRegister of string

(** [Jump.encode] takes [Ast.jump option] and returns [int list] corresponding to the [Ast.jump] constructor *)
module Jump : sig
        val encode : Ast.jump option -> int list
end = struct
        let encode_j (j : Ast.jump) : int list = 
                match j with
                | JGT -> [0; 0; 1]
                | JEQ -> [0; 1; 0]
                | JGE -> [0; 1; 1]
                | JLT -> [1; 0; 0]
                | JNE -> [1; 0; 1]
                | JLE -> [1; 1; 0]
                | JMP -> [1; 1; 1]

        let encode (jump : Ast.jump option) : int list = 
                match jump with
                | Some j -> encode_j j
                | None   -> [0; 0; 0]
end


(** [Dest.encode] takes [Ast.destination option] and returns [int list] corresponding to the [Ast.destination] selected *)
module Dest : sig
        val encode : Ast.destination option -> int list
end = struct
        let encode_r (r : Ast.register) : int list = 
                match r with
                | M -> [0; 0; 1]
                | D -> [0; 1; 0]
                | A -> [1; 0; 0]
        
        let encode_d (d : Ast.destination) : int list = 
                let vecM = if List.mem Ast.M d then encode_r Ast.M else [0; 0; 0] in
                let vecD = if List.mem Ast.D d then encode_r Ast.D else [0; 0; 0] in
                let vecA = if List.mem Ast.A d then encode_r Ast.A else [0; 0; 0] in
                Vector.add_vector (Vector.add_vector vecM vecD) vecA

        let encode (dest : Ast.destination option) : int list = 
                match dest with
                | Some d -> encode_d d
                | None   -> [0; 0; 0]     
end


(** [Const.encode] takes [Ast.const] and returns [int list] corresponding to the [Ast.const] constructor *)
module Const : sig
        val encode : Ast.const -> int list
end = struct
        let encode (c : Ast.const) : int list = 
                match c with
                | Zero      -> [0; 1; 0; 1; 0; 1; 0]
                | One       -> [0; 1; 1; 1; 1; 1; 1]
                | MinusOne  -> [0; 1; 1; 1; 0; 1; 0]
end


(** [Unary.encode] takes [Ast.unOp] and [Ast.register] and returns [int list] corresponding to the chosen unOp and register *)
module Unary : sig
        val encode : Ast.unOp -> Ast.register -> int list
end = struct
        let encode_r (r : Ast.register) : int list = 
                match r with
                | D -> [0; 0; 0; 1; 1]
                | A -> [0; 1; 1; 0; 0]
                | M -> [1; 1; 1; 0; 0]
        let encode_op (uop : Ast.unOp) : int list = 
                match uop with
                | Identity -> [0; 0]
                | BitNot   -> [0; 1]
                | Minus    -> [1; 1]
                | Succ     -> [1; 1]
                | Pred     -> [1; 0]
        
        let succ (r : Ast.register) : int list =
                match r with
                | D -> [0; 0; 1; 1; 1; 1; 1]
                | A -> [0; 1; 1; 0; 1; 1; 1]
                | M -> [1; 1; 1; 0; 1; 1; 1]
        
        let encode (uop : Ast.unOp) (r : Ast.register) : int list = 
                match uop with
                | Ast.Succ      -> succ r
                | _             -> encode_r r @ encode_op uop       
end


(** [Binary.encode] takes [Ast.biOp] and [Ast.register] and returns [int list] corresponding to the chosen biOp and register *)
module Binary : sig
        val encode : Ast.biOp -> Ast.register -> int list
end = struct
        let encode_r (r : Ast.register) : int list = 
                match r with
                | Ast.A -> [0]
                | Ast.M -> [1]
                | Ast.D -> raise (UnexpectedRegister "Can not apply binary operations on register D")
        let encode_op (bop : Ast.biOp) : int list = 
                match bop with
                | Add     -> [0; 0; 0; 0; 1; 0]
                | Sub     -> [0; 1; 0; 0; 1; 1]
                | SubFrom -> [0; 0; 0; 1; 1; 1]
                | BinAnd  -> [0; 0; 0; 0; 0; 0]
                | BinOr   -> [0; 1; 0; 1; 0; 1]

        let encode (bop : Ast.biOp) (r : Ast.register)  : int list = 
                encode_r r @ encode_op bop
end


(** [Comp.encode] takes [Ast.computation] and returns [int list]. It binds the [Const.encode], [Unary.encode] and [Binary.encode] into a single encode function *)
module Comp : sig
        val encode : Ast.computation -> int list
end = struct
        let encode (comp : Ast.computation) : int list = 
                match comp with
                | Ast.Constant c -> Const.encode c
                | Ast.Unary (u, r) -> Unary.encode u r
                | Ast.Binary (b, r) -> Binary.encode b r
end


(** [Cinst.encode] takes [Ast.c_inst] and returns [int list]. It binds the [Jump.encode], [Dest.encode] and [Comp.encode] into a single encode function *)
module Cinst : sig
        val encode : Ast.c_inst -> int list
end = struct
        let encode (c : Ast.c_inst) : int list = 
                match c with (dest, comp, jump) -> (Comp.encode comp) @ (Dest.encode dest) @ (Jump.encode jump)

end


(** [Instruction.encode] takes [(string, int) Hashtable.t] and [string Ast.instruction] and returns [int list] by reolving any AInst or using [Cinst.encode] to encode any CInst *)
module Instruction : sig
        val encode : (string, int) Hashtbl.t -> string Ast.instruction -> int list
end = struct
        let resolve_symbol (h : (string, int) Hashtbl.t) (symbol : string) : int = 
                match int_of_string_opt symbol with
                | Some x -> x
                | None   -> 
                        if Hashtbl.mem h symbol then
                                Hashtbl.find h symbol
                        else 
                                failwith "machine.Ainst.retrieve_int: unexpected `CInst _' argument"


        let encode (h : (string, int) Hashtbl.t) (a : string Ast.instruction) : int list = 
                match a with
                | AInst symbol -> let resolved = resolve_symbol h symbol in 
                        Vector.fill_truncate 15 (Vector.to_binary resolved)
                | CInst c      -> Cinst.encode c

end


module LabelTable : sig
        val populate : ?offset:int -> (string, int) Hashtbl.t -> string Ast.program -> unit
end = struct
        let rec populate ?(offset=0) (h : (string, int) Hashtbl.t) (p : string Ast.program) : unit = 
                match p with
                | [] -> ()
                | head :: tail -> 
                        (
                        match Ast.Block.label_opt head with
                        | Some Label l -> 
                                if Hashtbl.mem h l then failwith "machine.LabelTable.populate: repeated label"
                                else Hashtbl.add h l offset
                        | None -> () 
                        ) ;
                        
                        let new_offset = offset + Ast.Block.length head in
                        populate ~offset:new_offset h tail;
end


(** TODO: make this code better *)
module VarTable : sig
        val populate : ?offset:int -> (string, int) Hashtbl.t -> string Ast.program -> unit
end = struct
        let rec init_registers ?(start_index=0) ?(end_index=15) (h : (string, int) Hashtbl.t) : unit = 
                if start_index <= end_index then 
                        (
                        Hashtbl.add h ("R" ^ string_of_int start_index) start_index;
                        init_registers h ~start_index: (start_index + 1) ;
                        )

        let init_symbols (h : (string, int) Hashtbl.t) : unit = 
                (
                Hashtbl.add h  "SCREEN" 16384;
                Hashtbl.add h     "KBD" 24756;
                Hashtbl.add h      "SP" 0;
                Hashtbl.add h     "LCL" 1;
                Hashtbl.add h     "ARG" 2;
                Hashtbl.add h    "THIS" 3;
                Hashtbl.add h    "THAT" 4;
                )

        let init (h : (string, int) Hashtbl.t) : unit = 
                (
                init_registers h;
                init_symbols h;
                )


        let rec edit_instructions ?(offset=16) (h : (string, int) Hashtbl.t) (insts : string Ast.instruction list) : unit = 
                match insts with
                | [] -> ()
                | CInst _ :: tail -> edit_instructions h tail
                | AInst v :: tail -> 
                        (
                        if not(Hashtbl.mem h v) then Hashtbl.add h v 0;
                        let new_offset = offset + 1 in
                        edit_instructions ~offset:new_offset h tail
                        )

        let rec edit_program ?(offset=16) (h : (string, int) Hashtbl.t) (p : string Ast.program) : unit = 
                match p with
                | [] -> ()
                | b :: bs -> 
                        (
                        let insts = Ast.Block.instructions b in
                        edit_instructions h insts;
                        edit_program ~offset:offset h bs
                        )

        let populate ?(offset=16) (h : (string, int) Hashtbl.t) (p : string Ast.program) : unit = 
                init h;
                edit_program ~offset:offset h p
end


module Block : sig
        val encode : (string, int) Hashtbl.t -> string Ast.Block.t -> int list list
end = struct
        let encode (h : (string, int) Hashtbl.t) (b : string Ast.Block.t) : int list list = 
                let insts = Ast.Block.instructions b in
                List.map (Instruction.encode h) insts
end

module Program : sig
        val encode : (string, int) Hashtbl.t -> string Ast.program -> int list list
        val encode_pretty_int : (string, int) Hashtbl.t -> string Ast.program -> int list
        val encode_pretty_string : (string, int) Hashtbl.t -> string Ast.program -> string list
end = struct
        let rec encode (h : (string, int) Hashtbl.t) (p : string Ast.program) : int list list = 
                match p with
                | [] -> []
                | b :: bs -> (Block.encode h b) @ encode h bs
        
        let encode_pretty_int (h : (string, int) Hashtbl.t) (p : string Ast.program) : int list = 
                List.map Vector.to_int (encode h p)
        
        let encode_pretty_string (h : (string, int) Hashtbl.t) (p : string Ast.program) : string list = 
                List.map Vector.to_string (encode h p)
end