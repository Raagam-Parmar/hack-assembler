exception UnexpectedRegister of string

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


module Const : sig
        val encode : Ast.const -> int list
end = struct
        let encode (c : Ast.const) : int list = 
                match c with
                | Zero      -> [0; 1; 0; 1; 0; 1; 0]
                | One       -> [0; 1; 1; 1; 1; 1; 1]
                | MinusOne  -> [0; 1; 1; 1; 0; 1; 0]
end


module Unary : sig
        val encode : Ast.register -> Ast.unOp -> int list
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
        
        let encode (r : Ast.register) (uop : Ast.unOp) : int list = 
                match uop with
                | Ast.Succ      -> succ r
                | _             -> encode_r r @ encode_op uop       
end


module Binary : sig
        val encode : Ast.register -> Ast.biOp -> int list
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

        let encode (r : Ast.register) (bop : Ast.biOp) : int list = 
                encode_r r @ encode_op bop
end