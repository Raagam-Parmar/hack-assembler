{
        open Parser
        open Lexing

        (** [next_line lexbuf] updates the position of the current lexical buffer [lexbuf]
        to point towards the start of the next line *)
        let next_line (lexbuf : lexbuf) =
                let pos = lexbuf.lex_curr_p in
                lexbuf.lex_curr_p <- 
                {
                        pos with pos_bol = pos.pos_cnum;
                        pos_lnum = pos.pos_lnum + 1
                }

        (** [reduce_label s] reduces [s], which matches with regex-label and returns the usable label from it as string *)
        let reduce_label (s : string) : string = 
                let length = String.length s in
                        String.trim (String.sub s 1 (length-2))


        (** [reduce_a_inst_str s] reduces [s], which matches with regex-a_inst_str and returns the usable string address *)
        let reduce_a_inst_str (s : string) : string = 
                let length = String.length s in
                        String.trim (String.sub s 1 (length-1))


        (** [reduce_a_inst_int s] reduces [s], which matches with regex-a_inst_int and returns the usable int address *)
        let reduce_a_inst_int (s : string) : int = 
                let length = String.length s in
                let string_int = String.trim (String.sub s 1 (length-1)) in
                int_of_string string_int


        (** [reduce_dest s] reduces [s], which matches with regex-a_inst_str and returns the usable string address  *)
        let reduce_dest (s : string) : string = 
                (if String.contains s 'A' then "A" else "") ^ 
                (if String.contains s 'M' then "M" else "") ^ 
                (if String.contains s 'D' then "D" else "")


        (** [reduce_dest_to_type s] reduces a string which may have repeated A՚s, M՚s or D՚s into a string with at most one occurence of each *)
        let reduce_dest_to_type (s : string) : Ast.dest_ident = 
                let reduced_s = reduce_dest s in
                        match reduced_s with
                        | ""    -> N
                        | "M"   -> M
                        | "D"   -> D
                        | "MD"  -> MD
                        | "A"   -> A
                        | "AM"  -> AM
                        | "AD"  -> AD
                        | "AMD" -> AMD
                        | _     -> failwith "lexer: unknown dest"
        

        (** [reduce_jump_to_type s] returns the Ast.jump_ident corresponding to [s] *)
        let reduce_jump_to_type (s : string) : Ast.jump_ident = 
                match s with
                | ""    -> JNL
                | "JGT" -> JGT
                | "JEQ" -> JEQ
                | "JGE" -> JGE
                | "JLT" -> JLT
                | "JNE" -> JNE
                | "JLE" -> JLE
                | "JMP" -> JMP
                | _     -> failwith "lexer: unknown jump"


        (** [reduce_comp_to_type s] returns the Ast.comp_ident corresponding to [s] *)
        let reduce_comp_to_type (s : string) : Ast.comp_ident = 
                match s with
                | "0"   -> Comp_Zero
                | "1"   -> Comp_One
                | "-1"  -> Comp_MinusOne
                | "D"   -> Comp_D
                | "A"   -> Comp_A
                | "M"   -> Comp_M
                | "!D"  -> Comp_NotD
                | "!A"  -> Comp_NotA
                | "!M"  -> Comp_NotM
                | "-D"  -> Comp_MinusD
                | "-A"  -> Comp_MinusA
                | "-M"  -> Comp_MinusM
                | "D+1" -> Comp_DPlusOne
                | "A+1" -> Comp_APlusOne
                | "M+1" -> Comp_MPlusOne
                | "D-1" -> Comp_DMinusOne
                | "A-1" -> Comp_AMinusOne
                | "M-1" -> Comp_MMinusOne
                | "D+A" -> Comp_DPlusA
                | "D+M" -> Comp_DPlusM
                | "D-A" -> Comp_DMinusA
                | "D-M" -> Comp_DMinusM
                | "A-D" -> Comp_AMinusD
                | "M-D" -> Comp_MMinusD
                | "D&A" -> Comp_DAndA
                | "D&M" -> Comp_DAndM
                | "D|A" -> Comp_DOrA
                | "D|M" -> Comp_DOrM
                | _     -> failwith "lexer: unknown comp"


        (** [reduce_c_inst_type c] converts [c] to the fully built Ast.c_inst using the above helper functions *)
        let reduce_c_inst_type (c : string) : Ast.c_inst = 
                        let flat_c_tokens = List.flatten (List.map (String.split_on_char ';') (String.split_on_char '=' c)) in
                                if String.contains c ';' then 
                                        if String.contains c '=' then ((reduce_dest_to_type (List.nth flat_c_tokens 0)),
                                                                (reduce_comp_to_type (List.nth flat_c_tokens 1)),
                                                                (reduce_jump_to_type (List.nth flat_c_tokens 2)))
                                        else (N,
                                        (reduce_comp_to_type (List.nth flat_c_tokens 0)),
                                        (reduce_jump_to_type (List.nth flat_c_tokens 1)))
                                else
                                        if String.contains c '=' then ((reduce_dest_to_type (List.nth flat_c_tokens 0)),
                                                                (reduce_comp_to_type (List.nth flat_c_tokens 1)),
                                                                JNL)
                                        else (N, 
                                        (reduce_comp_to_type (List.nth flat_c_tokens 1)),
                                        JNL)

}

let space        = [' ' '\t']
let spaces       = space+
let newline      = '\r' | '\n' | "\r\n"

let ignore       = space | newline

let digit        = ['0'-'9']
let lower_char   = ['a'-'z']
let upper_char   = ['A'-'Z']
let allowed_misc = ':' | '.' |'$' | '_'
let register     = 'A' | 'D' | 'M'
let constant     = '0' | '1' | "-1"
let b_op         = '+' | '-' | '&' | '|'


let int          = digit+
let allow_first  = lower_char | upper_char | allowed_misc
let allow_rest   = lower_char | upper_char | allowed_misc | digit
let allow_string = (allow_first) (allow_rest)*

let label        = "(" (space*) (allow_string) (space*) ")"

let a_inst_str   = "@" (space*) (allow_string)
let a_inst_int   = "@" (space*) int

let destination  = register+

let bnot_reg     = '!' register
let minus_reg    = '-' register
let succ_reg     = register "+1"
let pred_reg     = register "-1"
let d_bop_reg_1  = 'D' b_op ('A' | 'M')
let d_bop_reg_2  = ('A' | 'M') '-' 'D'
let computation  = constant | bnot_reg | minus_reg | succ_reg | pred_reg | d_bop_reg_1 | d_bop_reg_2 | register

let jump         = "JGT" | "JGE" | "JEQ" | "JNE" | "JLE" | "JLT" | "JMP"

let cinst        = (destination '=')? computation (';' jump)?

rule read = 
        parse
        | eof           { EOF }
        | ignore        { read lexbuf }
        | "/*"          { skipMultiComment lexbuf }
        | "//"          { skipComment lexbuf }
        | a_inst_str    { ASTR (reduce_a_inst_str (Lexing.lexeme lexbuf)) }
        | a_inst_int    { AINT (reduce_a_inst_int (Lexing.lexeme lexbuf)) }
        | label         { LABEL (reduce_label (Lexing.lexeme lexbuf)) }
        | cinst         { CINST (reduce_c_inst_type (Lexing.lexeme lexbuf)) }

and skipComment = 
        parse
        | [^'\n']       { skipComment lexbuf }
        | eof           { EOF }
        | _             { read lexbuf }
        
and skipMultiComment = 
        parse
        | [^'*']        { skipMultiComment lexbuf }
        | '*'           { endMultiComment lexbuf }
        | eof           { EOF }

and endMultiComment =
        parse
        | '/'           { read lexbuf }
        | eof           { EOF }
        | _             { skipMultiComment lexbuf }
