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

        let rec reduce_a_inst (a_inst : string) : string =
        if (a_inst.[0] = '@') || (a_inst.[0] = ' ') then
                let length = String.length a_inst in
                let reduced_a_inst = String.sub a_inst 1 (length - 1) in
                reduce_a_inst reduced_a_inst
        else
                a_inst




}

let space        = [' ' '\t']
let spaces       = space+
let newline      = '\r' | '\n' | "\r\n"

let digit        = ['0'-'9']
let int          = digit+

let lower_char   = ['a'-'z']
let upper_char   = ['A'-'Z']
let allowed_misc = ':' | '.' |'$' | '_'
let allow_first  = lower_char | upper_char | allowed_misc
let allow_rest   = lower_char | upper_char | allowed_misc | digit

let allow_string = (allow_first) (allow_rest)*

let jump         = "JGT" | "JGE" | "JEQ" | "JNE" | "JLE" | "JLT" | "JMP"

(* let register     = 'A' | 'D' | 'M' *)

let label        = '(' (allow_string) ')'

let ainst        = '@' (spaces*) (allow_string) | '@' (spaces*) (int)


rule read = 
        parse
        | eof           { EOF }
        (* | '0'           { ZERO }
        | '1'           { ONE }
        | "-1"          { MINUS_ONE }
        | '!'           { BNOT }
        | '|'           { BOR }
        | '&'           { BAND }
        | '-'           { MINUS }
        | '+'           { PLUS }
        | '='           { EQUAL }
        | ';'           { SEMI }
        | 'D'           { D }
        | 'A'           { A }
        | 'M'           { M }

        | label         { LABEL (Lexing.lexeme lexbuf) }
        | jump          { JUMP (Lexing.lexeme lexbuf) }
        | ainst         { AINST (reduce_a_inst (Lexing.lexeme lexbuf)) }
         *)