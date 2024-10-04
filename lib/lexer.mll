{
        open Parser
        (* open Lexing *)

}

let space        = [' ' '\t']
let spaces       = space+
let newline      = '\r' | '\n' | "\r\n"

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
        