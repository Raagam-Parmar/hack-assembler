(* open Lib

(** [parse s] parses [s] into an AST *)
let parse (s : string) : unit = 
        let lexbuf = Lexing.from_string s in
        let ast = Parser.main Lexer.read lexbuf in
        ast *)