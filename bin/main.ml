open Lib

(*
(** [parse s] parses [s] into an AST *)
let parse (s : string) : unit = 
        let lexbuf = Lexing.from_string s in
        let ast = Parser.main Lexer.read lexbuf in
        ast *)


let b1 : string Ast.Block.t = (Some (Label "START"), [])
let b2 : string Ast.Block.t = (Some (Label "FOO"), [AInst "2"; CInst (Some [D],Unary (Identity, A), None)])
let b3 : string Ast.Block.t = ( Some (Label "BAR"), [ AInst "3"; CInst (Some [D], Binary (Add, A), None); AInst "0"; CInst (Some [M], Unary (Identity, M), None) ] )

let p : string Ast.program = [b1; b2; b3]

let h = Hashtbl.create 10
let () = Machine.LabelTable.populate h p
let () = Machine.VarTable.populate h p

let sl = Machine.Program.encode_pretty_string h p
let () = List.iter print_endline sl