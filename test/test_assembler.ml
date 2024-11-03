(* open OUnit2
open Lib

let make_test name value (ast : string Ast.program) = 
        name >:: (fun _ -> assert_equal value (Lib.Machine.Program.encode_pretty_string ) ) *)