%token EOF                      // End Of File token

%token <string> LABEL           // Label token carrying a string

%start <unit> main

%%

main:
        | EOF { () }
        // | program; EOF { $1 }
        ;

// program:
//         | block { [$1] }
//         | block; program { $1 :: $2 }
//         ;

// block:
//         | LABEL; instruction_list { (Some (Ast.Label $1), $2) }
//         | instruction_list { (None, $1) }
//         | LABEL { (Some (Ast.Label $1), []) }
//         ;

// instruction_list:
//         | instruction { [$1] }
//         | instruction; instruction_list { $1 :: $2 }
//         ;

// instruction:
//         | a_instruction { Ast.AInst $1 }
//         | c_instruction { CInst $1 }
//         ;

// // a_instruction:
//         | 
