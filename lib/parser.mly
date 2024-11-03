%token EOF                      // End Of File token

%token ZERO
%token ONE
%token MINUS_ONE
%token BNOT
%token BOR
%token BAND
%token MINUS
%token PLUS
%token EQUAL
%token SEMI
%token A
%token D
%token M

%token <string> LABEL           // Label token carrying a string
%token <string> AINST           // AInst token carrying a string
%token <Ast.jump> JUMP            // Jump token carrying a string


%start <string Ast.program> main

%%

main:
        | EOF { [] }
        | program; EOF { $1 }
        ;

program:
        | block { [$1] }
        | block; program { $1 :: $2 }
        ;

block:
        | LABEL; instruction_list { (Some (Ast.Label $1), $2) }
        | instruction_list { (None, $1) }
        | LABEL { (Some (Ast.Label $1), []) }
        ;

instruction_list:
        | instruction { [$1] }
        | instruction; instruction_list { $1 :: $2 }
        ;

instruction:
        | a_instruction { Ast.AInst $1 }
        | c_instruction { Ast.CInst $1 }
        ;

a_instruction:
        | AINST { $1 }
        ;

c_instruction:
        | dest; comp; jump { (Some $1, $2, Some $3) }
        | dest; comp { (Some $1, $2, None) }
        | comp; jump { (None, $1, Some $2) }
        | comp { (None, $1, None) }
        ;

dest:
        | register_list; EQUAL { $1 }
        ;

register_list:
        | register { [$1] }
        | register; register_list { $1 :: $2 }
        ;

register:
        | A { Ast.A }
        | D { Ast.D }
        | M { Ast.M }
        ;

comp:
        | const { Ast.Constant $1 }
        // | register { $1 }
        | BNOT; register { Ast.Unary (BitNot, $2) }
        | MINUS; register { Ast.Unary (Minus, $2) }
        | register; PLUS; ONE { Ast.Unary (Succ, $1) }
        | register; MINUS_ONE { Ast.Unary (Pred, $1) }
        | D; binary_op; A { Ast.Binary ($2, A) }
        | D; binary_op; M { Ast.Binary ($2, M) }
        | A; MINUS; D { Ast.Binary (SubFrom, A) }
        | M; MINUS; D { Ast.Binary (SubFrom, M) }
        | register { Ast.Unary (Identity, $1) }
        ;

const:
        | ZERO { Ast.Zero }
        | ONE { Ast.One }
        | MINUS_ONE { Ast.MinusOne }
        ;


binary_op:
        | PLUS { Ast.Add }
        | MINUS { Ast.Sub }
        | BAND { Ast.BinAnd }
        | BOR { Ast.BinOr }
        ;

jump:
        | SEMI; JUMP { $2 }
        ;