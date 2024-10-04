%token EOF                      // End Of File token

%token <string> LABEL           // Label token carrying a string
%token <string> ASTR            // a_inst StrAddr token carrying a string
%token <int> AINT               // a_inst IntAddr token carrying an
%token <Ast.c_inst> CINST       // c_inst token carrying Ast.c_inst

%token NEWLINE                  // newline '\n' token

%start <Ast.statement list> main

%%

main:
        | sl = statement_list; EOF { sl }
        ;

statement_list:
        | s1 = statement; NEWLINE; s2 = statement_list { s1 :: s2 }
        | s = statement { [s] }
        ;

statement:
        | a = ainstruction { Ast.AInst a }
        | c = cinstruction { Ast.CInst c }
        | l = label { l }
        ;

label:
        | l = LABEL { Ast.Label l }
        ;

ainstruction:
        | i = AINT { Ast.IntAddr i }
        | s = ASTR; { Ast.StrAddr s }
        ;

cinstruction:
        | c = CINST { c }