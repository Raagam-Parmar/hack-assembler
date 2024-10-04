%token EOF                      // End Of File token

%token <string> LABEL           // Label token carrying a string
%token <string> ASTR            // a_inst StrAddr token carrying a string
%token <int> AINT               // a_inst IntAddr token carrying an
%token <Ast.c_inst> CINST       // c_inst token carrying Ast.c_inst

%token NEWLINE                  // newline '\n' token

%start <unit> main

%%

main:
        | EOF { () }
        ;