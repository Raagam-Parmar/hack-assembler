type jump = 
        | JGT
        | JEQ
        | JGE
        | JLT
        | JNE
        | JLE
        | JMP

type register = A | D | M

type destination = register list

type const = 
        | Zero
        | One
        | MinusOne

type unOp = 
        | Identity
        | BitNot
        | Minus
        | Succ
        | Pred

type biOp = 
        | Add
        | Sub
        | SubFrom
        | BinAnd
        | BinOr

type computation = 
        | Constant of const
        | Unary of unOp * register
        | Binary of biOp * register
        (* the binary operations are applied to D, register which is implied *)


type a_inst = 
        | IntAddr of int (* this is the final form of a_inst *)
        | StrAddr of string  (* this is how a_inst will look like in beginning, all numbers will be strings "1", "2", ... *)

type c_inst = destination option * computation * jump option 

type instruction = 
        | AInst of a_inst
        | CInst of c_inst


type label = Label of string

type block = label option * instruction list

type program = block list
