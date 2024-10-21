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
        (** the binary operations are applied to D, register which is implied *)

type c_inst = destination option * computation * jump option 

type 'v instruction = 
        | AInst of 'v
        | CInst of c_inst


(** reevaluate if lael has to be of type string, it can be of any type... *)
type label = Label of string

type 'v block = label option * 'v instruction list

type 'v program = 'v block list
