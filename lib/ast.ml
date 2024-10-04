type jump_ident = 
        | JNL
        | JGT
        | JEQ
        | JGE
        | JLT
        | JNE
        | JLE
        | JMP

type dest_ident = 
        | N
        | M
        | D
        | MD
        | A
        | AM
        | AD
        | AMD

type comp_ident = 
        | Comp_Zero
        | Comp_One
        | Comp_MinusOne
        | Comp_D
        | Comp_A
        | Comp_M
        | Comp_NotD
        | Comp_NotA
        | Comp_NotM
        | Comp_MinusD
        | Comp_MinusA
        | Comp_MinusM
        | Comp_DPlusOne
        | Comp_APlusOne
        | Comp_MPlusOne
        | Comp_DMinusOne
        | Comp_AMinusOne
        | Comp_MMinusOne
        | Comp_DPlusA
        | Comp_DPlusM
        | Comp_DMinusA
        | Comp_DMinusM
        | Comp_AMinusD
        | Comp_MMinusD
        | Comp_DAndA
        | Comp_DAndM
        | Comp_DOrA
        | Comp_DOrM

type c_inst = dest_ident * comp_ident * jump_ident 

type a_inst = 
        | IntAddr of int
        | StrAddr of string

type statement = 
        | AInst of a_inst
        | CInst of c_inst
        | Label of string