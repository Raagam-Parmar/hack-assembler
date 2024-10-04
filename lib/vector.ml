let rec add_vector_combined a = 
        match a with
        | [] -> []
        | head :: tail -> (fst head + snd head) :: (add_vector_combined tail)

let add_vector (a : int list) (b : int list) : int list = 
        let c = List.combine a b in
        add_vector_combined c

(* let ( ++ ) (a : 'a list) (b : 'a list) : 'a list = a @ b  *)