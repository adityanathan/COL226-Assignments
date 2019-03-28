(* Dummy implementation *)
type bigint = sign * int list
  and sign = Neg | NonNeg
exception Not_implemented

let add n1 n2 = raise Not_implemented
let mult n1 n2 = raise Not_implemented
let sub n1 n2 = raise Not_implemented
let div n1 n2 = raise Not_implemented
let rem n1 n2 = raise Not_implemented
let minus n1 = raise Not_implemented
let abs n1 = raise Not_implemented
let eq n1 n2 = raise Not_implemented
let gt n1 n2 = raise Not_implemented
let lt n1 n2 = raise Not_implemented
let geq n1 n2 = raise Not_implemented
let leq n1 n2 = raise Not_implemented
let print_num n1 = raise Not_implemented
let mk_big n = raise Not_implemented
