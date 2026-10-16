type nat = Z | S of nat 

type prim = 
  | Proj of int * int 
  | Zero of int 
  | Succ 
  | Comp of prim * prim list
  | Rho of int * prim * prim

type exp = 
  | Val of value 
  | App of prim * exp list

and value = 
  | VNat of nat
  | VFun of prim

type definition = { name : string; arity : int; body : prim }
type file = { definitions : definition list; result : (exp * Ty.t) option }
