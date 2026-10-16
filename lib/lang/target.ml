type nat = Z | S of nat 

type partial = 
  | Proj of int * int 
  | Zero of int 
  | Succ 
  | Comp of partial * partial list
  | Rho of int * partial * partial
  | Mu of int * partial

type exp = 
  | Val of value 
  | App of partial * exp list

and value = 
  | VNat of nat
  | VFun of partial

type definition = { name : string; arity : int; body : partial }
type file = { definitions : definition list; result : (exp * Ty.t) option }
