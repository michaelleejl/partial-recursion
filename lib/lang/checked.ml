type prim =
  | CProj of int * int
  | CZero of int
  | CSucc
  | CComp of prim * prim list
  | CRho of int * prim * prim
  | CVar of string

type exp =
  | CNat of int
  | CFun of prim
  | CApp of prim * exp list

type definition = { name : string; arity : int; body : prim }
type file = { definitions : definition list; result : (exp * Ty.t) option }
