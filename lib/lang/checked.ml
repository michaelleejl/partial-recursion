type partial =
  | CProj of int * int
  | CZero of int
  | CSucc
  | CComp of partial * partial list
  | CRho of int * partial * partial
  | CMu of int * partial
  | CVar of string

type exp =
  | CNat of int
  | CFun of partial
  | CApp of partial * exp list

type definition = { name : string; arity : int; body : partial }
type file = { definitions : definition list; result : (exp * Ty.t) option }
