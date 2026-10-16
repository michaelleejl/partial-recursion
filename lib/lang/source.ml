open Text.Located

type partial = prim_desc located

and prim_desc =
  | SProj of int * int
  | SZero of int
  | SSucc
  | SComp of partial * partial list
  | SRho of int * partial * partial
  | SMu of int * partial
  | SVar of string

type exp = exp_desc located

and exp_desc =
  | SNat of int
  | SFun of partial
  | SApp of partial * exp list

type definition = { name : string located; body : partial }
type file = { definitions : definition list; result : exp option }
