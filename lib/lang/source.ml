open Text.Located

type prim = prim_desc located

and prim_desc =
  | SProj of int * int
  | SZero of int
  | SSucc
  | SComp of prim * prim list
  | SRho of int * prim * prim
  | SVar of string

type exp = exp_desc located

and exp_desc =
  | SNat of int
  | SFun of prim
  | SApp of prim * exp list

type definition = { name : string located; body : prim }
type file = { definitions : definition list; result : exp option }
