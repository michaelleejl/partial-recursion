open Lang.Checked
open Lang.Target
module Env = Map.Make (String)

let rec nat_of_int = function 0 -> Z | n -> S (nat_of_int (n - 1))

let rec prim env : Lang.Checked.prim -> Lang.Target.prim = function
  | CVar name -> (match Env.find_opt name env with Some p -> p | None -> assert false)
  | CProj (n, i) -> Proj (n, i)
  | CZero n -> Zero n
  | CSucc -> Succ
  | CComp (f, gs) -> Comp (prim env f, List.map (prim env) gs)
  | CRho (n, f, g) -> Rho (n, prim env f, prim env g)

let rec exp env : Lang.Checked.exp -> Lang.Target.exp = function
  | CNat n -> Val (VNat (nat_of_int n))
  | CFun p -> Val (VFun (prim env p))
  | CApp (p, es) -> App (prim env p, List.map (exp env) es)

let file ({ definitions; result } : Lang.Checked.file) : Lang.Target.file =
  let env, definitions =
    List.fold_left
      (fun (env, ds) ({ name; arity; body } : Lang.Checked.definition) ->
        let body = prim env body in
        (Env.add name body env, ({ name; arity; body } : Lang.Target.definition) :: ds))
      (Env.empty, []) definitions
  in
  { definitions = List.rev definitions; result = Option.map (fun (e, ty) -> (exp env e, ty)) result }
