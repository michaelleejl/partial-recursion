open Lang.Checked
open Lang.Target
module Env = Map.Make (String)

let rec nat_of_int = function 0 -> Z | n -> S (nat_of_int (n - 1))

let rec partial env : Lang.Checked.partial -> Lang.Target.partial = function
  | CVar name -> (match Env.find_opt name env with Some p -> p | None -> assert false)
  | CProj (n, i) -> Proj (n, i)
  | CZero n -> Zero n
  | CSucc -> Succ
  | CComp (f, gs) -> Comp (partial env f, List.map (partial env) gs)
  | CRho (n, f, g) -> Rho (n, partial env f, partial env g)
  | CMu (n, f) -> Mu (n, partial env f)

let rec exp env : Lang.Checked.exp -> Lang.Target.exp = function
  | CNat n -> Val (VNat (nat_of_int n))
  | CFun p -> Val (VFun (partial env p))
  | CApp (p, es) -> App (partial env p, List.map (exp env) es)

let file ({ definitions; result } : Lang.Checked.file) : Lang.Target.file =
  let env, definitions =
    List.fold_left
      (fun (env, ds) ({ name; arity; body } : Lang.Checked.definition) ->
        let body = partial env body in
        (Env.add name body env, ({ name; arity; body } : Lang.Target.definition) :: ds))
      (Env.empty, []) definitions
  in
  { definitions = List.rev definitions; result = Option.map (fun (e, ty) -> (exp env e, ty)) result }
