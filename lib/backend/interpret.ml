open Lang.Target

let rec split prefix = function
  | [] -> failwith "empty list"
  | [x] -> List.rev prefix, x
  | x::xs -> split (x::prefix) xs

let rec exp = function 
  | Val v -> v 
  | App (p, es) -> 
    let unwrap = function VNat n -> n | _ -> failwith "type error" in 
    let args = List.map (Fun.compose unwrap exp) es in 
    VNat ((prim p) args)
and prim e = match e with
  | Proj(_, i) -> fun args -> List.nth args (i-1) 
  | Zero _ -> fun args -> Z 
  | Succ -> (function [x] -> S x |_-> failwith "error")
  | Comp (e, es) -> fun args ->
   (prim e) (List.map (fun e -> (prim e) args) es)
  | Rho (n, f, g) -> 
    recurse (prim f) (prim g) 
and recurse f g args =
  let xs, n = split [] args in 
  match n with
  | Z -> f xs
  | S n -> g (xs @ [n; recurse f g (xs@[n])])



