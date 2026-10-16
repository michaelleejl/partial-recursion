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
    VNat ((partial p) args)
and partial e = match e with
  | Proj(_, i) -> fun args -> List.nth args (i-1) 
  | Zero _ -> fun args -> Z 
  | Succ -> (function [x] -> S x |_-> failwith "error")
  | Comp (e, es) -> fun args ->
   (partial e) (List.map (fun e -> (partial e) args) es)
  | Rho (n, f, g) -> 
    recurse (partial f) (partial g) 
  | Mu (n, f) -> 
    minimise (partial f) Z
and recurse f g args =
  let xs, n = split [] args in 
  match n with
  | Z -> f xs
  | S n -> g (xs @ [n; recurse f g (xs@[n])])
and minimise f n args = 
  match f (args @ [n]) with 
  | Z -> n
  | S _ -> (minimise f (S n) args)



