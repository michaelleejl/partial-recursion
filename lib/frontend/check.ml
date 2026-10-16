open Text.Located
open Lang.Source
open Lang.Checked
open Lang.Ty

type error =
  | Projection_out_of_range of { at : Text.Span.t; arity : int; index : int }
  | Composition_count of { at : Text.Span.t; expected : int; given : int }
  | Composition_mismatch of {
      at : Text.Span.t;
      first : Text.Span.t;
      expected : int;
      given : int;
    }
  | Rho_base of { at : Text.Span.t; n : int; given : int }
  | Rho_step of { at : Text.Span.t; n : int; given : int }
  | Mu_body of { at : Text.Span.t; n : int; given : int }
  | Minimisation_disabled of { at : Text.Span.t }
  | Argument_count of { at : Text.Span.t; expected : int; given : int }
  | Argument_is_function of { at : Text.Span.t; arity : int }

  | Unbound_name of { at : Text.Span.t; name : string; bound : string list }
  | Defined_below of { at : Text.Span.t; name : string; definition : Text.Span.t }
  | Self_reference of { at : Text.Span.t; name : string }
  | Duplicate_name of { at : Text.Span.t; first : Text.Span.t; name : string }

exception Error of error

let arguments = function 1 -> "1 argument" | n -> Printf.sprintf "%d arguments" n
let functions = function 1 -> "1 function" | n -> Printf.sprintf "%d functions" n

let report : error -> Text.Report.t = function
  | Projection_out_of_range { at; arity; index } ->
      {
        summary = Printf.sprintf "proj[%d, %d] has no argument %d" arity index index;
        label = { at; text = Printf.sprintf "choose an index from 1 to %d" arity };
        notes = [];
      }
  | Composition_count { at; expected; given } ->
      {
        summary =
          Printf.sprintf "this function takes %s, but is composed with %s"
            (arguments expected) (functions given);
        label = { at; text = Printf.sprintf "takes %s" (arguments expected) };
        notes = [];
      }
  | Composition_mismatch { at; first; expected; given } ->
      {
        summary = "the functions in a composition must all take the same number of arguments";
        label = { at; text = Printf.sprintf "takes %s" (arguments given) };
        notes = [ { at = first; text = Printf.sprintf "the first takes %s" (arguments expected) } ];
      }
  | Rho_base { at; n; given } ->
      {
        summary =
          Printf.sprintf "rho[%d] needs a base function that takes %s, not %d" n
            (arguments n) given;
        label = { at; text = Printf.sprintf "takes %s" (arguments given) };
        notes = [];
      }
  | Rho_step { at; n; given } ->
      {
        summary =
          Printf.sprintf "rho[%d] needs a step function that takes %s, not %d" n
            (arguments (n + 2)) given;
        label = { at; text = Printf.sprintf "takes %s" (arguments given) };
        notes = [];
      }
  | Mu_body { at; n; given } ->
      {
        summary =
          Printf.sprintf "mu[%d] needs a function that takes %s, not %d" n
            (arguments (n + 1)) given;
        label = { at; text = Printf.sprintf "takes %s" (arguments given) };
        notes = [];
      }
  | Minimisation_disabled { at } ->
      {
        summary = "mu needs minimisation, which is off";
        label = { at; text = "run pr -m FILE to turn it on" };
        notes = [];
      }
  | Argument_count { at; expected; given } ->
      {
        summary =
          Printf.sprintf "this function takes %s, but is given %d" (arguments expected) given;
        label = { at; text = Printf.sprintf "takes %s" (arguments expected) };
        notes = [];
      }
  | Argument_is_function { at; arity } ->
      {
        summary = "an argument must be a number, but this is a function";
        label = { at; text = Printf.sprintf "has type nat^%d -> nat" arity };
        notes = [];
      }

  | Unbound_name { at; name; bound } ->
      {
        summary = Printf.sprintf "%s is not defined" name;
        label =
          {
            at;
            text =
              (match bound with
              | [] -> Printf.sprintf "define it with \"let %s = ...\"" name
              | bound -> Printf.sprintf "defined above this point: %s" (String.concat ", " bound));
          };
        notes = [];
      }
  | Defined_below { at; name; definition } ->
      {
        summary = Printf.sprintf "%s is used before its definition" name;
        label = { at; text = "used here" };
        notes =
          [ { at = definition; text = Printf.sprintf "%s is defined here; move its definition above its use" name } ];
      }
  | Self_reference { at; name } ->
      {
        summary = Printf.sprintf "%s is used in its own definition" name;
        label = { at; text = "a definition cannot refer to itself" };
        notes = [];
      }
  | Duplicate_name { at; first; name } ->
      {
        summary = Printf.sprintf "%s is defined twice" name;
        label = { at; text = "defined again here" };
        notes = [ { at = first; text = "first defined here" } ];
      }


let () = Text.Report.register (function Error e -> Some (report e) | _ -> None)

module Env = Map.Make (String)

type position = Above of int | Here | Below of Text.Span.t

let rec partial ~primitive env ({ at; v } : Lang.Source.partial) : Lang.Checked.partial * int = match v with
  | SVar name -> (
      match Env.find_opt name env with
      | Some (Above a) -> (CVar name, a)
      | Some Here -> raise (Error (Self_reference { at; name }))
      | Some (Below definition) -> raise (Error (Defined_below { at; name; definition }))
      | None ->
          let bound = Env.fold (fun n p ns -> match p with Above _ -> n :: ns | _ -> ns) env [] in
          raise (Error (Unbound_name { at; name; bound = List.rev bound })))
  | SProj (n, i) -> 
    if i < 1 || i > n then raise (Error (Projection_out_of_range { at; arity = n; index = i }))
    else (CProj (n, i), n)
  | SZero n -> (CZero n, n)
  | SSucc -> (CSucc, 1)
  | SComp (f, gs) -> 
    let f', a = partial ~primitive env f in 
    let checked = List.map (partial ~primitive env) gs in 
    if List.length gs <> a then 
      raise (Error (Composition_count { at = f.at; expected = a; given = List.length gs }));
    (match List.combine gs checked with
     | [] -> (CComp (f', []), 0)
     | (g, (_, b)) :: rest -> 
       List.iter (fun ((g' : Lang.Source.partial), (_, b')) -> 
           if b' <> b then 
             raise (Error (Composition_mismatch { at = g'.at; first = g.at; expected = b; given = b' })))
         rest;
       (CComp (f', List.map fst checked), b))
  | SRho (n, f, g) -> 
    let f', a = partial ~primitive env f in 
    let g', b = partial ~primitive env g in 
    if a <> n then raise (Error (Rho_base { at = f.at; n; given = a }))
    else if b <> n+2 then raise (Error (Rho_step { at = g.at; n; given = b }))
    else (CRho (n, f', g'), n+1)
  | SMu (n, f) -> 
    if primitive then raise (Error (Minimisation_disabled { at }));
    let f', a = partial ~primitive env f in 
    if a <> n+1 then raise (Error (Mu_body { at = f.at; n; given = a }))
    else (CMu (n, f'), n)

let rec exp ~primitive env ({ v; _ } : Lang.Source.exp) : Lang.Checked.exp * Lang.Ty.t = match v with
  | SNat n -> (CNat n, TNat)
  | SFun p -> 
    let p, a = partial ~primitive env p in 
    (CFun p, TFun a)
  | SApp (p, es) -> 
    let p', a = partial ~primitive env p in 
    if List.length es <> a then 
      raise (Error (Argument_count { at = p.at; expected = a; given = List.length es }));
    let es' = List.map (fun (e : Lang.Source.exp) -> match exp ~primitive env e with
        | e', TNat -> e'
        | _, TFun arity -> raise (Error (Argument_is_function { at = e.at; arity })))
        es in 
    (CApp (p', es'), TNat)

let file ~primitive ({ definitions; result } : Lang.Source.file) : Lang.Checked.file =
  let env =
    List.fold_left
      (fun env ({ name; _ } : Lang.Source.definition) ->
        match Env.find_opt name.v env with
        | Some (Below first) -> raise (Error (Duplicate_name { at = name.at; first; name = name.v }))
        | _ -> Env.add name.v (Below name.at) env)
      Env.empty definitions
  in
  let env, definitions =
    List.fold_left
      (fun (env, ds) ({ name; body } : Lang.Source.definition) ->
        let body, arity = partial ~primitive (Env.add name.v Here env) body in
        (Env.add name.v (Above arity) env, ({ name = name.v; arity; body } : Lang.Checked.definition) :: ds))
      (env, []) definitions
  in
  { definitions = List.rev definitions; result = Option.map (exp ~primitive env) result }
