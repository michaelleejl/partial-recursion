open Partial_recursion
open Frontend
open Backend

let rec int_of_nat = function Lang.Target.Z -> 0 | Lang.Target.S n -> 1 + int_of_nat n

let () =
  match Sys.argv with
  | [| _; file |] -> (
      let source = In_channel.with_open_text file In_channel.input_all in
      try
        let { definitions; result } : Lang.Target.file =
          Parse.file file source |> Check.file |> Elaborate.Inline.file
        in
        List.iter
          (fun ({ name; arity; _ } : Lang.Target.definition) ->
            Printf.printf "%s : nat^%d -> nat\n" name arity)
          definitions;
        match result with
        | None -> ()
        | Some (_, TFun arity) -> Printf.printf "<fun> : nat^%d -> nat\n" arity
        | Some (e, TNat) -> (
            match Interpret.exp e with
            | VNat n -> Printf.printf "%d\n" (int_of_nat n)
            | VFun _ -> assert false)
      with e -> (
        match Text.Report.try_run e with
        | Some report ->
            prerr_string (Text.Report.render ~source report);
            exit 1
        | None -> raise e))
  | _ ->
      prerr_endline "usage: pr FILE";
      exit 2
