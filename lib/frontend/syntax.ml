type error =
  | Unexpected_character of { at : Text.Span.t; character : char }
  | Unexpected_token of { at : Text.Span.t; token : string }
  | Unexpected_end of { at : Text.Span.t }
  | Reserved_name of { at : Text.Span.t; name : string }

exception Error of error

let report : error -> Text.Report.t = function
  | Unexpected_character { at; character } ->
      {
        summary = Printf.sprintf "unrecognised character %C" character;
        label = { at; text = "remove it" };
        notes = [];
      }
  | Unexpected_token { at; token } ->
      {
        summary = Printf.sprintf "'%s' is out of place" token;
        label = { at; text = "not expected here" };
        notes = [];
      }
  | Unexpected_end { at } ->
      {
        summary = "the input ends in the middle of an expression";
        label = { at; text = "more expected here" };
        notes = [];
      }
  | Reserved_name { at; name } ->
      {
        summary = Printf.sprintf "%s is a reserved word" name;
        label = { at; text = "choose another name" };
        notes = [];
      }

let () = Text.Report.register (function Error e -> Some (report e) | _ -> None)
