type message = { at : Span.t; text : string }
type t = { summary : string; label : message; notes : message list }

let handlers : (exn -> t option) list ref = ref []
let register handler = handlers := handler :: !handlers
let try_run e = List.find_map (fun handler -> handler e) !handlers

let line_at source (p : Lexing.position) =
  let stop =
    Option.value (String.index_from_opt source p.pos_bol '\n')
      ~default:(String.length source)
  in
  String.sub source p.pos_bol (stop - p.pos_bol)

let block ~source ~severity { at = (start, stop); text } summary =
  let line = line_at source start in
  let number = string_of_int start.pos_lnum in
  let gutter = String.make (String.length number) ' ' in
  let column = start.pos_cnum - start.pos_bol in
  let width = max 1 (min (String.length line - column) (stop.pos_cnum - start.pos_cnum)) in
  let caret = String.make column ' ' ^ String.make width '^' in
  String.concat "\n"
    [
      Printf.sprintf "%s: %s" severity summary;
      Printf.sprintf "%s--> %s:%d:%d" gutter start.pos_fname start.pos_lnum (column + 1);
      Printf.sprintf "%s |" gutter;
      Printf.sprintf "%s | %s" number line;
      Printf.sprintf "%s | %s" gutter (if text = "" then caret else caret ^ " " ^ text);
    ]

let render ~source { summary; label; notes } =
  String.concat "\n\n"
    (block ~source ~severity:"error" label summary
    :: List.map (fun note -> block ~source ~severity:"note" { note with text = "" } note.text) notes)
  ^ "\n"
