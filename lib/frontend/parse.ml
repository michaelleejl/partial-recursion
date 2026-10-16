let file filename source =
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf filename;
  try Parser.main Lexer.token lexbuf
  with Parser.Error ->
    let at = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
    raise
      (Syntax.Error
         (match Lexing.lexeme lexbuf with
         | "" -> Unexpected_end { at }
         | token -> Unexpected_token { at; token }))
