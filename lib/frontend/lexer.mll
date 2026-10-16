{
open Parser
}

rule token = parse
  | [' ' '\t' '\r']+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | ['0'-'9']+ as n { INT (int_of_string n) }
  | "zero" { ZERO }
  | "proj" { PROJ }
  | "succ" { SUCC }
  | "rho" { RHO }
  | "mu" { MU }
  | "let" { LET }
  | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* as x { IDENT x }
  | '.' { DOT }
  | '=' { EQUAL }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | eof { EOF }
  | _ as c {
      let at = (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) in
      raise (Syntax.Error (Unexpected_character { at; character = c })) }
