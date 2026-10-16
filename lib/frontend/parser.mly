%{
open Text.Located
open Lang.Source
%}

%token <int> INT
%token <string> IDENT
%token ZERO PROJ SUCC RHO LET
%token EQUAL DOT COMMA LPAREN RPAREN LBRACK RBRACK
%token EOF

%start <Lang.Source.file> main

%%

main:
  | ds = list(definition) r = option(exp) EOF { { definitions = ds; result = r } }

definition:
  | LET n = IDENT EQUAL p = prim { { name = { at = $loc(n); v = n }; body = p } }
  | LET n = reserved EQUAL prim
    { raise (Syntax.Error (Reserved_name { at = $loc(n); name = n })) }

reserved:
  | ZERO { "zero" }
  | PROJ { "proj" }
  | SUCC { "succ" }
  | RHO { "rho" }
  | LET { "let" }

exp:
  | n = INT { { at = $loc; v = SNat n } }
  | p = prim { { at = $loc; v = SFun p } }
  | p = prim es = tuple(exp) { { at = $loc; v = SApp (p, es) } }
  | p = prim e = arg { { at = $loc; v = SApp (p, [e]) } }

arg:
  | n = INT { { at = $loc; v = SNat n } }
  | p = bare { { at = $loc; v = SFun p } }
  | p = bare es = tuple(exp) { { at = $loc; v = SApp (p, es) } }
  | p = bare e = arg { { at = $loc; v = SApp (p, [e]) } }

prim:
  | p = prim DOT ps = tuple(prim) { { at = $loc; v = SComp (p, ps) } }
  | p = prim DOT q = bare { { at = $loc; v = SComp (p, [q]) } }
  | p = atom { p }

atom:
  | p = bare { p }
  | LPAREN p = prim RPAREN { p }

bare:
  | ZERO LBRACK n = INT RBRACK { { at = $loc; v = SZero n } }
  | PROJ LBRACK n = INT COMMA i = INT RBRACK { { at = $loc; v = SProj (n, i) } }
  | SUCC { { at = $loc; v = SSucc } }
  | x = IDENT { { at = $loc; v = SVar x } }
  | RHO LBRACK n = INT RBRACK LPAREN f = prim COMMA g = prim RPAREN { { at = $loc; v = SRho (n, f, g) } }

%inline tuple(X):
  | LPAREN xs = separated_list(COMMA, X) RPAREN { xs }
