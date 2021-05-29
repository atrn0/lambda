rule main = parse
| (* ignore spacing and newline characters *)
  [' ' '\009' '\012']+     { main lexbuf }
| "\\" { Parser.BACKSLASH }
| "λ" { Parser.LAMBDA }
| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "." { Parser.DOT }
| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { Parser.ID (Lexing.lexeme lexbuf) }
| ['\n'] { Parser.EOL }
