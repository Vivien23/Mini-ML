{
  open Parser
exception Eof
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | eof                              { EOF } (* fin du fichier *)
  | '+'                              { PLUS }
  | '*'                              { TIMES }
  | '-'                              { MINUS }
  | '/'                              { DIV }
  | '('                              { LPAREN }
  | ')'                              { RPAREN }
  | ['0'-'9']+ as s                  { INT (int_of_string s) }
  | "true"|"false" as s              { BOOL (bool_of_string s) }
  | ">="                             { GEQ }
  | "<="                             { LEQ }
  | "<>"                             { DIFF }
  | '='                              { EQUAL }
  | '<'                              { LESS }
  | '>'                              { GREATER }
  | "||"                             { OR }
  | "&&"                             { AND }
  | "not"                            { NOT } 
  | "if"                             { IF }
  | "then"                           { THEN }
  | "else"                           { ELSE }
  | "let"                            { LET }
  | "in"                             { IN }
  | ";;"                             { IN_SEP }
  | "fun"                            { FUN }
  | "->"                             { ARROW }
  | "ref"                            { REF }
  | ":="                             { ASSOC }
  | "!"                              { BANG }
  | ";"                              { SEP }
  | "begin"                          { BEGIN }
  | "end"                            { END }
  | ","                              { COMMA }
  | "function"                       { FUNCTION }
  | "match"                          { MATCH }
  | "with"                           { WITH }
  | "|"                              { PIPE }
  | ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as s { VAR (s) }

