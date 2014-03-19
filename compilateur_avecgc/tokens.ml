type token = 
    (* tokens de controle *)
  | BAD of char | EOF
	(* tokens classiques *)
  | IDENT of string
  | NUMBER of Int32.t
  | STRING of string
  | LPAREN | RPAREN
  | LBRACE | RBRACE
  | DOT | COMMA | COLON | SEMICOLON
  | EQUAL
  | EQ | NE | LT | LE | GT | GE
  | NOT     | AND     | OR
  | BIN_NOT | BIN_AND | BIN_OR | BIN_XOR
  | BIN_LSL | BIN_LSR 
  | ADD | SUB | MUL | DIV | MOD
	(* mots cles *)
  | INT
  | CLASS | EXTENDS | DEF | VAL
  | WHILE | IF | ELSE | VAR | SET | DO
  | FALSE | TRUE | NEW | THIS | RETURN
  | PRINTINT | PRINTCHAR | READINT | READCHAR   
  (** META LANGAGE **)
  | PRINTASM of string 
  | DEF_IGNORE 
  (** Tableaux **)
  | DIESE
  | LCROCHET | RCROCHET
  | ARRAY_T
	
let keywords = 
  let keywords = Hashtbl.create 10 in
  let add_keyword str tok = Hashtbl.add keywords str tok in
    add_keyword "Array" ARRAY_T;
    add_keyword "Int" INT;
    add_keyword "class" CLASS;
    add_keyword "extends" EXTENDS;
    add_keyword "def" DEF;
    add_keyword "asm" DEF_IGNORE;
    add_keyword "val" VAL;
    add_keyword "while" WHILE;
    add_keyword "if" IF;
    add_keyword "else" ELSE;
    add_keyword "var" VAR;
    add_keyword "set" SET;
    add_keyword "do" DO;
    add_keyword "false" FALSE;
    add_keyword "true" TRUE;
    add_keyword "new" NEW;
    add_keyword "this" THIS;
    add_keyword "return" RETURN;
    add_keyword "printInt" PRINTINT;
    add_keyword "printChar" PRINTCHAR;
    add_keyword "readInt" READINT;
    add_keyword "readChar" READCHAR;
    keywords
      
let find_keyword tok = 
  Hashtbl.fold  (fun s t' str -> if tok = t' then Some(s) else str) keywords None
    
let string_of_token = function
  | BAD(c) -> "<bad character of code:"^(string_of_int (Char.code c))^">"
  | EOF -> "<eof>"
  | IDENT(s) -> "IDENT("^s^")"
  | NUMBER(n) -> "NUMBER("^(Int32.to_string n)^")"
  | STRING(s) -> "STRING("^s^")"
  (** TABLEAUX **)
  | DIESE -> "#"
  | LCROCHET -> "["
  | RCROCHET -> "]"
  (** /TABLEAUX **)
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | DOT -> "." 
  | COMMA -> ","
  | COLON -> ":"
  | SEMICOLON -> ";"
  | EQUAL -> "="
  | EQ -> "=="
  | NE -> "!="
  | LT -> "<"
  | LE -> "<="
  | GT -> ">"
  | GE -> ">="
  | NOT -> "!"
  | AND -> "&&"
  | OR -> "||"
  | BIN_NOT -> "~"
  | BIN_AND -> "&"
  | BIN_OR -> "|"
  | BIN_XOR -> "^"
  | BIN_LSL -> "<<"
  | BIN_LSR -> ">>"
  | ADD -> "+"
  | SUB -> "-"
  | MUL -> "*"
  | DIV -> "/"
  | MOD -> "%"
  | PRINTASM (s) -> "@"^s^"@"
  | t ->
      begin
	match find_keyword t with
	  | Some(s) -> s
	  | None -> "<?>"
      end

let token_of_keyword str = 
  try Hashtbl.find keywords str 
  with Not_found -> IDENT(str)
