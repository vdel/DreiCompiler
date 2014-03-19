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
  | NOT | AND | OR
  | BIN_NOT | BIN_AND | BIN_OR | BIN_XOR
  | BIN_LSL | BIN_LSR
  | ADD | SUB | MUL | DIV | MOD
	(* mots cles *)
  | INT
  | CLASS | EXTENDS | DEF | VAL
  | WHILE | IF | ELSE | VAR | SET | DO
  | FALSE | TRUE | NEW | THIS | RETURN
  | PRINTINT | PRINTCHAR | READINT | READCHAR
  (* Ajout META LANGAGE *)
  | PRINTASM of string
  | DEF_IGNORE
  (** TABLEAUX **)
  | DIESE
  | LCROCHET | RCROCHET
  | ARRAY_T
  (** /TABLEAUX **)

val string_of_token : token -> string

val token_of_keyword : string -> token

