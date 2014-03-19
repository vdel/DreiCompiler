let is_eof = function
  | None -> true
  | Some(_) -> false

let is_char c = function
  | None -> false
  | Some(c') -> c == c'

let is_not f c = not (f c)

let ( ||| ) f f' c = (f c) || (f' c)
 
let is_digit = function
  | None -> false
  | Some(c) -> (match c with '0'..'9' -> true | _ -> false)

let is_letter = function
  | None -> false
  | Some(c) -> (match c with 'a'..'z' | 'A'..'Z' -> true | _ -> false)

let is_whitespace = (is_char ' ') ||| (is_char '\t') ||| (is_char '\012') ||| (is_char '\n')

let string_such_that chars p =
  let buf = Buffer.create 10 in
    while(p (CharReader.current_char chars)) do
      match CharReader.current_char chars with
	| Some(c) -> 
	    begin
	      CharReader.next_char chars;
	      Buffer.add_char buf c
	    end
	| _ -> ()
    done;
    Buffer.contents buf

let string_such_that_first chars c1 p =
  let buf = Buffer.create 10 in
    let _ = Buffer.add_char buf c1 in
    while(p (CharReader.current_char chars)) do
      match CharReader.current_char chars with
	| Some(c) -> 
	    begin
	      CharReader.next_char chars;
	      Buffer.add_char buf c
	    end
	| _ -> ()
    done;
    Buffer.contents buf


type t = { chars: CharReader.t; mutable token: Tokens.token; mutable start: int * int }

let current_token o = o.token

let rec read_token o = 
  let accept_token tok = 
    CharReader.next_char o.chars;
    tok
  in
    if is_whitespace (CharReader.current_char o.chars) then 
      begin
	CharReader.next_char o.chars;
	read_token o
      end
    else
      begin
	o.start <- CharReader.get_position o.chars;
	match CharReader.current_char o.chars with
	    None -> Tokens.EOF
	  | Some('/') ->
	      begin
		CharReader.next_char o.chars;
		match CharReader.current_char o.chars with
		  | Some('/') ->
		      while
			CharReader.current_char o.chars <> Some('\n') 
			&& CharReader.current_char o.chars <> None 
		      do
			CharReader.next_char o.chars
		      done;
		      read_token o

                  | Some('*') -> 
                      let ok = ref 0 in
                      while
			!ok <> 2 
			&& CharReader.current_char o.chars <> None 
		      do begin                        
			CharReader.next_char o.chars;
                        if (CharReader.current_char o.chars = Some('/')
                            && !ok = 1) then ok := 2
                        else if (CharReader.current_char o.chars = Some('*')) then ok := 1
                        else ok := 0
		      end done;
		      CharReader.next_char o.chars;
		      read_token o

		  | _ -> Tokens.DIV
	      end

	  | Some('@') ->
	      let _ = CharReader.next_char o.chars in
              let is_premature_end = is_eof ||| (is_char '\n') in
              let str = string_such_that o.chars (is_not ((is_char '@') ||| is_premature_end)) in
                if is_premature_end (CharReader.current_char o.chars) then
                  Report.fail_at_pos o.start "printAsm: unterminated string, give me a @"
                else accept_token (Tokens.PRINTASM("  "^str))
    (** TABLEAUX **)
    | Some('[') -> accept_token (Tokens.LCROCHET)
    | Some(']') -> accept_token (Tokens.RCROCHET)
        (*let _ = CharReader.next_char o.chars in
               (** A VOir *
                begin
                  let rec aux () =
                    let c =  begin match CharReader.current_char o.chars with
                      | None -> Report.fail_at_pos o.start "Array: Horreur !"
                      | Some(a) -> a end
                    in 
                    if (is_letter(Some(c))) then 
                    begin
                      let _ = CharReader.next_char o.chars in
                      let str = string_such_that_first o.chars c ((is_letter ||| is_digit ||| (is_char '_'))) in
                      let c1 = CharReader.current_char o.chars in

                      if (not((is_char ';' ||| is_char ']') c1)) then 
                        Report.fail_at_pos o.start "Array: Givet me a ] or ;..."
                      else if (is_char ';' c1) then (IDENT(str))::(aux ()) 
                      else if (is_char ']' c1) then [IDENT(str)]
                      else Report.fail_at_pos o.start "Array: Give me a ] or ;..."
                    end

                    (* Si c'est un nombre *)
                    else if (is_digit(Some(c))) then
                    begin
	                    let str = string_such_that o.chars is_digit in
		                  begin
		                    try
		                      Tokens.NUMBER(Int32.of_string string)
		                    with Failure(_) -> 
		                      Report.fail_at_pos o.start ("Integer too big: "^string);
		                      Tokens.NUMBER(Int32.zero)
		                  end
                    end 
                    else
                      Report.fail_at_pos o.start "Array: Give me an argument"
                  in
                    accept_token (Tokens.ARRAY(aux()))
                end*)
    (** FIN TABLEAU **)*) 
	  | Some('-') -> accept_token Tokens.SUB
	  | Some('+') -> accept_token Tokens.ADD
(** TABLEAUX **)
	  | Some('#') -> accept_token Tokens.DIESE
(** /TABLEAUX **)
	  | Some('*') -> accept_token Tokens.MUL
	  | Some('%') -> accept_token Tokens.MOD
	  | Some('(') -> accept_token Tokens.LPAREN
	  | Some(')') -> accept_token Tokens.RPAREN
	  | Some('{') -> accept_token Tokens.LBRACE
	  | Some('}') -> accept_token Tokens.RBRACE
	  | Some('.') -> accept_token Tokens.DOT
	  | Some(',') -> accept_token Tokens.COMMA
	  | Some(':') -> accept_token Tokens.COLON
	  | Some(';') -> accept_token Tokens.SEMICOLON
          | Some('^') -> accept_token Tokens.BIN_XOR
          | Some('~') -> accept_token Tokens.BIN_NOT
	  | Some('=') -> 
	      begin
		let _ = CharReader.next_char o.chars in
		match CharReader.current_char o.chars with
		  | Some('=') -> accept_token Tokens.EQ
		  | _ -> Tokens.EQUAL
	      end
	  | Some('<') -> 
	      begin
		let _ = CharReader.next_char o.chars in
		match CharReader.current_char o.chars with
		  | Some('=') -> accept_token Tokens.LE
                  | Some('<') -> accept_token Tokens.BIN_LSL
		  | _ -> Tokens.LT
	      end
	  | Some('>') -> 
	      begin
		let _ = CharReader.next_char o.chars in
		match CharReader.current_char o.chars with
		  | Some('=') -> accept_token Tokens.GE
                  | Some('>') -> accept_token Tokens.BIN_LSR
		  | _ -> Tokens.GT
	      end
	  | Some('&') ->
	      begin
		let _  = CharReader.next_char o.chars in
		  match CharReader.current_char o.chars with
		    | Some('&') -> accept_token Tokens.AND
		    | _ -> Tokens.BIN_AND
	      end
	  | Some('|') ->
	      begin
		let _ = CharReader.next_char o.chars in
		  match CharReader.current_char o.chars with
		    | Some('|') -> accept_token Tokens.OR
		    | _ -> Tokens.BIN_OR
	      end
	  | Some('!') -> 
	      begin
		let _ = CharReader.next_char o.chars in
		  match CharReader.current_char o.chars with
		    | Some('=') -> accept_token Tokens.NE
		    | _ -> Tokens.NOT
	      end

	  | Some('"') ->
	      let _ = CharReader.next_char o.chars in
              let is_premature_end = is_eof ||| (is_char '\n') in
              let string = string_such_that o.chars (is_not ((is_char '"') ||| is_premature_end)) in
                if is_premature_end (CharReader.current_char o.chars) then
                  Report.fail_at_pos o.start "unterminated string"
                else accept_token (Tokens.STRING(string))
	  | c when is_letter(c) ->
	      let string = string_such_that o.chars (is_letter ||| is_digit ||| (is_char '_')) in
		Tokens.token_of_keyword string
	  | c when is_char '0' c -> accept_token (Tokens.NUMBER(Int32.zero))
	  | c when is_digit c ->
	      let string = string_such_that o.chars is_digit in
		begin
		  try
		    Tokens.NUMBER(Int32.of_string string)
		  with Failure(_) -> 
		    Report.fail_at_pos o.start ("Integer too big: "^string);
		    Tokens.NUMBER(Int32.zero)
		end
	  | Some(c) -> 
              Report.fail_at_pos o.start ("bad character of code "^(string_of_int (Char.code c)));
	      accept_token (Tokens.BAD(c))
      end

let next_token o = o.token <- read_token o

let get_position o = o.start

let new_scanner chars = 
  let o = { chars = chars; 
	    token = Tokens.EOF;
	    start = (0,0) }
  in 
    next_token o;
    o

let main filename =
  let in_channel = if filename = "--" then stdin else open_in "gc.drei" in
  let chars = CharReader.charReader_of_in_channel in_channel in
  let scanner = new_scanner chars in
    while current_token scanner <> Tokens.EOF do
      print_string (Tokens.string_of_token (current_token scanner));
      print_newline();
      next_token scanner
    done;

  let in_channel = if filename = "--" then stdin else open_in filename in
  let chars = CharReader.charReader_of_in_channel in_channel in
  let scanner = new_scanner chars in
    while current_token scanner <> Tokens.EOF do
      print_string (Tokens.string_of_token (current_token scanner));
      print_newline();
      next_token scanner
    done
