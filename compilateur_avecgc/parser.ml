open Tokens

let accept_token scanner t =
  let t' = Scanner.current_token scanner in
    if t = t' then Scanner.next_token scanner
    else Report.fail_at_pos (Scanner.get_position scanner) ("expected token: "^(string_of_token t)^" but found:"^(string_of_token t'))

let check_token scanner t =
  let t' = Scanner.current_token scanner in
    if t = t' then (Scanner.next_token scanner; true)
    else false

let parse_ident scanner =
  match Scanner.current_token scanner with
    | IDENT(s) -> Scanner.next_token scanner; Ast.mk_name s
    | t -> Report.fail_at_pos (Scanner.get_position scanner) ("expected an identifier but found:"^(string_of_token t))

let rec parse_type scanner = match Scanner.current_token scanner with
(** TABLEAUX **)
  | ARRAY_T -> Scanner.next_token scanner; Ast.Array_Type(parse_type scanner)
(**/TABLEAUX **)
  | INT -> Scanner.next_token scanner; Ast.Int_Type
  | _ ->(*Scanner.next_token scanner;*) Ast.Class_Type(parse_ident scanner)

let parse_formal scanner =
  let pos=Scanner.get_position scanner in
  let ident = parse_ident scanner in
  Ast.Formal(pos,ident,(accept_token scanner COLON;parse_type scanner))

let rec parse_formals scanner = 
  let rec aux () = 
    if check_token scanner COMMA then 
      let t=parse_formal scanner in t::(aux ())
    else
      []
  in
    match Scanner.current_token scanner with
      | IDENT(_) -> let t=parse_formal scanner in t::(aux ())
      | _ -> []

let parse_comp_op scanner =
  let pos=Scanner.get_position scanner in
  if check_token scanner EQ then Ast.CO_EQ(pos)
  else if check_token scanner NE then Ast.CO_NE(pos)
  else if check_token scanner LT then Ast.CO_LT(pos)
  else if check_token scanner LE then Ast.CO_LE(pos)
  else if check_token scanner GT then Ast.CO_GT(pos)
  else (accept_token scanner GE; Ast.CO_GE(pos))

let rec parse_program scanner =
  let rec aux () =
    if Scanner.current_token scanner = CLASS then
      let t=parse_class scanner in t::(aux ())
    else
      []
  in
  let classes = aux () in Ast.Program(classes, parse_statement scanner)

and parse_class scanner =
  let pos=Scanner.get_position scanner in
    accept_token scanner CLASS;
    let ident = parse_ident scanner in
    let ex_ident =  (if check_token scanner EXTENDS then Some(parse_ident  scanner) else None) in
      Ast.ClassDecl(pos, ident, ex_ident, 
            (accept_token scanner LBRACE; 
            let res = parse_members scanner
            in 
              accept_token scanner RBRACE; 
              res))

and parse_members scanner =
  let rec aux () =
    match Scanner.current_token scanner with
      | VAL | DEF | DEF_IGNORE ->
	  let t = (parse_member scanner) in t::(aux ())
      | _ -> []
  in aux ()

and parse_member scanner = 
  if check_token scanner VAL then
    begin
      let pos = Scanner.get_position scanner in
      let res = Ast.Mem_Field(Ast.FieldDecl(pos, parse_formal scanner))
      in
        accept_token scanner SEMICOLON;
        res
    end
  else if (Scanner.current_token scanner = DEF_IGNORE) then
    begin
      accept_token scanner DEF_IGNORE;
      let pos=Scanner.get_position scanner in
      let ident = parse_ident scanner in
      let res = Ast.MethodHeader(pos,ident,(accept_token scanner LPAREN; parse_formals scanner))
      in
        accept_token scanner RPAREN;
        if check_token scanner LBRACE then  
   	  begin
	    let res1 = Ast.Mem_Method(Ast.Method_Def_ignore_Short(pos, res, parse_statements scanner))
            in
	      accept_token scanner RBRACE;
              res1
	  end
        else
  	  begin
	    let type_name = (accept_token scanner COLON; parse_type scanner) in
              Ast.Mem_Method(Ast.Method_Def_ignore_Long(pos,res,type_name,
						 (accept_token scanner EQUAL;
						  let res1 = parse_statements scanner
						  in 
						    accept_token scanner SEMICOLON;
						    res1)))
	  end
      end
  else
    begin
      accept_token scanner DEF;
      let pos=Scanner.get_position scanner in
      let ident = parse_ident scanner in
      let res = Ast.MethodHeader(pos,ident,(accept_token scanner LPAREN; parse_formals scanner))
      in
        accept_token scanner RPAREN;
        if check_token scanner LBRACE then  
   	  begin
	    let res1 = Ast.Mem_Method(Ast.Method_Def_Short(pos, res, parse_statements scanner))
            in
	      accept_token scanner RBRACE;
              res1
	  end
        else
  	  begin
	    let type_name = (accept_token scanner COLON; parse_type scanner) in
              Ast.Mem_Method(Ast.Method_Def_Long(pos,res,type_name,
						 (accept_token scanner EQUAL;
						  let res1 = parse_expression scanner
						  in 
						    accept_token scanner SEMICOLON;
						    res1)))
	  end
      end

and parse_vardecl scanner = 
  let pos=Scanner.get_position scanner in
  accept_token scanner VAR;
    let formal = parse_formal scanner in
      Ast.VarDecl(pos, formal, 
		  (accept_token scanner EQUAL;
		   let res = parse_expression scanner
		   in 
		     accept_token scanner SEMICOLON;
		     res
		  ))

and parse_statements scanner = 
  let rec aux () = 
    match Scanner.current_token scanner with
      | WHILE | IF | SET | DO | PRINTINT | PRINTCHAR | LBRACE ->
	  let t = Ast.Stat_Statement(parse_statement scanner) in t::(aux ())
      | VAR -> 
	  let t = Ast.Stat_Var_Decl(parse_vardecl scanner) in t::(aux ())
      | PRINTASM(s) -> 
          let t = Ast.Stat_Statement(parse_statement scanner) in t::(aux ())

      | _ -> []
  in
  let pos=Scanner.get_position scanner in 
    Ast.Statements(pos,aux ())

and parse_statement scanner =
  let pos=Scanner.get_position scanner in
    match Scanner.current_token scanner with
      | PRINTASM(s) -> Scanner.next_token scanner; Ast.Print_Asm(s)

      | WHILE ->
	  Scanner.next_token scanner;
	  accept_token scanner LPAREN;
	  let expr = parse_expression scanner in
	    Ast.Sta_While(pos,expr,
			  (accept_token scanner RPAREN;
			   parse_statement scanner))
      | IF ->
	  Scanner.next_token scanner;
	  accept_token scanner LPAREN;
	  let expr = parse_expression scanner in
	  let stat = (accept_token scanner RPAREN;
		      parse_statement scanner) in
	    Ast.Sta_If(pos,expr,stat,
		     (if check_token scanner ELSE then parse_statement scanner
                      else Ast.Sta_Bloc(Scanner.get_position scanner, Ast.Statements(Scanner.get_position scanner,[]))))
      | SET ->
	  Scanner.next_token scanner;
	  let ident = parse_ident scanner in
	    Ast.Sta_Set(pos,ident,
			(accept_token scanner EQUAL;
			 let res = parse_expression scanner 
			 in
			   accept_token scanner SEMICOLON; 
			   res))         
      | DO ->
	  Scanner.next_token scanner;
	  Ast.Sta_Do(pos,
		     (let res = parse_expression scanner
		      in
			accept_token scanner SEMICOLON;
			res))
      | PRINTINT ->
	  Scanner.next_token scanner;
	  accept_token scanner LPAREN;
	  Ast.Sta_PrintInt(pos,
			   (let res = parse_expression scanner
			    in
			      accept_token scanner RPAREN;
			      accept_token scanner SEMICOLON;
			      res))
      | PRINTCHAR ->
	  Scanner.next_token scanner;
	  accept_token scanner LPAREN;
	  Ast.Sta_PrintChar(pos,
			    (let res = parse_expression scanner
			     in
			       accept_token scanner RPAREN;
			       accept_token scanner SEMICOLON;
                     res))
      | _ ->
	  accept_token scanner LBRACE;
	  Ast.Sta_Bloc(pos,
		       (let res = parse_statements scanner 
			in
			  accept_token scanner RBRACE; 
			  res))
and invert_list l = 
  List.fold_left (fun a b -> b::a) [] l
and parse_expression scanner = 
  let pos = Scanner.get_position scanner in
  let sum_expr = parse_sum_expression scanner in
  Ast.Expression(pos,sum_expr,
		 (match Scanner.current_token scanner with
		   | EQ | NE | LT | LE | GT | GE ->
		       let comp_op = parse_comp_op scanner in
			 Some (comp_op,parse_sum_expression scanner)
		   | _ -> None))
and signed_factor_of_term_negation term = (* t --> !t *)
  let Ast.Term(pos,_,_)=term in
    Ast.SignedFactor(pos,Some (Ast.NO_NOT pos),Ast.Factor(pos,Ast.Fact_Expr(pos,Ast.Expression(pos,Ast.SumExpression(pos,term,[]),None)),[]))
and t1_or_t2 term1 term2 pos_or = (* e1 || e2 --> !(!(e1) && !(e2)) *)
  let Ast.Term(pos1,_,_)=term1 
  in
    Ast.Term(pos1,signed_factor_of_term_negation(Ast.Term(pos1,signed_factor_of_term_negation term1,[(Ast.PO_AND(pos_or),signed_factor_of_term_negation term2)])),[])   
and parse_sum_expression scanner =
  let pos=Scanner.get_position scanner 
  and t = ref (parse_term scanner) 
  and q = ref [] in
  let rec aux () =
    let pos = Scanner.get_position scanner in
    if check_token scanner ADD then 
      begin
	q := (Ast.SO_ADD (pos),parse_term scanner) :: !q;
	aux ()
      end
    else if check_token scanner SUB then 
      begin
	q := (Ast.SO_SUB (pos),parse_term scanner) :: !q;
	aux ()
      end
    else if check_token scanner BIN_OR then 
      begin
	q := (Ast.SO_BIN_OR (pos),parse_term scanner) :: !q;
	aux ()
      end
    else if check_token scanner BIN_XOR then 
      begin
	q := (Ast.SO_BIN_XOR (pos),parse_term scanner) :: !q;
	aux ()
      end
    else if check_token scanner OR then 
      begin
	begin
	  let pos_or=Scanner.get_position scanner in
	    match !q with
	      | [] -> t:= t1_or_t2 !t (parse_term scanner) (pos_or)
	      | (op,term)::r -> q:=(op,t1_or_t2 term (parse_term scanner) (pos_or))::r
	end;
	aux ()
      end
  in 
    aux();
    Ast.SumExpression (pos,!t,(invert_list !q))
and parse_term scanner = 
  let rec aux () =
    let pos = Scanner.get_position scanner in    
  (** TABLEAUX **) 
  if check_token scanner DIESE then 
  let t = (Ast.PO_DIESE pos, parse_signedfactor scanner) in t::(aux ())
      else 
  (** /TABLEAUX **)
    if check_token scanner MUL then
	let t = (Ast.PO_MUL pos,parse_signedfactor scanner) in t::(aux ())
      else if check_token scanner DIV then 
	let t = (Ast.PO_DIV pos,parse_signedfactor scanner) in t::(aux ())
      else if check_token scanner MOD then 
	let t = (Ast.PO_MOD pos,parse_signedfactor scanner) in t::(aux ()) 
      else if check_token scanner AND then 
	let t = (Ast.PO_AND pos,parse_signedfactor scanner) in t::(aux ())
      else if check_token scanner BIN_AND then 
	let t = (Ast.PO_BIN_AND pos,parse_signedfactor scanner) in t::(aux ())
      else if check_token scanner BIN_LSL then 
	let t = (Ast.PO_BIN_LSL pos,parse_signedfactor scanner) in t::(aux ())
      else if check_token scanner BIN_LSR then 
	let t = (Ast.PO_BIN_LSR pos,parse_signedfactor scanner) in t::(aux ())
      else []
  in 
  let pos = Scanner.get_position scanner in
  let factor = parse_signedfactor scanner in
    Ast.Term(pos,factor,aux ())
and parse_signedfactor scanner = 
  let pos = Scanner.get_position scanner
  in
    if check_token scanner NOT then Ast.SignedFactor(pos,Some (Ast.NO_NOT pos),parse_factor scanner)
    else if check_token scanner BIN_NOT then Ast.SignedFactor(pos,Some (Ast.NO_BIN_NOT pos),parse_factor scanner) 
    else if check_token scanner SUB then Ast.SignedFactor(pos,Some (Ast.NO_SUB pos),parse_factor scanner) 
    else Ast.SignedFactor(pos,None,parse_factor scanner)
and parse_factor scanner = 
  let rec aux () =
    if check_token scanner DOT then
      let ident = parse_ident scanner in
      let t = (ident,if Scanner.current_token scanner = LPAREN then Some (parse_params scanner) else None) in t::(aux ())
    else
      []
  in
  let pos = Scanner.get_position scanner in
  let factor = parse_simple_factor scanner in
    Ast.Factor(pos,factor,aux ())

and expr_of_simplefactor sf =
  let pos = match sf with
    | Ast.Fact_Ident(p,_)
    (** TABLEAUX **)
    | Ast.Fact_Array(p,_)
    (** /TABLEAUX **)
    | Ast.Fact_Number(p,_)
    | Ast.Fact_RD_Int(p)
    | Ast.Fact_RD_Char(p) 
    | Ast.Fact_Expr(p,_)
    | Ast.Fact_Body(p,_,_)
    | Ast.Fact_New(p,_,_) -> p
  in
    Ast.Expression(pos,Ast.SumExpression(pos,Ast.Term(pos,Ast.SignedFactor(pos,None,Ast.Factor(pos,sf,[])),[]),[]),None)
and simple_factor_of_string pos s =
  let t=String.length s in 
  let rec aux i =
    if i=t then 
      Ast.Fact_New(pos,(Ast.mk_name "Nil"),Ast.Arguments(pos,Ast.Expressions(pos,[])))
    else
      Ast.Fact_New(pos,(Ast.mk_name "Cons"),Ast.Arguments(pos,Ast.Expressions(pos,[expr_of_simplefactor (Ast.Fact_Number(pos,Int32.of_int (int_of_char s.[i])));expr_of_simplefactor (aux(i+1))])))
  in
    aux 0
and parse_simple_factor scanner =
  let pos = Scanner.get_position scanner in 
    match Scanner.current_token scanner with
      (** TABLEAUX **)
      | LCROCHET  ->
          Ast.Fact_Array(pos, parse_array scanner)
 
      (** /TABLEAUX **)
      | IDENT(s)  -> Scanner.next_token scanner; Ast.Fact_Ident(pos,(Ast.mk_name s))
      | THIS      -> Scanner.next_token scanner; Ast.Fact_Ident(pos,(Ast.mk_name "this"))
      | NUMBER(n) -> Scanner.next_token scanner; Ast.Fact_Number(pos,n)
      | STRING(s) -> Scanner.next_token scanner; simple_factor_of_string pos s
      | TRUE      -> Scanner.next_token scanner; Ast.Fact_Number(pos,Int32.of_int 1)
      | FALSE     -> Scanner.next_token scanner; Ast.Fact_Number(pos,Int32.of_int 0)
      | READINT   -> Scanner.next_token scanner; Ast.Fact_RD_Int(pos)
      | READCHAR  -> Scanner.next_token scanner; Ast.Fact_RD_Char(pos)
      | LPAREN    ->
	  Scanner.next_token scanner;
	  let x = Ast.Fact_Expr(pos,parse_expression scanner) in accept_token scanner RPAREN; x
      | LBRACE    ->
	  Scanner.next_token scanner;
	  let stat = parse_statements scanner in
	  let x = Ast.Fact_Body(pos,stat,(accept_token scanner RETURN;parse_expression scanner)) in accept_token scanner RBRACE; x
      | _ -> 
	  accept_token scanner NEW;
	  let ident = parse_ident scanner in 
	  Ast.Fact_New(pos,ident,parse_params scanner)

(** tABLEAUX **)
and parse_array scanner = 
  let rec aux () = 
    if check_token scanner SEMICOLON then 
      let t = (parse_expression scanner) in t::(aux())
    else
      []
  in
  let pos = Scanner.get_position scanner in     
    accept_token scanner LCROCHET;
    Ast.Arguments(pos,Ast.Expressions(pos,
				      (if check_token scanner RCROCHET then []
				       else 
					   let t = parse_expression scanner in
					   let x = t::(aux()) 
					   in 
					     accept_token scanner RCROCHET;
					     x
				      )))
(** /TABLEAUX **)

and parse_params scanner = 
  let rec aux () = 
    if check_token scanner COMMA then 
      let t = (parse_expression scanner) in t::(aux())
    else
      []
  in
  let pos = Scanner.get_position scanner in     
    accept_token scanner LPAREN;
    Ast.Arguments(pos,Ast.Expressions(pos,
				      (if check_token scanner RPAREN then []
				       else 
					   let t = parse_expression scanner in
					   let x = t::(aux()) 
					   in 
					     accept_token scanner RPAREN;
					     x
				      )))

let parse scanner = 
  let x = parse_program scanner in accept_token scanner EOF; x

let main affich filename =
  let in_channel = if filename = "--" then stdin else open_in filename in
  let chars = CharReader.charReader_of_in_channel in_channel in
  let scanner = Scanner.new_scanner chars in
    if affich then
      begin
	Print.print_tree (parse scanner)
      end
    else
      let _ = parse scanner in print_string "Parsing successful\n"



	
  
				 
