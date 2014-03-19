open Ast;;

let rec print_type t = 
  match t with
(** TABLEAUX **)
    | Array_Type(t2) -> Printf.printf "Array"
(** /TABLEAUX **)
    | Class_Type(n) -> Printf.printf "%s" (get_name n)
    | Int_Type             -> Printf.printf "Int"
    | _                    -> ()
and print_formal (Formal(_, n, f_type)) = 
  Printf.printf "%s : " (get_name n);
  print_type f_type
and print_fielddecl (FieldDecl(pos, form)) =
  Printf.printf("val ");
  print_formal(form);
  Printf.printf(";\n");
and print_methodheader b (MethodHeader(_, n, f_list)) =
  let rec aux = function
    | []   -> ()
    | [f]  -> print_formal f;
    | f::q -> 
	begin
	  print_formal f;
	  Printf.printf(",");
	  aux q
	end
   in
    if b then Printf.printf "def %s(" (get_name n)    
    else Printf.printf "asm %s(" (get_name n);
    aux f_list;
    Printf.printf(")")
and print_methoddef methode = match methode with
  (** Ajout META LANGAGE **)
  | Method_Def_ignore_Long (_, m_header, m_type, m_stats) -> print_methodheader false m_header;
                                                     Printf.printf " : ";
                                                     print_type m_type;
                                                     Printf.printf " = ";
                                                     print_statements m_stats;
                                                     Printf.printf ";\n";                                       
  | Method_Def_ignore_Short (_, m_header, m_stats)       -> print_methodheader false m_header;
                                                     Printf.printf " {\n";
                                                     print_statements m_stats;
                                                     Printf.printf "}\n"


  | Method_Def_Long (_, m_header, m_type, m_expr) -> print_methodheader true m_header;
                                                     Printf.printf " : ";
                                                     print_type m_type;
                                                     Printf.printf " = ";
                                                     print_expression m_expr;
                                                     Printf.printf ";\n";
  | Method_Def_Short (_, m_header, m_stats)       -> print_methodheader true m_header;
                                                     Printf.printf " {\n";
                                                     print_statements m_stats;
                                                     Printf.printf "}\n"
and print_member mem = match mem with
  | Mem_Field(field)   -> print_fielddecl(field)
  | Mem_Method(methode) -> print_methoddef(methode)
and print_classdecl (ClassDecl(pos, n, ext_id, members)) =
  let rec aux = function
    | []   -> ()
    | m::q -> print_member m;
        aux q
  in
    Printf.printf "class %s" (get_name n);
    begin
      match ext_id with
	| None                 -> ()
	| Some(ext_n) -> Printf.printf " extends %s" (get_name ext_n)
    end;
    Printf.printf(" {\n");
    aux members;
    Printf.printf("}\n\n")
and print_vardecl (VarDecl(_, form, expr)) = 
  Printf.printf("var ");
  print_formal(form);
  Printf.printf("=");
  print_expression(expr);
  Printf.printf(";\n")
and print_statements_t s = match s with
  | Stat_Var_Decl(s_vardecl) -> print_vardecl s_vardecl
  | Stat_Statement(s_stat)   -> print_statement s_stat
and print_statements (Statements(_, s_list)) = 
  let rec aux = function
    | []   -> ()
    | s::q -> print_statements_t s;
              aux q
  in
    aux s_list
and print_statement s = match s with
  | Print_Asm (s)                     -> print_string ("@"^s^"@");

  | Sta_While(_, s_expr, s_stat)      -> Printf.printf "while(";
                                         print_expression s_expr;
                                         Printf.printf ")";
                                         print_statement s_stat;
  | Sta_If(_, s_expr, s_sta1, s_sta2) -> Printf.printf "if(";
                                         print_expression s_expr;
                                         Printf.printf ")\n";
                                         print_statement s_sta1;
                                         Printf.printf "else\n";
                                         print_statement s_sta2;
					 Printf.printf "";
  | Sta_Set(_, s_n, s_expr)    -> Printf.printf "set %s=" (get_name s_n);
                                         print_expression s_expr;
                                         Printf.printf ";\n";
  | Sta_Do(_, s_expr)                 -> Printf.printf "do ";
                                         print_expression s_expr;
                                         Printf.printf ";\n";
  | Sta_PrintInt(_, s_expr)           -> Printf.printf "printInt(";
                                         print_expression s_expr;
                                         Printf.printf ");\n";
  | Sta_PrintChar(_, s_expr)          -> Printf.printf "printChar(";
                                         print_expression s_expr;
                                         Printf.printf ");\n";
  | Sta_Bloc(_, s_stats)              -> Printf.printf "{\n";
                                         print_statements s_stats;
                                         Printf.printf "}\n"
and print_expression (Expression(_, g_sumexpr, opt)) = 
  begin
    match opt with
      | Some(op, d_sumexpr) -> 
	  print_string "(";
	  print_sumexpression g_sumexpr;  
	  print_compop op;
	  print_sumexpression d_sumexpr;
	  print_string ")"
      | None -> print_sumexpression g_sumexpr;  
  end;

and print_compop op =
  match op with
    | CO_EQ _ -> print_string " == "
    | CO_NE _ -> print_string " != "
    | CO_LT _ -> print_string " < "
    | CO_GT _ -> print_string " > "
    | CO_LE _ -> print_string " <= "
    | CO_GE _ -> print_string " >= "
and print_sumexpression (SumExpression(_,terme,l)) =
  let rec aux l = 
    match l with
      | (op,terme)::q -> 
	  print_sumop op;
	  print_term terme;
	  aux q;
      | [] -> ()
  in
    match l with
      | [] ->  print_term terme;
      | _ -> 
	  begin
	    print_string "(";
	    print_term terme;
	    aux l;
	    print_string ")"
	  end
and print_sumop op =
  match op with
    | SO_ADD _ -> print_string "+"
    | SO_SUB _ -> print_string "-"
    | SO_BIN_OR _ -> print_string "|"
    | SO_BIN_XOR _ -> print_string "^"
and print_signedfactor (SignedFactor(_,opt,factor)) =
  begin
    match opt with
      | Some c -> 
	  begin
	    print_negateop c;
	    print_string "(";
	    print_factor factor;
	    print_string ")"
	  end
      | None -> print_factor factor
  end  
and print_term (Term(_,signedfact,l)) =
  let rec aux l = 
    match l with
      | (op,signedfact)::q -> 
	  print_prodop op;
	  print_signedfactor signedfact;
	  aux q;
      | [] -> ()
  in
    match l with
      | [] -> print_signedfactor signedfact;
      | _ -> 
	  print_string "(";
	  print_signedfactor signedfact;	  
	  aux l;
	  print_string ")"
and print_negateop op=
  match op with
    | NO_SUB _ -> print_string "-"
    | NO_NOT _ -> print_string "!"
    | NO_BIN_NOT _ -> print_string "~"
and print_prodop op = 
  match op with
    (** TABLEAUX **)
    | PO_DIESE _ -> print_string " # "
    (** /TABLEAUX **)
    | PO_MUL _ -> print_string " * "
    | PO_DIV _ -> print_string " / "
    | PO_MOD _ -> print_string " % "
    | PO_AND _ -> print_string " && "
    | PO_BIN_AND _ -> print_string " & "
    | PO_BIN_LSL _ -> print_string " << "
    | PO_BIN_LSR _ -> print_string " >> "
and print_simplefactor fact = 
  match fact with
    (** TABLEAUX **)
    | Fact_Array (_,l) -> print_array l
    (** /TABLEAUX **)
    | Fact_Ident (_,n) -> print_string (get_name n)
    | Fact_Number (_,i) -> print_string (Int32.to_string i)
    | Fact_RD_Int _ -> print_string "readInt"
    | Fact_RD_Char _ ->print_string "readChar"
    | Fact_Expr (_,expr) -> print_expression expr;
    | Fact_Body (_,stat,expr) ->
	begin
	  print_string "{";
	  print_statements stat;
	  print_string "return ";
	  print_expression expr;
	  print_string "\n}";
	end
    | Fact_New (_,n,args) ->
	begin
	  Printf.printf "new %s" (get_name n);
	  print_arguments args;
	end
and print_factor (Factor(_,simplefact,l))=
  let rec aux l =
    match l with
      | (n,None)::q -> 
	  begin
	    Printf.printf ".%s" (get_name n);
	    aux q
	  end
      | (n,Some arg)::q ->
	  begin
	    Printf.printf ".%s" (get_name n);
	    print_arguments arg;
	    aux q
	  end
      | [] -> ()
  in
    print_simplefactor simplefact;
    aux l

(** TABLEAUX **)
and print_array l = 
  print_string "[";
  print_array_inner l;
  print_string "]"

and print_array_inner (Arguments(_,(Expressions(_,liste))))=
  let rec aux l = match l with
  | [] -> ()
  | [e] -> print_expression e
  | e::q -> print_expression e; print_string ";";
            aux q
  in
    aux liste

(** /TABLEAUX **)

and print_arguments (Arguments(_,exprs))=
  print_string "(";
  print_expressions exprs;
  print_string ")";
and print_expressions (Expressions(_,l))=
  match l with 
    | [] -> ()
    | [e] -> print_expression e
    | t::q -> 
	begin
	  print_expression t;
	  List.iter (fun e -> print_string ", "; print_expression e) q
	end
and print_tree (Program (l,stat)) =
  List.iter print_classdecl l;
  print_statement stat
;;
