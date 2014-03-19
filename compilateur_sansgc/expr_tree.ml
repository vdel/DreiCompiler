open Ast

type bin_op=
  |COMP_OP of Ast.compop
  |SUM_OP  of Ast.sumop
  |PROD_OP of Ast.prodop

type un_op=
  |NEGATE_OP of Ast.negateop

type member_tree =
  |FIELD  of Ast.name
  |METHOD of Ast.name*(expr_tree list)
and expr_tree=
  |BIN_OP of bin_op*expr_tree*expr_tree
  |UN_OP  of un_op*expr_tree
  |CONST  of Int32.t
  |RD_INT
  |RD_CHAR  
  |VAR    of Ast.name
  |BODY   of Ast.statements * expr_tree
  |NEW    of Ast.name*(expr_tree list)
  |OBJECT of expr_tree * member_tree
  |ARRAY  of expr_tree * expr_tree
  |NEW_ARRAY of expr_tree list

(**convertit une Ast.expression en expr_tree**)
let rec expression2tree (Expression(_,sumexpr1,op))=
  match op with
    |None -> sumexpression2tree sumexpr1
    |Some (comp_op,sumexpr2) -> BIN_OP (COMP_OP comp_op,sumexpression2tree sumexpr1,sumexpression2tree sumexpr2)
(**convertit une Ast.sumexpression en expr_tree**)
and sumexpression2tree (SumExpression(_,t1,l))=
  let tree=ref (term2tree t1) in
  let rec aux l =
  match l with
    |[] -> !tree
    |(sum_op,t2)::q -> tree:=BIN_OP(SUM_OP sum_op, !tree,term2tree t2);
                       aux q                         
  in
    aux l
(**convertit un Ast.term en expr_tree**)
and term2tree (Term(_,sf1,l))=
  let tree=ref (signedfactor2tree sf1) in
  let rec aux l =
  match l with
    |[] -> !tree
    |(prod_op,sf2)::q -> 
    begin
      match prod_op with 
        | PO_DIESE _ -> tree:=ARRAY(!tree,signedfactor2tree sf2); aux q
        | _ ->
        begin
          match q with
            |(PO_DIESE _,sf3)::q2 -> tree:=BIN_OP(PROD_OP prod_op, !tree,ARRAY(signedfactor2tree sf2,signedfactor2tree sf3)); aux q2
            | _ -> tree:=BIN_OP(PROD_OP prod_op, !tree,signedfactor2tree sf2); aux q 
        end
    end
  in
    aux l
(**convertit un Ast.signedfactor en expr_tree**)
and signedfactor2tree (SignedFactor(_,negate_op,factor))=
  match negate_op with
    |None -> factor2tree factor
    |Some neg_op-> UN_OP(NEGATE_OP neg_op,factor2tree factor)
(**convertit un Ast.factor en expr_tree**)
and factor2tree (Factor(_,simplefactor,l))=
  let tree = ref (simplefactor2tree simplefactor) in 
  let rec aux l=
  match l with
    |[] -> !tree
    |(name,args)::q -> tree:=OBJECT(!tree,
                         match args with
                           |None           -> FIELD name
                           |Some arguments -> METHOD (name,arguments2tree arguments)); 
                       aux q
  in
    aux l
(**convertit un Ast.simplefactor en expr_tree**)
and simplefactor2tree sf=
  match sf with
    | Fact_Array(_, args) -> NEW_ARRAY (arguments2tree args)
    | Fact_Ident(_,name) -> VAR name
    | Fact_Number (_,n) -> CONST n 
    | Fact_RD_Int _ -> RD_INT
    | Fact_RD_Char _ -> RD_CHAR
    | Fact_Expr (_,expr) -> expression2tree expr
    | Fact_Body (_,statements,expr) -> BODY(statements,expression2tree expr)
    | Fact_New  (_,name,args) -> NEW (name,arguments2tree args)
(**convertit un Ast.arguments en une liste de expr_tree**)
and arguments2tree (Arguments(_,Expressions(_,expr_list)))=
  List.map expression2tree expr_list

let rec optimise_tree tree=
  match tree with
    |UN_OP(op,CONST n) ->
    begin
      match op with
        |NEGATE_OP (NO_SUB _)    -> CONST (Int32.neg n)
        |NEGATE_OP (NO_NOT _)    -> if n=Int32.zero then CONST (Int32.of_int 1) else CONST (Int32.of_int 0)
        |NEGATE_OP (NO_BIN_NOT _) -> CONST (Int32.lognot n)
    end
    |BIN_OP(op,CONST n1,CONST n2) ->
    begin
      match op with
        |COMP_OP (CO_EQ _)-> if (Int32.compare n1 n2) = 0 then CONST (Int32.of_int 1) else CONST (Int32.of_int 0) 
        |COMP_OP (CO_NE _)-> if (Int32.compare n1 n2) <> 0 then CONST (Int32.of_int 1) else CONST (Int32.of_int 0) 
        |COMP_OP (CO_LT _)-> if (Int32.compare n1 n2) < 0 then CONST (Int32.of_int 1) else CONST (Int32.of_int 0) 
        |COMP_OP (CO_GT _)-> if (Int32.compare n1 n2) > 0 then CONST (Int32.of_int 1) else CONST (Int32.of_int 0) 
        |COMP_OP (CO_LE _)-> if (Int32.compare n1 n2) <= 0 then CONST (Int32.of_int 1) else CONST (Int32.of_int 0) 
        |COMP_OP (CO_GE _)-> if (Int32.compare n1 n2) >= 0 then CONST (Int32.of_int 1) else CONST (Int32.of_int 0) 
        |SUM_OP (SO_ADD _)-> CONST (Int32.add n1 n2) 
        |SUM_OP (SO_SUB _)-> CONST (Int32.sub n1 n2) 
        |SUM_OP (SO_BIN_OR _) -> CONST (Int32.logor n1 n2) 
        |SUM_OP (SO_BIN_XOR _)-> CONST (Int32.logxor n1 n2) 
        |PROD_OP (PO_MUL _)   -> CONST (Int32.mul n1 n2) 
        |PROD_OP (PO_DIV _)   -> CONST (Int32.div n1 n2) 
        |PROD_OP (PO_MOD _)   -> if n2<>Int32.zero then CONST (Int32.rem n1 n2)  else tree
        |PROD_OP (PO_AND _)   -> if n1<>Int32.zero && n2<>Int32.zero then CONST (Int32.of_int 1)  else CONST (Int32.of_int 0) 
        |PROD_OP (PO_BIN_AND _)-> CONST (Int32.logand n1 n2) 
        |PROD_OP (PO_BIN_LSL _)->
          if (Int32.compare (Int32.abs n2) (Int32.of_int 31))>0 then
            CONST (Int32.of_int 0) 
          else
            let n=Int32.to_int n2 in
            if n>=0 then CONST (Int32.shift_left n1 n) 
                    else CONST (Int32.shift_right_logical n1 (-n)) 
        |PROD_OP ((*PO_BIN_LSR*) _)->
          if (Int32.compare (Int32.abs n2) (Int32.of_int 31))>0 then
            CONST (Int32.of_int 0) 
          else
            let n=Int32.to_int n2 in
            if n>=0 then CONST (Int32.shift_right_logical n1 n) 
                    else CONST (Int32.shift_left n1 (-n)) 
    end
    |UN_OP(op,tree) -> 
    begin
      let new_tree=optimise_tree tree in 
      let result=UN_OP(op,new_tree) in
      
      match new_tree with
        |CONST _ -> optimise_tree result
        | _ -> result
    end
    |BIN_OP(SUM_OP (SO_ADD _),UN_OP(NEGATE_OP (NO_SUB _),g),d) -> optimise_tree (BIN_OP(SUM_OP (SO_SUB (0,0)),d,g))
    |BIN_OP(SUM_OP (SO_ADD _),g,UN_OP(NEGATE_OP (NO_SUB _),d)) -> optimise_tree (BIN_OP(SUM_OP (SO_SUB (0,0)),g,d))
    |BIN_OP(SUM_OP (SO_SUB _),g,UN_OP(NEGATE_OP (NO_SUB _),d)) -> optimise_tree (BIN_OP(SUM_OP (SO_ADD (0,0)),g,d))
    |BIN_OP(PROD_OP (PO_BIN_LSR _),g,d) -> optimise_tree (BIN_OP(PROD_OP (PO_BIN_LSL (0,0)),optimise_tree g,optimise_tree (UN_OP(NEGATE_OP (NO_SUB (0,0)),d))))
    |BIN_OP(op1,g1,d1) ->
    begin
      let new_d1=optimise_tree d1
      and new_g1=optimise_tree g1 in
      match new_g1,new_d1 with
        |CONST _,CONST _ -> optimise_tree (BIN_OP(op1,new_g1,new_d1))
        |BIN_OP(op2,g2,d2),CONST _->
        begin
        match op1,op2 with      
          | _,_ when compare_op op1 op2 ->
          begin
          match op1 with    
            |SUM_OP (SO_SUB _) ->
            begin
            match g2,d2 with
              |_,CONST _ -> BIN_OP(op1,g2,optimise_tree (BIN_OP(SUM_OP (SO_ADD (0,0)),d2,new_d1)))
              |_         -> BIN_OP(op1,optimise_tree (BIN_OP(op2,g2,new_d1)),d2)
            end
            |PROD_OP (PO_DIV _) ->
            begin
            match g2,d2 with
              |_,CONST _ -> BIN_OP(op1,g2,optimise_tree (BIN_OP(PROD_OP (PO_MUL (0,0)),d2,new_d1)))
              |_         -> BIN_OP(op1,optimise_tree (BIN_OP(op2,g2,new_d1)),d2)
            end
            |SUM_OP (SO_ADD _)
            |SUM_OP (SO_BIN_OR _)
            |SUM_OP (SO_BIN_XOR _)
            |PROD_OP (PO_MUL _)
            |PROD_OP (PO_AND _)
            |PROD_OP (PO_BIN_AND _) ->
            begin
            match g2,d2 with
              |_,CONST _ -> BIN_OP(op1,g2,optimise_tree (BIN_OP(op2,d2,new_d1)))
              |_         -> BIN_OP(op1,optimise_tree (BIN_OP(op2,g2,new_d1)),d2)
            end                 
            |_ -> BIN_OP(op1,new_g1,new_d1)
          end
          |SUM_OP (SO_ADD _),SUM_OP (SO_SUB _)->
          begin
          match g2,d2 with
            |CONST _,_ -> BIN_OP(SUM_OP (SO_SUB (0,0)),optimise_tree (BIN_OP(SUM_OP (SO_ADD (0,0)),g2,new_d1)),d2)
            |_         -> BIN_OP(SUM_OP (SO_ADD (0,0)),optimise_tree (BIN_OP(SUM_OP (SO_SUB (0,0)),new_d1,d2)),g2)
          end
          |SUM_OP (SO_SUB _),SUM_OP (SO_ADD _) ->
          begin
          match g2,d2 with
            |CONST _,_ -> BIN_OP(SUM_OP (SO_ADD (0,0)),optimise_tree (BIN_OP(SUM_OP (SO_SUB (0,0)),g2,new_d1)),d2)
            |_         -> BIN_OP(SUM_OP (SO_ADD (0,0)),optimise_tree (BIN_OP(SUM_OP (SO_SUB (0,0)),new_d1,d2)),g2)
          end
          |_ -> BIN_OP(op1,new_g1,new_d1)
        end
        |CONST _,BIN_OP(op2,g2,d2)->
        begin
        match op1,op2 with      
          | _,_ when compare_op op1 op2 ->
          begin
          match op1 with    
            |SUM_OP (SO_SUB _) ->
            begin
            match g2,d2 with
              |_,CONST _ -> BIN_OP(op1,optimise_tree (BIN_OP(SUM_OP (SO_ADD (0,0)),d2,new_g1)),g2)
              |_         -> BIN_OP(op1,d2,optimise_tree (BIN_OP(op2,g2,new_g1)))
            end
            |SUM_OP (SO_ADD _)
            |SUM_OP (SO_BIN_OR _)
            |SUM_OP (SO_BIN_XOR _)
            |PROD_OP (PO_MUL _)
            |PROD_OP (PO_AND _)
            |PROD_OP (PO_BIN_AND _) ->
            begin
            match g2,d2 with
              |_,CONST _ -> BIN_OP(op1,g2,optimise_tree (BIN_OP(op2,new_g1,d2)))
              |_         -> BIN_OP(op1,d2,optimise_tree (BIN_OP(op2,g2,new_g1)))
            end                 
            |_ -> BIN_OP(op1,new_g1,new_d1)
          end
          |SUM_OP (SO_ADD _),SUM_OP (SO_SUB _)->
          begin
          match g2,d2 with
            |CONST _,_ -> BIN_OP(SUM_OP (SO_SUB (0,0)),optimise_tree (BIN_OP(SUM_OP (SO_ADD (0,0)),g2,new_g1)),d2)
            |_         -> BIN_OP(SUM_OP (SO_ADD (0,0)),optimise_tree (BIN_OP(SUM_OP (SO_SUB (0,0)),new_g1,d2)),g2)
          end
          |SUM_OP (SO_SUB _),SUM_OP (SO_ADD _) ->
          begin
          match g2,d2 with
            |CONST _,_ -> BIN_OP(SUM_OP (SO_SUB (0,0)),optimise_tree (BIN_OP(SUM_OP (SO_SUB (0,0)),g2,new_g1)),d2)
            |_         -> BIN_OP(SUM_OP (SO_SUB (0,0)),optimise_tree (BIN_OP(SUM_OP (SO_SUB (0,0)),new_g1,d2)),g2)
          end
          |_ -> BIN_OP(op1,new_g1,new_d1)
        end
        |_ -> BIN_OP(op1,new_g1,new_d1)
    end
    |_ -> tree
(**renvoit vrai si les deux opérateurs sont égaux, ignore la position**)
and compare_op op1 op2=
  match op1,op2 with
    |COMP_OP op1,COMP_OP op2->
    begin
      match op1,op2 with
        | CO_EQ _,CO_EQ _
        | CO_NE _,CO_NE _
        | CO_LT _,CO_LT _
        | CO_GT _,CO_GT _
        | CO_LE _,CO_LE _
        | CO_GE _,CO_GE _ -> true
        | _ -> false
    end
    |SUM_OP op1,SUM_OP op2 ->
    begin
      match op1,op2 with
        | SO_ADD _,SO_ADD _
        | SO_SUB _,SO_SUB _
        | SO_BIN_OR _,SO_BIN_OR _
        | SO_BIN_XOR _,SO_BIN_XOR _ -> true
        | _ -> false
    end
    |PROD_OP op1,PROD_OP op2 ->
    begin
      match op1,op2 with
        | PO_MUL _,PO_MUL _
        | PO_DIV _,PO_DIV _
        | PO_MOD _,PO_MOD _
        | PO_AND _,PO_AND _
        | PO_BIN_AND _,PO_BIN_AND _
        | PO_BIN_LSL _,PO_BIN_LSL _
        | PO_BIN_LSR _,PO_BIN_LSR _ -> true
        | _ -> false
    end
    | _ -> false
and print_tree tree=
  match tree with
    |CONST n -> print_string (Int32.to_string n)
    |BIN_OP (_,g,d) -> print_string "("; print_tree g; print_string "o"; print_tree d; print_string ")"
    |VAR name -> print_string (Ast.get_name name)
    | _ -> ()

let convert_expr expr=
  let t = expression2tree expr in
  optimise_tree t
