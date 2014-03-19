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
val expression2tree : Ast.expression -> expr_tree

(**convertit une Ast.sumexpression en expr_tree**)
val sumexpression2tree : Ast.sumexpression -> expr_tree

(**convertit un Ast.term en expr_tree**)
val term2tree : Ast.term -> expr_tree

(**convertit un Ast.signedfactor en expr_tree**)
val signedfactor2tree : Ast.signedfactor -> expr_tree

(**convertit un Ast.factor en expr_tree**)
val factor2tree : Ast.factor -> expr_tree

(**convertit un Ast.simplefactor en expr_tree**)
val simplefactor2tree : Ast.simplefactor -> expr_tree

(**convertit un Ast.arguments en une liste de expr_tree**)
val arguments2tree : Ast.arguments -> expr_tree list

(**optimise un arbre expr_type**)
val optimise_tree : expr_tree -> expr_tree

(**renvoit vrai si les deux opérateurs sont égaux, ignore la position**)
val compare_op : bin_op -> bin_op -> bool

(**convertit une expression en un arbre de type expr_type**)
val convert_expr : Ast.expression -> expr_tree

