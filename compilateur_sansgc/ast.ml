(** Contient tous les types utilisés pour décrire l'arbre de syntaxe abstraite *)

type position = int * int
type label    = string
type registre = int

(* type.ml *)
type dtype = 
  | TClass of string
  | TArray of dtype
  | TInt
  | TNone
  | TBad
;;

let rec print_type t =
  match t with
    | TArray(d) -> (print_type d)^" Array"
    | TClass(s) -> s
    | TInt -> "Int"
    | TNone -> "TNone"
    | _ -> "TBad"
;;

type var_symbol =
{ 
  var_type : dtype;
  mutable var_offset : int option;
}  
and field_symbol =
{ 
  field_type : dtype;
  mutable field_offset : int option;
}
and method_symbol =
{ 
  mutable params_type : dtype list;
  mutable return_type : dtype;
  mutable method_label : label option;
  mutable method_offset: int option;  (*index de la méthode dans la vmt*) 
} 
and class_symbol =
{ 
  mutable parents : string list;
  mutable fields  : field_scope;
  mutable methods : method_scope;
  mutable class_label: label option;
  mutable address : int option
} 
and var_scope    = ((string * var_symbol) list) list
and field_scope  = (string * field_symbol) list
and method_scope = (string * method_symbol) list
and class_scope = (string * class_symbol) list

type symbol =
  | V of var_symbol
  | F of field_symbol
  | M of method_symbol
  | C of class_symbol

type name = {
  name : string;
  mutable symbol : symbol option;
	    }

let mk_name str = { name = str; symbol = None}

let get_name n = n.name

let set_symbol x s =
  assert ( x.symbol = None );
  x.symbol <- Some s

let get_offset symbol =
  let offset =
  match symbol with
    |Some (V var_symb) -> var_symb.var_offset
    |Some (F field_symb) -> field_symb.field_offset
    |Some (M method_symb) -> method_symb.method_offset
    |Some (C class_symb) -> class_symb.address
    | _ -> Report.fail "Erreur dans Ast.get_offset: symbole incorrect"
  in match offset with
    |None -> Report.fail "Erreur dans Ast.get_offset: offset inconnu"
    |Some offset -> offset

type program =
    Program of classdecl list * statement
  and classdecl =
    ClassDecl of position * name * (name option) * member list
  and member = 
    | Mem_Field  of fielddecl
    | Mem_Method of methoddef
  and fielddecl = 
    FieldDecl of position * formal
  and methoddef = 
    | Method_Def_Long   of position * methodheader * ttype * expression
    | Method_Def_Short  of position * methodheader * statements
(** META LANGAGE **)
    | Method_Def_ignore_Long   of position * methodheader * ttype * statements
    | Method_Def_ignore_Short  of position * methodheader * statements
  and methodheader = 
    MethodHeader of position * name * (formal list) 
  and formal = 
    Formal of position * name * ttype
  and ttype =
    | Array_Type of ttype
    | Class_Type of name
    | Int_Type
    | No_Type
  and vardecl = 
    VarDecl of position * formal * expression
  and statements_t = 
    | Stat_Var_Decl  of vardecl
    | Stat_Statement of statement
  and statements = 
    Statements of position * (statements_t list)
  and statement = 
    | Print_Asm     of (string)    
    | Sta_While     of position * expression * statement
    | Sta_If        of position * expression * statement * statement
    | Sta_Set       of position * name * expression
    | Sta_Do        of position * expression
    | Sta_PrintInt  of position * expression
    | Sta_PrintChar of position * expression
    | Sta_Bloc      of position * statements
  and expression = 
    Expression of position * sumexpression * ((compop*sumexpression) option)
  and compop = 
    | CO_EQ of position
    | CO_NE of position 
    | CO_LT of position
    | CO_GT of position 
    | CO_LE of position 
    | CO_GE of position
  and sumexpression = SumExpression of position * term * ((sumop*term) list)
  and sumop =
    | SO_ADD of position
    | SO_SUB of position
    | SO_BIN_OR  of position
    | SO_BIN_XOR of position
  and signedfactor =  SignedFactor of position * (negateop option) * factor
  and term = Term of position * signedfactor * ((prodop * signedfactor) list)
  and negateop = 
    | NO_SUB of position
    | NO_NOT of position
    | NO_BIN_NOT of position
  and prodop = 
    (** TABLEAUX **)
    | PO_DIESE of position
    (** /TABLEAUX **)
    | PO_MUL of position
    | PO_DIV of position
    | PO_MOD of position
    | PO_AND of position
    | PO_BIN_AND of position
    | PO_BIN_LSL of position
    | PO_BIN_LSR of position
  and simplefactor = 
    | Fact_Ident of position * name
    | Fact_Number of position * Int32.t
(** TABLEAUX **)
    | Fact_Array of position * (arguments)
(** /TABLEAUX **)
    | Fact_RD_Int of position
    | Fact_RD_Char of position
    | Fact_Expr of position * expression
    | Fact_Body of position * statements * expression
    | Fact_New  of position * name * arguments
  and factor = 
    Factor of position * simplefactor * ((name*(arguments option)) list)
  and arguments =
    Arguments of position * expressions
  and expressions =
    Expressions of position * (expression list)


let rec dtype_of_ttype t =
  match t with
    | Array_Type(t1) -> TArray(dtype_of_ttype t1)
    | Class_Type (n) -> TClass(get_name n)
    | Int_Type -> TInt
    | No_Type -> TNone
   


let aff_tree tree = ();;
