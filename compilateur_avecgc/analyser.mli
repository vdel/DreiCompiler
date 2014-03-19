(** Effectue l'analyse de type de l'arbre de syntaxe abstraite *)

(** {3 Point d'entrée du module } *)

val main : string -> unit


(** {3 Outils de débug } *)

(** Affichage d'une erreur "type attendu <> type reçu" *)
val error_wrong_type : Ast.dtype -> Ast.dtype -> string
val print_class_scope : (string * Ast.class_symbol) list -> unit
val print_field_scope : Ast.field_scope -> unit
val print_method_scope : Ast.method_scope -> unit


(** {3 Fonctions basiques diverses } *)

(** Extrait les champs et méthodes d'une liste d_members *)
val analyse_get_members : Ast.member list -> (string * Ast.field_symbol) list * (string * Ast.method_symbol) list
val analyse_xunion_m : (string * Ast.method_symbol) list -> Ast.method_scope -> string list -> Ast.method_scope
val analyse_xunion_f : (string * Ast.field_symbol) list -> Ast.field_scope -> Ast.field_scope

(** {3 Fonctions de traitement des contextes de variable } *)

(** renvoit le type de var dans le contexte de variable amélioré (groupe bleu) *)
val find_type_var : Ast.var_scope -> string -> Ast.var_symbol

(** si une var existe dans le bloc courant, renvoit le symbole associée, sinon, soulève Not_Found *)
val find_var_in_current_scope : Ast.var_scope -> string -> Ast.var_symbol

(** ajoute var au contexte de variable du bloc courant *)
val add_var : Ast.var_scope -> string * Ast.var_symbol -> Ast.var_scope

(** crée un nouveau bloc dans le contexte de variable *)
val add_block : Ast.var_scope -> Ast.var_scope


(** {5 Analyse de l'arbre de syntaxe abstraite } *)



(** Ajoute une classe à un c_scope *)
val analyse_addclass : (string * Ast.class_symbol) list -> Ast.name -> string list -> Ast.field_scope -> Ast.method_scope -> (string * Ast.class_symbol) list

(** Effectue l'insertion de classe *)
val analyse_class : (string * Ast.class_symbol) list ->
       Ast.classdecl -> (string * Ast.class_symbol) list

(** Analyse la définition d'une classe *)
val analyse_classdef : (string * Ast.class_symbol) list Pervasives.ref -> Ast.classdecl -> unit

(** Retourne true si la classe c_id a été insérée dans le c_scope *)
val analyse_exist_class : (string * Ast.class_symbol) list -> string -> bool


val analyse_expression : (string * Ast.class_symbol) list ->
       Ast.var_scope -> Ast.expression -> Ast.dtype

val analyse_factor : (string * Ast.class_symbol) list -> Ast.var_scope -> Ast.factor -> Ast.dtype

(** Analyse d'un champ *)
val analyse_field : (string * Ast.class_symbol) list Pervasives.ref ->
       Ast.name -> Ast.fielddecl -> Ast.symbol -> unit

(** Retourne le type du Formal : AFAIRE TBAD *)
val analyse_formal : (string * Ast.class_symbol) list -> Ast.formal -> Ast.dtype

(** Analyse des membres d'une classe *)
val analyse_member : string ->
       (string * Ast.class_symbol) list Pervasives.ref ->
       Ast.name -> Ast.symbol -> Ast.member -> unit

val analyse_method : string ->
       (string * Ast.class_symbol) list Pervasives.ref ->
       Ast.name -> Ast.methoddef -> Ast.symbol -> unit

(** Effectue l'analyse de type du programme *)
val analyse_program : Ast.program -> unit

val analyse_signedfactor : (string * Ast.class_symbol) list ->
       Ast.var_scope -> Ast.signedfactor -> Ast.dtype

val analyse_simplefactor : (string * Ast.class_symbol) list ->
       Ast.var_scope -> Ast.simplefactor -> Ast.dtype

val analyse_statement : (string * Ast.class_symbol) list ->
       Ast.var_scope -> Ast.statements_t -> Ast.var_scope

(** Retourne true si t1 est un sous-type de t2 *)
val analyse_is_subtype : (string * Ast.class_symbol) list -> Ast.dtype -> Ast.dtype -> bool

val analyse_sumexpression : (string * Ast.class_symbol) list ->
       Ast.var_scope -> Ast.sumexpression -> Ast.dtype

val analyse_term : (string * Ast.class_symbol) list -> Ast.var_scope -> Ast.term -> Ast.dtype

val analyse_type : Ast.position -> (string * Ast.class_symbol) list -> Ast.dtype -> unit

val main : string -> unit
