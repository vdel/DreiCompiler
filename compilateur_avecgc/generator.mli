open Ast

(** Ce Module contient les primitives de génération du code final en assembleur RISC à partir de l'arbre d'analyse *)

(**************************************)

(** Génère un nouveau label non déjà utilisé *)
val new_label          : unit -> label

(** Génère le code correspondant au label option, ne génère rien si on a None *)
val generate_label     : label option -> unit


(** Génère le code de l'expression passée en argument *)
val generate_expression: Ast.expression -> unit

(** Génère le code correspondant à : si l'expression vaut l'entier (booléen) alors on saute sur le label *)
val generate_condition : label -> bool -> Ast.expression -> unit 


(** Génère le code du statement *)
val generate_statement : Ast.statement    -> unit

(** génère l'appel au SYSCALL PRINT_CHAR *)
val generate_printchar : Ast.expression   -> unit

(** Génère l'appel au SYSCALL PRINT_INT *)
val generate_printint  : Ast.expression   -> unit

(** Génère le code correspondant au if (expr) then stat1 else stat2 *)
val generate_if        : Ast.expression   -> Ast.statement -> Ast.statement -> unit

(** Génère le code correspondant au while(expr) stat *)
val generate_while     : Ast.expression   -> Ast.statement -> unit

(** Génère le code d'un champs ou d'une méthode *)
val generate_member    : Ast.member       -> unit

(** Génère le code d'une méthode *)
val generate_method    : Ast.methoddef    -> unit (* MOD *)

(** Génère le prologue d'une méthode *)
val generate_prolog    : unit             -> unit

(** Et ici l'épilogue, en tenant compte de la taille de l'enregistrement d'activation *)
val generate_epilog    : int -> int       -> unit (* MOD *)


(** Génère le code de la classe passée en argument *)
val generate_class     : Ast.classdecl    -> unit (* MOD *)

(** Génère la VMT d'une classe *)
val generate_vmt       : Ast.classdecl    -> unit

(** Génère le label d'une méthode *)
val generate_method_label : Ast.methodheader -> unit (* MOD *)

(** Génère le code intégral d'un programme *)
val generate_program   : Ast.program      -> unit

(** Fonction appelée par le module Main *)
val main : string -> unit
