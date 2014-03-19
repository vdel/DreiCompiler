open Ast

(** Ce Module contient les primitives de g�n�ration du code final en assembleur RISC � partir de l'arbre d'analyse *)

(**************************************)

(** G�n�re un nouveau label non d�j� utilis� *)
val new_label          : unit -> label

(** G�n�re le code correspondant au label option, ne g�n�re rien si on a None *)
val generate_label     : label option -> unit


(** G�n�re le code de l'expression pass�e en argument *)
val generate_expression: Ast.expression -> unit

(** G�n�re le code correspondant � : si l'expression vaut l'entier (bool�en) alors on saute sur le label *)
val generate_condition : label -> bool -> Ast.expression -> unit 


(** G�n�re le code du statement *)
val generate_statement : Ast.statement    -> unit

(** g�n�re l'appel au SYSCALL PRINT_CHAR *)
val generate_printchar : Ast.expression   -> unit

(** G�n�re l'appel au SYSCALL PRINT_INT *)
val generate_printint  : Ast.expression   -> unit

(** G�n�re le code correspondant au if (expr) then stat1 else stat2 *)
val generate_if        : Ast.expression   -> Ast.statement -> Ast.statement -> unit

(** G�n�re le code correspondant au while(expr) stat *)
val generate_while     : Ast.expression   -> Ast.statement -> unit

(** G�n�re le code d'un champs ou d'une m�thode *)
val generate_member    : Ast.member       -> unit

(** G�n�re le code d'une m�thode *)
val generate_method    : Ast.methoddef    -> unit (* MOD *)

(** G�n�re le prologue d'une m�thode *)
val generate_prolog    : unit             -> unit

(** Et ici l'�pilogue, en tenant compte de la taille de l'enregistrement d'activation *)
val generate_epilog    : int -> int       -> unit (* MOD *)


(** G�n�re le code de la classe pass�e en argument *)
val generate_class     : Ast.classdecl    -> unit (* MOD *)

(** G�n�re la VMT d'une classe *)
val generate_vmt       : Ast.classdecl    -> unit

(** G�n�re le label d'une m�thode *)
val generate_method_label : Ast.methodheader -> unit (* MOD *)

(** G�n�re le code int�gral d'un programme *)
val generate_program   : Ast.program      -> unit

(** Fonction appel�e par le module Main *)
val main : string -> unit
