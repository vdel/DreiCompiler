open Ast
(* generator.ml *)
let gc_pourri = 1

(* pour obtenir facilement le registre numéro [i] *)
let reg i = "R" ^ string_of_int i

let r_return: registre = 1
let r_sp:     registre = 30
let r_lnk:    registre = 31
let max_reg:  registre = 27 (*registre maximum accessible*)

let last_label = ref 0

let new_label () =
  let s = "label_"^(string_of_int (!last_label)) in
    last_label := !last_label + 1;
    s

(*opération sur le bloc courant *)
let rec frame_size = ref 0
(* alloue n octects dans le bloc courant *)
and incFrameSize n = (frame_size:= !frame_size + n)

(* libere n octects dans le bloc courant *)
and decFrameSize n = frame_size:= !frame_size - n

(* renvoit la taille du bloc courant *)
and getFrameSize () = !frame_size

(* opérations sur les registres *)
(* le registre rsp est le registre courant: il est vide, il attend d'être initialisé *)
and rsp = ref 1
and reg_saved = ref [] 
(* alloue un nouveau registre, crie si on na plus de 29 registres alloués *)
and freshReg () =
  rsp:= !rsp+1; 
  if !rsp>max_reg then 
    Report.fail ("Plus de "^(string_of_int max_reg)^" registres alloués, je ne sais pas quoi faire !!!")
  else
    !rsp
(*libère le dernier registre alloué *) 
and dropReg () = 
  rsp:= !rsp-1; 
  if !rsp<1 then 
    Report.fail "Nombre de registres alloués inférieur à 1 !!!"
  else
    ()
(*prochain registre alloué: il est vide, il attend d'être initialisé*)
and topReg () = !rsp
and sndReg () = !rsp-1
(*nombre de registres libres*)
and nbrReg () = max_reg-topReg()
(*pour sauver les registres dans la pile*)
and save_reg n =
  let ret = ref [] in
  for i = topReg()-1 downto topReg()-n do
    push i; ret := i::(!ret);
  done;
  rsp:=topReg()-n;
  !ret
(*pour restorer les registres de la pile*)
and load_reg l = 
  List.iter (fun x -> incr rsp; pop x) l

(*fonctions de sortie: permettent de connaitre la taille du code écrit jusqu'ici*)
and addr=ref 0
(*tient à jours l'adresse courante, affiche une instruction et éventuellement un commentaire*)
and print instr commentaire=
  addr:= !addr+4;
  print_string instr;
  if commentaire<>"" then
  begin
    for i=(String.length instr) to 30 do
      print_string " "
    done;
    Printf.printf "// %s" commentaire
  end;
  print_string "\n"; 
and print_3 instr a1 a2 a3 commentaire=
  print ("  "^instr^" "^a1^" "^a2^" "^a3) commentaire
and print_2 instr a1 a2 commentaire=
  print ("  "^instr^" "^a1^" "^a2) commentaire
and print_1 instr a1 commentaire=
  print ("  "^instr^" "^a1^" ") commentaire
and print_0 instr commentaire=
  print ("  "^instr) commentaire
and print_label label=
  Printf.printf "%s:\n" label;

(* empiler la valeur dans le registre [r] sur la pile *)
and push r =
  incFrameSize 4;
  print_3 "PSH" (reg r) (reg r_sp) "4" ""
 
(* dépiler, en lisant dans un registre *)
and pop r = 
  decFrameSize 4;
  print_3 "POP" (reg r) (reg r_sp) "4" ""

and generate_program (Program(class_decl_list,stat)) = 
  print_label "start";
  print_2 "BEQ" "R0" "init" "saute au code d’initialisation\n";

  (* Génération des VMTs *)
  print_label "vmts";
  List.iter generate_vmt class_decl_list;
  Printf.printf "\n";

  (* Les méthodes *)
  print_label "methods";
  Printf.printf "\n";
  List.iter generate_class class_decl_list; 
  frame_size:=0;

(** TABLEAUX **)
  print_label "error_" ;
  print_3 "ADDI" "R1" "R0" "97" "";
  print_3 "SYSCALL" "R1" "0" "6" "";
  print_1 "RET" "R0" "";
(** /TABLEAUX **)

(** GC **)
if (gc_pourri = 1) then
begin
  (* Le GC *)
  print_label "gc_init_";
  (* Initialise le GC *)
  print_3 "ADDI" "R3" "R1" "4" "on met l'adresse du tas + 4";
  print_3 "ADDI" "R4" "R0" "5000" "on met la taille du tas";
 (* print_3 ADDI" "R1" "R2" "0" "on met l'adresse du tas dans R1;*)
  (* On stocke dans le premier bloc du tas l'objet Gc *)
  print_3 "ORIU" "R2" "R0" "4" "adresse de la VMT"; 
  print_3 "STW" "R2" "R3" "0" "";
  print_3 "ADD" "R2" "R0" "R1" "this.memory";
  print_3 "STW" "R2" "R3" "4" "";
(*  print_3 "ADDI" "R2" "R0" "3000" "";*)
  print_3 "STW" "R4" "R3" "8" "this.max_size";
 
  print_3 "DIVI" "R4" "R4" "2" "memto/memfrom";
  print_3 "ADD" "R4" "R2" "R4" "";
  print_3 "STW" "R4" "R3" "12" "this.memto";

  (* On va maintenant lancer Gc.Init(taille) *)
  print_3 "LDW" "R4" "R3" "0" "adresse de la vmt dans R3";
  print_3 "LDW" "R4" "R4" "0" "adresse de Init dans R3";
  push (3);
  print_3 "ADDI" "R2" "R0" "20" "taille, argument s";
  push (2);
  print_3 "ORIU" (reg r_lnk) "R0" (string_of_int (!addr+8)) "";     (*on saute*)
  print_1 "RET" "R4" "On appelle Init(s)"; 
  (* Fin de l'initialisation *)
  print_2 "BEQ" "R0" "main" ""
end;
(** /GC **)

  (* Le code principal *)
  print_label "main";
  generate_statement stat;
  Printf.printf "\n";

  (* On fait un flush de la sortie standard *)
  print_3 "SYSCALL" "0" "0" "15" "";
  print_1 "RET" "R0" "quitte l’emulateur";

  (* code d'initialisation *)
  print_label "init";
  print_3 "SYSCALL" (reg r_sp) "0" "13" "initialise le pointeur de pile";
  print_3 "ORIU" "R1" "R0" "((init >> 16) & 0xffff)" "";
  print_3 "LSHI" "R1" "R1" "16" "";
  print_3 "ORIU" "R1" "R1" "(init & 0xffff)" "le tas commence en init";
  print_3 "SUB" "R2" (reg r_sp) "R1" "taille memoire sans le code";
  print_3 "DIVIU" "R2" "R2" "(3*4)" "coupe en trois morceaux";
  print_3 "LSHI" "R2" "R2" "1" "deux tiers pour le tas";
  print_3 "ORIU" "R3" "R0" (string_of_int r_sp) ("registre de pile ="^(string_of_int r_sp));
  print_3 "LSHI" "R3" "R3" "27" "";
  print_3 "OR" "R2" "R2" "R3" "";  
  (** GC **)
  if (gc_pourri = 1) then
    print_2 "BEQ" "R0" "gc_init_" "initialisation du GC"
  else begin
  (** /GC **)
    print_3 "SYSCALL" "R1" "R2" "11" "initialise le GC de l'emulateur";  
    print_2 "BEQ" "R0" "main" "saute a la fonction principale" 
  (** GC **) end (** /GC **)


(* Regarde si expr -> boolean et si oui,
   saute sur label *)
and generate_condition label boolean expr = 
  generate_expression expr;
  (* Test de comparaison *)
  if (boolean) then
    print_2 "BNE" (reg (topReg())) label ""
  else
    print_2 "BEQ" (reg (topReg())) label "";

and generate_boolean_mask instr =
  print_2 instr (reg (topReg ())) "3" "";
  (* si c'est faux *)
  print_3 "ADDI" (reg (topReg ())) "R0" "0" "";
  print_1 "BSR" "2" "";
  (* si c'est vrai *)
  print_3 "ADDI" (reg (topReg ())) "R0" "1" "";

(* Génère le code calculant une expression et le place dans  *) 
and generate_expression expr =
  begin
  match expr with
    | Expression(_, sumexpr, None)                -> generate_sumexpression sumexpr
    | Expression(_, sumexpr1, Some(op, sumexpr2)) ->
      begin
        generate_sumexpression sumexpr1;
        let _ = freshReg () in
          generate_sumexpression sumexpr2;
          let instr = 
            match op with
              | CO_EQ _ -> "BEQ"
              | CO_NE _ -> "BNE"
              | CO_LT _ -> "BLT"
              | CO_GT _ -> "BGT"
              | CO_LE _ -> "BLE"
              | CO_GE _ -> "BGE"
          in
 	  print_3 "CMP" (reg (sndReg ())) (reg (sndReg ())) (reg (topReg ())) "";
          dropReg ();
          generate_boolean_mask instr;                    
      end
  end;

and generate_sumexpression sumexpr =
  let rec generate_list l =
    match l with
      | []                 -> SO_ADD(0,0) (*impossible*)
      | [(op,terme)]       -> generate_term terme; op
      | (new_op, terme)::q -> 
        let op = generate_list q in (*résultat du calcul dans topReg: renvoit l'opération à faire avec le terme de gauche*)
        let _ = freshReg () in 
          generate_term terme; (*maintenant dans topReg: terme de gauche, dans sndReg: terme de droite*)
          process_op op;
          new_op
  and process_op op =
    begin
      match op with
        | SO_ADD _ -> print_3 "ADD" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
        | SO_SUB _ -> print_3 "SUB" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
        | SO_BIN_OR _ -> print_3 "OR" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
        | SO_BIN_XOR _ -> print_3 "XOR" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
    end;
    dropReg ()
  in  
  match sumexpr with
    | SumExpression(_, terme, []) -> generate_term terme
    | SumExpression(_, terme, l)  ->
      begin
        let saved_registers = if nbrReg()=0 then save_reg 1 else [] in
        let op = generate_list l in (*résultat du calcul dans topReg: renvoit l'opération à faire avec le terme de gauche*)
        let _ = freshReg () in 
          generate_term terme; (*maintenant dans topReg: terme de gauche, dans sndReg: terme de droite*)
          process_op op;
          load_reg saved_registers
      end

and generate_term terme=
  let rec generate_list l =
    match l with
      | []                      -> PO_MUL(0,0) (*impossible*)
      | [(op, signedfact)]      -> generate_signedfactor signedfact; op
      | (new_op, signedfact)::q -> 
        let op = generate_list q in (*résultat du calcul dans topReg: renvoit l'opération à faire avec le facteur de gauche*)
        let _ = freshReg () in 
          generate_signedfactor signedfact; (*maintenant dans topReg: facteur de gauche, dans sndReg: facteur de droite*)
          process_op op;
          new_op
  and process_op op =
    begin
      match op with
(** TABLEAUX **)
        | PO_DIESE _ -> 
            let r1,r2 = topReg(),sndReg() in
            let _ = freshReg () in
            print_3 "MULI" (reg r2) (reg r2) "4" "";
            print_3 "LDW" (reg (topReg())) (reg r1) "0" "";

            print_3 "ADD" (reg r1) (reg r2) (reg r1) "";
          
            print_3 "SUB" (reg (topReg ())) (reg r2) (reg (topReg())) "";
            print_2 "BGE" (reg (topReg ())) "error_" "";
            let _ = dropReg() in
            print_3 "LDW" (reg r2) (reg r1) "4" "";
 
 (** /TABLEAUX **)
        | PO_MUL _ -> print_3 "MUL" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
        | PO_DIV _ -> print_3 "DIV" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
        | PO_MOD _ -> print_3 "MOD" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
        | PO_AND _ -> print_3 "AND" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
                      generate_boolean_mask "BNE"
        | PO_BIN_AND _ -> print_3 "AND" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
        | PO_BIN_LSL _ -> print_3 "ASH" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
        | PO_BIN_LSR _ ->
          print_3 "SUB" (reg (sndReg ())) "R0" (reg (sndReg ())) "";
          print_3 "ASH" (reg (sndReg ())) (reg (topReg ())) (reg (sndReg ())) "";
    end;
    dropReg ()
  in  
  match terme with
    | Term(_, signedfact,[]) -> generate_signedfactor signedfact
    | Term(_, signedfact,l)  -> 
      begin
        let saved_registers = if nbrReg()=0 then save_reg 1 else [] in
        let op = generate_list l in (*résultat du calcul dans topReg: renvoit l'opération à faire avec le facteur de gauche*)
        let _ = freshReg () in 
          generate_signedfactor signedfact; (*maintenant dans topReg: facteur de gauche, dans sndReg: facteur de droite*)
          process_op op;
          load_reg saved_registers
      end
and generate_signedfactor signedfact =
  match signedfact with
    | SignedFactor(_, None, fact)    -> generate_factor fact
    | SignedFactor(_, Some op, fact) -> 
      begin
        generate_factor fact;
        match op with
          | NO_SUB _ -> print_3 "SUB" (reg (topReg ())) "R0" (reg (topReg ())) "";
          | NO_NOT _ -> generate_boolean_mask "BEQ";
          | NO_BIN_NOT _ ->
            let saved_registers = if nbrReg()=0 then save_reg 1 else [] in
            let _ = freshReg () in
            print_3 "ORIU" (reg (topReg ())) "R0" "0xFFFF" "";
            print_3 "ASHI" (reg (topReg())) (reg (topReg())) "16" "";
            print_3 "ORIU" (reg (topReg ())) (reg (topReg())) "0xFFFF" "";
            print_3 "XOR" (reg (sndReg ())) (reg (sndReg ())) (reg (topReg ())) "";
            dropReg();
      end

and generate_factor fact =
  match fact with
    | Factor(_, simplefact, []) -> generate_simplefactor simplefact
    | Factor(_, simplefact, l)  -> generate_simplefactor simplefact; (*on récupère l'adresse de la classe*)
  let rec aux l =
    match l with
    | [] -> ()
    | (member,args)::q ->
    begin
      match member.symbol with
        |Some F _ -> 
          let offset=Ast.get_offset member.symbol in
          print_3 "LDW" (reg (topReg ())) (reg (topReg ())) (string_of_int offset) ("valeur de "^(get_name member)^" dans "^(reg (topReg ())));  (*on charge la valeur du champ*)
          aux q;
        |Some M _ -> 
          let offset=Ast.get_offset member.symbol in
            let reg_address=topReg () in
            let l=save_reg ((topReg ())-1) in (*on sauve les registres, y compris l'adresse de la classe qui va servir à initialiser this*)
            push(reg_address);
            (*on charge l'adresse de la vmt*)
            print_3 "LDW" (reg (topReg ())) (reg reg_address) "0" ("adresse de la vmt dans "^(reg (topReg ())));
            (*on charge l'adresse de la méthode*)     
            print_3 "LDW" (reg (topReg ())) (reg (topReg ())) (string_of_int offset) ("adresse de "^(get_name member)^" dans "^(reg (topReg ())));     
            begin
            match args with
              |None -> ()
              |Some (Arguments (_,Expressions(_,args))) ->
                let saved_registers = if nbrReg()=0 then save_reg 1 else [] in
                let _ = freshReg () in
                generate_args args;
                decFrameSize (((List.length args) + 1)*4);
                dropReg();
                load_reg saved_registers
            end;
            print_3 "ORIU" (reg r_lnk) "R0" (string_of_int (!addr+8)) "";     (*on saute*)
            print_1 "RET" (reg (topReg ())) "";     (*on saute*)
            print_3 "ADD" (reg reg_address) "R0" (reg r_return) "ON LOAD LES REg-iSters ! *";
            load_reg l;
            aux q;
        |_ -> Report.fail "bizar bizar dans generate.ml"
    end
  in aux l
(*écrit les arguments d'une fonction dans la pile*)
and generate_args l=
  match l with
    | [] -> ()
    | expr::q -> 
      generate_expression expr;
      push (topReg());
      generate_args q

and generate_simplefactor simplefact =
  match simplefact with
    (** TABLEAUX **)
    | Fact_Array(_, args) ->  generate_array(args)
    (** /TABLEAUX **)
    | Fact_Ident(_, var) ->
      begin
        match var.symbol with
          |None        -> Report.fail "Erreur au typage: symbole inconnu"
          |Some (V var_s)->
            begin 
              match var_s.var_offset with
                |None        -> Report.fail ("Erreur à la génération du code: offset de la variable "^(get_name var)^" inconnu")
                |Some offset -> 
                  (* on charge la valeur de la variable *)
                  print_3 "LDW" (reg (topReg ())) (reg r_sp) (string_of_int ((getFrameSize ())-offset)) (get_name var);
            end
          | _ -> 
            Report.fail "Erreur au typage: on aurait dû avoir un symbole de variable"
      end
    | Fact_Number(_, n)         ->
        if (Int32.shift_right n 16 = Int32.of_int 0) then print_3 "ADDI" (reg (topReg ())) "R0" (Int32.to_string n) ""
        else begin
          print_3 "ORIU" (reg (topReg())) "R0" ("(("^(Int32.to_string n)^" >> 16) & 0xffff)") "";
          print_3 "LSHI" (reg (topReg())) (reg (topReg())) "16" "";
          print_3 "ORIU" (reg (topReg())) (reg (topReg())) ("("^(Int32.to_string n)^" & 0xffff)") "";
        end
    | Fact_RD_Int _             -> 
      print_3 "SYSCALL" "0" "0" "15" "";
      print_3 "SYSCALL" (reg (topReg ())) "0" "2" "";
    | Fact_RD_Char _            -> 
      print_3 "SYSCALL" "0" "0" "15" "";
      print_3 "SYSCALL" (reg (topReg ())) "0" "1" "";
    | Fact_Expr(_, expr)        -> generate_expression expr
    | Fact_Body(_, stats, expr) ->
        begin
          generate_statements stats;
          generate_expression expr   
        end
    | Fact_New(_, classe, args) -> generate_new (classe,args)

  
(* ************** *)
(* LES STATEMENTS *)
(* ************** *)
and generate_statements (Statements(_, stat_t_list)) =
  let generate_statement_t = function
      | Stat_Var_Decl (VarDecl(_, Formal(_, varname, _), expr)) ->
        begin
          match varname.symbol with
            | None         -> Report.fail "Erreur au typage: symbole inconnu"
            | Some (V var) -> 
              begin	
                generate_expression expr; 
                push (topReg());
                var.var_offset <- Some (getFrameSize ());                
              end
            | _ -> Report.fail "Erreur au typage: on aurait dû avoir un symbole de variable"
        end
      | Stat_Statement stat -> generate_statement stat
  in
    List.iter generate_statement_t stat_t_list

and generate_statement statement=
  let last_frame_size= getFrameSize() in
  begin
  match statement with
    | Sta_While     (_, expr, s_stat)                  -> generate_while expr s_stat
    | Sta_If        (_, expr, s_stat1, s_stat2)        -> generate_if expr s_stat1 s_stat2
    | Sta_Set       (_, varname, expr)                 -> generate_set varname expr
    | Sta_Do        (_, expr)                          -> generate_expression expr 
    | Sta_PrintChar (_, expr)                          -> generate_printchar expr
    | Sta_PrintInt  (_, expr)                          -> generate_printint expr
    | Sta_Bloc      (_, stats)                         -> generate_statements stats
    | Print_Asm     (s)                                -> print s ""
  end;
  if getFrameSize()-last_frame_size<>0 then
  begin
    print_3 "ADDI" (reg r_sp) (reg r_sp) (string_of_int (getFrameSize()-last_frame_size)) "on dépile les variables déclarées dans le bloc";
    frame_size:=last_frame_size;
  end


and generate_while expr stat = 
  let s = new_label () in
  print_label ("while_"^s);
  generate_condition ("finwhile_"^s) false expr;
  generate_statement stat;
  print_1 "JSR" ("while_"^s) "";
  print_label ("finwhile_"^s)

and generate_if expr stat1 stat2 =
  let s1 = new_label () in   
  generate_condition ("else_"^s1) false expr;
  generate_statement stat1; 
  print_1 "JSR" ("finelse_"^s1) "";
  print_label ("else_"^s1);
  generate_statement stat2;
  print_label ("finelse_"^s1)

and generate_set varname expr = match varname.symbol with
  | None         -> Report.fail "Erreur au typage: symbole inconnu"
  | Some (V var) -> 
    begin
      generate_expression expr; 
      match var.var_offset with
        | None       -> Report.fail "Erreur à la génération du code: offset de variable inconnu"
        | Some offset->
          print_3 "STW" (reg (topReg ())) (reg r_sp) (string_of_int (getFrameSize()-offset)) "";   
    end
  | _            -> Report.fail "Erreur au typage: on aurait dû avoir un symbole de variable"    

and generate_printchar expr = 
  (* Le résultat sera dans le registre R_RETURN *)
  generate_expression expr;
  print_3 "SYSCALL" (reg (topReg ())) "0" "6" ""

and generate_printint expr = 
  generate_expression expr;
  print_3 "SYSCALL" (reg (topReg ())) "0" "7" ""

(* ******* *)
(* MEMBERS *)
(* ******* *)
and generate_member member = match member with
  | Mem_Field(fdecl)    -> ()
  | Mem_Method(methdef) -> generate_method methdef

(* Génère le code de la méthode mdef *)
and generate_method mdef = 
  let args_block_size=ref 0 in
  rsp := 1;
  begin
  match mdef with
    | Method_Def_Long(_, m_header, _, m_expr) -> generate_method_label m_header;
                                                 args_block_size:= getFrameSize();
                                                 generate_prolog ();                                                 
                                                 generate_expression(m_expr)  
    | Method_Def_Short(_, m_header, m_stats)  -> generate_method_label m_header;
                                                 args_block_size:= getFrameSize();
                                                 generate_prolog ();                                                 
                                                 generate_statements(m_stats)
(** META LANGAGE                 **)
    | Method_Def_ignore_Long(_, m_header, _, m_stats) -> generate_method_label m_header;
                                                 args_block_size:= getFrameSize();
                                                 generate_prolog ();                                                 
                                                 generate_statements(m_stats)  
    | Method_Def_ignore_Short(_, m_header, m_stats) -> generate_method_label m_header;
                                                 args_block_size:= getFrameSize();
                                                 generate_prolog ();                                                 
                                                 generate_statements(m_stats)

  end;
  generate_epilog !args_block_size (((getFrameSize()) - !args_block_size) - 4)  

(* Génère l'enregistrement d'activation *)
and generate_method_label header = 
  match header with
    | MethodHeader(_, m_name, formal_l) -> frame_size:=4*(List.length formal_l)+4;
      match m_name.symbol with
        | Some(M(m_symbol)) -> generate_label m_symbol.method_label
        | _                 -> Report.error "..pas de symbole pour le header de methode\n";
      
and generate_label m_label = match m_label with
  | None        -> Report.error "Pas de Label ?\n"
  | Some(label) -> print_label label

(* Pour abstraire un peu *)
and generate_prolog () =
  (* On sauvegarde R_LNK *)
  push 31

and generate_epilog taille_parametres taille_variables =

  if taille_variables<> 0 then (* on va chercher l'adresse de retour *)
    print_3 "ADDI" (reg r_sp) (reg r_sp) (string_of_int taille_variables) "on libère les variables";
  pop r_lnk;
  (* On libère l'enregistrement d'activation *)
  if taille_parametres<> 0 then
    print_3 "ADDI" (reg r_sp) (reg r_sp) (string_of_int taille_parametres) "on libère les arguments";
  (* On reloade LNK *)
  print_1 "RET" (reg r_lnk) "";

and generate_class (ClassDecl(_, class_name, _, member)) = 
  List.iter generate_member member

and generate_vmt (ClassDecl(_,class_name,parent,member)) =
  match class_name.symbol with
    | Some (C class_symb) ->
      begin
        let class_label="class_"^(get_name class_name) in 
          class_symb.class_label<-Some class_label;
          print_label class_label;
          class_symb.address<-Some !addr;
          match parent with
            | None ->
              let offset=ref 0 in
              List.iter (fun (meth_name,meth_symb) ->
                           let meth_label=(get_name class_name)^"_"^meth_name in
                             meth_symb.method_label<-Some meth_label;
                             meth_symb.method_offset<-Some !offset;
                             offset:= !offset+4;
                             print_1 "DATA" meth_label ""
                        ) class_symb.methods;
                offset:=0;
                List.iter (fun (field_name,field_symb) ->
                             offset:= !offset+4;
                             field_symb.field_offset<-Some !offset;
                          ) class_symb.fields       
            | Some parent_name -> 
              begin
                match parent_name.symbol with
                  | Some (C parent_symb) ->
                    let offset = ref (-4) in
                    List.iter (fun (method_name,method_symbol) ->
                                 try
                                   let ancien_symbol = List.assoc method_name parent_symb.methods in
                                   method_symbol.method_offset<-ancien_symbol.method_offset
                                 with |_ -> ()
                              ) class_symb.methods;
                    let methods=List.fast_sort (fun (_,meth_symb1) (_,meth_symb2) ->
                      match meth_symb1.method_offset,meth_symb2.method_offset with
                        |None,_ -> 1
                        |_,None -> -1
                        |Some offset1,Some offset2 -> 
                          if offset1> !offset then 
                            offset:=offset1
                          else
                            if offset2> !offset then 
                              offset:=offset2;
                          offset1-offset2
                      ) class_symb.methods
                      in
                      List.iter (fun (meth_name,meth_symb) -> 
                                  match meth_symb.method_label with
                                  |None ->
                                    let meth_label=(get_name class_name)^"_"^meth_name in
                                      meth_symb.method_label<-Some meth_label;
                                      offset:= !offset + 4;
                                      meth_symb.method_offset<- Some !offset;                                      
                                      print_1 "DATA" meth_label ""
                                  |Some label -> print_1 "DATA" label ""
                                ) methods;
                      offset:=0;
                      List.iter (fun (_,field_symb) ->  
                                  match field_symb.field_offset with
                                    |None -> Report.error "Erreur Ã  la gÃ©nÃ©ration: offset de champ inconnu"
                                    |Some off_set -> if off_set> !offset then offset:=off_set
                                ) parent_symb.fields;   

                      List.iter (fun (_,field_symb) ->
                                  if field_symb.field_offset=None then
                                  begin
                                    offset:= !offset+4;
                                    field_symb.field_offset<-Some !offset;
                                  end
                                ) class_symb.fields

                  | _ -> Report.error "Erreur au typage: symbole de classe incorrect" 
              end
      end
    |_ -> Report.error "Erreur au typage: symbole de classe incorrect"

(** TABLEAUX **)
and generate_array (Arguments(_, Expressions(_,args))) = 
  let saved_registers = if nbrReg()=0 then save_reg 1 else [] in
  let offset = ref 4   in
(** AJOUT *)

(** GC **)
  let l = 
    if (gc_pourri = 1) then save_reg ((topReg ())-1)
                       else []
  in
 (* push(reg_address); utile ??????? *)
(** /GC **)

  let r1 = topReg () in
  let r2 = freshReg () in

  (* Taille de l'objet *)
  print_3 "ADDI" (reg r1) "R0" (string_of_int ((List.length args)*4 + 4)) "create array";
  
(** GC **)
  if (gc_pourri = 1) then 
  begin
    (* Appel au GC *)
    print_3 "ORIU" (reg r2) "R0" "((init >> 16) & 0xffff)" "";
    print_3 "LSHI" (reg r2) (reg r2) "16" "";
    print_3 "ORIU" (reg r2) (reg r2) "(init & 0xffff) + 4" "";
    push(r2); (* this *)
    print_3 "LDW" (reg r2) (reg r2) "0" ""; (* addresse de la vmt *)
    print_3 "LDW" (reg r2) (reg r2) "4" "";  (* Adresse de la fonction malloc *)
    push(r1); (* size *)
    decFrameSize 8;  
    print_3 "ORIU" (reg r_lnk) "R0" (string_of_int (!addr+8)) "";     (*on saute*)
    print_1 "RET" (reg r2) "appel au Gc: malloc()";
    print_3 "ADD" (reg r1) "R0" (reg r_return) "ON LOAD LES REg-iSters ! *";
    load_reg (l);
  end
  else
(** /GC **)
  print_3 "SYSCALL" (reg r1) (reg r1) "12" "";

  (* On give la taille ! *)
  print_3 "ORIU" (reg r2) "R0" (string_of_int ((List.length args)*4)) "";
  print_3 "STW" (reg r2) (reg r1) "0" "";

  (* On give les arg ! *)
  List.iter (fun e -> generate_expression e; print_3 "STW" (reg r2) (reg r1) (string_of_int !offset) ""; offset := !offset + 4) args;
  dropReg();
  if saved_registers<>[] then
  begin
    print_3 "OR" (reg (r1+1)) "R0" (reg r1) "";
    load_reg saved_registers;
  end


(** /TABLEAUX **)

and generate_new (classe, Arguments(_, Expressions(_,args))) = 
  let saved_registers = if nbrReg()=0 then save_reg 1 else [] in
  let offset = ref 4   in
  let vmt_addr = Ast.get_offset classe.symbol in
(** AJOUT *)

(** GC **)
  let l = 
    if (gc_pourri = 1) then save_reg ((topReg ())-1)
                       else []
  in
 (* push(reg_address); utile ??????? *)
(** /GC **)

  let r1 = topReg () in
  let r2 = freshReg () in

  (* Taille de l'objet *)
  print_3 "ADDI" (reg r1) "R0" (string_of_int ((List.length args)*4 + 4)) "new";
  
(** GC **)
  if (gc_pourri = 1) then 
  begin
    (* Appel au GC *)
    print_3 "ORIU" (reg r2) "R0" "((init >> 16) & 0xffff)" "";
    print_3 "LSHI" (reg r2) (reg r2) "16" "";
    print_3 "ORIU" (reg r2) (reg r2) "(init & 0xffff) + 4" "";
    push(r2); (* this *)
    print_3 "LDW" (reg r2) (reg r2) "0" ""; (* addresse de la vmt *)
    print_3 "LDW" (reg r2) (reg r2) "4" "";  (* Adresse de la fonction malloc *)
    push(r1); (* size *)
    decFrameSize 8;  
    print_3 "ORIU" (reg r_lnk) "R0" (string_of_int (!addr+8)) "";     (*on saute*)
    print_1 "RET" (reg r2) "appel au Gc: malloc()";
    print_3 "ADD" (reg r1) "R0" (reg r_return) "ON LOAD LES REg-iSters ! *";
    load_reg (l);
  end
  else
(** /GC **)
  print_3 "SYSCALL" (reg r1) (reg r1) "12" "";


  print_3 "ORIU" (reg r2) "R0" (string_of_int vmt_addr) "";
  print_3 "STW" (reg r2) (reg r1) "0" "";
  (* On give les arg ! *)
  List.iter (fun e -> generate_expression e; print_3 "STW" (reg r2) (reg r1) (string_of_int !offset) ""; offset := !offset + 4) args;
  dropReg();
  if saved_registers<>[] then
  begin
    print_3 "OR" (reg (r1+1)) "R0" (reg r1) "";
    load_reg saved_registers;
  end

(**********  PARTIE EN PLUS  ***********)

and main filename =
  let in_channel = if filename = "--" then stdin else open_in filename in
  let chars = CharReader.charReader_of_in_channel in_channel in
  let scanner = Scanner.new_scanner chars in
  let tree = Parser.parse scanner in
  Analyser.analyse_program tree;
  Report.exit_on_error();
  generate_program tree;
