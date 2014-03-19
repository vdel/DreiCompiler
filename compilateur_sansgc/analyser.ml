open Ast;;

let print_bool b = 
  if (b) then Report.error_at_pos (0,0) "Vrai" else Report.error_at_pos (0,0) "Faux"

let rec error_wrong_type t_attendu t_obtenu =
  "Mauvaise correspondance de type: '"^(Ast.print_type t_attendu)^"' attendu, '"^(Ast.print_type t_obtenu)^"' obtenu."


and analyse_program (Ast.Program(class_list,s)) =
  let gc = ref (List.fold_left analyse_class [] class_list) in
    List.iter (analyse_classdef gc) class_list;
    let _ = analyse_statement !gc [[]] (Ast.Stat_Statement s) in ()

(* Extrait les champs et méthodes d'une liste d_members *)
and analyse_get_members d_members = 
  (* On sépare les fields des methods dans deux accumulateurs
     et on renvoie un couple de liste de ces champs et méthodes
   *)
  let rec aux methods fields = function
  | []   -> (fields, methods)
  | a::q ->
      begin
        match a with
        | Ast.Mem_Field(Ast.FieldDecl(pos, Ast.Formal(_, f_name, f_type))) ->            
            (* Au passage on met à jour le symbole avec le type attendu *)
            aux methods (fields@[(f_name.name, {field_type = Ast.dtype_of_ttype f_type; field_offset=None} )]) q

        | Ast.Mem_Method(Ast.Method_Def_Long(pos, MethodHeader(_, m_name, m_formals), m_type, m_expr)) -> 
          let m_plist = List.map (fun (Formal(_, _, f_type)) -> Ast.dtype_of_ttype f_type) m_formals
          in   
            (* Au passage on met à jour le symbole avec les types des paramètres et de retour de la méthode attendus *)
            aux (methods@[(get_name m_name, { params_type = m_plist ; return_type = Ast.dtype_of_ttype m_type; method_label=None; method_offset = None})]) fields q    
        | Ast.Mem_Method(Ast.Method_Def_Short(pos, m_header, Ast.Statements(_, m_stats))) -> 
          begin
            match m_header with Ast.MethodHeader(_, m_name, m_formals) ->
              let m_plist = List.map (fun (Ast.Formal(_, _, f_type)) -> Ast.dtype_of_ttype f_type) m_formals
              in 
                (* Ici par défaut, le type d'une méthode à définition courte est TNone, c'est comme ça *)
                aux (methods@[(get_name m_name, { params_type = m_plist ; return_type = Ast.TNone; method_label=None; method_offset = None})]) fields q    
          end


(** META LANGAGE                                                 **)

        | Ast.Mem_Method(Ast.Method_Def_ignore_Long(pos, MethodHeader(_, m_name, m_formals), m_type, m_expr)) -> 
          let m_plist = List.map (fun (Formal(_, _, f_type)) -> Ast.dtype_of_ttype f_type) m_formals
          in   
            (* Au passage on met à jour le symbole avec les types des paramètres et de retour de la méthode attendus *)
            aux (methods@[(get_name m_name, { params_type = m_plist ; return_type = Ast.dtype_of_ttype m_type; method_label=None; method_offset = None})]) fields q    
        | Ast.Mem_Method(Ast.Method_Def_ignore_Short(pos, m_header, Ast.Statements(_, m_stats))) -> 
          begin
            match m_header with Ast.MethodHeader(_, m_name, m_formals) ->
              let m_plist = List.map (fun (Ast.Formal(_, _, f_type)) -> Ast.dtype_of_ttype f_type) m_formals
              in 
                (* Ici par défaut, le type d'une méthode à définition courte est TNone, c'est comme ça *)
                aux (methods@[(get_name m_name, { params_type = m_plist ; return_type = Ast.TNone; method_label=None; method_offset = None})]) fields q    

          end            
      end 
  in
    aux [] [] d_members
    
(* Effectue l'union disjointe de méthodes 
 * l1 et l2 sont des method_scope
 *)
and analyse_xunion_m l1 l2 l_stock =  match l1 with 
  | [] -> l2
  | (a,b)::q -> 
    try   
      let _ =  (List.assoc a l2) in
      if (List.mem a l_stock) then Report.error ("Méthode \'"^(a)^"\' déjà définie dans la classe courante.");
      analyse_xunion_m q ((a,b)::(List.filter (fun (name,_) -> not (a=name)) l2)) (a::l_stock)
    with 
     | Not_found -> analyse_xunion_m q ((a,b)::l2) (a::l_stock)
     | _ -> []
(* Idem mais pour les champs. pourquoi Caml nous oblige-t-il à faire deux
   fonctions identiques? Pour nous faire rager ?
  *)
and analyse_xunion_f l1 l2  =  match l2 with 
  | [] -> l1
  | (a,b)::q -> 
    try   
      let _ = (List.assoc a l1) in
      (* On n'a pas le droit de redéfinir un field dans une sous-classe,
         néanmoins on dit qu'on ne garde que celui de la classe parente
         et on tente de continuer
       *)
      Report.error ("Champ \'"^(a)^"\' déjà défini dans la classe parente ou courante.") ;
      analyse_xunion_f l1 q
    with 
     | Not_found -> let l = analyse_xunion_f l1 q in (a,b)::l
     | _ -> []

(* Supprime l'élément t de la liste tq f(t) est vrai *)
and list_supp_c f l=
  match l with
    | [] -> []
    | t::q -> if f t then (list_supp_c f q) 
		     else t::(list_supp_c f q)
(* la même, mais en différent (les conneries à Caml, normal quoi) *)
and list_supp_m f l=
  match l with
    | [] -> []
    | t::q -> if f t then (list_supp_m f q) 
		     else t::(list_supp_m f q)
(* renvoit le type de var dans le contexte de variable amélioré (groupe bleu) *)
and find_type_var var_scope var=
  match var_scope with
    | [] -> raise Not_found;
    | [t] -> List.assoc var t
    | t::q ->
      begin
        try
          List.assoc var t
        with | _ ->
          find_type_var q var
      end
(* si une var existe dans le bloc courant, renvoit le symbole associée, sinon, soulève Not_Found *)
and find_var_in_current_scope var_scope var=
  match var_scope with
    | [] -> raise Not_found
    | t::q -> List.assoc var t
(* ajoute var au contexte de variable du bloc courant *)
and add_var var_scope var=
  match var_scope with
    | [] -> Report.error "Contexte de variable vide, c'est pas normal, cf analyser.ml, ligne 13"; [[var]]
    | t::q -> (var::t)::q
(* crée un nouveau bloc dans le contexte de variable *)
and add_block var_scope= []::var_scope


(* Analyse/insertion des classes *)

(* Retourne true si la classe c_id a été insérée
   dans le c_scope 
 *)
and analyse_exist_class c_scope c_id = 
  try 
    let _ = List.assoc c_id c_scope in
    true
  with
   | Not_found -> false
   | _ -> true


(* Ajoute une classe à un c_scope *)
and analyse_addclass c_scope c_id c_parents c_f_scope c_m_scope =
  let symb = { parents = c_parents; fields = c_f_scope; methods = c_m_scope; class_label=None; address=None} in
    Ast.set_symbol c_id (C symb);
    (Ast.get_name c_id, symb)::c_scope

(* Effectue l'insertion de classe *)
and analyse_class c_scope (Ast.ClassDecl(d_pos, d_id, d_ext_name, d_members)) =
  let f_scope2 = ref [] and m_scope2 = ref [] and parents = ref [] in
  if (analyse_exist_class c_scope (get_name d_id)) then 
     (let _ = Report.error_at_pos d_pos ("Classe \'"^(get_name d_id)^"\' déjà insérée.") in
     c_scope)
  else
  begin
    
    begin
      match d_ext_name with
      (* Ici on regarde si on est dans le cas d'une classe héritant d'une autre ou pas *)
        | None           -> parents := [get_name d_id]
        | Some(d_ext_id) ->  
            begin
              try
                let c = List.assoc (get_name d_ext_id) c_scope in
                Ast.set_symbol d_ext_id (C c);
                parents  := (get_name d_id)::c.parents;
                f_scope2 := c.fields;
                m_scope2 := c.methods
              with 
                | Not_found -> Report.error_at_pos d_pos ("Classe \'"^(Ast.get_name d_ext_id)^"\' non définie."); 
                | _         -> ()
            end        
    end;
    (* Union exclusive *)
    let (f_scope_tmp, m_scope_tmp) = analyse_get_members d_members
    in   
      f_scope2 := analyse_xunion_f f_scope_tmp (!f_scope2);
      m_scope2 := analyse_xunion_m m_scope_tmp (!m_scope2) [];
      analyse_addclass c_scope d_id !parents !f_scope2 !m_scope2
  end 

(* Analyse la définition d'une classe *)
and analyse_classdef c_scope_ref (Ast.ClassDecl(d_pos, d_name, _, d_members)) =
  match d_name.symbol with
  |Some (C class_symbol) ->
    let rec aux = function
      | []                                                               		       -> ()
      | (Mem_Field(FieldDecl(f_pos, Formal(_, f_id, _))) as d_member)::q 		       ->
          let field_symbol = List.assoc (get_name f_id) class_symbol.fields in
          analyse_member (get_name d_name) c_scope_ref f_id (F field_symbol) d_member;
          aux q
      | (Mem_Method(Method_Def_Long(_, (MethodHeader(_, m_id, _)),_ ,_)) as d_member)::q -> 
          let meth_symbol = List.assoc (get_name m_id) class_symbol.methods in
          analyse_member (get_name d_name) c_scope_ref m_id (M meth_symbol) d_member; 
          aux q
      | (Mem_Method(Method_Def_Short(_,(MethodHeader(_, m_id, _)),_)) as d_member)::q    -> 
          let meth_symbol = List.assoc (get_name m_id) class_symbol.methods in
          analyse_member (get_name d_name) c_scope_ref m_id (M meth_symbol) d_member; 
          aux q
(** META LANGAGE                                                  *)
      | (Mem_Method(Method_Def_ignore_Long(_, (MethodHeader(_, m_id, _)),_ ,_)) as d_member)::q -> 
          let meth_symbol = List.assoc (get_name m_id) class_symbol.methods in
          analyse_member (get_name d_name) c_scope_ref m_id (M meth_symbol) d_member; 
          aux q
      | (Mem_Method(Method_Def_ignore_Short(_,(MethodHeader(_, m_id, _)),_)) as d_member)::q    -> 
          let meth_symbol = List.assoc (get_name m_id) class_symbol.methods in
          analyse_member (get_name d_name) c_scope_ref m_id (M meth_symbol) d_member; 
          aux q

    in
      if (analyse_exist_class (!c_scope_ref) (get_name d_name))
        then
          aux d_members
        else 
          ()
  |_ -> ()
	
(* Analyse des membres d'une classe *)
and analyse_member c c_scope_ref m_id symbol= function
  | Mem_Field(m_field)   -> analyse_field c_scope_ref m_id m_field symbol
  | Mem_Method(m_method) -> analyse_method c c_scope_ref m_id m_method symbol
(* Analyse d'un champ *)
and analyse_field c_scope_ref f_id (FieldDecl(f_pos, Formal(_, _, f_type))) field_symbol=
  analyse_type f_pos !c_scope_ref (Ast.dtype_of_ttype f_type);
  Ast.set_symbol f_id field_symbol

(* Retourne le type du Formal : AFAIRE TBAD *)
and analyse_formal c_scope (Formal(pos, f_name, f_type)) = 
  analyse_type pos c_scope (dtype_of_ttype f_type);
  Ast.dtype_of_ttype f_type

(* Retourne true si t1 est un sous-type de t2 *)
and analyse_is_subtype c_scope t1 t2 = 
  match t1 with 
  (** TABLEAUX **)
  | TArray(dt1) -> begin
                   match t2 with
                   | TArray(dt2) -> analyse_is_subtype c_scope dt1 dt2
                   | TBad -> true
                   | _ -> false
                  end
  (** /TABLEAUX **)
  | TInt      -> begin
                   match t2 with 
                   | TInt | TBad -> true
                   | _           -> false
                 end
  | TNone     -> begin
                  match t2 with 
                  | TNone | TBad -> true
                  | _            -> false
                end
  | TClass(id) -> begin
                    try
                      let symbol = List.assoc id c_scope in
                        match t2 with 
                          | TClass(id) -> List.mem id symbol.parents
                          | TBad       -> true
                          | _          -> false
                    with 
                      | Not_found -> 
                          Report.error_at_pos (-1,-1) ("Classe "^id^" inexistante");
                          false
                      | _ -> false
                  end                         
  | TBad       -> true

(* Analyse une méthode || MODIFIEE LE 5 JANVIER *)
(* Dans cette fonction, _c_ est la classe courante *)
and analyse_method c_name c_scope_ref m_id m_def meth_symbol=
match meth_symbol with
|M meth_symbol -> 
begin
let c_scope = !c_scope_ref in
  (* Cette fonction récupère la liste de type des paramètres
     d'une méthode *)
try
  let c = List.assoc c_name c_scope in

  (* Cette fonction récupère une liste de type
     à partir d'une liste de formals
   *)
  let list_formals_type m_formals = 
    let rec aux = function
      | []   -> []
      | a::q -> (analyse_formal c_scope a)::(aux q)
    in
      aux m_formals
  in

  (* vérifie le sous-typage correct dans les classes parentes *)
  let rec aux1 pos ptype type_ret = function   
    | []   -> ()
    | a::q -> 
      begin
        try 
          (* On récupère la méthode de la classe parente,
             si elle existe *)
          let mp = List.assoc (get_name m_id) (List.assoc a c_scope).methods in
          (* A priori, on a déjà typé la classe parente *)
          (* On vérifie que les relations de sous-typage sont bien respectées *)
          if not(
            (analyse_is_subtype c_scope type_ret mp.return_type) && 
             (List.fold_left2 
               (fun a b c -> (a && (analyse_is_subtype c_scope b c)))
               true
               mp.params_type
               ptype)) then (Report.error_at_pos pos ("Les paramètres de '"^(get_name m_id)^"' n'ont pas le même type que dans la classe parente '"^a^"'")) (* erreur     *)
                       else () (* pas erreur *);
        with  _ -> () (* La méthode n'est pas héritée d'une classe parente *)
      end;
      aux1 pos ptype type_ret q
        
  in
  (* crée un var_scope à partir des arguments de la méthode *) 
  let rec aux2 formals_list types_list offset= match formals_list,types_list with
    | (Ast.Formal(_, name, _))::q1, dtype::q2 -> 
         let symbole = { var_type = dtype; var_offset = Some offset} in
           set_symbol name (V(symbole));
           (get_name name, symbole)::(aux2 q1 q2 (offset+4))
    | _ -> []
  in

  (*aux3 vérifie les relations de sous-typages entre classes parentes et calcule le var_scope nécessaire à l'évaluation du corps de la méthode*) 
  let aux3 (MethodHeader(pos, m_name, m_formals)) b m_dtype = 
    (* Type de retour *)
    analyse_type pos c_scope m_dtype;     
    let ptype = list_formals_type m_formals in
      if b then aux1 pos ptype m_dtype c.parents
           else ();
      (* on met à jour le symbole de la méthode pour que les types soient bien actualisés *)
      meth_symbol.params_type <- ptype;
      meth_symbol.return_type <- m_dtype;
      set_symbol m_name (M meth_symbol);
      let new_methods_list = 
        (get_name m_id, meth_symbol):: (list_supp_m (fun (string_name,_) -> string_name=(get_name m_id)) c.methods)
      in
      (* et on met à jour le class_scope avec le nv class_symbol*)
      c.methods<-new_methods_list;
      (*c_scope_ref :=
        (c_name,c)::(list_supp_c (fun (string_name,_) -> string_name = c_name) c_scope); *)
      (* Le nouveau var_scope *)
      [("this", { var_type = (Ast.TClass c_name); var_offset=Some(4)})::(aux2 m_formals ptype 8)]
  in    

  match m_def with
    | Method_Def_Long(pos, m_header, m_type, m_expr)  ->
        let type_obtenu = analyse_expression !c_scope_ref (aux3 m_header true (Ast.dtype_of_ttype m_type)) m_expr
        in if (not(analyse_is_subtype !c_scope_ref type_obtenu (Ast.dtype_of_ttype m_type)))   
             then Report.error_at_pos pos (error_wrong_type (Ast.dtype_of_ttype m_type) type_obtenu)
             else () 
                  
    | Method_Def_Short(_, m_header, Statements(_,stats_l)) ->
        let _ = List.fold_left (analyse_statement !c_scope_ref) (aux3 m_header true Ast.TNone) stats_l in
	    ()
    (** META LANGAGE, on met à TBAD, c'est un artifice **)
    | Method_Def_ignore_Long(pos, m_header, m_type, Statements(_,m_stats))  ->
       let _ = List.fold_left (analyse_statement !c_scope_ref) (aux3 m_header false Ast.TBad) m_stats in
	    ()
                  
    | Method_Def_ignore_Short(_, m_header, Statements(_,stats_l)) ->
        let _ = List.fold_left (analyse_statement !c_scope_ref) (aux3 m_header false Ast.TBad) stats_l in
	    ()
           
with
  | Not_found -> Report.error_at_pos (-1,-1) ("Classe \'"^c_name^"\' non définie.")
  | _ -> ()
end
|_ -> ()

(* Analyse le type *)
and analyse_type pos c_scope = function
  | TClass(c_id)        -> if not(analyse_exist_class c_scope c_id) then
                             Report.error_at_pos pos ("Classe \'"^c_id^"\' non définie.")
                           else
                             ()
  (** TABLEAUX **)
  | TArray _ -> ()
  (** /TABLEAUX **)
  | TInt | TNone | TBad -> ()


(* analyse une expression dans le contexte de classe gc et le contexte de variables gv *)
and analyse_expression gc (gv:Ast.var_scope) t =
  match t with
    (* si c'est une sumexpression sans opérateur de comparaison, on passe la main à sumexpression *)
    | Ast.Expression(_,se,None) -> analyse_sumexpression gc gv se   
    (* s'il, y a un opérateur de comparaison, on analyse les membres de part et d'autre...*)
    | Ast.Expression(pos,se1,Some(op,se2)) -> 
	begin
	  let t1 = analyse_sumexpression gc gv se1
	  and t2 = analyse_sumexpression gc gv se2
	  in
	    (* ... puis on vérifie leur type *)
	    match op with
	      | Ast.CO_EQ(pos) | Ast.CO_NE(pos) -> 
		  begin
			(* pour == et !=, il faut que chacun soit sous-type de l'autre *)
		    if analyse_is_subtype gc t1 t2 && analyse_is_subtype gc t2 t1 then
		      t1  (* les deux types sont égaux *)
		    else
		      begin
			Report.error_at_pos pos "Impossible de comparer les deux opérandes"; 
			Ast.TBad   (* puis on renvoit TBad *)
		      end
		  end
	      | _ ->
		  begin
			(* pour les autres comparateurs, t1 et t2 doivent être TInt et on renvoit TInt *)
		    if t1 <> Ast.TInt && t1<>Ast.TBad then
		      begin
			let Ast.SumExpression(pos,_,_)= se1 in Report.error_at_pos pos (error_wrong_type Ast.TInt t1)
		      end;
		    if t2 <> Ast.TInt && t2<>Ast.TBad then
		      begin
			let Ast.SumExpression(pos,_,_)= se2 in Report.error_at_pos pos (error_wrong_type Ast.TInt t2)
		      end;
		    Ast.TInt
		  end
	end
(* pour analyser le type des sumexpressions *)
and analyse_sumexpression gc gv se =
  (* une fonction auxiliaire qui vérifie que tous les Terms de la somme sont de type TInt *)
  let rec aux l =
    match l with
      | [] -> ()
      | (_,t)::q ->
	  begin 
	    let type_term = analyse_term gc gv t in 
	      begin
		if type_term <> Ast.TInt then
		  let Ast.Term(pos,_,_) = t in Report.error_at_pos pos (error_wrong_type Ast.TInt type_term)
	      end;
	      aux q
	  end
  in
    match se with
	(* soit on a qu'un seul term: pas besoin de aux *)
      | Ast.SumExpression(_,t,[]) -> analyse_term gc gv t 
        (* soit on en a plusieurs: on appelle aux et on renvoit TInt parce que on fait une somme*) 
      | Ast.SumExpression(_,t,l) -> 
	  begin 
	    let type_term = analyse_term gc gv t in 
	      begin
		if type_term <> Ast.TInt then
		  let Ast.Term(pos,_,_) = t in Report.error_at_pos pos (error_wrong_type Ast.TInt type_term)
	      end;
	      aux l;
	      Ast.TInt
	  end
(* Analyse du type d'un term: idem que ci-dessus avec aux, etc ... *)
and analyse_term gc gv t = 
  let rec aux l =
    match l with
      | [] -> ()
      | (_,sf)::q ->
	  begin 
	    let type_sfactor = analyse_signedfactor gc gv sf in 
	      begin
		if type_sfactor <> Ast.TInt then
		  let Ast.SignedFactor(pos,_,_) = sf in Report.error_at_pos pos (error_wrong_type Ast.TInt type_sfactor)
	      end;
	      aux q
	  end
  in
    match t with                                              
      | Ast.Term(_,sf,[]) -> analyse_signedfactor gc gv sf
      | Ast.Term(pos,sf,l) -> 
	  begin 
	    let type_sfactor = analyse_signedfactor gc gv sf in 
	      begin
          match type_sfactor with 
(** TABLEAUX **)
          | Ast.TArray a ->
              if (List.length l <> 1) then let _ = Report.error_at_pos pos (error_wrong_type Ast.TInt type_sfactor) in TBad
              else begin  
                let (prod, fac) = List.hd l in begin 
                match prod with | Ast.PO_DIESE _ -> let s_type = analyse_signedfactor gc gv fac in 
                                    if (s_type <> Ast.TInt) then let _ = Report.error_at_pos pos (error_wrong_type Ast.TInt s_type) in TBad
                                                            else a
                                | _ -> let _ = Report.error_at_pos pos (error_wrong_type Ast.TInt type_sfactor) in TBad
                end
              end 
(** /TABLEAUX **)
          | Ast.TInt -> 
              let (prod, fac) = List.hd l in 
              begin match prod with 
                | Ast.PO_DIESE _ -> Report.error_at_pos pos ("Il faut du Array"); TBad
                | _ ->            aux l; Ast.TInt
              end
          | _ -> let Ast.SignedFactor(pos,_,_) = sf in Report.error_at_pos pos (error_wrong_type Ast.TInt type_sfactor); TBad
	
	      end
    end

(* Analyse du type d'un signedfactor *)
and analyse_signedfactor gc gv sf =
  match sf with
	(* s'il n'y a pas d'opérateur de négation: *)
    | Ast.SignedFactor(_,None,f) -> analyse_factor gc gv f
	(* s'il y en a un, on sait qu'on renvoit du TInt quoi qu'il arrive *)
    | Ast.SignedFactor(_,Some op,f) -> 
	begin
	  let type_factor = analyse_factor gc gv f in
	    begin
	      if type_factor <> Ast.TInt then
		let Ast.Factor(pos,_,_) = f in Report.error_at_pos pos (error_wrong_type Ast.TInt type_factor)
	    end;
	    Ast.TInt
	end
and analyse_factor gc gv (Ast.Factor(pos,sf,l)) =
  (* une  première fonction auxiliaire qui vérifie que , lors d'un appel de méthode, 
     les expressions passées en argument sont bien du type attendu:
	type_list est la list des types attendus pour chaque argument, 
	et expr_list, la liste des expressions passées en argument *) 
  let rec verif_args type_list expr_list =
    match type_list,expr_list with
      | (t1::tl,e1::el) ->
	  let type_expr = analyse_expression gc gv e1 in
	    if type_expr<>Ast.TBad && type_expr<>t1 then
	      begin
		let Ast.Expression(pos_e,_,_) = e1 in
		  Report.error_at_pos pos_e (error_wrong_type t1 type_expr)
	      end;
	    verif_args tl el		
      | _ -> ()   (* ce cas n'arrive jamais, on vérifie avant que les deux listes ont la même taille *)
  in 
  (* une deuxième fonction auxiliaire qui est appelée pour remonter la liste des objets appelés.
     Par exemple, pour a.b.c.print_obj(), on passe à aux l'objet en cours (initialement a),
     ainsi que la liste des champs et des méthodes appelés et aux s'occupe de vérifier que ceux-ci
     appartiennent à la classe parente, ont le bon type et sont appelés avec le bon nombre d'arguments *)
  let rec aux type_op l =  (* type_op est le type de l'objet courant, l la liste des champs ou méthode appelés *)
    match l with
      | [] -> type_op (* la liste est vide: on a fini, on renvoit le type courant *)
      | (n,arg_opt)::q ->   (* sinon on regarde le premier objet de nom n appelé avec les arguments arg_opt *)
	  match type_op with
		(* si le type courant n'est pas une classe, impossible de choisir un membre: on renvoit TBad *)
	    | Ast.TInt | Ast.TNone -> Report.error_at_pos pos "L'opérande doit être une classe"; Ast.TBad
(** TABLEAUX **)
      | Ast.TArray _ -> TBad	
(** /TABLEAUX **)	                
	    | Ast.TBad -> type_op
		(* si c'est une classe de nom class_name *) 
	    | Ast.TClass(class_name) ->
		try
		  begin			
			(* on récupère le symbole associé *)
		    let symb = List.assoc class_name gc in
			(* et on regarde les arguments *)
		      match arg_opt with
			| None -> (* il y en a pas:l'objet n est donc un champ *)
			  begin
			    try
			      let field = List.assoc (Ast.get_name n) symb.fields in
				Ast.set_symbol n ((F field));  (* on récupère le symbole et met à jour le nom *)
				aux field.field_type q 	(* et on appelle aux avec le type du champ et la liste des membres restants *)	      
			    with |_ -> (* List.assoc soulève une exeception: le champ n'existe pas *)
				begin
				  Report.error_at_pos pos ("Champs \'"^(Ast.get_name n)^"\' non défini.");
				  Ast.TBad
				end
			  end
			| Some(Ast.Arguments(_,Ast.Expressions(_,arg_list))) -> (* il y a des arguments: n est une méthode *)
			    begin
			      try
				let meth = List.assoc (Ast.get_name n) symb.methods in (* on récupère le symbole *)
				Ast.set_symbol n (M meth); (* on met à jour le nom *)
				(* on calcule la longueur des liste des types attendus et des expressions passées *)
				let l1 = List.length meth.params_type
				and l2 = List.length arg_list
				in
				  if l1 = l2 then (* si c'est la même tout va bien *)
				      (* on vérifie le type des arguments *)
				      verif_args meth.params_type arg_list
				  else
				      (* sinon, on affiche une erreur *)
				      Report.error_at_pos pos ("La méthode est appliquée à "^(string_of_int l2)^" au lieu de "^(string_of_int l1)^" argument(s).");
				    (* et on continue le travail avec le type de retour de la méthode *)
				  aux meth.return_type q
			      with
				| _ -> (* si List.assoc renvoit une erreur: la méthode n'existe pas *)
				    begin
				      Report.error_at_pos pos ("Méthode \'"^(Ast.get_name n)^"\' non définie.");
				      Ast.TBad
				    end
			    end
		  end
		with
		  | _ ->  (* si List.assoc renvoit une erreur: la classe courant n'existe pas *)
		      begin
			Report.error_at_pos pos ("Classe \'"^class_name^"\' non définie.");
			Ast.TBad
		      end
  in (* on appelle aux sur le type du simplefactor initial*)
    aux (analyse_simplefactor gc gv sf) l 

and analyse_simplefactor gc gv sf = 
  match sf with
    | Fact_Ident(pos,n) ->
	begin
	  try  (* on récupère le symbole de la variable et on met à jour le champ symbol de son nom *)
	    let sym = find_type_var gv (Ast.get_name n) in
	      Ast.set_symbol n (V sym);
	      sym.var_type  (* et on renvoit son type *)
	  with
	    | _ -> 
		begin
		  Report.error_at_pos pos ("Symbole \'"^(Ast.get_name n)^"\' non défini.");
		  Ast.TBad
		end	
	end
    | Fact_Array(_,Ast.Arguments(_,Ast.Expressions(_,arg_list))) -> begin
	      (* la même fonction que tout à l'heure qui vérifie que les arguments collent avec le type attendu *)
	      let rec verif_args type_att expr_list =
	        match expr_list with
	          | e1::el ->
		            let type_expr = analyse_expression gc gv e1 in
		            if not (analyse_is_subtype gc type_expr type_att) then
		            begin
		              let Expression(pos_e,_,_) = e1 in
			            Report.error_at_pos pos_e (error_wrong_type type_att type_expr)
		            end;
		            verif_args type_att el		
	          | _ -> ()  (* cas impossible, les deux liste font la même taille *)
	      in
          let att_type = analyse_expression gc gv (List.hd arg_list) in
          verif_args att_type (List.tl arg_list);
          Ast.TArray(att_type)
      end
    | Fact_RD_Int(_)
    | Fact_RD_Char (_)   (* tout ca, ca renvoit du TInt *)
    | Fact_Number(_,_) -> Ast.TInt
    (* on apelle simplement analyse_expression *)
    | Fact_Expr (_,e) -> analyse_expression gc gv e
    (* pour le corps d'une méthode *)
    | Fact_Body (_,Ast.Statements(_,l),e) -> 
	(* on évalue les statements en mettant à jour le contexte de variables *)
	let gv' = List.fold_left (analyse_statement gc) gv l in
	(* et on renvoit le type de l'expression du return *)
	  analyse_expression gc gv' e
    | Fact_New (pos,n,Ast.Arguments(_,Ast.Expressions(_,arg_list))) ->
	(* la même fonction que tout à l'heure qui vérifie que les arguments collent avec le type attendu *)
	let rec verif_args type_list expr_list =
	  match type_list,expr_list with
	    | (t1::tl,e1::el) ->
		let type_expr = analyse_expression gc gv e1 in
		  if not (analyse_is_subtype gc type_expr t1) then
		    begin
		      let Expression(pos_e,_,_) = e1 in
			Report.error_at_pos pos_e (error_wrong_type t1 type_expr)
		    end;
		  verif_args tl el		
	    | _ -> ()  (* cas impossible, les deux liste font la même taille *)
	in
	  try
	    begin
		(* on récupère le symbole de l'objet que l'on crée *)			
	      let obj = List.assoc (Ast.get_name n) gc in
		(* on met à jour le Ast.name *)
	      Ast.set_symbol n (C obj);
		(* on récupère les types des champs à initialiser *)
	      let type_list = List.map (fun x -> let (_,t) = x in t.field_type) obj.fields
	      in
	      let l1 = List.length type_list
	      and l2 = List.length arg_list
	      in  (* si on a le nombre de champs attendus *)
		if l1 = l2 then 
		  verif_args type_list arg_list  (* on vérifie leur type *)
		else
		  (* sinon on crie *)
		  Report.error_at_pos pos ("Le constructeur de '"^(Ast.get_name n)^"' attend "^(string_of_int l1)^" arguments et non "^(string_of_int l2)^".");	    
		(* et on renvoit le type de la nouvelle classe *)
		Ast.TClass(Ast.get_name n)
	    end
	  with
	    | _ ->  (* au cas où le nom de classe est erroné *)
		begin
		  Report.error_at_pos pos ("Classe \'"^(Ast.get_name n)^"\' non définie.");
		  Ast.TBad
		end
(* prend en argument un Ast.statements_t *)
and analyse_statement gc gv s =
  match s with 
	(* IF *)
    | Ast.Stat_Statement(Ast.Sta_If (_,t,s1,s2)) ->
	begin
	  begin
	  (* le test du IF doit être de type TInt *)
	    match analyse_expression gc gv t with
	      | Ast.TInt | Ast.TBad -> () (* ok *)
	      | bad_type -> let Ast.Expression(pos,_,_)= t in Report.error_at_pos pos (error_wrong_type Ast.TInt bad_type)
            end;
	    (* on vérifie que tout va bien dans le THEN ELSE *) 
	    let _ = analyse_statement gc gv (Ast.Stat_Statement s1) 
	    and _ = analyse_statement gc gv (Ast.Stat_Statement s2)
	    in gv
	end
	(* WHILE *)
    | Ast.Stat_Statement(Ast.Sta_While (_,t,s1)) ->
	begin
	  begin
	    (* le test du WHILE doit être de type TInt *)
	    match analyse_expression gc gv t with
	      | Ast.TInt | Ast.TBad -> ()  (* ok *)
	      | bad_type -> let Ast.Expression(pos,_,_)= t in Report.error_at_pos pos (error_wrong_type Ast.TInt bad_type)
	  end;		
	    (* on vérifie que l'intérieur de la boucle est correct *)
	  let _ = analyse_statement gc gv (Ast.Stat_Statement s1) 
	  in gv
	end
	(* Déclaration de variable *)
    | Ast.Stat_Var_Decl(Ast.VarDecl(pos1,Ast.Formal(pos2,n,ttypeT),t)) ->
      begin
	let typeT = Ast.dtype_of_ttype ttypeT in (* on passe de ttype à dtype *)
	  analyse_type pos1 gc typeT;  (* on vérifie que le type est bien formé *)
	  let var_symbol = {var_type=typeT; var_offset=None} in
	    Ast.set_symbol n (V var_symbol); (* on met à jour le name *)
	    let typeexpr = analyse_expression gc gv t  (* type de l'expression qui initialise la variable *)
	    in
	      if not (analyse_is_subtype gc typeexpr typeT) then  (* on vérifie la relation de sous-typage *)
	        begin
		  Report.error_at_pos pos2 (error_wrong_type typeT typeexpr); 
		  gv
                end
	      else
	        try (* si le sous-typage est correct, on vérifie que la variable n'est pas déjà déclarée *) 
		  let s = Ast.get_name n in
		  let _ = find_var_in_current_scope gv s in
		    Report.error_at_pos pos1 ("Nom de variable déjà déclaré: "^s);
		    gv				    			
	        with
		  | _ -> 
		  begin
		    add_var gv (Ast.get_name n,var_symbol);    (* on ajoute la variable au contexte de variable *)
		  end
	end
	(* SET *)
    | Ast.Stat_Statement(Ast.Sta_Set(pos,n,t)) ->
	begin
	(* string de nom de la variable *)
	  let s = Ast.get_name n in
	    try
	      let typeT = find_type_var gv s in  (* on récupère le symbole et on met à jour le name *)
		Ast.set_symbol n (V typeT);
	      let typeexpr = analyse_expression gc gv t  (* type de l'expression *)
	      in
		if not (analyse_is_subtype gc typeexpr typeT.var_type) then   (* on vérifie la relation de sous-typage *)
		Report.error_at_pos pos (error_wrong_type typeT.var_type typeexpr);
		gv
	    with
	      | _ ->  (* si la variable n'existe pas: problème *) 
  		begin
                  Report.error_at_pos pos ("Symbole \'"^s^"\' non défini.");
		  gv
		end	  
	end
    | Ast.Stat_Statement(Ast.Sta_Do(pos,t)) -> (* on évalue l'expression, ca ne change pas le contexte de variables *)
	let _ = analyse_expression gc gv t in gv

    (** ATTENTION, META TRUC ASM **)
    | Ast.Stat_Statement(Ast.Print_Asm(s)) -> 
	gv
    (** **)


    | Ast.Stat_Statement(Ast.Sta_PrintInt(pos,t)) 
    | Ast.Stat_Statement(Ast.Sta_PrintChar(pos,t)) ->
	begin
	  match analyse_expression gc gv t with  (* les expressions en arguments doivent être de type TInt *)
	    | Ast.TInt | Ast.TBad -> gv
	    | bad_type -> let Ast.Expression(pos,_,_)= t in Report.error_at_pos pos (error_wrong_type Ast.TInt bad_type); gv
	end
    | Ast.Stat_Statement(Ast.Sta_Bloc(_,Ast.Statements(_,l))) -> (* on évalue le bloc: il faut rajouter un nouveau bloc temporaire au contexte de variables *)
	let new_gv = add_block gv in
	let _ = List.fold_left (fun gv2 stat -> analyse_statement gc gv2 stat) new_gv l in gv


and print_class_scope cs=    (* petites fonctions d'affichage pour débuggage *)
  match cs with
		| [] -> ()
    | (s,class_symbol)::q -> 
			Printf.printf "CLASSE: %s\n" s;
			List.iter (fun s -> Printf.printf " -> PARENTS: %s\n" s) class_symbol.parents; 
                        print_field_scope class_symbol.fields;
			print_method_scope class_symbol.methods;
                        Printf.printf("\n");
			print_class_scope q;
and print_field_scope fs=
	match fs with
		| [] -> ()
		| (s,field_symbol)::q ->
			Printf.printf "  FIELD: %s, type %s\n" s (Ast.print_type field_symbol.field_type);
			print_field_scope q;
and print_method_scope ms = 
  match ms with 
  | [] -> ()
  | (s,method_symbol)::q1 -> 
    match method_symbol.params_type with
      | [] -> 
          Printf.printf "  MEMBER: %s : %s(" s (Ast.print_type method_symbol.return_type); print_string ")\n";
          print_method_scope q1;
      | t::q2 ->
          Printf.printf "  MEMBER: %s : %s(%s" s (Ast.print_type method_symbol.return_type) (Ast.print_type t);
          List.iter (fun dtype -> Printf.printf ",%s" (Ast.print_type dtype)) q2;
          print_string ")\n";
          print_method_scope q1

			
and main filename=
  let in_channel = if filename = "--" then stdin else open_in filename in
  let chars = CharReader.charReader_of_in_channel in_channel in
  let scanner = Scanner.new_scanner chars in
  let tree = Parser.parse scanner in
  analyse_program tree;
  Report.exit_on_error ();
  print_string "Analyse successful\n"  
and list_on () = ()
;;

