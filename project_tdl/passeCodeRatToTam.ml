(* Module de la passe de g´en´eration de code *)
(* doit être conforme à l'interface Passe *)
open Tds
open Type
(* open Exceptions *)
open Ast
open Tam
open Code 

type t1 = Ast.AstPlacement.programme
type t2 = string


(* analyse_tam_deref : AstPlacement.affectable -> typ * string *)
(* Paramètre a : l'affectable à analyser *)
(* Fonction auxiliaire qui traîte les déréférencements *)
let rec analyse_tam_deref a =
  begin
    match a with
    | AstType.Ident ia ->
      begin
        match info_ast_to_info ia with
        | InfoVar(_, Pointeur typ, d, r) -> (typ, (load 1 d r))
        | _ -> failwith "erreur analyse_tam_deref"
      end
    | AstType.Deref af ->
      let (t, code) = analyse_tam_deref af in
      begin
        match t with
        | Pointeur typ -> (typ, code^(loadi 1))
        | _ -> failwith "erreur analyse_tam_deref"
      end
    | _ -> failwith "erreur"
  end

    
(* analyse_tam_affectable : AstPlacement.affectable -> string *)
(* Paramètre a : l'affectable à analyser *)
(* Paramètre is_left : indique s'il s'agit s'une affectation*)
(* Transforme l'affectable en langage de la machine TAM *)
let analyse_tam_affectable a is_left = 
  match a with 
  | AstType.Ident ia ->
    begin
      match info_ast_to_info ia with 
      | InfoVar (_,typ,d,r) -> if is_left then store (getTaille typ) d r 
                                else load (getTaille typ) d r
      | InfoConst (_,i) -> loadl_int i
      | _ -> failwith "erreur analyse type expression infovar"
    end
  | AstType.Deref af -> let (t, code) = analyse_tam_deref af in
  if is_left then code^(storei (getTaille t))
  else code^(loadi (getTaille t))


  | AstType.AccesTab (_, _) -> failwith "TODO"
  (* | AstType.AccesTab(af,e) -> 
                              let expr = (analyse_tam_expression e) in
                              let a = analyse_tam_affectable af is_left in 
                              a^expr^(loadi (1)) *)
                              
                             
                              
(* analyse_tam_expression : AstPlacement.expression -> string *)
(* Paramètre e : l'expression à analyser *)
(* Transforme l'expression en langage de la machine TAM *)
let rec analyse_tam_expression e = 
  match e with 

  | AstType.Affectable a -> analyse_tam_affectable a false

  | Null -> loadl_int 0

  | New t -> (loadl_int (getTaille t))^(subr "MAlloc")

  | Adresse ia ->
    begin
      match info_ast_to_info ia with
       | InfoVar(_,_,d,r) -> loada d r
       | _ -> failwith "error analyse_tam_exp Adresse"
    end

  | AstType.Booleen b -> loadl_int (if b then 1 else 0)

  | AstType.Entier n -> loadl_int n

  | AstType.Unaire (op,expr) -> 

    begin
      match op with 
        | AstType.Numerateur -> (analyse_tam_expression expr)^(pop 0 1)
        | AstType.Denominateur -> (analyse_tam_expression expr)^(pop 1 1)
      end


  | AstType.Binaire(op,expr_g,expr_d) ->
    begin
    match op with 
      | Fraction -> (analyse_tam_expression expr_g)^(analyse_tam_expression expr_d)
      | PlusInt -> (analyse_tam_expression expr_g)^(analyse_tam_expression expr_d)^(subr "IAdd")
      | PlusRat -> (analyse_tam_expression expr_g)^(analyse_tam_expression expr_d)^(call "SB" "RAdd")
      | MultInt -> (analyse_tam_expression expr_g)^(analyse_tam_expression expr_d)^(subr "IMul")
      | MultRat -> (analyse_tam_expression expr_g)^(analyse_tam_expression expr_d)^(call "SB" "RMul")
      | EquInt -> (analyse_tam_expression expr_g)^(analyse_tam_expression expr_d)^(subr "IEq")
      | EquBool -> (analyse_tam_expression expr_g)^(analyse_tam_expression expr_d)^(subr "IEq")
      | Inf -> (analyse_tam_expression expr_g)^(analyse_tam_expression expr_d)^(subr "ILss")
    end

  | AstType.AppelFonction(ia,lexpr) -> 
    begin
      let rec aux l = 
        begin
          match l with 
          | [] -> ""
          | t::q -> t^(aux q) 
        end
        in 
      let list = List.map (analyse_tam_expression) lexpr in 
      match info_ast_to_info ia with 
        |InfoFun (name, _, _) -> (aux list)^(call "SB" (label name))
        | _ -> failwith "erreur appel fonction"
    end
  (* Pas sûr pour le tableau *)
   | AstType.InitTab le -> let list_val = List.map analyse_tam_expression le in
                          let taille_tab = (List.length le) in 
                          let code = List.fold_right (^) list_val "" in 
                          (*Il faudrait obtenir le type des expression dans la liste, on suppose que c est que des entiers *)
                          code^(loadl_int 1)^(loadl_int taille_tab)^(call "SB" "RMul")^(subr "MAlloc")^(loadi (1)) 
                          
                         
                         

  | AstType.CreateTab (typ,e) ->
                            
                              let size_tab = analyse_tam_expression e in 
                              let size_typ = getTaille typ in
                              size_tab^(loadl_int size_typ)^(call "SB" "RMul")^(subr "MAlloc")





(* analyse_tam_instruction : AstPlacement.instruction -> string *)
(* Paramètre i : l'instruction à analyser *)
(* Transforme l'instruction en langage de la machine TAM *)
let rec analyse_tam_instruction i =
  match i with
  | AstPlacement.Declaration (ia, e) ->
    begin
    match info_ast_to_info ia with
    | InfoVar(_,t,d,r) -> 
      (push (getTaille t))^(analyse_tam_expression e)^(store (getTaille t) d r)

    | _ -> failwith "Erreur Declaration tam instruction"  
    end
       
  | AstPlacement.Affectation (a,e) ->
    let code_exp = analyse_tam_expression e in
    let code_aff = analyse_tam_affectable a true in
    code_exp^code_aff

  
  | AstPlacement.AffichageInt e -> (analyse_tam_expression e)^(subr "IOut")
  | AstPlacement.AffichageBool e -> (analyse_tam_expression e)^(subr "BOut")
  | AstPlacement.AffichageRat e -> (analyse_tam_expression e)^(call "SB" "ROut")

  | AstPlacement.Conditionnelle (e,b1,b2) -> 
      let etiqF = (label (getEtiquette())) in 
      let etiqEnd = (label (getEtiquette())) in 
      (analyse_tam_expression e)^(jumpif 0 etiqF)^(analyse_tam_bloc b1)^(jump etiqEnd)^(etiqF)^(analyse_tam_bloc b2)^(etiqEnd)
      
     

  | AstPlacement.TantQue (e,b) ->
      let sd = (label (getEtiquette())) in 
      let sf = (label (getEtiquette())) in
      (sd)^(analyse_tam_expression e)^(jumpif 0 sf)^(analyse_tam_bloc b)^(jump sd)^(sf)

      

  | AstPlacement.Retour (e,tr,tp) -> (analyse_tam_expression e)^(return tr tp)

  | AstPlacement.Empty -> ""
  | AstPlacement.For (ia,e1,e2,e3,b) ->
      let sd = (label (getEtiquette())) in 
      let sf = (label (getEtiquette())) in
      begin
        match info_ast_to_info ia with
        | InfoVar(_,t,d,r) ->
        let decl = (push (getTaille t))^(analyse_tam_expression e1)^(store (getTaille t) d r) in
        let incr = (analyse_tam_expression e3)^(store (getTaille t) d r) in
        decl^(sd)^(analyse_tam_expression e2)^(jumpif 0 sf)^(analyse_tam_bloc b)^incr^(jump sd)^(sf)
    
        | _ -> failwith "Erreur For tam instruction"  
      end
      (* (sd)^(analyse_tam_expression e)^(jumpif 0 sf)^(analyse_tam_bloc b)^(jump sd)^(sf) *)

  | CallGoto ia -> begin 
                  match info_ast_to_info ia with 
                    | InfoLabel(n) -> (jump n)
                    | _ -> failwith "erreur Callgoto"
                  end   

  | DefGoto ia -> begin 
                  match info_ast_to_info ia with 
                    | InfoLabel(n) -> let s = (label n) in (s)
                    | _ -> failwith "erreur Defgoto"
                  end   

(* analyse_tam_bloc : tds -> info_ast option -> AstPlacement.bloc -> string *)
(* Paramètre bloc : bloc à analyser *)
(* Transforme le bloc en langage de la machine TAM *)
and analyse_tam_bloc (li,_) =

  
  let rec aux l = 
    begin
      match l with 
      | [] -> ""
      | t::q -> t^(aux q) 
    end
    in 
  
  let list = List.map (analyse_tam_instruction) li in 

  aux list



(* analyse_tam_fonction : tds -> AstPlacement.fonction -> string *)
(* Paramètre : la fonction à analyser *)
(* Transforme la fonction en langage de la machine TAM *)
let analyse_tam_fonction (AstPlacement.Fonction(ia, _, b)) =
  match info_ast_to_info ia with 
  |InfoFun (name, _, _) -> (label name)^(analyse_tam_bloc b)^(halt)
  | _ -> failwith "erreur analyse tam fonction "

  

(* analyser : AstSyntax.programme -> string *)
(* Paramètre : le programme à analyser *)
(* Transforme le programme en langage de la machine TAM *)
let analyser (AstPlacement.Programme (f_list,prog)) =
  let fn_list = List.map (analyse_tam_fonction) f_list in
  let bloc = analyse_tam_bloc prog in
  let rec aux l = 
    match l with 
    | [] -> ""
    | t::q -> t^(aux q) 
  in 
  (getEntete ())^(aux fn_list)^(label "main")^bloc^(halt)

  
