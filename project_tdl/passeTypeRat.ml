(* Module de la passe de gestion du typage *)
(* doit être conforme à l'interface Passe *)
open Tds
open Type
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


let rec analyse_type_affectable a =
  begin
  match a with
  | AstTds.Ident ia ->
    begin
      match info_ast_to_info ia with
      | InfoVar (_,t,_,_) -> (t, AstType.Ident ia)
      | InfoConst _ -> (Int, AstType.Ident ia)
      | _ -> failwith "erreur analyse type affecatble"
    end
  | AstTds.Deref af -> let na = analyse_type_affectable af in
                          begin
                          match na with
                          | (Pointeur t, naff) -> (t, AstType.Deref naff)
                          | _ -> failwith "erreur analyse type affecatble"
                          end
  | AstTds.AccesTab (af,e) -> let (ta, af_typ) = analyse_type_affectable af in
                              let (te, e_typ) = analyse_type_expression e in
                              if (est_compatible te Int) then
                                begin
                                  match ta with
                                  | Tableau t1 -> (t1, AstType.AccesTab (af_typ, e_typ))
                                  | _ -> failwith "erreur analyse_type_affectable AccesTab"
                                end
                              else
                                raise (TypeInattendu (te, Int))
  end



(* analyse_type_expression : AstTds.expression -> typ * AstType.expression *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie lebon typage et tranforme l'expression
en une expression de type AstType.expression *)
(* Erreur si mauvais typage *)
and analyse_type_expression e =
  match e with
  | AstTds.Booleen b -> (Bool, AstType.Booleen b)

  | AstTds.Entier n -> (Int, AstType.Entier n)

  | AstTds.Unaire (op,expr) -> let (typ, e) = 
    analyse_type_expression expr in
    begin
      match typ with
      | Rat -> 
        begin
        match op with 
          | AstSyntax.Numerateur -> (Int,AstType.Unaire(AstType.Numerateur, e))
          | AstSyntax.Denominateur -> (Int,AstType.Unaire(AstType.Denominateur, e))
        end
      | _ -> raise(TypeInattendu (typ,Rat))

    end
 
  | AstTds.Binaire(op,expr_g,expr_d) -> 
    let (typg,eg) =  analyse_type_expression expr_g in 
    let (typd,ed) =  analyse_type_expression expr_d in 

    begin 

    match typg, typd with

      | Bool, Bool -> 
        begin
                    match op with 
                        | Equ -> (Bool, AstType.Binaire(AstType.EquBool, eg,ed))
                        | _ -> raise(TypeBinaireInattendu(op, typg,typd))
        end 
      | Int, Int -> 

        begin 
                  match op with 
                    | Plus -> (Int, AstType.Binaire(AstType.PlusInt, eg,ed))
                    | Mult -> (Int, AstType.Binaire(AstType.MultInt, eg,ed))
                    | Equ -> (Bool, AstType.Binaire(AstType.EquInt, eg,ed))
                    | Inf -> (Bool, AstType.Binaire(AstType.Inf, eg,ed))
                    | Fraction -> (Rat, AstType.Binaire(AstType.Fraction, eg,ed))
        end 
      | Rat, Rat -> 
        begin
                  match op with 
                    | Mult -> (Rat, AstType.Binaire(AstType.MultRat, eg,ed))
                    | Plus -> (Rat, AstType.Binaire(AstType.PlusRat, eg,ed))
                    | _ -> raise(TypeBinaireInattendu(op, typg,typd))
        end 
      | _ -> raise(TypeBinaireInattendu(op, typg,typd)) 

    end 


  | AstTds.AppelFonction(ia,lexpr) ->

    let list_aux = List.map (analyse_type_expression) lexpr in 
    let list_expr = List.map (fun (_,y) -> y) list_aux in
    let list_typ = List.map (fun (x,_) -> x) list_aux in

    begin
      match info_ast_to_info ia with 
      | InfoFun (_,typ,lt) ->  begin 
                              if est_compatible_list lt list_typ then
                                    (typ, AstType.AppelFonction (ia,list_expr))
                              else 
                                raise (TypesParametresInattendus (list_typ,lt))
                              end 
      | _ -> failwith "erreur appel function"
    end
  | AstTds.Affectable a -> let (t,af) = (analyse_type_affectable a) in 
                            (t, AstType.Affectable af)  
  | AstTds.Null -> (Pointeur(Undefined), AstType.Null)
  | AstTds.New t -> (Pointeur t, AstType.New t)
  | AstTds.Adresse ia -> begin 
                              match info_ast_to_info ia with 
                              | InfoVar (_,t,_,_) -> (Pointeur t, AstType.Adresse ia)
                              | InfoConst _ -> (Pointeur Int, AstType.Adresse ia)
                              | _ -> failwith "erreur adresse"
                            end
  (* TODO : Gérer la taille du tableau *)
  | AstTds.CreateTab (t,exp) -> let (t1, exp_typ) = analyse_type_expression exp in
                                if est_compatible t1 Int then (Tableau t, AstType.CreateTab (t, exp_typ))
                                else raise (TypeInattendu (t1, Int))
  | AstTds.InitTab le -> let le_typ = List.map analyse_type_expression le in
                        if (est_compatible_list_typ (List.map fst le_typ)) then
                          begin
                            match le_typ with
                            | (t,_)::_ -> (Tableau t, AstType.InitTab (List.map snd le_typ))
                            | _ -> failwith "erreur analyse type expression InitTab"
                          end
                        else
                          failwith "Types incompatibles Tableau"
                          


(* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie le bon typage et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvais typage *)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, ia, e) ->
      
    let (typ, expr) = analyse_type_expression e in
    if (Type.est_compatible typ t) then 
      begin
      Tds.modifier_type_variable t ia;
      AstType.Declaration(ia, expr)
      end
    else 
      raise (TypeInattendu(typ,t))
        

  | AstTds.Affectation (af,e) -> 
      
      let (typ_exp, expr) = analyse_type_expression e in 
      let (typ_af, af) = analyse_type_affectable af in

      if est_compatible typ_af typ_exp then 
        AstType.Affectation(af,expr)

      else 
        raise (TypeInattendu(typ_exp,typ_af))

     
  
  | AstTds.Affichage e ->
    let (typ,expr) = analyse_type_expression e in 
    begin 
      match typ with 
        | Bool -> AstType.AffichageBool expr 
        | Int -> AstType.AffichageInt expr
        | Rat -> AstType.AffichageRat expr
        | _ -> failwith " type undefined"

    end 



     
  | AstTds.Conditionnelle (e,b1,b2) ->
      let (typ,expr) = analyse_type_expression e in 
      let b1_typ = analyse_type_bloc b1 in 
      let b2_typ = analyse_type_bloc b2 in
      
      if Type.est_compatible Bool typ then 
        AstType.Conditionnelle(expr,b1_typ,b2_typ)
      else 
        raise (TypeInattendu (typ,Bool))

  | AstTds.TantQue (e,b) ->
    let (typ,expr) = analyse_type_expression e in 
    let bloc = analyse_type_bloc b in 
    
    if Type.est_compatible Bool typ then 
      AstType.TantQue(expr,bloc)
    else 
      raise (TypeInattendu (typ,Bool))

      

  | AstTds.Retour (e,ia) -> 
    let (typ,expr) = analyse_type_expression e in 
    begin
    match info_ast_to_info ia with 
          | InfoFun (_,t,_) -> 
                                if (Type.est_compatible typ t) then 

                                  AstType.Retour(expr, ia)
                                
                                else 
                                  raise (TypeInattendu(typ,t))

          | _ -> failwith "Retour erreur"
    end

  | AstTds.Empty -> AstType.Empty
  | AstTds.For (ia,e1,e2,e3,b) ->
    let (t1, e1_typ) = analyse_type_expression e1 in
    if (est_compatible t1 Int) then
      begin
        modifier_type_variable Int ia;
        let (t2, e2_typ) = analyse_type_expression e2 in
        let (t3, e3_typ) = analyse_type_expression e3 in
        let b_typ = analyse_type_bloc b in
        if (est_compatible t2 Bool) then
          begin
            if (est_compatible t3 Int) then
              AstType.For(ia,e1_typ,e2_typ,e3_typ,b_typ)
            else
              raise (TypeInattendu (t3,Int))
          end
        else
          raise (TypeInattendu (t2,Bool))
      end
    else
      raise (TypeInattendu (t1,Int))

  | CallGoto ia -> AstType.CallGoto(ia)
  
  | DefGoto ia  ->  AstType.DefGoto(ia)
      
  

(* analyse_type_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre bloc : liste d'instructions à analyser *)
(* Vérifie le bon typage et tranforme le bloc en un bloc de type AstType.bloc *)
(* Erreur si mauvais typage *)
and analyse_type_bloc bloc =
  List.map(analyse_type_instruction) bloc


(* Fonction auxiliaure qui modifie le type des paramètres et renvoie
    les paramètres avec les bons type *)
let rec modifier_list_param lp = 
  match lp with 
    | [] -> []
    | (t,ia)::q ->  begin
                    Tds.modifier_type_variable t ia;
                    ia::(modifier_list_param q)
                    end 




(* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
(* Paramètre : la fonction à analyser *)
(* Vérifie le bon typage et tranforme la fonction en une fonction de type AstType.fonction *)
(* Erreur si mauvais typage *)
let analyse_type_fonction (AstTds.Fonction(t,ia,lp,li)) = 
  let typ_list = List.map fst lp in 
  modifier_type_fonction t typ_list ia;
  let ia_param_list = modifier_list_param lp in 
  let bloc = analyse_type_bloc li in 
  AstType.Fonction(ia,ia_param_list,bloc)
  

(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie le bon typage et tranforme le programme en un programme de type AstType.programme *)
(* Erreur si mauvais typage *)
let analyser (AstTds.Programme (fonctions,prog)) =
  let nf = List.map analyse_type_fonction fonctions in
  let nb = analyse_type_bloc prog in
  AstType.Programme (nf,nb) 
