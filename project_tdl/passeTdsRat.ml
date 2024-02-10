(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme


(* analyse_tds_label : tds -> AstSyntax.instruction list -> unit *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des label et remplit la TDS des label *)
(* Erreur si mauvaise utilisation des labels *)
let rec analyse_tds_label tds li =
  let analyse_instruction_label tds instr =
    begin 
    match instr with
    | AstSyntax.DefGoto label -> begin 
                                    match (chercherLabel tds label) with
                                      | Some _ -> raise(DoubleDeclaration label) 
                                      | None -> let info = InfoLabel (label) in 
                                                let ia = info_to_info_ast info in 
                                                ajouterLabel tds label ia;
                                    end 
    
    | AstSyntax.For (_,_,_,_,_,b) ->  analyse_tds_label tds b

    | AstSyntax.TantQue (_,b) -> analyse_tds_label tds b

    | AstSyntax.Conditionnelle (_,t,e) -> analyse_tds_label tds t; 
                                            analyse_tds_label tds e 

    | _ -> () (*Ne rien faire*)

    end 
  in
  List.iter (fun instr -> analyse_instruction_label tds instr) li;



and analyse_tds_affectable tds a is_left =
  match a with
  | AstSyntax.Ident n ->
    begin
      match chercherGlobalement tds n with
      | None -> raise (IdentifiantNonDeclare n)
      | Some info ->
        begin
          match info_ast_to_info info with
          | InfoVar _ -> AstTds.Ident info
          | InfoFun _ -> raise (MauvaiseUtilisationIdentifiant n)
          | InfoLabel _ -> raise (MauvaiseUtilisationIdentifiant n)
          | InfoConst _ -> if is_left then raise (MauvaiseUtilisationIdentifiant n) else AstTds.Ident info
          
        end
      end
  | AstSyntax.Deref af -> let na = analyse_tds_affectable tds af is_left in
                          AstTds.Deref na
  | AstSyntax.AccesTab (af, e) -> let af_tds = analyse_tds_affectable tds af is_left in
                                  let e_tds = analyse_tds_expression tds e in
                                  AstTds.AccesTab (af_tds, e_tds)
  
  

(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_expression tds e = 
  match e with 

  | AstSyntax.Booleen b -> AstTds.Booleen b

  | AstSyntax.Entier n -> AstTds.Entier n

  | AstSyntax.Unaire (op,expr) -> AstTds.Unaire (op,analyse_tds_expression tds expr)
 
  | AstSyntax.Binaire(op,expr_g,expr_d) -> AstTds.Binaire (op,analyse_tds_expression tds expr_g,analyse_tds_expression tds expr_d)

  | AstSyntax.AppelFonction(n,lexpr) -> 
    
    begin 
      match chercherGlobalement tds n with 
        | Some info_ast -> 
          begin 
            match info_ast_to_info info_ast with
              | InfoFun _ -> AstTds.AppelFonction(info_ast, List.map(analyse_tds_expression tds) lexpr)

              | _ -> raise (MauvaiseUtilisationIdentifiant n)

          end 

        | None -> raise(IdentifiantNonDeclare n) 

    end 
  | AstSyntax.Affectable a -> AstTds.Affectable(analyse_tds_affectable tds a false)
  | AstSyntax.Null -> AstTds.Null
  | AstSyntax.New t -> AstTds.New t
  | AstSyntax.Adresse id -> begin 
                              match chercherGlobalement tds id with 
                                | Some ia -> 
                                  begin 
                                    match info_ast_to_info ia with
                                      | InfoFun _ -> raise (MauvaiseUtilisationIdentifiant id)
                                      | _ -> AstTds.Adresse ia
                                  end 
                                | None -> raise(IdentifiantNonDeclare id) 
                            end
  | AstSyntax.CreateTab (t,exp) -> AstTds.CreateTab (t, analyse_tds_expression tds exp)
  | AstSyntax.InitTab le -> AstTds.InitTab (List.map (analyse_tds_expression tds) le)





(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analysLocalementer *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_instruction tds oia i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale,
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *)
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
            et l'expression remplacée par l'expression issue de l'analyse *)
            AstTds.Declaration (t, ia, ne)
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale,
            il a donc déjà été déclaré dans le bloc courant *)
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (a,e) ->
      AstTds.Affectation((analyse_tds_affectable tds a true), (analyse_tds_expression tds e))
  
      | AstSyntax.Constante (n,v) ->
      begin
        match chercherLocalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds locale,
             il n'a donc pas été déclaré dans le bloc courant *)
          (* Ajout dans la tds de la constante *)
          ajouter tds n (info_to_info_ast (InfoConst (n,v)));
          (* Suppression du noeud de déclaration des constantes devenu inutile *)
          AstTds.Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds oia t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds oia e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds oia b in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.TantQue (nc, bast)
  | AstSyntax.Retour (e) ->
      begin
      (* On récupère l'information associée à la fonction à laquelle le return est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une fonction *)
      | Some ia ->
        (* Analyse de l'expression *)
        let ne = analyse_tds_expression tds e in
        AstTds.Retour (ne,ia)
      end
  | AstSyntax.For (n1,e1,e2,n2,e3,b) ->
    if (n1 <> n2) then raise (MauvaiseUtilisationIdentifiant n2)
    else
      (* Création de l'information associée à l'identfiant *)
      let info = InfoVar (n1,Undefined, 0, "") in
      (* Création du pointeur sur l'information *)
      let ia = info_to_info_ast info in
      (* Ajout de l'information (pointeur) dans la tds *)
      ajouter tds n1 ia;
      let e1_tds = analyse_tds_expression tds e1 in
      let e2_tds = analyse_tds_expression tds e2 in
      let e3_tds = analyse_tds_expression tds e3 in
      let b_tds = analyse_tds_bloc tds oia b in
      (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
      et l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.For (ia, e1_tds, e2_tds, e3_tds, b_tds)
      
  | AstSyntax.CallGoto n -> begin 
                           match chercherLabel tds n with 
                            | Some info -> AstTds.CallGoto(info)
                            | None -> raise (IdentifiantNonDeclare n)
                            end   
  
  | AstSyntax.DefGoto n -> begin
                          match chercherLabel tds n with 
                              | Some info -> AstTds.DefGoto(info)
                              | None -> raise (IdentifiantNonDeclare n)
                          end



(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc oia) li in


   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  = 

  let chercher tds_f (typ, ident) =  
    match chercherLocalement tds_f ident with 
      | Some _ -> raise(DoubleDeclaration ident) 
      
      | None -> let info = InfoVar(ident, Undefined,0, "") in 
                let ia = info_to_info_ast info in 
                ajouter tds_f ident ia;
               
                (typ,ia) in

  match chercherGlobalement maintds n with 
    | None -> let ia = info_to_info_ast (InfoFun (n,Undefined,[])) in 
                        ajouter maintds n ia;

                        let tds_fille = creerTDSFille maintds in 
                        
                        let lp_tds = List.map (chercher tds_fille) lp in 

                        let tds_bloc = analyse_tds_bloc tds_fille (Some ia) li in 
                        
                        AstTds.Fonction(t,ia,lp_tds,tds_bloc)

    | Some _ -> raise(DoubleDeclaration n)
  
  

  



(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions, prog)) =
  let tds = creerTDSMere () in
  (* Appeler l'analyse des labels *)
  analyse_tds_label tds prog;
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None prog in
  AstTds.Programme (nf, nb)

