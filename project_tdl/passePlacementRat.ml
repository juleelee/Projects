(* Module de la passe de placement mémoire *)
(* doit être conforme à l'interface Passe *)
open Tds
open Type
(* open Exceptions *)
open Ast

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

(* analyse_placement_instruction : int -> string -> AstType.instruction -> AstPlacement.instruction *)
(* Paramètre dep : le déplacement dans la mémoire *)
(* Paramètre reg : le registre dans la mémoire *)
(* Paramètre i : l'instruction à analyser *)
(* Tranforme l'instruction en une instruction de type AstPlacement.instruction *)

let rec analyse_placement_instruction dep reg i =
  match i with
  | AstType.Declaration (ia, e) ->
    begin
    match info_ast_to_info ia with
    | InfoVar(_,t,_,_) -> 
      modifier_adresse_variable dep reg ia;
      (AstPlacement.Declaration(ia, e), getTaille(t))
    | _ -> failwith "Erreur Declaration Placement instruction"  
    end
        
  | AstType.Affectation (ia,e) ->
    (AstPlacement.Affectation(ia, e), 0)

  | AstType.AffichageInt e -> (AstPlacement.AffichageInt(e), 0)
  | AstType.AffichageBool e -> (AstPlacement.AffichageBool(e), 0)
  | AstType.AffichageRat e -> (AstPlacement.AffichageRat(e), 0)

     
  | AstType.Conditionnelle (e,b1,b2) -> 
      let b1_pl = analyse_placement_bloc dep reg b1 in 
      let b2_pl = analyse_placement_bloc dep reg b2 in
      
      (AstPlacement.Conditionnelle(e,b1_pl,b2_pl),0)

  | AstType.TantQue (e,b) ->
      let bloc = analyse_placement_bloc dep reg b in 
      (AstPlacement.TantQue(e,bloc), 0)

      

  | AstType.Retour (e,ia) -> 
    begin
    match info_ast_to_info ia with
    | InfoFun(_, typ, typ_list) -> let taille_params = List.fold_right (+) (List.map getTaille typ_list) 0 in
                                    (AstPlacement.Retour(e, getTaille(typ), taille_params),0)
    | _ -> failwith "erreur Retour Placement"
    end

  | AstType.Empty -> (AstPlacement.Empty,0)
  | AstType.For (ia,e1,e2,e3,b) ->
      begin
        match info_ast_to_info ia with
        | InfoVar(_,Int,_,_) -> 
          modifier_adresse_variable dep reg ia;
          
          let b_plcmt = analyse_placement_bloc (dep+1) reg b in
          (AstPlacement.For(ia,e1,e2,e3,b_plcmt), 0)
        | _ -> failwith "Erreur Placement instruction For"  
      end
  | AstType.CallGoto ia -> (AstPlacement.CallGoto(ia), 0)

  | AstType.DefGoto ia -> (AstPlacement.DefGoto(ia),0)



(* analyse_placement_bloc : int -> string -> AstType.bloc -> AstPlacement.bloc *)
(* Paramètre dep : le déplacement dans la mémoire *)
(* Paramètre reg : le registre dans la mémoire *)
(* Paramètre li : liste d'instruction à analyser *)
(* Tranforme l'instruction en un bloc de type AstPlacement.bloc *)
and analyse_placement_bloc dep reg li =
  match li with
  |[] -> ([],0)
  |h::t -> let (i1,t1) = analyse_placement_instruction dep reg h in
            let (il,tl) = analyse_placement_bloc (dep+t1) reg t in
            (i1::il,t1+tl)



(* analyse_tds_fonction : string -> AstType.fonction -> AstPlacement.fonction *)
(* Paramètre reg : le registre de la mémoire *)
(* Paramètre : la fonction à analyser *)
(* Transforme la fonction en une fonction de type AstPlacement.fonction *)

let analyse_placement_fonction reg (AstType.Fonction(ia, ia_list, b)) =
  let rec aux lp reg dep = 
    begin
    match lp with
    |[] -> []
    |h::tl -> 
      begin
        match info_ast_to_info h with
        | InfoVar(_,t,_,_) -> let var = dep - getTaille(t) in
          modifier_adresse_variable var reg h;
          h::(aux tl reg var)
        | _ -> failwith "Error placement fonction"
      end
    end
  in
  let bloc = analyse_placement_bloc 3 reg b in
  let list_parms = aux (List.rev ia_list) reg 0 in
  AstPlacement.Fonction(ia, List.rev list_parms, bloc)

  

(* analyser : AstType.programme -> AstPlacement.programme *)
(* Paramètre : le programme à analyser *)
(* Transforme le programme en un programme de type AstPlacement.programme *)
let analyser (AstType.Programme (f_list,prog)) =
  let fn_list = List.map (analyse_placement_fonction "LB") f_list in
  let bloc = analyse_placement_bloc 0 "SB" prog in
  AstPlacement.Programme(fn_list,bloc)
