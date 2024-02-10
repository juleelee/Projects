with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

-- Definition de structures de donnees associatives sous forme d'une liste
-- chaine associative

-- R1 : Concevoir et initialiser un routeur
package Table_Routage is

    type T_Table_Routage is limited private;
    
    -- R2 : Initialiser une table de routage.  La table de routage est vide.

    -- nom : Initialiser
    -- semantique : Initialiser la table de routage a initialiser
    -- parametres :
    --      Table de routage : Mode Out T_Table_Routage; -- la table de routage
    --      param : Mode In T_Param ; -- contient le fichier texte avec les informations de la table de routage (destination,masque...) 
    -- post-condition : Est_Vide(Table_Routage)
    -- tests :
    --      entrees : . sortie : Table_routage = null.
    procedure Initialiser(param : in T_Param; Table_routage: out T_Table_Routage);
   
    -- Est-ce qu une Table_Routage est vide ?

    -- nom : Est_Vide
    -- semantique : Determiner si la table de routage est vide ou non.
    -- parametres :
    --      T_Table_Routage : Mode In T_Table_Routage; 
   
    function Est_Vide (Table_Routage : in T_Table_Routage) return Boolean;


	-- Obtenir le nombre d elements d une Table_Routage. 
	function Taille (Table_Routage : in T_Table_Routage) return Integer with
		Post => Taille'Result >= 0
			and (Taille'Result = 0) = Est_Vide (Table_Routage);
    


    -- nom : Enregistrer
    -- semantique : Enregistrer une adresse, un masque, une sortie dans la table de routage.
    -- parametres :
    --      Table_Routage : Mode In/Out T_Table_Routage; -- la table de routage
    --      Adresse : Mode In T_Adresse_IP; -- l adresse IP a ajouter
    --      Masque : Mode In T_Adresse_IP; -- le masque à ajouter
    --      Sortie : Mode In Unbounded_String; -- la sortie a ajouter
    -- post-condition : Taille(Table_Routage)  = Taille(Table_Routage)'Old +1 .
    procedure Enregistrer (Table_Routage : in out T_Table_Routage ; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP; Sortie : in Unbounded_String ) with
        Post =>  Taille (Table_Routage) = Taille(Table_Routage)'Old +1; 

	

    -- nom : Supprimer
    -- semantique : Supprimer une adresse ( masque, interface) de la table de routage 
    -- parametres :
    --      Table_Routage : Mode In/Out T_Table_Routage; -- la table de routage
    --      adresse : Mode In adresse -- supprimer les donnees associe a cette adresse
    
    -- post-condition :  Taille (Table_Routage) = Taille (Table_Routage)'Old - 1
    -- Exception : Cle_Absente_Exception si Cle n est pas utilisee dans la Table_Routage
	procedure Supprimer (Table_Routage : in out T_Table_Routage ; adresse : in T_Adresse_IP) with
		Post =>  Taille (Table_Routage) = Taille (Table_Routage)'Old - 1 -- un element de moins
			and not Adresse_Presente(Table_Routage, adresse);         -- la cle a ete supprimee
    


    -- nom : Vider
    -- semantique : Vider entierement la table de routage
    -- parametres :
    --      Table_routage : Mode In/Out T_Table_Routage; -- le table de routage a vider
    -- post-condition : Est_Vide(Table_Routage)
    -- tests :
    --      entrees : Table_Routage. sortie : Table_Routage = null.
	procedure Vider (Table_Routage : in out T_Table_Routage) with
		Post => Est_Vide (Table_Routage);

    -- nom : Afficher
    -- Semantique : Affiche la table de routage 
    -- parametres :
    --      Table_routage : Mode In/Out T_Table_Routage; -- le table de routage a afficher

    procedure Afficher(Table_Routage : in T_Table_Routage; file : File_Type);

    procedure Get_Interface(Adresse_IP: in T_Adresse_IP; Table_Routage: in T_Table_Routage; interf : out Unbounded_String ; taille_masque_interf : out Integer);
    -- fonction qui permettent d acceder aux differentes valeur ou pointe les pointeurs (car c est en limited private)
    function Get_Adresse(Table_Routage: in T_Table_Routage) return T_Adresse_IP;
    function Get_Masque(Table_Routage: in T_Table_Routage) return T_Adresse_IP;
    function Get_Sortie(Table_Routage: in T_Table_Routage) return Unbounded_String;
    function Get_Suivant(Table_Routage: in T_Table_Routage) return T_Table_Routage;
    
    function Is_Command_And_Then_Execute(ligne : String; tr : T_Table_Routage; file_output : File_Type; num_ligne : Integer) return Boolean;
    
    -- nom : Is_Command_And_Then_Execute_LCA
    -- semantique : ligne de commande pour un cache de type LCA  
    -- parametres :
    --      tr : Mode In T_Table_Routage; -- la table de routage
    --      ligne : Mode In String;
    --      file_output : Mode In File_Type;
    --      num_ligne : Mode In Integer; 
 
    function Is_Command_And_Then_Execute_LCA(ligne : String; tr : T_Table_Routage; file_output : File_Type; num_ligne : Integer) return Boolean;
    
    -- nom : Is_Command_And_Then_Execute_TREE
    -- semantique : ligne de commande pour un cache de type TREE
    -- parametres :
    --      tr : Mode In T_Table_Routage; -- la table de routage
    --      ligne : Mode In String;
    --      file_output : Mode In File_Type;
    --      num_ligne : Mode In Integer; 
    function Is_Command_And_Then_Execute_TREE(ligne : String; tr : T_Table_Routage; file_output : File_Type; num_ligne : Integer) return Boolean;
    
    -- nom :  Adresse_Presente
    -- semantique : Vérifie si une adresse est présente dans la table de routage  
    -- parametres :
    --      Table_Routage : Mode In T_Table_Routage; -- la table de routage
    --      adresse  : Mode In T_Adresse_IP;
    function Adresse_Presente (Table_Routage : in T_Table_Routage ; adresse : in T_Adresse_IP) return Boolean;

     -- nom :  Recuperer_Masque_Plus_Long
    -- semantique : Récupere le masque le plus long pour le mettre dans le cache   
    -- parametres :
    --      Table : Mode In T_Table_Routage; -- la table de routage
    --      Adresse  : Mode In T_Adresse_IP;
    --      Masque : Mode in T_ADRESSE_IP;
    -- 
    function Recuperer_Masque_Plus_Long(Table : in T_Table_Routage ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP) return T_Adresse_IP;
    
    
private

    -- R2 : Concevoir et initialiser un routeur

    type T_Cellule;

    type T_Table_Routage is access T_Cellule;

    type T_Cellule is
        record
            Adresse : T_Adresse_IP;
            Masque : T_Adresse_IP;
            Sortie : Unbounded_String;
            Suivant : T_Table_Routage;
        end record;

end Table_Routage;


