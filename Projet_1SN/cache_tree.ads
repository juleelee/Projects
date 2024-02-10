with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package cache_tree is

    type T_Param_Cache is private;
    type T_Arbre is private;

    -- nom : Initialiser_Arbre
    -- sémantique : Initialiser le cache
    -- paramètres :
    --      Arbre : Mode Out T_Arbre; -- le cache
    -- post-condition : Est_Vide(Arbre)
    procedure Initialiser_Arbre(Arbre : out T_Arbre) with
        Post => Est_Vide(Arbre);


    -- nom : Initialiser_Cache
    -- sémantique : Initialiser les paramètres du cache
    -- paramètres :
    --      Cache : Mode Out T_Param_Cache; -- les paramètres du cache
    --      Taille_Max : Mode In Integer; -- la taille maximale du cache (sa capacité)
    --      Politique : Mode In T_Politique; -- la politique du cache
    -- post-condition : Cache.Taille = 0 and Cache.Defauts = 0 and Cache.Demandes = 0 and Cache.Enregistrement = 0
    procedure Initialiser_Cache(Cache: out T_Param_Cache; Taille_Max : in Integer; Politique : in T_Politique) with
        Post => Taille_Cache(Cache) = 0;


    -- nom : Est_Vide
    -- sémantique : Savoir si le cache est vide ou non
    -- paramètres :
    --      Arbre : Mode in T_Arbre; -- le cache
    function Est_Vide(Arbre : in T_Arbre) return Boolean;


    -- nom : Vider
    -- sémantique : Vider entièrement le cache
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre; -- le cache à vider
    -- post-condition : Est_Vide(Arbre)
    procedure Vider(Arbre : in out T_Arbre) with
		Post => Est_Vide(Arbre);


    -- nom : Taille_Cache
    -- sémantique : Permet de récupérer la taille du cache
    -- paramètres :
    --      Cache : Mode In T_Param_Cache; -- le cache
    -- post-condition : Taille_Cache'Result >= 0
    function Taille_Cache(Cache : in T_Param_Cache) return Integer with
        Post => Taille_Cache'Result >= 0;


    -- nom : Frequence_Arbre
    -- sémantique : Permet de récupérer la fréquence d'une cellule du cache
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- l'arbre
    -- post-condition : Frequence_Arbre'Result >= 0
    function Frequence_Arbre(Arbre : in T_Arbre) return Integer with
        Post => Frequence_Arbre'Result >= 0;


    -- nom : Demandes_Cache
    -- sémantique : Permet de récupérer le nombre de demandes dans le cache
    -- paramètres :
    --      Cache : Mode In T_Param_Cache; -- le cache
    -- post-condition : Demandes_Cache'Result >= 0
	function Demandes_Cache(Cache : in T_Param_Cache) return Integer with
	    Post => Demandes_Cache'Result >= 0;


    -- nom : Defauts_Cache
    -- sémantique : Permet de récupérer le nombre de défauts dans le cache
    -- paramètres :
    --      Cache : Mode In T_Param_Cache; -- le cache
    -- post-condition : Defauts_Cache'Result >= 0
	function Defauts_Cache(Cache : in T_Param_Cache) return Integer with
        Post => Defauts_Cache'Result >= 0;


    -- nom : Enregistrement_Cache
    -- sémantique : Permet de récupérer le nombre d'enregistrements dans le cache
    -- paramètres :
    --      Cache : Mode In T_Param_Cache; -- le cache
    -- post-condition : Enregistrement_Cache'Result >= 0
	function Enregistrement_Cache(Cache : in T_Param_Cache) return Integer with
        Post => Enregistrement_Cache'Result >= 0;


    -- nom : Enregistrer
    -- sémantique : Enregistrer une adresse, un masque et une sortie dans le cache
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre; -- le cache
    --      Cache : Mode In/Out T_Param_Cache; -- les paramètres du cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse IP à ajouter
    --      Masque : Mode In T_Adresse_IP; -- le masque à ajouter
    --      Sortie : Mode In Unbounded_String; -- la sortie à ajouter
    -- post-condition : Taille_Cache(Arbre) = Taille_Cache(Arbre)'Old + 1
    procedure Enregistrer(Arbre : in out T_Arbre; Cache : in out T_Param_Cache; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP; Sortie : in Unbounded_String) with
        Post => Taille_Cache(Cache) = Taille_Cache(Cache)'Old + 1;


    -- nom : Supprimer
    -- sémantique : Supprimer un élément du cache selon la politique
    -- paramètres :
    --      Arbre : Mode In/Out T_Arbre -- le cache
    --      Cache : Mode In/Out T_Param_Cache; -- les paramètres du cache
    --      Min_Identifiant_Frequence : Mode In Integer; -- l'identifiant minimum pour des cellules ayant une même fréquence donnée
    -- pré-condition : Est_Plein(Arbre)
    -- post-condition : Taille_Cache(Arbre) = Taille_Cache(Arbre)'Old - 1
    procedure Supprimer(Arbre : in out T_Arbre; Cache : in out T_Param_Cache; Min_Identifiant_Frequence : in Integer) with
        Pre => Est_Plein(Cache),
        Post => Taille_Cache(Cache) = Taille_Cache(Cache)'Old - 1;


    -- nom : Est_Plein
    -- sémantique : Permet de savoir si le cache est plein ou non
    -- paramètres :
    --      Cache : Mode In T_Param_Cache; -- les paramètres du cache
    -- pré-condition : Taille_Cache(Cache) > 0
    function Est_Plein(Cache : in T_Param_Cache) return Boolean with
        Pre => Taille_Cache(Cache) > 0;


    -- nom : Afficher_Arbre
    -- sémantique : Permet d'afficher le cache
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- le cache à afficher
    procedure Afficher_Arbre(Arbre : in T_Arbre);


    -- nom : Afficher_Statistiques_Cache
    -- sémantique : Permet d'afficher les statistiques relatives au cache et à sa politique
    -- paramètres :
    --      Cache : Mode In T_Param_Cache; -- le cache dont les statistiques doivent être affichées
    procedure Afficher_Statistiques_Cache(Cache : in T_Param_Cache);


    -- nom : Recherche_Identifiant_Max
    -- sémantique : Permet de trouver l'identifiant maximal, cela est nécessaire pour la politique LRU. En effet, lorsque j'utilise le max, je regarde si l'identifiant de la route utilisée
    -- correspond au max des identifiants. Dans ce cas, c'est la route la plus récemment identifiée. Sinon, on affecte son identifiant à max + 1. Pour la suppression, il ne reste qu'à regarder
    -- l'identifiant minimum.
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- le cache
    --      Max : Mode In/Out Integer; -- le maximum
    -- post-condition : Recherche_Identifiant_Max'Result >= 0
    function Recherche_Identifiant_Max(Arbre : in T_Arbre; Max : in out Integer) return Integer with
        Post => Recherche_Identifiant_Max'Result >= 0;


    -- nom : Recherche_Identifiant_Min
    -- sémantique : Permet de trouver l'identifiant minimum, cela est nécessaire pour les politiques FIFO et LRU.
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- le cache
    --      Min : Mode In/Out Integer; -- le minimum
    -- post-condition : Recherche_Identifiant_Min'Result >= 0
    function Recherche_Identifiant_Min(Arbre : in T_Arbre; Min : in out Integer) return Integer with
        Post => Recherche_Identifiant_Min'Result >= 0;


    -- nom : Recherche_Frequence_Min
    -- sémantique : Permet de trouver la fréquence minimale, cela est nécessaire pour la politique LFU.
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- le cache
    --      Min : Mode In/Out Integer; -- le minimum
    -- post-condition : Recherche_Frequence_Min'Result >= 0
    function Recherche_Frequence_Min(Arbre : in T_Arbre; Min : in out Integer) return Integer with
        Post => Recherche_Frequence_Min'Result >= 0;


    -- nom : Recherche_Identifiant_Frequence_Min
    -- sémantique : Permet de trouver l'identifiant minimum pour des cellules ayant uniquement la même fréquence, cela est nécessaire pour la politique LFU.
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- le cache
    --      Frequence : Mode In Integer; -- la fréquence
    --      Min : Mode In/Out Integer; -- le minimum
    -- post-condition : Recherche_Identifiant_Frequence_Min'Result >= 0
    function Recherche_Identifiant_Frequence_Min(Arbre : in T_Arbre; Frequence : in Integer; Min : in out Integer) return Integer with
        Post => Recherche_Identifiant_Frequence_Min'Result >= 0;


    -- nom : Chercher_Arbre
    -- sémantique : Permet de renvoyer la sortie correspondante à une adresse dans le cache
    -- paramètres :
    --      Arbre : Mode In T_Arbre; -- le cache
    --      Cache : Mode In/Out T_Param_Cache; -- les paramètres du cache
    --      Adresse : Mode In T_Adresse_IP; -- l'adresse
    function Chercher_Arbre(Arbre : in T_Arbre; Cache : in out T_Param_Cache; Adresse : in T_Adresse_IP) return Unbounded_string;

private

    type T_Arbre_Cellule;

    type T_Arbre is access T_Arbre_Cellule;

    -- je stocke les paramètres du cache ici
    type T_Param_Cache is record
        Taille : Integer;
        Taille_Max : Integer;
        Defauts : Integer;
        Demandes : Integer;
        Enregistrement : Integer; -- nombre d'enregistrement dans le cache (pas borné par la taille, le mettre par défaut à 0)
        Politique : T_Politique;
    end record;

    type T_Arbre_Cellule is record
        Adresse : T_Adresse_IP;
        Masque : T_Adresse_IP;
        Sortie : Unbounded_String;
        Gauche : T_Arbre;
        Droite : T_Arbre;
        Frequence : Integer; -- pour appliquer la politique LFU
        Feuille : Boolean; -- pour savoir si la cellule est active ou non, c'est-à-dire si c'est une feuille
        Identifiant : Integer; -- pour appliquer les politiques LRU et FIFO
        Hauteur : Integer;
    end record;

end cache_tree;
