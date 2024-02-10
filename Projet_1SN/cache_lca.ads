with tools; use tools;
with Table_Routage; use Table_Routage;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package CACHE_LCA is

   TAILLE_MAX : Integer;
   POLITIQUE : T_Politique;

   type T_RECENT_LCA is array (1 .. 1000) of T_Adresse_IP;
   RECENT_LCA : T_RECENT_LCA;

   type T_CACHE_LCA is limited private;

   -- Initialiser le cache.  Le cache est vide.
   procedure Initialiser(Cache: out T_CACHE_LCA ; Taille : Integer ; Pol : T_Politique) with
     Post => Est_Vide(Cache);

   -- Le cache est-il plein ?
   function Est_Plein(Cache : T_CACHE_LCA) return Boolean with
     Post => (Taille(Cache) = TAILLE_MAX) = Est_Plein'Result;

   -- Le cache est-il vide ?
   function Est_Vide(Cache : in T_CACHE_LCA) return Boolean;

   -- Supprimer tous les elements du cache.
   procedure Vider(Cache : in out T_CACHE_LCA) with
     Post => Est_Vide(Cache);

   -- Renvoie la taille du cache.
   function Taille(Cache : T_CACHE_LCA) return Integer with
     Post => Taille'Result >= 0
     and (Taille'Result = 0) = Est_Vide(Cache);

   -- Supprimer un element du cache, suivant la politique demandee au prealable par l'utilisateur.
   procedure Supprimer(Cache : in out T_CACHE_LCA) with
     Post => Taille(Cache) = Taille(Cache)'Old - 1;

   -- Savoir si une adresse est presente dans le cache.
   function Adresse_Presente(Cache : in T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return Boolean;

   -- Recuperer dans le cache le masque associe a l'adresse demandee.
   function Recuperer_Masque_Cache(Cache : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return T_Adresse_IP with
     Pre => Adresse_Presente(Cache, Adresse);

   function Recuperer_Eth_Cache0(Cache : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return Unbounded_String;

   -- Recuperer dans le cache l'interface associee a l'adresse demandee. Null est renvoyé dans le cas contraire.
   function Recuperer_Eth_Cache(Cache : in T_CACHE_LCA ; Adresse : T_Adresse_IP) return Unbounded_String with
     Pre => Adresse_Presente(Cache, Adresse);

   -- Enregistrer une nouvelle route dans le cache.
   procedure Enregistrer(Cache : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in Unbounded_String);

private

   type T_Cellule;

   type T_CACHE_LCA is access T_Cellule;

   type T_Cellule is
      record
         Adresse : T_ADRESSE_IP;
         Masque : T_ADRESSE_IP;
         Eth : Unbounded_String;
         Frequence : Integer;
         Suivant : T_CACHE_LCA;
      end record;

end CACHE_LCA;
