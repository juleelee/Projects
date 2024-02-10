with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions;    use routeur_exceptions;
with tools; use tools;
with Ada.Unchecked_Deallocation;

package body CACHE_LCA is

   procedure Free is
     new Ada.Unchecked_Deallocation (T_Cellule, T_CACHE_LCA);

   -- Initialiser le cache LCA et le tableau RECENT_LCA utilisé dans la politique LRU

   procedure Initialiser(Cache: out T_CACHE_LCA ; Taille : Integer ; pol : T_Politique) is
   begin
      Cache := null;
      TAILLE_MAX := Taille;
      POLITIQUE := pol;
      for i in 1 .. 1000 loop
         RECENT_LCA(i) := 0;
      end loop;
   end Initialiser;

   -- Savoir si le cache est plein ou non

   function Est_Plein(Cache : in T_CACHE_LCA) return Boolean is
   begin
      return Taille(Cache) = TAILLE_MAX;
   end Est_Plein;

   -- Savoir si le cache est vide ou non

   function Est_Vide(Cache : in T_CACHE_LCA) return Boolean is
   begin
      return Cache = null;
   end Est_Vide;


   -- Vider le cache

   procedure Vider(Cache : in out T_CACHE_LCA) is
      Cache0 : T_CACHE_LCA;
   begin
      while Cache.all.Suivant /= null loop
         Cache0 := Cache;
         Cache := Cache.all.Suivant;
         Free(Cache0);
      end loop;
      Free(Cache);
   end Vider;

   -- Connaitre la taille d'une liste chainee

   function Taille (Cache : in T_CACHE_LCA) return Integer is
      n : integer;
      Cache0 : T_CACHE_LCA;
   begin
      n := 0;
      Cache0 := Cache;
      while Cache0 /= null loop
         Cache0 := Cache0.all.Suivant;
         n := n + 1;
      end loop;
      return n;
   end Taille;

   -- Politique FIFO

   procedure Supprimer_FIFO(Cache : in out T_CACHE_LCA) is
      Cache0 : T_CACHE_LCA;
   begin
      Cache0 := Cache;
      Cache := Cache.all.Suivant;
      Free(Cache0);
   end Supprimer_FIFO;

   -- Politique LRU

   procedure Ajouter_Recent(Adresse : in T_ADRESSE_IP) is
      i : integer;
   begin
      i := 1;
      while RECENT_LCA(i) /= 0 loop
         i := i + 1;
      end loop;
      RECENT_LCA(i) := Adresse;
   end Ajouter_Recent;

   procedure Supprimer_Recent(Adresse : in T_ADRESSE_IP) is
      i : integer;
   begin
      i := 1;
      while RECENT_LCA(i) /= Adresse loop
         i := i + 1;
      end loop;
      while RECENT_LCA(i) /= 0 loop
         RECENT_LCA(i) := RECENT_LCA(i+1);
         i := i + 1;
      end loop;
   end Supprimer_Recent;

   procedure Supprimer_LRU(Cache : in out T_CACHE_LCA) is
      Cache0 : T_CACHE_LCA;
   begin
      if Cache.all.Adresse = RECENT_LCA(1) then
         Cache0 := Cache;
         Cache := Cache.all.Suivant;
         Free(Cache0);
      else
         Supprimer_LRU(Cache.all.Suivant);
      end if;
   end Supprimer_LRU;

   -- Politique LFU

   function Adresse_LFU(Cache : in T_CACHE_LCA) return integer is
      Cache0 : T_CACHE_LCA;
      Freq_min : integer;
   begin
      Cache0 := Cache;
      Freq_min := 1;
      while not(Est_Vide(Cache0)) loop
         if Cache0.all.Frequence < Freq_min then
            Freq_min := Cache0.all.Frequence;
         else
            null;
         end if;
         Cache0 := Cache0.all.Suivant;
      end loop;
      return Freq_min;
   end Adresse_LFU;

   procedure Supprimer_LFU(Cache : in out T_CACHE_LCA) is
      Freq_min : integer;
      Cache0 : T_CACHE_LCA;
   begin

      Freq_min := Adresse_LFU(Cache);

      if Cache.all.Frequence = Freq_min then
         Cache0 := Cache;
         Cache := Cache.all.Suivant;
         Free(Cache0);
      else
         Supprimer_LFU(Cache.all.Suivant);
      end if;

   end Supprimer_LFU;

   -- Supprimer un element du cache si ce dernier est plein, en suivant une politique particuliÃ¨re

   procedure Supprimer(Cache : in out T_CACHE_LCA) is
   begin
      case POLITIQUE is
         when FIFO => Supprimer_FIFO(Cache);
         when LRU => Supprimer_LRU(Cache);
         when LFU => Supprimer_LFU(Cache);
         when others => raise Politique_non_valide_exception;
      end case;
   end Supprimer;

   -- Savoir si une adresse est presente ou non dans le cache

   function Adresse_Presente(Cache : in T_CACHE_LCA ; Adresse : in T_Adresse_IP) return Boolean is
      Presence : Boolean;
      Adresse_Masquee : T_ADRESSE_IP;
      Cache0 : T_CACHE_LCA;
   begin
      Cache0 := Cache;
      if Cache0 = null then
         Presence := False;
      else
         Adresse_Masquee := Adresse AND Cache0.all.Masque;
         if Cache0.all.Adresse = Adresse_Masquee then
            Presence := True;
         else
            return Adresse_Presente(Cache0.all.Suivant, Adresse);
         end if;
      end if;
      return Presence;
   end Adresse_Presente;

   -- Recuperer dans le cache le masque associe a l'adresse demandee.

   function Recuperer_Masque_Cache(Cache : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return T_Adresse_IP is
      Masque : T_ADRESSE_IP;
   begin
      if Cache = null then
         raise Adresse_Absente_Exception;
      elsif Cache.all.Adresse = Adresse then
         Masque := Cache.all.Masque;
         Supprimer_Recent(Adresse);
         Ajouter_Recent(Adresse);
         Cache.all.Frequence := Cache.all.Frequence + 1;
      else
         Masque := Recuperer_Masque_Cache(Cache.all.Suivant, Adresse);
      end if;
      return Masque;
   end Recuperer_Masque_Cache;

<<<<<<< HEAD
   function Recuperer_Eth_Cache0(Cache : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP) return Unbounded_String is
      Eth : Unbounded_String;
      begin
      if Cache = null then
         raise Adresse_Absente_Exception;
      elsif Cache.all.Adresse = Adresse then
         Eth := Cache.all.Eth;
      else
         Eth := Recuperer_Eth_Cache0(Cache.all.Suivant, Adresse);
      end if;
      return Eth;
   end Recuperer_Eth_Cache0;

   -- Recuperer dans le cache l'interface associee a l'adresse demandee. Null est renvoyé dans le cas contraire.
=======
   -- Recuperer dans le cache l'interface associee a l'adresse demandee. Null est renvoyÃ© dans le cas contraire.
>>>>>>> 0ef7719c96454b458a3ab776fe15a4f691a1a7cc

   function Recuperer_Eth_Cache(Cache : in T_CACHE_LCA ; Adresse : T_Adresse_IP) return Unbounded_String is
      Cache_Temp : T_CACHE_LCA;
   begin
      Cache_Temp := Cache;
      while Cache_Temp /= null loop
         if Is_Equal_With_Mask(Adresse, Cache_Temp.all.Adresse, Cache_Temp.all.Masque) then
            return Cache_Temp.all.Eth;
         else
            Cache_Temp := Cache_Temp.all.Suivant;
         end if;
      end loop;
      raise Adresse_Absente_Exception;
   end Recuperer_Eth_Cache;

   -- Enregistrer une route (adresse, masque et interface) dans le cache

   procedure Enregistrer(Cache : in out T_CACHE_LCA ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP ; Eth : in Unbounded_String) is
   begin
      if Est_Vide(Cache) then
         Cache := new T_Cellule'(Adresse, Masque, Eth, 1, Null);
         Ajouter_Recent(Adresse);
      else
         Enregistrer(Cache.all.Suivant, Adresse, Masque, Eth);
      end if;
   end Enregistrer;

end CACHE_LCA;
