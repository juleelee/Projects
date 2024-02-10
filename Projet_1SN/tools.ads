with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package tools is

    type T_Politique is (FIFO, LRU, LFU);
    -- un enregistrement qui permet de stocker les differentes donnees sur la table de routage manipulee
    type T_Param is record

        taille_cache : Integer; -- Parametre [-c]
        afficher_stats : Boolean; -- Parametre [-s] et [-S]
        file_table_routage : Unbounded_String; -- Parametre [-t]
        file_paquets : Unbounded_String; -- Parametre [-p]
        file_resultats : Unbounded_String; -- Parametre [-r]
        politique : T_Politique; -- Parametre [-P]

    end record;
    -- type des adresses IP
    type T_Adresse_IP is mod 2 ** 32;

    function Initialiser_Param return T_Param;

    procedure Afficher_Param(param : T_Param);

    procedure Remplir_Param(param : out T_Param);

    function Get_taille_binaire_masque(adresse : in T_Adresse_IP) return Integer;


    -- Unbounded_String_To_Adresse_IP permet de transformer une adresse IP (de type character, ex : 147.0.0.0) en type T_Adresse_IP
    function Unbounded_String_To_Adresse_IP(ligne : in Unbounded_String) return T_Adresse_IP;
    --Adresse_IP_To_String est l'operation inverse de  Unbounded_String_To_Adresse_IP
    function Adresse_IP_To_String(adresse : in T_Adresse_IP) return String;

    function Construct_Mask(taille_masque : in Integer) return T_Adresse_IP;

    function Apply_Masque(adresse : in T_Adresse_IP; masque : in T_Adresse_IP) return T_Adresse_IP;

    function Is_Equal_With_Mask(adresse1 : in T_Adresse_IP; adresse2 : in T_Adresse_IP; masque : in T_Adresse_IP) return Boolean;

end tools;
