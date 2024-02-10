with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions; use routeur_exceptions;
with tools; use tools;
with Table_Routage; use Table_Routage;
with Ada.Exceptions; use Ada.Exceptions;

procedure Routeur is

    param : T_Param;

    tr : T_Table_Routage;

    -- Afficher l'usage
    procedure Afficher_Usage is
    begin
        New_Line;
        Put_Line ("Usage : " & Command_Name & " [parametres]");
        New_Line;
        Put_Line ("   Voici la liste des paramètres possibles :");
        Put_Line ("   -c <taille> : Défini la taille du cache. Valeur par défaut = 10");
        Put_Line ("   -p FIFO|LRU|LFU : Défini la politique utilisée pour le cache. Valeur par défaut = FIFO");
        Put_Line ("   -s : Afficher les statistiques. Activée par défaut");
        Put_Line ("   -S : Ne pas afficher les statistiques. Désactivée par défaut");
        Put_Line ("   -t <fichier> : Définit le nom du fichier contenant les routes de la table de routage. Valeur par défaut = table.txt");
        Put_Line ("   -p <fichier> : Définit le nom du fichier contenant les paquets à router. Valeur par défaut = paquets.txt");
        Put_Line ("   -r <fichier> : Définit le nom du fichier contenant les résultats. Valeur par défaut = resultats.txt");
        New_Line;
    end Afficher_Usage;

    File_paquet : File_Type;
    File_resultat : File_Type;
    ligne : Unbounded_String;

    num_ligne : Integer;

    interf : Unbounded_String;
    taille_masque : Integer;
begin

    param := Initialiser_Param;

    begin

        Remplir_param(param);

        Afficher_Param(param);

        Initialiser(param => param, Table_routage => tr);

        New_Line;
        Afficher(tr, Standard_Output);

        -- PAQUETS :
        Open (File => File_paquet, Mode => In_File, Name => To_String(param.file_paquets));

        Create (File => File_resultat, Mode => Out_File, Name => To_String(param.file_resultats));

        num_ligne := 1;

        While not End_Of_File (File_paquet) Loop

            ligne := To_Unbounded_String(Get_Line(File_paquet));
            Trim(ligne, Both);

            -- Pas de commandes car il n'y a pas de cache et la fonction en demande une

            Get_Interface(Unbounded_String_To_Adresse_IP(ligne), tr, interf, taille_masque);

            Put_Line(File_resultat, To_String(ligne) & " " & To_String(interf));

            num_ligne := num_ligne + 1;

        end loop;

        Close (File_paquet);
        Close (File_resultat);


    exception
        when Option_non_valide_exception => Afficher_Usage;
        when Name_Error => raise Name_Error with "Un des fichiers passés en paramètres n'existe pas !";
        when COMMAND_FIN_CALLED => Put_Line("Fin du programme.");

        when E : others => Put_Line (Exception_Message (E));
    end;

end Routeur;
