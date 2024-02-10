with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;

with routeur_exceptions; use routeur_exceptions;
with tools; use tools;
with Table_Routage; use Table_Routage;
with cache_tree; use cache_tree;

procedure routeur_LA is

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

    -- PARTIE CACHE
    cache : T_Param_Cache;
    arbre : T_Arbre;
    adresse : T_Adresse_IP;

    adresse_Cache, masque_Cache : T_Adresse_IP;

begin

    param := Initialiser_Param;

    begin

        Remplir_param(param);

        Afficher_Param(param);

        Table_Routage.Initialiser(param         => param,
                                  Table_routage => tr);

        cache_tree.Initialiser_Cache(cache, param.taille_cache, param.politique);
        cache_tree.Initialiser_Arbre(arbre);

        New_Line;
        Table_Routage.Afficher(tr, Standard_Output);

        -- PAQUETS :
        Open (File => File_paquet, Mode => In_File, Name => To_String(param.file_paquets));

        Create (File => File_resultat, Mode => Out_File, Name => To_String(param.file_resultats));

        num_ligne := 1;

        While not End_Of_File (File_paquet) Loop

            ligne := To_Unbounded_String(Get_Line(File_paquet));
            Trim(ligne, Both);
            adresse := Unbounded_String_To_Adresse_IP(ligne);

            if Is_Command_And_Then_Execute(To_String(ligne), tr, File_resultat, num_ligne) then
                null;
            else

                begin
                    -- On cherche dans le cache
                    interf := Chercher_Arbre(Arbre   => arbre,
                                             Cache   => cache,
                                             Adresse => adresse);
                exception
                        -- si pas trouvé : on cherche à la mano dans la table de routage
                    when others =>

                        Table_Routage.Get_Interface(Unbounded_String_To_Adresse_IP(ligne), tr, interf, taille_masque);

                        masque_Cache := Recuperer_Masque_Plus_Long(Table   => tr,
                                                                   Adresse => adresse,
                                                                   Masque  => Construct_Mask(taille_masque));

                        adresse_Cache := Apply_Masque(adresse => adresse,
                                                      masque  => masque_Cache);

                        cache_tree.Enregistrer(Arbre     => arbre,
                                               Cache     => cache,
                                               Adresse   => adresse_Cache,
                                               Masque    => masque_Cache,
                                               Sortie    => interf,
                                               Politique => param.politique);
                end;

                Put_Line(File_resultat, To_String(ligne) & " " & To_String(interf));

            end if;

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

end routeur_LA;
