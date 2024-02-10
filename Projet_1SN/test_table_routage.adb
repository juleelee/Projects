with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions; use routeur_exceptions;
with tools; use tools;
with table_routage; use table_routage;

procedure Test_Table_Routage is

    tr : T_Table_Routage;
    param : T_Param;
    File_paquet : File_Type;
    File_resultat : File_Type;
    ligne : Unbounded_String;
    num_ligne : Integer;

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

    function Convert_Unbounded_String_To_T_Adresse_IP(ligne : Unbounded_String) return T_Adresse_IP is
        Adresse_Converti : T_Adresse_IP := 0;
        mot : Unbounded_String;
        N : Integer;
        j : Integer := 1;
    begin
            
        for i in 0..3 loop 
            mot := Null_Unbounded_String;
                
            N := length(ligne);
                
            while j <= N and then Element(ligne, j) /= '.' loop
                
                mot := mot & Element(ligne, j);
                j := j+1;
                    
            end loop;
                
            -- Enregistrer la destination et le masque une fois converti dans le routeur et enregistrer l'interface dans le routeur
              
            Adresse_Converti := Adresse_Converti + T_Adresse_IP'Value(To_String(mot)) * (2 ** (24-8*i));
            
            j := j + 1;
            
        end loop;
        return Adresse_Converti;
            
    end Convert_Unbounded_String_To_T_Adresse_IP;


    procedure Tester_Initialiser is 
        Table_Routage : T_Table_Routage;
    begin
        pragma Assert (Est_Vide(Table_Routage));
        pragma Assert (Taille(Table_Routage) = 0);

        Initialiser(param, Table_Routage);

        pragma Assert (not(Est_Vide(Table_Routage)));

        Put_Line("Les tests de 'Initialiser' sont réussis !");
    end Tester_Initialiser; 

    procedure Tester_Get_Taille_Binaire is
        adresse : T_Adresse_IP;
    begin 
        adresse := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("255.255.0.0")); 

        pragma Assert (Get_taille_binaire_masque(adresse) = 16);
        pragma Assert (Get_taille_binaire_masque(1) = 0);
        pragma Assert (Get_taille_binaire_masque(255) = 0);
        pragma Assert (Get_taille_binaire_masque(255 * (2**24)) = 8);
        
        Put_Line("Les tests de 'Get_Taille_Binaire' sont réussis !");
    end Tester_Get_Taille_Binaire;
    
    procedure Tester_Enregister_Supprimer is
        adresse : T_Adresse_IP;
        masque : T_Adresse_IP;
        sortie : Unbounded_String;
        table_routage : T_Table_Routage;
        table_temp : T_Table_Routage;
    begin
        Initialiser(param, table_routage);

        Vider(table_routage);

        -- Reprendre la procédure Enregistrer du mini projet 2 pour une LCA ...
        adresse := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("192.168.0.0"));
        masque := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("255.255.0.0"));
        sortie := To_Unbounded_String("eth0");

        Enregistrer(table_routage, adresse, masque, sortie);
        table_temp := Table_Routage;
        
        pragma Assert (Get_Adresse(table_routage) = adresse);
        pragma Assert (Get_Masque(table_routage) = masque);
        pragma Assert (Get_Sortie(table_routage) = sortie);
        
        adresse := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("112.128.3.56"));
        masque := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("255.255.255.0"));
        sortie := To_Unbounded_String("eth2");

        Enregistrer(table_routage, adresse, masque, sortie);
        
        Table_Routage := Get_Suivant(Table_Routage);
    
        pragma Assert (Get_Adresse(Table_Routage) = adresse);
        pragma Assert (Get_Masque(Table_Routage) = masque);
        pragma Assert (Get_Sortie(Table_Routage) = sortie);

        adresse := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("127.187.34.0"));
        masque := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("255.255.255.255"));
        sortie := To_Unbounded_String("eth1");

        Enregistrer(table_routage, adresse, masque, sortie);
        Table_Routage := Get_Suivant(Table_Routage);
        pragma Assert (Get_Adresse(Table_Routage) = adresse);
        pragma Assert (Get_Masque(Table_Routage) = masque);
        pragma Assert (Get_Sortie(Table_Routage) = sortie);
        table_routage := table_temp;
        pragma Assert (Taille(table_routage) = 3);
        Supprimer(table_routage, adresse);

        pragma Assert (Taille(table_routage) = 2);

        adresse := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("112.128.3.56"));
        Supprimer(table_routage, adresse);

        pragma Assert (Taille(table_routage) = 1);

        adresse := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("192.168.0.0"));
        Supprimer(table_routage, adresse);

        pragma Assert (Taille(table_routage) = 0);
        Vider(table_temp);
        vider(Table_Routage );
        Put_Line("Les tests de 'Enregistrer' et 'Supprimer' sont réussis !");
    end Tester_Enregister_Supprimer;

    procedure Tester_Adresse_Presente is
        table_routage : T_Table_Routage;
        adresse : T_Adresse_IP;
        adresse2 : T_Adresse_IP;
        masque : T_Adresse_IP;
        sortie : Unbounded_String;
    begin
        Initialiser(param, table_routage);

        adresse := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("192.168.0.0"));
        adresse2 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("192.168.14.0"));
        masque := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("255.255.0.0"));
        sortie := To_Unbounded_String("eth0");

        Enregistrer(table_routage, adresse, masque, sortie);

        pragma Assert (Adresse_Presente(table_routage, adresse) = True);
        pragma Assert(Adresse_Presente(table_routage, adresse2) = False);

        Vider (table_routage);
        

        Put_Line("Les tests de 'Adresse_Presente' sont réussis !");
    end Tester_Adresse_Presente;

begin
    param := Initialiser_Param;

    begin

        Remplir_param(param);

        Afficher_Param(param);

        Initialiser(param => param, Table_routage => tr);
        

        -- PAQUETS :
        Open (File => File_paquet, Mode => In_File, Name => To_String(param.file_paquets));

        Create (File => File_resultat, Mode => Out_File, Name => To_String(param.file_resultats));

        num_ligne := 1;

        While not End_Of_File (File_paquet) Loop

            ligne := To_Unbounded_String(Get_Line(File_paquet));

            if Is_Command_And_Then_Execute(To_String(ligne), tr, File_resultat, num_ligne) then
               null;
            else
                Put_Line(File_resultat, To_String(ligne) & " " & To_String(Get_Interface(Unbounded_String_To_Adresse_IP(ligne), tr)));
            end if;

            num_ligne := num_ligne + 1;

        end loop;

        Close (File_paquet);
        Close (File_resultat);

    exception
        when Option_non_valide_exception => Afficher_Usage;
        when Name_Error => raise Name_Error with "Un des fichiers passés en paramètres n'existe pas !";
        when COMMAND_FIN_CALLED => Put_Line("Fin du programme.");

    end;
    New_Line;
    Tester_Initialiser;
    Tester_Get_Taille_Binaire;
    Tester_Enregister_Supprimer;
    Tester_Adresse_Presente;
    
end Test_Table_Routage;
