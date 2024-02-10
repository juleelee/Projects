with Ada.Unchecked_Deallocation;
with Routeur_Exceptions; use Routeur_Exceptions;


-- R1 : Concevoir et initialiser un routeur
package body Table_Routage is
    
    procedure Free is
        new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Table_Routage);
    
    -- R2 : Initialiser un routeur 
    procedure Initialiser(param : in T_Param; Table_Routage: out T_Table_Routage) is
        File : File_Type;
        Donnee : array(1..3) of Unbounded_String;
    
        
        iterateur : Integer;
        ligne : Unbounded_String;
        
        N : Integer;
        
        table_routage_temp : T_Table_Routage;
        
    begin
        Open (File => File,
              Mode => In_File,
              Name => To_String(param.file_table_routage));
        
        -- Concevoir une nouvelle cellule dans le routeur pour stocker les donnees de la prochaine ligne 
        Table_Routage := new T_Cellule;
        table_routage_temp := Table_Routage;
        
        While not End_Of_File (File) Loop
            
            ligne := To_Unbounded_String(Get_Line(File));
            N := length(ligne);
            iterateur := 1;
            
            -- R3 : Concevoir un tableau de taille 3 qui va stocker des chaînes de caracteres( Destination , masque et interface) 
            for j in 1..3 loop
                
                Donnee(j) := Null_Unbounded_String;
                
                
                while iterateur <= N and then Element(ligne, iterateur) /= ' ' loop
                    
                    Donnee(j) := Donnee(j) & Element(ligne, iterateur);
                    
                    iterateur := iterateur + 1;
                    
                end loop;
                iterateur := iterateur + 1;
                   
            end loop;
            
            --R3: Convertir l’adresse IP de la destination et du masque  
            --R3 :  Enregistrer la destination et le masque une fois converti dans le routeur et enregistrer l interface dans le routeur.
            table_routage_temp.all.Adresse := Unbounded_String_To_Adresse_IP(Donnee(1));
            table_routage_temp.all.Masque := Unbounded_String_To_Adresse_IP(Donnee(2));
            table_routage_temp.all.Sortie := Donnee(3);


            -- R3 : Concevoir une nouvelle cellule dans le routeur pour stocker les données de la prochaine ligne.           
            if not End_Of_File (File) then
                table_routage_temp.Suivant := new T_Cellule;
                table_routage_temp := table_routage_temp.Suivant;
            else
                null;
            end if;
                 
        end loop;
        
        Close (File);   
        
    end Initialiser;
        
    function Est_Vide (Table_Routage : in T_Table_Routage) return Boolean is
    begin
	
        return Table_Routage = null;	
    end;
    
    function Taille (Table_Routage : in T_Table_Routage) return Integer is
        result : Integer; 
        table_temp : T_Table_Routage;
    begin
        result := 0;
        table_temp:= Table_Routage;
        while not(Est_Vide(table_temp)) loop 
            result := result +1; 
            table_temp := table_temp.all.Suivant;
        end loop;
    
        return result;

    end Taille;

    
    procedure Enregistrer (Table_Routage : in out T_Table_Routage ; Adresse : in T_Adresse_IP ; Masque : in T_Adresse_IP; Sortie : in Unbounded_String) is
    begin
        if Est_Vide (Table_Routage) then
            -- Structure vide
            Table_Routage := new T_Cellule'(Adresse, Masque, Sortie, null); -- On crée une nouvelle cellule
			
        else
            Enregistrer (Table_Routage.All.Suivant, Adresse, Masque, Sortie); -- Récursivité sur le pointeur suivant
        end if;
    end Enregistrer;

    procedure Supprimer (Table_Routage : in out T_Table_Routage ; adresse : in T_Adresse_IP) is
        Table_A_Supp : T_Table_Routage;
    begin
        if Est_Vide(Table_Routage) then
            raise Adresse_Absente_Exception;
        elsif Table_Routage.all.Adresse = adresse then
            Table_A_Supp := Table_Routage;
            Table_Routage:= Table_Routage.all.Suivant;
            Free(Table_A_Supp);
        else
            Supprimer(Table_Routage.all.Suivant, adresse);
        end if;
    end Supprimer;

    procedure Vider (Table_Routage : in out T_Table_Routage) is
    begin
        if not(Est_Vide(Table_Routage)) then
            Vider(Table_Routage.all.Suivant); -- .all permet d'acceder au contenu de l'adresse que pointe le pointeur  
            Free (Table_Routage);
        else
            Null;
        end if;
		
    end Vider;
    
    procedure Afficher(Table_Routage : in T_Table_Routage; file : File_Type) is
        
        table_temp : T_Table_Routage := Table_Routage;
    
    begin
        
        while table_temp /= null loop
            
            Put_Line(file, Adresse_IP_To_String(table_temp.all.Adresse) & 
                         " " & Adresse_IP_To_String(table_temp.all.Masque) &
                         " " & To_String(table_temp.all.Sortie)); 
            
            table_temp := table_temp.all.Suivant;
        end loop;
        
    end;

    
    function Adresse_Correspond(adresse1 : T_Adresse_IP; adresse2 : T_Adresse_IP; Masque: T_Adresse_IP) return Boolean is
    begin    
        return (adresse1 AND Masque) = (adresse2 AND Masque);
    end Adresse_Correspond;
    
    
    
    -- retourne l'interface associée à cette adresse IP
    procedure Get_Interface(Adresse_IP: in T_Adresse_IP; Table_Routage: in T_Table_Routage; interf : out Unbounded_String ; taille_masque_interf : out Integer) is
        
        table_temp : T_Table_Routage;
        
        interface_max : Unbounded_String;
        taille_masque_max : Integer := -1;
        taille_masque : Integer;
        
    begin
        
        table_temp := Table_Routage;
        
        -- Parcourir les adresses ip
        while not Est_Vide(table_temp) loop
            
            if Adresse_Correspond(Adresse_IP, table_temp.all.Adresse, table_temp.all.Masque) then
                
                taille_masque := Get_taille_binaire_masque(table_temp.all.Masque);
                
                if taille_masque > taille_masque_max then
                    taille_masque_max := taille_masque;
                    interface_max := table_temp.all.Sortie;
                else
                    null;
                end if;
            else
                null;
            end if;
            
            table_temp := table_temp.all.Suivant;
                
        end loop;
        
        interf := interface_max;
        taille_masque_interf := taille_masque_max;
        
    end Get_Interface;
    
    function Get_Adresse ( Table_Routage : T_Table_Routage ) return T_Adresse_IP is 
    begin 
        return Table_Routage.all.Adresse;
    end Get_Adresse; 

    function Get_Masque ( Table_Routage : T_Table_Routage ) return T_Adresse_IP is 
    begin 
        return Table_Routage.all.Masque;
    end Get_Masque; 

    function Get_Sortie(Table_Routage: in T_Table_Routage) return Unbounded_String is 
    begin 
        return Table_Routage.all.Sortie;
    end Get_Sortie; 

    function Get_Suivant(Table_Routage: in T_Table_Routage) return T_Table_Routage is 
    begin 
        return Table_Routage.all.Suivant; 
    end Get_Suivant;

    
    function Is_Command_And_Then_Execute_LCA(ligne : in String; tr : in T_Table_Routage; file_output : File_Type; num_ligne : Integer) return Boolean is
        type Commandes is (Table, Cache, Stat, Fin); 
    begin
        
        case Commandes'Value(ligne) is
        when Table =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Table (" & Integer'Image(num_ligne) & " )");
            Afficher(Table_Routage => tr,
                     file          => file_output);
        when Cache =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Cache (" & Integer'Image(num_ligne) & " )");
           
            
        when Stat =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Stat (" & Integer'Image(num_ligne) & " )");
            Put_Line(file_output, "prochainement...");
            
        when Fin =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Fin (" & Integer'Image(num_ligne) & " )");
            raise COMMAND_FIN_CALLED;
        end case;
        
        Put_Line(file_output, "--------------------------------");
        
        return True;
        
    exception
        when Constraint_Error => return False;
    end Is_Command_And_Then_Execute;
   
    function Is_Command_And_Then_Execute_TREE(ligne : in String; tr : in T_Table_Routage; file_output : File_Type; num_ligne : Integer, Arbre : T_Arbre) return Boolean is
        type Commandes is (Table, Cache, Stat, Fin); 
    begin
        
        case Commandes'Value(ligne) is
        when Table =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Table (" & Integer'Image(num_ligne) & " )");
            Afficher(Table_Routage => tr,
                     file          => file_output);
        when Cache =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Cache (" & Integer'Image(num_ligne) & " )");
            Put_Line(file_output, "prochainement...");
        when Stat =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Stat (" & Integer'Image(num_ligne) & " )");
            Put_Line(file_output, "prochainement...");
        when Fin =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Fin (" & Integer'Image(num_ligne) & " )");
            raise COMMAND_FIN_CALLED;
        end case;
        
        Put_Line(file_output, "--------------------------------");
        
        return True;
        
    exception
        when Constraint_Error => return False;
    end Is_Command_And_Then_Execute;
   
    function Is_Command_And_Then_Execute(ligne : in String; tr : in T_Table_Routage; file_output : File_Type; num_ligne : Integer, Arbre : T_Arbre) return Boolean is
        type Commandes is (Table, Cache, Stat, Fin); 
    begin
        
        case Commandes'Value(ligne) is
        when Table =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Table (" & Integer'Image(num_ligne) & " )");
            Afficher(Table_Routage => tr,
                     file          => file_output);
        when Cache =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Cache (" & Integer'Image(num_ligne) & " )");
            Put_Line(file_output, "prochainement...");
        when Stat =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Stat (" & Integer'Image(num_ligne) & " )");
            Put_Line(file_output, "prochainement...");
        when Fin =>
            New_Line(File    => file_output);
            Put_Line(file_output, "Fin (" & Integer'Image(num_ligne) & " )");
            raise COMMAND_FIN_CALLED;
        end case;
        
        Put_Line(file_output, "--------------------------------");
        
        return True;
        
    exception
        when Constraint_Error => return False;
    end Is_Command_And_Then_Execute;
    
    function Adresse_Presente (Table_Routage : in T_Table_Routage ; adresse : in T_Adresse_IP) return Boolean is
    begin	
        if not(Est_Vide(Table_Routage)) then 
            if Table_Routage.all.Adresse = adresse then 
                return true;
            else 
                return Adresse_Presente(Table_Routage.all.Suivant, adresse);
            end if;
        else 
            return false;
        end if; 
    end Adresse_Presente;
    
    function Recuperer_Masque_Plus_Long(Table : in T_Table_Routage ; Adresse : in T_ADRESSE_IP ; Masque : in T_ADRESSE_IP) return T_Adresse_IP is
        table_temp : T_Table_Routage := Table;
        Masque_max : T_ADRESSE_IP;
        Adresse_Masquee : T_ADRESSE_IP;
        Adresse_Masquee_tr : T_Adresse_IP;
    begin
        
        Masque_max := Masque; -- Au moins ce masque, car c'est la solution donnée
        
        Adresse_Masquee := Adresse AND Masque;
         
        while table_temp /= null loop
            
            Adresse_Masquee_tr := table_temp.all.Adresse AND Masque;
            
            if Adresse_Masquee = Adresse_Masquee_tr then
                -- ça match, donc on compare le masque
                if Get_taille_binaire_masque(Masque_max) < Get_taille_binaire_masque(table_temp.Masque) then
                    Masque_max := table_temp.Masque;
                else
                    null;
                end if;
            end if;
            
            table_temp := table_temp.all.Suivant;
            
        end loop;
            
        return Masque_max;
            
    end Recuperer_Masque_Plus_Long;
    
end Table_Routage;
