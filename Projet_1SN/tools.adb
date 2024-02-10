with Ada.Text_IO; use Ada.Text_IO;
with routeur_exceptions; use routeur_exceptions;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body tools is

    function Initialiser_Param return T_Param is

        param : T_Param;

    begin
        param.taille_cache := 10;
        param.afficher_stats := True;
        param.file_table_routage := To_Unbounded_String("table.txt");
        param.file_paquets := To_Unbounded_String("paquets.txt");
        param.file_resultats := To_Unbounded_String("resultats.txt");
        param.politique := FIFO;

        return param;
    end Initialiser_Param;

    procedure Remplir_Param(param : out T_Param) is

        iterateur : Integer := 1;

    begin
        while iterateur <= Argument_Count loop

            if Argument(iterateur)(1) = '-' and Argument(iterateur)'Length = 2 then

                case Argument(iterateur)(2) is

                    when 'c' =>
                        param.taille_cache := Integer'Value(Argument(iterateur + 1));
                        iterateur := iterateur + 1;

                    when 'P' =>
                        param.politique := T_Politique'Value(Argument(iterateur + 1));
                        iterateur := iterateur + 1;

                    when 's' => param.afficher_stats := True;

                    when 'S' => param.afficher_stats := False;

                    when 't' =>
                        param.file_table_routage := To_Unbounded_String(Argument(iterateur + 1));
                        iterateur := iterateur + 1;

                    when 'p' =>
                        param.file_paquets := To_Unbounded_String(Argument(iterateur + 1));
                        iterateur := iterateur + 1;

                    when 'r' =>
                        param.file_resultats := To_Unbounded_String(Argument(iterateur + 1));
                        iterateur := iterateur + 1;

                    when others => raise Option_non_valide_exception;

                end case;

                iterateur := iterateur + 1;

            else
                raise Option_non_valide_exception;
            end if;

        end loop;
    end Remplir_Param;

    procedure Afficher_Param(param : T_Param) is

    begin
        Put_Line("Affichage du fichier param :");
        Put_Line("   Taille du cache : " & Integer'Image(param.taille_cache));

        if param.afficher_stats then
            Put_Line("   Afficher les statistiques");
        else
            Put_Line("   Ne pas afficher les statistiques");
        end if;

        Put_Line("   Politique : " & T_Politique'Image(param.politique));
        Put_Line("   Fichier table de routage : " & To_String(param.file_table_routage));
        Put_Line("   Fichier paquets : " & To_String(param.file_paquets));
        Put_Line("   Fichier résultats : " & To_String(param.file_resultats));

    end Afficher_Param;

    function Get_taille_binaire_masque(adresse : T_Adresse_IP) return Integer is
        exposant : Integer := 31;
    begin
        while (adresse and 2 ** exposant) /= 0 loop
            exposant := exposant - 1;
        end loop;

        return (31 - exposant);
    end Get_taille_binaire_masque;


    --R3: Convertir l’adresse IP de la destination et du masque en T_Adesse_IP
    function Unbounded_String_To_Adresse_IP(ligne : in Unbounded_String) return T_Adresse_IP is
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
    end Unbounded_String_To_Adresse_IP;

    function Construct_Mask(taille_masque : in Integer) return T_Adresse_IP is
        masque : T_Adresse_IP := 0;
    begin

        for i in 1..taille_masque loop
            masque := masque + 2 ** (32-i);
        end loop;

        return masque;

    end Construct_Mask;


function Adresse_IP_To_String(adresse : in T_Adresse_IP) return String is
    UN_OCTET : constant T_Adresse_IP :=  2 ** 8;
    Result : Unbounded_String := Null_Unbounded_String;
begin
    for i in reverse 0..3 loop
        Result := Result & Trim(Integer'Image(Natural ((adresse / UN_OCTET ** i) mod UN_OCTET)), Ada.Strings.Left) & ".";
    end loop;


    return To_String(Result)(1..(Length(Result) - 1));
end Adresse_IP_To_String;


function Apply_Masque(adresse : T_Adresse_IP; masque : T_Adresse_IP) return T_Adresse_IP is
begin
    return adresse AND masque;
end Apply_Masque;


function Is_Equal_With_Mask(adresse1 : in T_Adresse_IP; adresse2 : in T_Adresse_IP; masque : in T_Adresse_IP) return Boolean is
begin
    return (adresse1 AND masque) = (adresse2 AND masque);
end Is_Equal_With_Mask;

end tools;
