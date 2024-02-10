TEB_pratique_100_sans = [];
TEB_theorique_100_sans = [];
phi = deg2rad(100);
for db = 0:0.5:6
    Eb_N0 = 10^(db/10);
    P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x)) + 1i*sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;




    % la phase porteuse 
 
exp_phi = exp(1j*phi);

% erreur de phase 
x_phi = x_bruite*exp_phi;


% filtre de reception 

x_choisi = x_phi;

x_chaine1_sortie = filter(porte,1,x_choisi);

% Echantillonnage aux instants optimaux
n0 = 4;
echantillon_chaine_TEB = x_chaine1_sortie(n0:Ns:end); 




% % Les constellations en sortie du mapping et de l’échantillonneur
%     figure();
%     plot(real(echantillon_chaine_TEB), imag(echantillon_chaine_TEB), 'r*');
%     axis([-16 16 -16 16]);
%     grid on;
%     title(['dB = ' num2str(db)]);


   
% Partie Reelle 
echantillon_chaine = real(echantillon_chaine_TEB); 

%Signal en sortie, Decision
bits_sortie_chaine1 = echantillon_chaine > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux_chaine1 = sum(bits_sortie_chaine1 ~= transmettre );

taux_err_binaire_chaine1 = nb_bits_faux_chaine1/length(transmettre);

    TEB_bis = taux_err_binaire_chaine1;
    TEB_pratique_100_sans = [TEB_pratique_100_sans TEB_bis];
    Pb = qfunc(cos(phi)*sqrt(2*Eb_N0)); % calcul de la probabilité d'erreur de bit
    TEB_t = Pb;
    TEB_theorique_100_sans = [TEB_theorique_100_sans TEB_t];

end 

figure(50)
semilogy(0:0.5:6,TEB_theorique_100_sans);
hold on;
semilogy(0:0.5:6,TEB_pratique_100_sans,'g');
hold off
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique sans correction de l'erreur de phase")

% Avec correction 

%% Tracé de TEB 

TEB_pratique_100_avec = [];
TEB_theorique_100_avec = [];
phi = deg2rad(100);
for db = 0:0.5:6
    Eb_N0 = 10^(db/10);
    P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x)) + 1i*sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;




    % la phase porteuse 
 
exp_phi = exp(1j*phi);

% erreur de phase 
x_phi = x_bruite*exp_phi;


% filtre de reception 

x_choisi = x_phi;

x_chaine1_sortie = filter(porte,1,x_choisi);

% Echantillonnage aux instants optimaux
n0 = 4;
echantillon_chaine_TEB = x_chaine1_sortie(n0:Ns:end); 

zm = echantillon_chaine_TEB;
phi_estime = 1/2*angle(sum(zm.^2));
phi_deg_estime = rad2deg(phi_estime)


% % Les constellations en sortie du mapping et de l’échantillonneur
%     figure();
%     plot(real(echantillon_chaine_TEB), imag(echantillon_chaine_TEB), 'r*');
%     axis([-16 16 -16 16]);
%     grid on;
%     title(['dB = ' num2str(db)]);

% Correction 
echantillon_chaine_TEB = echantillon_chaine_TEB.*exp(-1i*phi_estime);

% Partie Reelle 
echantillon_chaine = real(echantillon_chaine_TEB); 

%Signal en sortie, Decision
bits_sortie_chaine1 = echantillon_chaine > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux_chaine1 = sum(bits_sortie_chaine1 ~= transmettre );

taux_err_binaire_chaine1 = nb_bits_faux_chaine1/length(transmettre);

    TEB_bis = taux_err_binaire_chaine1;
    TEB_pratique_100_avec = [TEB_pratique_100_avec TEB_bis];
    Pb = qfunc(sqrt(2*Eb_N0)); % calcul de la probabilité d'erreur de bit
    TEB_t = Pb;
    TEB_theorique_100_avec = [TEB_theorique_100_avec TEB_t];

end 

figure(51)
semilogy(0:0.5:6,TEB_theorique_100_avec);
hold on;
semilogy(0:0.5:6,TEB_pratique_100_avec,'g');
hold off
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique avec correction de l'erreur de phase ")


% Comparaison des TEB avec et sans correction 
figure(52)
semilogy(0:0.5:6,TEB_pratique_100_avec);
hold on;
semilogy(0:0.5:6,TEB_pratique_100_sans,'g');
hold off
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{avec correction}","TEB_{sans correction}")
title("Comparaison des TEB avec correction et sans correction d'erreur de phase ( phi = 100° )")