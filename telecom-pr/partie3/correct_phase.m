%% Implantation de chaine passe-bas  ́equivalente `a la chaine de transmission sur porteuse
clear all 
close all

% Données 

% Fréquence d'echantillonnage
Fe = 24000; % Hz
Te = 1/Fe;



% nombre de symbole
M = 2; % ordre de modulation
nb_sym = log2(M);

% Débit binaire 
Rb = 6000; % bits/s

Tb = 1/Rb; 

% la durée symbole
Ts = nb_sym*Tb; 

% Facteur de suréchantillonage 
Ns = Ts/Te;



%% 

% Construction du signal modulé
% Signal a transmettre 
taille = 10000; %le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille); %génére aléatoirement taille bits


% - Mapping : symboles binaires a moyenne nulle.
transmettre_1 = 2*transmettre; 
transmettre_1 = transmettre_1 -1; % les 1 deviennent des 1 et les 0 deviennent des -1 
mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 

transmettre_peigne = kron(transmettre_1, mat ); % une forme de peigne 



% – Filtre de mise en forme rectangulaire de hauteur 1 et de duree  egale a la duree symbole.
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
x = filter(porte,1,transmettre_peigne); %le peigne convolué avec le filtre de mise en forme 






%%  Ajout du Bruit 

%Signal a bruit par bit souhaité a l entrée du récepteur Eb/N0
dB = 9;
Eb_N0 = 10^(dB/10);

P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence

P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit

sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x));

signal_bruite = x + bruit;

x_recu = x;


% la phase porteuse 
phi = deg2rad(100); 
exp_phi = exp(1j*phi);

% erreur de phase 
x_phi = x_recu*exp_phi;


% filtre de reception 

x_choisi = x_phi;

x_chaine1_sortie = filter(porte,1,x_choisi);

% Echantillonnage aux instants optimaux
n0 = 4;
echantillon_chaine1 = x_chaine1_sortie(n0:Ns:end); 




% Les constellations en sortie du mapping et de l’échantillonneur
    figure(42);
    plot(real(echantillon_chaine1), imag(echantillon_chaine1), 'r*');
    axis([-4 4 -4 4]);
    grid on;
    title('Les constellations en sortie de l échantillonneur')
 

% Estimation de l'erreur de phase porteuse
zm = echantillon_chaine1;
phi_estime = 1/2*angle(sum(zm.^2));
phi_deg_estime = rad2deg(phi_estime)
   % Quand phi = 40 on trouve comme estimation : 40 
   % mais quand phi = 100 on trouve comme estimation -80° ( c'est +- pi donc
   % +/- 180° 
% Partie Reelle 
echantillon_chaine = real(echantillon_chaine1); 

%Signal en sortie, Decision
bits_sortie_chaine1 = echantillon_chaine > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux_chaine1 = sum(bits_sortie_chaine1 ~= transmettre );

taux_err_binaire_chaine1 = nb_bits_faux_chaine1/length(transmettre)
%% Comparaison des TEB avec et sans correction 

%% Tracé de TEB 

TEB_pratique_40_sans = [];
TEB_theorique_40_sans = [];
phi = deg2rad(40);
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
    TEB_pratique_40_sans = [TEB_pratique_40_sans TEB_bis];
    Pb = qfunc(cos(phi)*sqrt(2*Eb_N0)); % calcul de la probabilité d'erreur de bit
    TEB_t = Pb;
    TEB_theorique_40_sans = [TEB_theorique_40_sans TEB_t];

end 
save('TEB_p_40_sans_correction.mat','TEB_pratique_40_sans');
save('TEB_t_40_sans_correction.mat','TEB_theorique_40_sans');

figure(70)
semilogy(0:0.5:6,TEB_theorique_40_sans);
hold on;
semilogy(0:0.5:6,TEB_pratique_40_sans,'g');
hold off
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique sans correction de l'erreur de phase ( phi = 40°)")

% Avec correction 

%% Tracé de TEB 

TEB_pratique_40_avec = [];
TEB_theorique_40_avec = [];
phi = deg2rad(40);
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
    TEB_pratique_40_avec = [TEB_pratique_40_avec TEB_bis];
    Pb = qfunc(sqrt(2*Eb_N0)); % calcul de la probabilité d'erreur de bit
    TEB_t = Pb;
    TEB_theorique_40_avec = [TEB_theorique_40_avec TEB_t];

end 
save('TEB_p_40_avec_correction.mat','TEB_pratique_40_avec');
save('TEB_t_40_avec_correction.mat','TEB_theorique_40_avec');

figure(61)
semilogy(0:0.5:6,TEB_theorique_40_avec);
hold on;
semilogy(0:0.5:6,TEB_pratique_40_avec,'g');
hold off
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique avec correction de l'erreur de phase (phi = 40°)")


% Comparaison des TEB avec et sans correction 
figure(60)
semilogy(0:0.5:6,TEB_pratique_40_avec);
hold on;
semilogy(0:0.5:6,TEB_pratique_40_sans,'g');
hold on;
semilogy(0:0.5:6,TEB_theorique_40_avec);

hold off
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{avec correction}","TEB_{sans correction}","TEB_{sans erreur}")
title("Comparaison des TEB avec correction et sans correction d'erreur de phase ( phi = 40° )")




%% Avec une erreur de phase de 100°


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
save('TEB_p_100_sans_correction.mat','TEB_pratique_100_sans');
save('TEB_t_100_sans_correction.mat','TEB_theorique_100_sans');

figure(50)
semilogy(0:0.5:6,TEB_theorique_100_sans);
hold on;
semilogy(0:0.5:6,TEB_pratique_100_sans,'g');
hold off

xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique sans correction de l'erreur de phase (phi = 100°)")

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
save('TEB_p_100_avec_correction.mat','TEB_pratique_100_avec');
save('TEB_t_100_avec_correction.mat','TEB_theorique_100_avec');

figure(51)
semilogy(0:0.5:6,TEB_theorique_100_avec);
hold on;
semilogy(0:0.5:6,TEB_pratique_100_avec,'g');

hold off
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique avec correction de l'erreur de phase ( phi = 100° )")


% Comparaison des TEB avec et sans correction 
figure(52)
semilogy(0:0.5:6,TEB_pratique_100_avec);
hold on;
semilogy(0:0.5:6,TEB_pratique_100_sans,'g');
hold on;
semilogy(0:0.5:6,TEB_theorique_40_avec);
hold off
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{avec correction}","TEB_{sans correction}","TEB_{sans erreur}")
title("Comparaison des TEB avec correction et sans correction d'erreur de phase ( phi = 100° )")


