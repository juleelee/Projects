
%% Étude de l'impact du bruit et du filtrage adapté, notion d'efficacité en puissance
Fe = 24000;%Hz
Te = 1/Fe; %seconde 

% Débit binaire 

Rb = 3000; % bits/s

Tb = 1/Rb; 

% Facteur de suréchantillonage 
Ns = Tb/Te; % à déterminer dans chaque cas

% la durée symbole
Ts = Ns*Te; 


% Signal a transmettre 
taille =10000; % le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille); %génére aléatoirement taille bits


% Signal selon le filtre de mise en forme
%% Chaine 1 
% - Mapping : symboles binaires a moyenne nulle.
transmettre_1 = 2*transmettre; 
transmettre_1 = transmettre_1 -1; % les 1 deviennent des 1 et les 0 deviennent des -1 
mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 

transmettre_peigne = kron(transmettre_1, mat ); % une forme de peigne 


V_0 = -1; % symbole du bit 0 
V_1 = 1; %symbole du bit 1 

% – Filtre de mise en forme rectangulaire de hauteur 1 et de duree  egale a la duree symbole.
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
x = filter(porte,1,transmettre_peigne); %le peigne convolué avec le filtre de mise en forme 


P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
M = 2; % ordre de la modulation
%Signal a bruit par bit souhaité a l entrée du récepteur Eb/N0
 dB = 0;
Eb_N0 = 10^(dB/10);
 % dB =10^(Eb/10*N0)
 % Eb/N0 = 10*log10(dB)

P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x));

x_bruite = x + bruit;

% filtre de reception 
x_choisi = x;
x_chaine1_sortie = filter(porte,1,x_choisi);

% Echantillonnage aux instants optimaux
n0 = 8;
echantillon_chaine1 = x_chaine1_sortie(n0:Ns:end); % de la forme [8 8 8 -8 -8 8 ...]

temps = ((n0*Te)-Te:(Ns*Te):length(echantillon_chaine1)*(Ns*Te)-Te);

echelle = (0:Te:taille*Ts-Te);
figure(16);
stem(temps,echantillon_chaine1,'o')
hold on 
plot(echelle, x_chaine1_sortie,'r');
xlabel("Temps en seconde")
ylabel("Signal")
legend('Signal échantillonné','Signal');
title("Échantillonnage du Signal ( chaine 1 ) ")
%exportgraphics(gcf,'./images/4-1-tempchaine1.png','Resolution',400);

% Diagramme de l'oeil

figure(17); 
plot(reshape(x_chaine1_sortie,Ns,length(x_chaine1_sortie)/Ns))
ylabel("Diagramme de l'oeil");
title("chaine1");
%%exportgraphics(gcf,'./images/4-1-diagoeilchaine1.png','Resolution',400);

%Signal en sortie 
bits_sortie_chaine1 = echantillon_chaine1 > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux_chaine1 = sum(bits_sortie_chaine1 ~= transmettre );
taux_err_binaire_chaine1 = nb_bits_faux_chaine1/length(transmettre); 


% TEB pratique et theorique 
TEB_pratique_1 = [];
TEB_theorique_1 = [];

%Tracé des différents diagrammes de l'oeil
        db = 5;
    Eb_N0 = 10^(db/10);
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine1_sortie = filter(porte,1,x_choisi);
    figure(35); 
    subplot(221);
    plot(reshape(x_chaine1_sortie,Ns,length(x_chaine1_sortie)/Ns));
    ylabel("Rapport = 5 dB")
    title("Chaine1")
        db = 8;
    Eb_N0 = 10^(db/10);
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine1_sortie = filter(porte,1,x_choisi);
    figure(35); 
    subplot(222);
    plot(reshape(x_chaine1_sortie,Ns,length(x_chaine1_sortie)/Ns))
    ylabel("Rapport = 8 dB")
    title("Chaine1")
        db = 10;
    Eb_N0 = 10^(db/10);
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine1_sortie = filter(porte,1,x_choisi);
    figure(35); 
    subplot(223);
    plot(reshape(x_chaine1_sortie,Ns,length(x_chaine1_sortie)/Ns));
    ylabel("Rapport = 10 dB")
    title("Chaine1")
        db = 12;
    Eb_N0 = 10^(db/10);
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine1_sortie = filter(porte,1,x_choisi);
    figure(35); 
    subplot(224);
    plot(reshape(x_chaine1_sortie,Ns,length(x_chaine1_sortie)/Ns))
    %ylabel("\frac{E_b}{N_O}=8 dB")
    ylabel("Rapport = 12 dB")
    title("Chaine1")

    %exportgraphics(gcf,'./images/4-1-valrapportchaine1.png','Resolution',400);


%Tracé de la courbe des TEB théo et simulé
for db = 0:0.5:8
    Eb_N0 = 10^(db/10);
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine1_sortie = filter(porte,1,x_choisi);
    % Echantiollonage
    n0 = 8;
    echantillon_chaine1 = x_chaine1_sortie(n0:Ns:end);
    bits_sortie_chaine1 = echantillon_chaine1 > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

    %Calcul taux d'erreur binaire
    nb_bits_faux_chaine1 = sum(bits_sortie_chaine1 ~= transmettre );
    TEB_p = nb_bits_faux_chaine1/length(transmettre); 

    TEB_pratique_1 = [TEB_pratique_1 TEB_p];
    TEB_t =qfunc(sqrt(2*Eb_N0));
    TEB_theorique_1 = [TEB_theorique_1 TEB_t];

end 

figure(18)
semilogy(0:0.5:8,TEB_theorique_1);
hold on;
semilogy(0:0.5:8,TEB_pratique_1,'g');
hold off;
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique chaine 1")
%%exportgraphics(gcf,'./images/4-1-compatebchaine1.png','Resolution',400);


%% Chaine 2
% - Mapping : symboles binaires a moyenne nulle.
transmettre_2 = 2*transmettre; 
transmettre_2 = transmettre_2 -1; % les 1 deviennent des 1 et les 0 deviennent des -1 
mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 

transmettre_peigne = kron(transmettre_2, mat ); % une forme de peigne 



V_0 = -1; % symbole du bit 0 
V_1 = 1; %symbole du bit 1 

% – Filtre de mise en forme rectangulaire de hauteur 1 et de duree  egale a la duree symbole.
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 

x = filter(porte,1,transmettre_peigne); %le peigne convolué avec le filtre de mise en forme 


P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
M = 2; % ordre de la modulation
%Signal a bruit par bit souhaité a l entrée du récepteur Eb/N0
 dB = 0;
Eb_N0 = 10^(dB/10);
 % dB =10^(Eb/10*N0)
 % Eb/N0 = 10*log10(dB)

P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x));

x_bruite = x + bruit;

% filtre de reception 
x_choisi = x;
x_chaine2_sortie = filter(ones(1,Ns/2),1,x_choisi);

% Echantillonnage aux instants optimaux



n0 = 8;
echantillon_chaine2 = x_chaine2_sortie(n0:Ns:end); % de la forme [8 8 8 -8 -8 8 ...]

temps = ((n0*Te)-Te:(Ns*Te):length(echantillon_chaine2)*(Ns*Te)-Te);

echelle = (0:Te:taille*Ts-Te);
figure(19);
stem(temps,echantillon_chaine2,'o')
hold on 
plot(echelle, x_chaine2_sortie,'r');
xlabel("Temps en seconde")
ylabel("Signal")
legend('Signal échantillonné','Signal');
title("Échantillonnage du Signal ( chaine 2 ) ")
%exportgraphics(gcf,'./images/4-1-tempchaine2.png','Resolution',400);

% Diagramme de l'oeil
% 
figure(20); 
plot(reshape(x_chaine2_sortie,Ns,length(x_chaine2_sortie)/Ns))
ylabel("Diagramme de l'oeil");
title("chaine2");
% %exportgraphics(gcf,'./images/4-1-diagoeilchaine2.png','Resolution',400);

%Signal en sortie 
bits_sortie_chaine2 = echantillon_chaine2 > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux_chaine2 = sum(bits_sortie_chaine2 ~= transmettre );
taux_err_binaire_chaine2 = nb_bits_faux_chaine2/length(transmettre); 

%Tracé des différents diagrammes de l'oeil
        db = 5;
    Eb_N0 = 10^(db/10);
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine2_sortie = filter(porte,1,x_choisi);
    figure(36); 
    subplot(221);
    plot(reshape(x_chaine2_sortie,Ns,length(x_chaine2_sortie)/Ns));
    ylabel("Rapport = 5 dB")
    title("Chaine2")
        db = 8;
    Eb_N0 = 10^(db/10);
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine2_sortie = filter(porte,1,x_choisi);
    figure(36); 
    subplot(222);
    plot(reshape(x_chaine2_sortie,Ns,length(x_chaine2_sortie)/Ns))
    ylabel("Rapport = 8 dB")
    title("Chaine2")
        db = 10;
    Eb_N0 = 10^(db/10);
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine2_sortie = filter(porte,1,x_choisi);
    figure(36); 
    subplot(223);
    plot(reshape(x_chaine2_sortie,Ns,length(x_chaine2_sortie)/Ns));
    ylabel("Rapport = 10 dB")
    title("Chaine2")
        db = 12;
    Eb_N0 = 10^(db/10);
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine2_sortie = filter(porte,1,x_choisi);
    figure(36); 
    subplot(224);
    plot(reshape(x_chaine2_sortie,Ns,length(x_chaine2_sortie)/Ns))
    %ylabel("\frac{E_b}{N_O}=8 dB")
    ylabel("Rapport = 12 dB")
    title("Chaine2")
%exportgraphics(gcf,'./images/4-1-valrapportchaine2.png','Resolution',400);

% TEB pratique et theorique 
TEB_pratique_2 = [];
TEB_theorique_2 = [];

for db = 0:0.5:8
    Eb_N0 = 10^(db/10);
    P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
% passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine2_sortie = filter(ones(1,Ns/2),1,x_choisi);
% Echantiollonage
    n0 = 8;
    echantillon_chaine2 = x_chaine2_sortie(n0:Ns:end);
    bits_sortie_chaine2 = echantillon_chaine2 > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

    %Calcul taux d'erreur binaire
    nb_bits_faux_chaine2 = sum(bits_sortie_chaine2 ~= transmettre );
    TEB_p = nb_bits_faux_chaine2/length(transmettre); 

    TEB_pratique_2 = [TEB_pratique_2 TEB_p];
    TEB_t =qfunc(sqrt(Eb_N0));
    TEB_theorique_2 = [TEB_theorique_2 TEB_t];

end 

figure(21)
semilogy(0:0.5:8,TEB_theorique_2);
hold on;
semilogy(0:0.5:8,TEB_pratique_2,'g');
hold off;
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique chaine 2")
%%exportgraphics(gcf,'./images/4-1-compatebchaine2.png','Resolution',400);




%% Bruit
Fe = 24000;%Hz
Te = 1/Fe; %seconde 

% nombre de symbole
M = 4;
nb_sym = log2(M);

% Débit binaire 

Rb = 3000; % bits/s

Tb = 1/Rb; 

% la durée symbole
Ts = nb_sym*Tb; 

% Facteur de suréchantillonage 
Ns = Ts/Te; % à déterminer dans chaque cas



%% Chaine 3
% - Mapping : 4 aire
% Symboles avec le mapping de Gray 
V_00 = -3; 
V_01 = -1;
V_10 = 3;
V_11 = 1;

transmettre_3 = reshape(transmettre,2,[])'; % groupe les bits 2 par 2 
index = bi2de(transmettre_3)';  % convertir ligne par ligne de binaire en décimal [ 3 2 2 1 1 0 0 0 1 3...]

lut = [-3, -1, 3, 1];  
transmettre_3 = lut(index +1); % en decimal en peut le voir comme des indices [ -3 -1 1 ...]

mat = zeros(1,Ns); 
mat(1)= 1; 
transmettre_3 = kron(transmettre_3, mat ); % une forme de peigne [-3 0 0 0 0 -1 0 0 0 0 1 0 0 ...]

porte = ones(1,Ns);
x = filter(porte,1,transmettre_3); % la convolution avec la porte


echelle = [0 : Te : ((taille*Ts))-Te]; 

P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence

%Signal a bruit par bit souhaité a l entrée du récepteur Eb/N0
 dB = 0;
Eb_N0 = 10^(dB/10);
 % dB =10^(Eb/10*N0)
 % Eb/N0 = 10*log10(dB)

P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x));

x_bruite = x + bruit;

% filtre de reception 
x_choisi = x;
x_chaine3_sortie = filter(ones(1,Ns),1,x_choisi);

% Echantillonnage aux instants optimaux

n0 = 16;
echantillon_chaine3 = x_chaine3_sortie(n0:Ns:end); 

temps = ((n0*2*Te)-2*Te:(Ns*2*Te):length(echantillon_chaine3)*(Ns*2*Te)-2*Te);

echelle = (0:2*Te:taille*Ts-Te);

figure(23);
stem(temps,echantillon_chaine3,'o')
hold on 
plot(echelle, x_chaine3_sortie,'r');
xlabel("Temps en seconde")
ylabel("Signal")
legend('Signal échantillonné','Signal');
title("Échantillonnage du Signal ( chaine 3 ) ")
%exportgraphics(gcf,'./images/4-1-tempchaine3.png','Resolution',400);

% Diagramme de l'oeil

figure(24); 
plot(reshape(x_chaine3_sortie,Ns,length(x_chaine3_sortie)/Ns))
ylabel("Diagramme de l'oeil");
title("chaine3");
%%exportgraphics(gcf,'./images/4-1-diagoeilchaine3.png','Resolution',400);

%% 
 dB = 5;
Eb_N0 = 10^(dB/10);

P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x));

x_bruite = x + bruit;

% filtre de reception 
x_choisi = x_bruite;
x_chaine3_sortie = filter(ones(1,Ns),1,x_choisi);

% Echantillonnage aux instants optimaux
n0 = 16;
echantillon_chaine3bis = x_chaine3_sortie(n0:Ns:end); 

temps = ((n0*2*Te)-2*Te:(Ns*2*Te):length(echantillon_chaine3bis)*(Ns*2*Te)-2*Te);

echelle = (0:2*Te:taille*Ts-Te);

% Diagramme de l'oeil

figure(45); 
title("Chaine3")
subplot(221)
plot(reshape(x_chaine3_sortie,Ns,length(x_chaine3_sortie)/Ns))
ylabel("Rapport = 5 dB")
%%
dB = 8;
Eb_N0 = 10^(dB/10);

P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x));

x_bruite = x + bruit;

% filtre de reception 
x_choisi = x_bruite;
x_chaine3_sortie = filter(ones(1,Ns),1,x_choisi);

% Echantillonnage aux instants optimaux
n0 = 16;
echantillon_chaine3bis = x_chaine3_sortie(n0:Ns:end); 

temps = ((n0*2*Te)-2*Te:(Ns*2*Te):length(echantillon_chaine3bis)*(Ns*2*Te)-2*Te);

echelle = (0:2*Te:taille*Ts-Te);

% Diagramme de l'oeil

figure(45);
title("Chaine3")
subplot(222)
plot(reshape(x_chaine3_sortie,Ns,length(x_chaine3_sortie)/Ns))
ylabel("Rapport = 8 dB")
%%
dB = 10;
Eb_N0 = 10^(dB/10);

P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x));

x_bruite = x + bruit;

% filtre de reception 
x_choisi = x_bruite;
x_chaine3_sortie = filter(ones(1,Ns),1,x_choisi);

% Echantillonnage aux instants optimaux
n0 = 16;
echantillon_chaine3bis = x_chaine3_sortie(n0:Ns:end); 

temps = ((n0*2*Te)-2*Te:(Ns*2*Te):length(echantillon_chaine3bis)*(Ns*2*Te)-2*Te);

echelle = (0:2*Te:taille*Ts-Te);

% Diagramme de l'oeil

figure(45);
title("Chaine3")
subplot(223)
plot(reshape(x_chaine3_sortie,Ns,length(x_chaine3_sortie)/Ns))
ylabel("Rapport = 10 dB")
%%
dB = 12;
Eb_N0 = 10^(dB/10);

P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x));

x_bruite = x + bruit;

% filtre de reception 
x_choisi = x_bruite;
x_chaine3_sortie = filter(ones(1,Ns),1,x_choisi);

% Echantillonnage aux instants optimaux
n0 = 16;
echantillon_signal = x_chaine3_sortie(n0:Ns:end); 

%


%%
% Démapping

%Signal en sortie 
V_00 = -3; 
V_01 = -1;
V_10 = 1;
V_11 = 3;



sortie_dec = echantillon_signal';
%Décision

%trouver les 00
ind_00 = echantillon_chaine3' < -32;
sortie_dec(ind_00) = -3;

% trouver les 01
ind_01 = (echantillon_chaine3' >= -32)&(echantillon_chaine3' <0);

sortie_dec(ind_01) = -1;


% trouver les 10
ind_10 = (echantillon_chaine3' > 32);
sortie_dec(ind_10) = 1;


% trouver les 11
ind_11 =  (echantillon_chaine3' >= 0)&(echantillon_chaine3' < 32);

sortie_dec(ind_11) = 3;



% mettre sous la forme [0 1 2 3 1 2 1 1 3 ...]'
sortie_dec = (sortie_dec + 3);
sortie_dec = sortie_dec/2;


% transformation en binaire
sortie_dec_bi = de2bi(sortie_dec);

% mettre sous la forme [ 1 0 1 1 1 0 ...]
sortie_dec_bi = sortie_dec_bi';
bits_sortie_chaine3 = reshape(sortie_dec_bi,1,[]);

%Calcul taux d'erreur binaire
nb_bits_faux_chaine3 = sum(bits_sortie_chaine3 ~= transmettre );
taux_err_binaire_chaine3 = nb_bits_faux_chaine3/length(transmettre); 




% TEB pratique et theorique 
TEB_pratique_3 = [];
TEB_theorique_3 = [];

for db = 0:0.5:8
    Eb_N0 = 10^(db/10);
    P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
% passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine3_sortie = filter(ones(1,Ns),1,x_choisi);
% Echantiollonage
    n0 = 16;
    echantillon_chaine3 = x_chaine3_sortie(n0:Ns:end);
    % trouver les 01
sortie_dec = echantillon_chaine3';
%Décision

%trouver les 00
ind_00 = echantillon_chaine3' < -32;
sortie_dec(ind_00) = -3;

% trouver les 01
ind_01 = (echantillon_chaine3' >= -32)&(echantillon_chaine3' <0);

sortie_dec(ind_01) = -1;


% trouver les 10
ind_10 = (echantillon_chaine3' > 32);
sortie_dec(ind_10) = 1;


% trouver les 11
ind_11 =  (echantillon_chaine3' >= 0)&(echantillon_chaine3' < 32);

sortie_dec(ind_11) = 3;

% mettre sous la forme [0 1 2 3 1 2 1 1 3 ...]'
sortie_dec = (sortie_dec + 3);
sortie_dec = sortie_dec/2;
% transformation en binaire
sortie_dec_bi = de2bi(sortie_dec);

% mettre sous la forme [ 1 0 1 1 1 0 ...]
sortie_dec_bi = sortie_dec_bi';
bits_sortie_chaine3 = reshape(sortie_dec_bi,1,[]);



    %Calcul taux d'erreur binaire
    nb_bits_faux_chaine3 = sum(bits_sortie_chaine3 ~= transmettre );
    TEB_p = nb_bits_faux_chaine3/length(transmettre); 

    TES = 2*((M-1)/M) * qfunc(sqrt(Eb_N0*6*log2(M)/(M^2 -1)));
    TEB_pratique_3 = [TEB_pratique_3 TEB_p];
    TEB_t = TES/log2(M);
    TEB_theorique_3 = [TEB_theorique_3 TEB_t];

end 

figure(25)
semilogy(0:0.5:8,TEB_theorique_3);
hold on;
semilogy(0:0.5:8,TEB_pratique_3,'g');
hold off;
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique chaine3")
%exportgraphics(gcf,'./images/4-1-compatebchaine3.png','Resolution',400);

%% Tracé des TEBs obtenu par simulation pour les chaines de transmission 1 et 2.


figure(26)
semilogy(0:0.5:8,TEB_pratique_1);
hold on;
semilogy(0:0.5:8,TEB_pratique_2,'g');
semilogy(0:0.5:8,TEB_theorique_2,'g');
semilogy(0:0.5:8,TEB_theorique_1);
hold off;
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_1","TEB_2")
title("Comparaison des TEB de la chaine 1 et 2")
%exportgraphics(gcf,'./images/4-1-compatebchaine12.png','Resolution',400);


%% Tracé des TEBs obtenu par simulation pour les chaines de transmission 1 et 3.

figure(27)
semilogy(0:0.5:8,TEB_pratique_1);
hold on;
semilogy(0:0.5:8,TEB_pratique_3,'g');
semilogy(0:0.5:8,TEB_theorique_3,'g');
semilogy(0:0.5:8,TEB_theorique_1);
hold off;
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_1","TEB_3")
title("Comparaison des TEB de la chaine 1 et 3")
%exportgraphics(gcf,'./images/4-1-compatebchaine13.png','Resolution',400);

