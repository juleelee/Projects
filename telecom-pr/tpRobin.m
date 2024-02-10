close all;
clear;
clc;
%% PARTIE 1 
%% Données 
% Frequence/Période d'echantillonage

Fe = 24000;%Hz
Te = 1/Fe; %seconde 

% Débit binaire 

Rb = 3000 % bits/s

Tb = 1/Rb; 

% Facteur de suréchantillonage 
Ns = Tb/Te % à déterminer dans chaque cas

% la durée symbole
Ts = Ns*Te; 


% Signal a transmettre 
taille = 20; % le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille) %génére aléatoirement taille bits



%% Les modulateurs 

%% Modulateteur 1 
% - Mapping : symboles binaires a moyenne nulle.
transmettre_1 = 2*transmettre; 
transmettre_1 = transmettre_1 -1; % les 1 deviennent des 1 et les 0 deviennent des -1 
mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 

transmettre_peigne = kron(transmettre_1, mat ); % une forme de peigne 

echelle = [0 : Te : (taille*Ts)-Te]; 

figure(1)
subplot(3,1,1);
stem(echelle,transmettre_peigne);grid on;
xlabel("Temps en secondes");
ylabel("e(t)");
title("Signal en entrée en peigne");

V_0 = -1; % symbole du bit 0 
V_1 = 1; %symbole du bit 1 

% – Filtre de mise en forme rectangulaire de hauteur 1 et de duree  egale a la duree symbole.
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
entree_conv = filter(porte,1,transmettre_peigne); %le peigne convolué avec le filtre de mise en forme 

% Tracé
figure(1); 
subplot(3,1,2);
stem(echelle,entree_conv);grid on;
xlabel("Temps en secondes");
ylabel("s(t)");
title("Signal en sortie avec le modulateur 1");
exportgraphics(gcf,'./images/temp_mod1.png','Resolution',400);



%% Modulateur 2 
% - Mapping : symboles 4-aires `a moyenne nulle.
% – Filtre de mise en forme : rectangulaire de hauteur 1 et de duree egale a la duree symbole.

% Symboles avec le mapping de Gray 
V_00 = -3; 
V_01 = -1;
V_10 = 3;
V_11 = 1;

transmettre_2 = reshape(transmettre,2,[])'; % groupe les bits 2 par 2 
index = bi2de(transmettre_2)';  % convertir ligne par ligne de binaire en décimal [ 3 2 2 1 1 0 0 0 1 3...]

lut = [-3, -1, 3, 1];  
transmettre_2 = lut(index +1); % en decimal en peut le voir comme des indices [ -3 -1 1 ...]

mat = zeros(1,2*Ns); 
mat(1)= 1; 
transmettre_2 = kron(transmettre_2, mat ); % une forme de peigne [-3 0 0 0 0 -1 0 0 0 0 1 0 0 ...]

porte = ones(1,2*Ns);
transmettre_2_conv = filter(porte,1,transmettre_2); % la convolution avec la porte


echelle = [0 : Te : ((taille*Ts))-Te]; 

figure(2)
subplot(3,1,1);
stem(echelle,transmettre_2);grid on;
xlabel("Temps en secondes");
ylabel("e(t)");
title("Signal en entrée en peigne");

% Tracé
figure(2); 
subplot(3,1,2)
stem(echelle,transmettre_2_conv);grid on;
xlabel("Temps en secondes");
ylabel("s(t)");
title("Signal en sortie avec le modulateur 2");
exportgraphics(gcf,'./images/temp_mod2.png','Resolution',400);


%% Modulateur 3 
% – Mapping : symboles binaires a moyenne nulle.
% - – Filtre de mise en forme : 

%Signal a transmettre sous forme de peigne 
transmettre_3 = 2*transmettre; 
transmettre_3 = transmettre_3 -1; % les 1 deviennent des 1 et les 0 deviennent des -1 
mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 
transmettre_3 = kron(transmettre_3, mat );

figure(3)
subplot(3,1,1);
stem(echelle,transmettre_3);grid on;
xlabel("Temps en secondes");
ylabel("e(t)");
title("Signal en entrée en peigne");

%une reponse impulsionnelle de longueur
L = 8; % optimisé a 8
N = L*Ns + 1;

% roll of entre 0 et 1 
alpha = 1;

%racine de cosinus sur eleve
h = rcosdesign(alpha,L,Ns);


transmettre_3_conv = filter(h,1,transmettre_3); %le peigne convolué avec le filtre de mise en forme 


% Tracé
figure(3); 
subplot(3,1,2);
stem(echelle,transmettre_3_conv);grid on;
xlabel("Temps en secondes");
ylabel("s(t)");
title("Signal en sortie avec le modulateur 3");
exportgraphics(gcf,'./images/temp_mod3.png','Resolution',400);


%% Trace des DSP estimé et théorique 
% modulateur 1 
figure(1); 
subplot(3,1,3)
DSP_1=pwelch(entree_conv,[],[],[],Fe,'twosided'); % DSP pratique 
f = linspace(-Fe/2,Fe/2, length(DSP_1));
S_x1 = (Ts)*sinc(f*Ts).^2;  % DSP  théorique 
plot(f,10*log10(fftshift(DSP_1)),"g"); grid on 
hold on;
plot(f,10*log10(S_x1),"r");
xlabel("Fréquence en Hz")
ylabel("Densité spectrale de puissance (dB)")
legend('Densité de puissance estimée','Densitee de puissance théorique');
title("Comparaison de la DSP estimée et théorique du modulateur 1");
exportgraphics(gcf,'./images/DSP_mod1.png','Resolution',400);



%modulateur 2 
figure(2); 
subplot(3,1,3)
DSP_2=pwelch(transmettre_2_conv,[],[],[],Fe,'twosided'); % DSP pratique 
f = linspace(-Fe/2,Fe/2, length(DSP_2));
S_x2 = 20*(Ts)*sinc(2*f*Ts).^2; % DSP théorique 

plot(f,10*log10(fftshift(DSP_2)),"g"); grid on
hold on;
plot(f,10*log10(S_x2));
xlabel("Fréquence en Hz")
ylabel("Densité spectrale de puissance (dB)")
legend('Densité de puissance estimée','Densitee de puissance théorique');
title("Comparaison de la DSP estimée et théorique du modulateur 2");
exportgraphics(gcf,'./images/DSP_mod2.png','Resolution',400);

%modulateur 3 
figure(3); 
subplot(3,1,3)
DSP_3=pwelch(transmettre_3_conv,[],[],[],Fe,'twosided'); % DSP pratique 
f = linspace(-Fe/2,Fe/2, length(DSP_3));
indice = 1; 
S_x3 = zeros(1,length(DSP_3));
for i = linspace(-Fe/2,Fe/2, length(DSP_3))
    
    if abs(i) <= (1-alpha)/(2*Ts)
        S_x3(indice) = 1;
    elseif  (abs(i) <= (1+alpha)/(2*Ts) )&&((1-alpha)/(2*Ts) <= abs(i))
        S_x3(indice) = (1 + cos((pi*Ts/alpha)*(abs(i)-((1-alpha)/(2*Ts)))));
    else

        S_x3(indice) = 0;
    end 
    indice = indice +1;
end 


plot(f,10*log10(fftshift(DSP_3/max(DSP_3))),"g"); grid on 
hold on;
plot(f,10*log10(S_x3));
xlabel("Fréquence en Hz")
ylabel("Densité spectrale de puissance (dB)")
legend('Densité de puissance estimée','Densitee de puissance théorique');
title("Comparaison de la DSP estimée et théorique du modulateur 3");
exportgraphics(gcf,'./images/DSP_mod3.png','Resolution',400);

%% Étude des interférences entre symbole et du critère de Nyquist

%Fréquence d'échantillonnage
Fe = 24000; %Hz
Te = 1/Fe;

% Débit binaire 
Rb = 3000; % bits/s
Tb = 1/Rb;

% Facteur de surechantillonnage
Ns = Tb/Te;

% 3.1 
% Bloc modulateur/démodulateur 
% Signal a transmettre 
taille = 20; % le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille) %génére aléatoirement taille bits
transmettre_1 = 2*transmettre; 
transmettre_1 = transmettre_1 -1; % les 1 deviennent des 1 et les 0 deviennent des -1 
mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 

transmettre_kron = kron(transmettre_1, mat ); % une forme de peigne 

echelle = [0 : Te : (taille*Ts)-Te]; 

% – Filtre de mise en forme rectangulaire de hauteur 1 et de duree  egale a la duree symbole.
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
conv_emission = filter(porte,1,transmettre_kron);  % Con
conv_reception_Signal = filter(porte,1,conv_emission);

% Tracé
figure(4); 
plot(echelle,conv_reception_Signal);grid on;
xlabel("Temps en secondes");
ylabel("s(t)");
title("Signal en sortie");
exportgraphics(gcf,'./images/3-1-sortie-filtre.png','Resolution',400);

% Le tracé de la Réponse impulsionnelle globale de la chaine de transmission, g
transmettre_impul = zeros(1,Ns);
transmettre_impul(floor(Ns/2)) = 1; % On envoie une impulsion

mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 

transmettre_kron = kron(transmettre_impul, mat ); % une forme de peigne 

echelle = [0 : Te : (Ns*Ts)-Te]; 
Signal_dirac = zeros(1,Ns);
Signal_dirac(1) = 1;
% – Filtre de mise en forme rectangulaire de hauteur 1 et de duree  egale a la duree symbole.
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
g_impul = conv(porte,porte);

% Tracé
figure(5);
subplot(2,1,1)
stem(conv(porte,porte));grid on;
xlabel("points");

N = 1000;
porte = ones(1,N); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
g_impul = conv(porte,porte)/N;
subplot(2,1,2)
stem(0 : Te : (length(g_impul)*Te - Te),g_impul);grid on;
xlabel("temps en seconde");
ylabel("g(t)");
title("Réponse impulsionnelle de g");
exportgraphics(gcf,'./images/3-1-rep-impulsionnelle.png','Resolution',400);


% échantillonage du Signal. n0 + mNs
n0 = 8;
echantillon = conv_reception_Signal(n0:Ns:end); % de la forme [8 8 8 -8 -8 8 ...]
temps = ((n0*Te)-Te:(Ns*Te):length(echantillon)*(Ns*Te)-Te);

echelle = (0:Te:taille*Ts-Te);
figure(6);
stem(temps,echantillon,'o')
hold on 
plot(echelle, conv_reception_Signal,'r');
xlabel("Temps en seconde")
ylabel("Signal")
legend('Signal échantillonné','Signal');
title("Échantillonnage du Signal")
exportgraphics(gcf,'./images/3-1-echantillonage.png','Resolution',400);

%Signal en sortie 
bits_sortie = echantillon > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 


% Diagramme de l'oeil
figure(8);
plot(reshape(conv_reception_Signal,Ns,length(conv_reception_Signal)/Ns))
ylabel("Diagramme de l'oeil");

%Calcul taux d'erreur binaire
nb_bits_faux = sum(bits_sortie ~= transmettre );
taux_err_binaire = nb_bits_faux/length(transmettre); % pour n0 = 8, le taux est nulle

%% 3.2

BW1 = 8000 %Hz
BW2 = 1000 %Hz

% BW1 = 8000 %Hz
% passe bas : canal
ordre = 61;
t = (-(ordre-1)/2:(ordre-1)/2)*Te;
Fc = BW1;
passe_bas = 2*Fc/Fe*sinc(2*Fc*t);


figure(9)
N = numel(passe_bas);
pb_freq = fft(passe_bas, N); 
Hc = abs(fftshift(pb_freq));
plot(linspace(-Fc,Fc,length(pb_freq)),Hc) % la Réponse en frequence du filtre canal.
xlabel("Fréquence en Hz")
ylabel("pbfreq(f)");
title("Passe-bas en fréquence");
exportgraphics(gcf,'./images/3-2-passe-basBW1.png','Resolution',400);


% Signal a transmettre 
taille = 20; % le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille) %génére aléatoirement taille bits


% Prise en compte du retard

transmettre_1 = 2*transmettre; 
transmettre_1 = transmettre_1 -1; % les 1 deviennent des 1 et les 0 deviennent des -1 
mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 

transmettre_kron = kron(transmettre_1, mat ); % une forme de peigne 

echelle = [0 : Te : (taille*Ts)-Te]; 


% – Filtre de mise en forme rectangulaire de hauteur 1 et de duree  egale a la duree symbole.
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
conv_emission = filter(porte,1,transmettre_kron);  
canal = filter(passe_bas,1,[conv_emission zeros(1, (ordre-1)/2)]);
conv_reception_Signal = filter(porte,1,canal((ordre+1)/2:end));
 
% Tracé de la Réponse impulsionnelle
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
%conv_emission = filter(porte,1,porte);  
canal = filter(passe_bas,1,[conv_emission zeros(1, (ordre-1)/2)]);
conv_reception_Signal = filter(porte,1,canal((ordre+1)/2:end));

% Tracé
figure(10); 
plot(echelle,conv_reception_Signal);grid on;
xlabel("Temps en secondes");
ylabel("s(t)");
title("Signal en sortie");
exportgraphics(gcf,'./images/3-2-sortieBW1.png','Resolution',400);

% Diagramme de l'oeil
figure(11);
plot(reshape(conv_reception_Signal,Ns,length(conv_reception_Signal)/Ns))
title("Diagramme de l'oeil");
exportgraphics(gcf,'./images/3-2-diagramme-oeilBW1.png','Resolution',400);


% H(f)
f = linspace(-Fc,Fc,length(pb_freq));
H = (Ts)*sinc(Ts*f);
H_abs =  abs(fftshift(H));
figure(12)
plot(linspace(-Fc,Fc,length(pb_freq)),H_abs) % la Réponse en frequence du filtre canal.
xlabel("Fréquence en Hz")
ylabel("|H(f)|");
title("Passe-bas en fréquence");
exportgraphics(gcf,'./images/3-2-passe-basH(f)BW1.png','Resolution',400);

% H(f)Hr(f)


HHr_abs = abs(H).^2;
figure(13)
semilogy(linspace(-Fc,Fc,length(HHr_abs)),HHr_abs/max(HHr_abs)) 
hold on
semilogy(linspace(-Fc,Fc,length(Hc)),Hc/max(Hc),'r') % la Réponse en frequence du filtre canal.
legend('|H(f)Hr(f)|','|Hc(f)|' )
xlabel("Fréquence en Hz")
ylabel("Réponse fréquentielle");
title("Superposition des DSP de |H(f)Hr(f)| et |Hc(f)| pour BW1");
exportgraphics(gcf,'./images/3-2-courbes-superposeesBW1.png','Resolution',400);
hold off

% Tracé de la reponse impulsionnelle
figure(14)
stem(conv(porte,conv(porte,passe_bas)));
xlabel("Fréquence en Hz")
ylabel("Réponse impulsionnelle");
title("Réponse impulsionnelle de la chaîne 1 pour BW2");
exportgraphics(gcf,'./images/3-2-repimpulBW1.png','Resolution',400);

% TEB
n0 = 8;
echantillon = conv_reception_Signal(n0:Ns:end); % de la forme [8 8 8 -8 -8 8 ...]
temps = ((n0*Te)-Te:(Ns*Te):length(echantillon)*(Ns*Te)-Te);

echelle = (0:Te:taille*Ts-Te);
figure(15);
stem(temps,echantillon,'o')
hold on 
plot(echelle, conv_reception_Signal,'r');
xlabel("Temps en seconde")
ylabel("Signal")
legend('Signal échantillonné','Signal');
title("Échantillonnage du Signal pour BW1")
exportgraphics(gcf,'./images/3-2-echantillonageBW1.png','Resolution',400);
hold off

%Signal en sortie 
bits_sortie = echantillon > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux = sum(bits_sortie ~= transmettre );
taux_err_binaire = nb_bits_faux/length(transmettre); 

%BW2 = 1000 Hz
% passe bas : canal
ordre = 61;
t = (-(ordre-1)/2:(ordre-1)/2)*Te;
Fc = BW2;
passe_bas = 2*Fc/Fe*sinc(2*Fc*t);


figure(9)
N = numel(passe_bas);
pb_freq = fft(passe_bas, N); 
Hc = abs(fftshift(pb_freq));
plot(linspace(-Fc,Fc,length(pb_freq)),Hc) % la Réponse en frequence du filtre canal.
xlabel("Fréquence en Hz")
ylabel("pbfreq(f)");
title("Passe-bas en fréquence");
exportgraphics(gcf,'./images/3-2-passe-basBW2.png','Resolution',400);


% Signal a transmettre 
taille = 20; % le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille) %génére aléatoirement taille bits


% Prise en compte du retard

transmettre_1 = 2*transmettre; 
transmettre_1 = transmettre_1 -1; % les 1 deviennent des 1 et les 0 deviennent des -1 
mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 

transmettre_kron = kron(transmettre_1, mat ); % une forme de peigne 

echelle = [0 : Te : (taille*Ts)-Te]; 


% – Filtre de mise en forme rectangulaire de hauteur 1 et de duree  egale a la duree symbole.
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
conv_emission = filter(porte,1,transmettre_kron);  
canal = filter(passe_bas,1,[conv_emission zeros(1, (ordre-1)/2)]);
conv_reception_Signal = filter(porte,1,canal((ordre+1)/2:end));
 
% Tracé de la Réponse impulsionnelle
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
%conv_emission = filter(porte,1,porte);  
canal = filter(passe_bas,1,[conv_emission zeros(1, (ordre-1)/2)]);
conv_reception_Signal = filter(porte,1,canal((ordre+1)/2:end));

% Tracé
figure(10); 
plot(echelle,conv_reception_Signal);grid on;
xlabel("Temps en secondes");
ylabel("s(t)");
title("Signal en sortie");
exportgraphics(gcf,'./images/3-2-sortieBW2.png','Resolution',400);

% Diagramme de l'oeil
figure(11);
plot(reshape(conv_reception_Signal,Ns,length(conv_reception_Signal)/Ns))
title("Diagramme de l'oeil");
exportgraphics(gcf,'./images/3-2-diagramme-oeilBW2.png','Resolution',400);


% H(f)
f = linspace(-Fc,Fc,length(pb_freq));
H = (Ts)*sinc(Ts*f);
H_abs =  abs(fftshift(H));
figure(12)
plot(linspace(-Fc,Fc,length(pb_freq)),H_abs) % la Réponse en frequence du filtre canal.
xlabel("Fréquence en Hz")
ylabel("|H(f)|");
title("Passe-bas en fréquence");
exportgraphics(gcf,'./images/3-2-passe-basH(f)BW2.png','Resolution',400);

% H(f)Hr(f)


HHr_abs = abs(H).^2;
figure(13)
semilogy(linspace(-Fc,Fc,length(HHr_abs)),HHr_abs/max(HHr_abs)) 
hold on
semilogy(linspace(-Fc,Fc,length(Hc)),Hc/max(Hc),'r') % la Réponse en frequence du filtre canal.
legend('|H(f)Hr(f)|','|Hc(f)|' )
xlabel("Fréquence en Hz")
ylabel("Réponse fréquentielle");
title("Superposition des DSP de |H(f)Hr(f)| et |Hc(f)| pour BW2");
exportgraphics(gcf,'./images/3-2-courbes-superposeesBW2.png','Resolution',400);
hold off

% Tracé de la reponse impulsionnelle
figure(14)
stem(conv(porte,conv(porte,passe_bas)));
xlabel("Fréquence en Hz")
ylabel("Réponse impulsionnelle");
title("Réponse impulsionnelle de la chaîne 1 pour BW2");
exportgraphics(gcf,'./images/3-2-repimpulBW2.png','Resolution',400);

% TEB
n0 = 8;
echantillon = conv_reception_Signal(n0:Ns:end); % de la forme [8 8 8 -8 -8 8 ...]
temps = ((n0*Te)-Te:(Ns*Te):length(echantillon)*(Ns*Te)-Te);

echelle = (0:Te:taille*Ts-Te);
figure(15);
stem(temps,echantillon,'o')
hold on 
plot(echelle, conv_reception_Signal,'r');
xlabel("Temps en seconde")
ylabel("Signal")
legend('Signal échantillonné','Signal');
title("Échantillonnage du Signal pour BW2")
exportgraphics(gcf,'./images/3-2-echantillonageBW2.png','Resolution',400);
hold off

%Signal en sortie 
bits_sortie = echantillon > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux = sum(bits_sortie ~= transmettre );
taux_err_binaire = nb_bits_faux/length(transmettre); 


%% Bruit
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
taille = 20; % le nombre de bits qu'on ve transmettre
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
title("Échantillonnage du Signal")
exportgraphics(gcf,'./images/4-1-tempchaine1.png','Resolution',400);

% Diagramme de l'oeil

figure(17); 
plot(reshape(x_chaine1_sortie,Ns,length(x_chaine1_sortie)/Ns))
ylabel("Diagramme de l'oeil");
exportgraphics(gcf,'./images/4-1-diagoeilchaine1.png','Resolution',400);

%Signal en sortie 
bits_sortie_chaine1 = echantillon_chaine1 > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux_chaine1 = sum(bits_sortie_chaine1 ~= transmettre );
taux_err_binaire_chaine1 = nb_bits_faux_chaine1/length(transmettre); 


% TEB pratique et theorique 
TEB_pratique = [];
TEB_theorique = [];

% %Tracé des différents diagrammes de l'oeil
%         db = 0;
%     Eb_N0 = 10^(db/10);
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
%     % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine1_sortie = filter(porte,1,x_choisi);
%     figure(35); 
%     subplot(221);
%     plot(reshape(x_chaine1_sortie,Ns,length(x_chaine1_sortie)/Ns));
%     ylabel("Rapport = 0 dB")
%         db = 2;
%     Eb_N0 = 10^(db/10);
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
%     % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine1_sortie = filter(porte,1,x_choisi);
%     figure(35); 
%     subplot(222);
%     plot(reshape(x_chaine1_sortie,Ns,length(x_chaine1_sortie)/Ns))
%     ylabel("Rapport = 2 dB")
%         db = 5;
%     Eb_N0 = 10^(db/10);
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
%     % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine1_sortie = filter(porte,1,x_choisi);
%     figure(35); 
%     subplot(223);
%     plot(reshape(x_chaine1_sortie,Ns,length(x_chaine1_sortie)/Ns));
%     ylabel("Rapport = 5 dB")
%         db = 8;
%     Eb_N0 = 10^(db/10);
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
%     % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine1_sortie = filter(porte,1,x_choisi);
%     figure(35); 
%     subplot(224);
%     plot(reshape(x_chaine1_sortie,Ns,length(x_chaine1_sortie)/Ns))
%     %ylabel("\frac{E_b}{N_O}=8 dB")
%     ylabel("Rapport = 8 dB")
% 
%     exportgraphics(gcf,'./images/4-1-valrapportchaine1.png','Resolution',400);


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

    TEB_pratique = [TEB_pratique TEB_p];
    TEB_t =qfunc(sqrt(2*Eb_N0));
    TEB_theorique = [TEB_theorique TEB_t];

end 

figure(18)
semilogy(0:0.5:8,TEB_theorique);
hold on;
semilogy(0:0.5:8,TEB_pratique,'g');
hold off;
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique")
%exportgraphics(gcf,'./images/4-1-compatebchaine1.png','Resolution',400);


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
title("Échantillonnage du Signal")
exportgraphics(gcf,'./images/4-1-tempchaine2.png','Resolution',400);

% Diagramme de l'oeil

figure(20); 
plot(reshape(x_chaine2_sortie,Ns,length(x_chaine2_sortie)/Ns))
ylabel("Diagramme de l'oeil");
exportgraphics(gcf,'./images/4-1-diagoeilchaine2.png','Resolution',400);

%Signal en sortie 
bits_sortie_chaine2 = echantillon_chaine2 > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux_chaine2 = sum(bits_sortie_chaine2 ~= transmettre );
taux_err_binaire_chaine2 = nb_bits_faux_chaine2/length(transmettre); 

%Tracé des différents diagrammes de l'oeil
%         db = 0;
%     Eb_N0 = 10^(db/10);
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
%     % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine2_sortie = filter(porte,1,x_choisi);
%     figure(36); 
%     subplot(221);
%     plot(reshape(x_chaine2_sortie,Ns,length(x_chaine2_sortie)/Ns));
%     ylabel("Rapport = 0 dB")
%         db = 2;
%     Eb_N0 = 10^(db/10);
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
%     % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine2_sortie = filter(porte,1,x_choisi);
%     figure(36); 
%     subplot(222);
%     plot(reshape(x_chaine2_sortie,Ns,length(x_chaine2_sortie)/Ns))
%     ylabel("Rapport = 2 dB")
%         db = 5;
%     Eb_N0 = 10^(db/10);
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
%     % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine2_sortie = filter(porte,1,x_choisi);
%     figure(36); 
%     subplot(223);
%     plot(reshape(x_chaine2_sortie,Ns,length(x_chaine2_sortie)/Ns));
%     ylabel("Rapport = 5 dB")
%         db = 8;
%     Eb_N0 = 10^(db/10);
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
%     % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine2_sortie = filter(porte,1,x_choisi);
%     figure(36); 
%     subplot(224);
%     plot(reshape(x_chaine2_sortie,Ns,length(x_chaine2_sortie)/Ns))
%     %ylabel("\frac{E_b}{N_O}=8 dB")
%     ylabel("Rapport = 8 dB")
% 
%     exportgraphics(gcf,'./images/4-1-valrapportchaine2.png','Resolution',400);

% TEB pratique et theorique 
TEB_pratique = [];
TEB_theorique = [];

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

    TEB_pratique = [TEB_pratique TEB_p];
    TEB_t =qfunc(sqrt(Eb_N0));
    TEB_theorique = [TEB_theorique TEB_t];

end 

figure(21)
semilogy(0:0.5:8,TEB_theorique);
hold on;
semilogy(0:0.5:8,TEB_pratique,'g');
hold off;
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique")
%exportgraphics(gcf,'./images/4-1-compatebchaine2.png','Resolution',400);




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

% Signal a transmettre 
taille = 20; % le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille); %génére aléatoirement taille bits
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
title("Échantillonnage du Signal")
exportgraphics(gcf,'./images/4-1-tempchaine3.png','Resolution',400);

% Diagramme de l'oeil

figure(24); 
plot(reshape(x_chaine3_sortie,Ns,length(x_chaine3_sortie)/Ns))
ylabel("Diagramme de l'oeil");
exportgraphics(gcf,'./images/4-1-diagoeilchaine3.png','Resolution',400);


% Démapping

%Signal en sortie 
V_00 = -3; 
V_01 = -1;
V_10 = 1;
V_11 = 3;


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
taux_err_binaire_chaine3 = nb_bits_faux_chaine3/length(transmettre); 

% TEB pratique et theorique 
TEB_pratique = [];
TEB_theorique = [];

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
    TEB_pratique = [TEB_pratique TEB_p];
    TEB_t = TES/log2(M);
    TEB_theorique = [TEB_theorique TEB_t];

end

figure(25)
semilogy(0:0.5:8,TEB_theorique);
hold on;
semilogy(0:0.5:8,TEB_pratique,'g');
hold off;
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique")
%exportgraphics(gcf,'./images/4-1-compatebchaine3.png','Resolution',400);

% %génération des diag de l'oeil
%         db = 0;
%     Eb_N0 = 10^(db/10);
%     P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
% % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine3_sortie = filter(ones(1,Ns),1,x_choisi);
%     figure(37); 
%     subplot(221);
%     plot(reshape(x_chaine3_sortie,Ns,length(x_chaine3_sortie)/Ns));
%     ylabel("Rapport = 0 dB")
% 
%         db = 2;
%     Eb_N0 = 10^(db/10);
%     P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
% % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine3_sortie = filter(ones(1,Ns),1,x_choisi);
%     figure(37); 
%     subplot(222);
%     plot(reshape(x_chaine3_sortie,Ns,length(x_chaine3_sortie)/Ns))
%     ylabel("Rapport = 2 dB")
% 
%         db = 5;
%     Eb_N0 = 10^(db/10);
%     P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
% % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine3_sortie = filter(ones(1,Ns),1,x_choisi);
%     figure(37); 
%     subplot(223);
%     plot(reshape(x_chaine3_sortie,Ns,length(x_chaine3_sortie)/Ns));
%     ylabel("Rapport = 5 dB")
%         db = 8;
%     Eb_N0 = 10^(db/10);
%     P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
%     P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
%     sigma = sqrt(P_b);
%     bruit = sigma*randn(1,length(x));
% % passage dans le canal
%     x_bruite = x + bruit;
%     x_choisi = x_bruite;
%     x_chaine3_sortie = filter(ones(1,Ns),1,x_choisi);
%     figure(37); 
%     subplot(224);
%     plot(reshape(x_chaine3_sortie,Ns,length(x_chaine3_sortie)/Ns))
%     %ylabel("\frac{E_b}{N_O}=8 dB")
%     ylabel("Rapport = 8 dB")
% 
%     exportgraphics(gcf,'./images/4-1-valrapportchaine3.png','Resolution',400);


%% Dernier graphe superposition
TEB_pratique1 = [];
TEB_theorique1 = [];

%Chaine 1 et 2


% Bruit
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

% Signal a transmettre 
taille = 20000; % le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille); %génére aléatoirement taille bits


%Chaine 1
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
%Signal a bruit par bit souhait
for db = 0:0.5:8
    Eb_N0 = 10^(db/10);
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x));
    
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;
    x_chaine1_sortie = filter(porte,1,x_choisi);
    
    % Echantillonage
    n0 = 8;
    echantillon_chaine1 = x_chaine1_sortie(n0:Ns:end);
    bits_sortie_chaine1 = echantillon_chaine1 > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 
    
    %Calcul taux d'erreur binaire
    nb_bits_faux_chaine1 = sum(bits_sortie_chaine1 ~= transmettre );
    TEB_p = nb_bits_faux_chaine1/length(transmettre); 

    TEB_pratique1 = [TEB_pratique1 TEB_p];
    TEB_t =qfunc(sqrt(2*Eb_N0));
    TEB_theorique1 = [TEB_theorique1 TEB_t];

end 

figure(40)
semilogy(0:0.5:8,TEB_theorique1);
hold on;
semilogy(0:0.5:8,TEB_pratique1,'g');
hold on 

TEB_pratique = [];
TEB_theorique = [];



%Chaine 2

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

    TEB_pratique = [TEB_pratique TEB_p];
    TEB_t = qfunc(sqrt(Eb_N0));
    TEB_theorique = [TEB_theorique TEB_t];

end 

semilogy(0:0.5:8,TEB_theorique);
hold on;
semilogy(0:0.5:8,TEB_pratique,'g');
hold off;
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique,chaîne 1}","TEB_{pratique,chaîne 1}","TEB_{theorique,chaîne 2}","TEB_{pratique,chaîne 2}")
title("Comparaison des TEB pratique et théorique")
exportgraphics(gcf,'./images/4-2-sup1et2.png','Resolution',400);



TEB_pratique = [];
TEB_theorique = [];




%Chaine 1 et 3


%Chaine 1
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

    TEB_pratique = [TEB_pratique TEB_p];
    TEB_t =qfunc(sqrt(2*Eb_N0));
    TEB_theorique = [TEB_theorique TEB_t];

end 

figure(40)
semilogy(0:0.5:8,TEB_theorique);
hold on;
semilogy(0:0.5:8,TEB_pratique,'g');
hold on 

TEB_pratique = [];
TEB_theorique = [];

%Chaine 3

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

% Signal a transmettre 
taille = 20; % le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille); %génére aléatoirement taille bits

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
    TEB_pratique = [TEB_pratique TEB_p];
    TEB_t = TES/log2(M);
    TEB_theorique = [TEB_theorique TEB_t];
end

semilogy(0:0.5:8,TEB_theorique);
hold on;
semilogy(0:0.5:8,TEB_pratique,'g');
hold off;
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique,chaîne 1}","TEB_{pratique,chaîne 1}","TEB_{theorique,chaîne 3}","TEB_{pratique,chaîne 3}")
title("Comparaison des TEB pratique et théorique")
exportgraphics(gcf,'./images/4-2-sup1et3.png','Resolution',400);






