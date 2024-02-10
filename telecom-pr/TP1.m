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
title("signal en entrée en peigne");

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
title("signal en sortie avec le modulateur 1");



%% Modulateur 2 
% - Mapping : symboles 4-aires `a moyenne nulle.
% – Filtre de mise en forme : rectangulaire de hauteur 1 et de duree egale a la duree symbole.

% Symboles avec le mapping de Gray 
V_00 = -3; 
V_01 = -1
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
title("signal en entrée en peigne");

% Tracé
figure(2); 
subplot(3,1,2)
stem(echelle,transmettre_2_conv);grid on;
xlabel("Temps en secondes");
ylabel("s(t)");
title("signal en sortie avec le modulateur 2");


%% Modulateur 3 
% – Mapping : symboles binaires a moyenne nulle.
% - – Filtre de mise en forme : 

%signal a transmettre sous forme de peigne 
transmettre_3 = 2*transmettre; 
transmettre_3 = transmettre_3 -1; % les 1 deviennent des 1 et les 0 deviennent des -1 
mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 
transmettre_3 = kron(transmettre_3, mat );

% figure(3)
subplot(3,1,1);
stem(echelle,transmettre_3);grid on;
xlabel("Temps en secondes");
ylabel("e(t)");
title("signal en entrée en peigne");

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
title("signal en sortie avec le modulateur 3");


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
title("Comparaison de la DSP estimée et théorique du modulateur 1")



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
title("Comparaison de la DSP estimée et théorique du modulateur 2")

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
title("Comparaison de la DSP estimée et théorique du modulateur 3")

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
conv_reception_signal = filter(porte,1,conv_emission);

% Tracé
figure(4); 
plot(echelle,conv_reception_signal);grid on;
xlabel("Temps en secondes");
ylabel("s(t)");
title("signal en sortie");

% Le tracé de la réponse impulsionnelle globale de la chaine de transmission, g
transmettre_impul = zeros(1,Ns);
transmettre_impul(floor(Ns/2)) = 1; % On envoie une impultion

mat = zeros(1,Ns); 
mat(1)= 1;  % [1 0 0 0 ...] 

transmettre_kron = kron(transmettre_impul, mat ); % une forme de peigne 

echelle = [0 : Te : (Ns*Ts)-Te]; 
signal_dirac = zeros(1,Ns);
signal_dirac(1) = 1;
% – Filtre de mise en forme rectangulaire de hauteur 1 et de duree  egale a la duree symbole.
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
conv_emission_2 = filter(porte,1,signal_dirac);  
conv_reception_2 = filter([porte zeros(1,length(porte))],1, porte );

% Tracé
figure(5);
subplot(2,1,1)
stem(conv(porte,porte));grid on;
xlabel("points");

subplot(2,1,2)
stem(0 : Te : (length(conv_reception_2)*Te - Te),conv_reception_2);grid on;
xlabel("temps en seconde")
ylabel("g(t)")
title("reponse impultionnelle de g")


% échantillonage du signal. n0 + mNs
n0 = 8;
echantillon = conv_reception_signal(n0:Ns:end); % de la forme [8 8 8 -8 -8 8 ...]
temps = ((n0*Te)-Te:(Ns*Te):length(echantillon)*(Ns*Te)-Te);

echelle = (0:Te:taille*Ts-Te);
figure(6);
stem(temps,echantillon,'o')
hold on 
plot(echelle, conv_reception_signal,'r');
xlabel("Temps en seconde")
ylabel("signal")
legend('signal échantillonné','signal');
title("Échantillonnage du signal")

%Signal en sortie 
bits_sortie = echantillon > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 


% Diagramme de l'oeil
figure(8);
plot(reshape(conv_reception_signal,Ns,length(conv_reception_signal)/Ns))
ylabel("Diagramme de l'oeil");

%Calcul taux d'erreur binaire
nb_bits_faux = sum(bits_sortie ~= transmettre );
taux_err_binaire = nb_bits_faux/length(transmettre); % pour n0 = 8, le taux est nulle

%% 3.2

BW1 = 8000 %Hz
BW2 = 1000 %Hz


% passe bas : canal
ordre = 61;
t = (-(ordre-1)/2:(ordre-1)/2)*Te;
Fc = BW1;
passe_bas = 2*Fc/Fe*sinc(2*Fc*t);


figure(9)
N = numel(passe_bas);
pb_freq = fft(passe_bas, N); 
Hc = abs(fftshift(pb_freq));
plot(linspace(-Fc,Fc,length(pb_freq)),Hc) % la réponse en frequence du filtre canal.
xlabel("Fréquence en Hz")
ylabel("pbfreq(f)");
title("Passe-bas en fréquence");


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
conv_reception_signal = filter(porte,1,canal((ordre+1)/2:end));
 
% Tracé de la réponse impulsionnelle
porte = ones(1,Ns); % le filtre de mise en forme [ 1 1 1 ...] de taille Ns 
%conv_emission = filter(porte,1,porte);  
canal = filter(passe_bas,1,[conv_emission zeros(1, (ordre-1)/2)]);
conv_reception_signal = filter(porte,1,canal((ordre+1)/2:end));

% Tracé
figure(10); 
plot(echelle,conv_reception_signal);grid on;
xlabel("Temps en secondes");
ylabel("s(t)");
title("signal en sortie");

% Diagramme de l'oeil
figure(11);
plot(reshape(conv_reception_signal,Ns,length(conv_reception_signal)/Ns))
ylabel("Diagramme de l'oeil");


% H(f)
f = linspace(-Fc,Fc,length(pb_freq));
H = (Ts)*sinc(Ts*f);
H_abs =  abs(fftshift(H));
figure(12)
plot(linspace(-Fc,Fc,length(pb_freq)),H_abs) % la réponse en frequence du filtre canal.
xlabel("Fréquence en Hz")
ylabel("|H(f)|");
title("Passe-bas en fréquence");

% H(f)Hr(f)

HHr = H.^2;
HHr_abs = abs(fftshift(HHr));
figure(13)
semilogy(linspace(-Fc,Fc,length(HHr_abs)),HHr_abs) 
hold on
semilogy(linspace(-Fc,Fc,length(Hc)),Hc,'r') % la réponse en frequence du filtre canal.
legend('|H(f)Hr(f)|','|Hc(f)|' )
xlabel("Fréquence en Hz")
ylabel("réponse fréquentielle|");
title("comparaison de courbe");
hold off

% TEB
n0 = 8;
echantillon = conv_reception_signal(n0:Ns:end); % de la forme [8 8 8 -8 -8 8 ...]
temps = ((n0*Te)-Te:(Ns*Te):length(echantillon)*(Ns*Te)-Te);

echelle = (0:Te:taille*Ts-Te);
figure(14);
stem(temps,echantillon,'o')
hold on 
plot(echelle, conv_reception_signal,'r');
xlabel("Temps en seconde")
ylabel("signal")
legend('signal échantillonné','signal');
title("Échantillonnage du signal")

%Signal en sortie 
bits_sortie = echantillon > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux = sum(bits_sortie ~= transmettre );
taux_err_binaire = nb_bits_faux/length(transmettre); 


%% Bruit
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

% Signal selon le filtre de mise en forme


P_x = mean(abs(x).^2); % Puissance du signal modulé en fréquence
M = 2; % ordre de la modulation
%signal a bruit par bit souhaité a l entrée du récepteur Eb/N0
Eb = 1;
N0 = 1;

P_b = (P_x*Ns)/(2*log2(M)*(Eb/N0)); % Puissance du bruit
sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x));

