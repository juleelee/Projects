close all;
clear;
clc;
%% Étude de modulateurs bande de base
Fe = 24000;%Hz
Te = 1/Fe; %seconde 

% nombre de symbole
M = 2;
nb_sym = log2(M);

% Débit binaire 

Rb = 3000; % bits/s

Tb = 1/Rb; 

% la durée symbole
Ts = nb_sym*Tb; 

% Facteur de suréchantillonage 
Ns = Ts/Te; % à déterminer dans chaque cas


% Signal a transmettre 
taille = 10000; % le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille); %génére aléatoirement taille bits

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
%exportgraphics(gcf,'./images/temp_mod1.png','Resolution',400);



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
%exportgraphics(gcf,'./images/temp_mod2.png','Resolution',400);


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
alpha = 0,2;

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
%exportgraphics(gcf,'./images/temp_mod3.png','Resolution',400);


%% Trace des DSP estimé et théorique 
% modulateur 1 
figure(1); 
subplot(3,1,3)
DSP_1=pwelch(entree_conv,[],[],[],Fe,'twosided'); % DSP pratique 
f = linspace(-Fe/2,Fe/2, length(DSP_1));
S_x1 = (Ts)*sinc(f*Ts).^2;  % DSP  théorique 
plot(f,10*log10(fftshift(DSP_1/max(DSP_1))),"g"); grid on 
hold on;
plot(f,10*log10(S_x1/max(S_x1)),"r");
xlabel("Fréquence en Hz")
ylabel("Densité spectrale de puissance (dB)")
legend('Densité de puissance estimée','Densitee de puissance théorique');
title("Comparaison de la DSP estimée et théorique du modulateur 1");
%exportgraphics(gcf,'./images/DSP_mod1.png','Resolution',400);

figure(43)
plot(f,10*log10(fftshift(DSP_1/max(DSP_1))),"g"); grid on 
xlabel("Fréquence en Hz")
ylabel("Densité spectrale de puissance (dB)")
title("La DSP estimée du modulateur 1");
%exportgraphics(gcf,'./images/DSP_solomod1.png','Resolution',400);

%modulateur 2 
figure(2); 
subplot(3,1,3)
DSP_2=pwelch(transmettre_2_conv,[],[],[],Fe,'twosided'); % DSP pratique 
f = linspace(-Fe/2,Fe/2, length(DSP_2));
S_x2 = 20*(Ts)*sinc(2*f*Ts).^2; % DSP théorique 

plot(f,10*log10(fftshift(DSP_2/max(DSP_2))),"g"); grid on
hold on;
plot(f,10*log10(S_x2/max(S_x2)));
xlabel("Fréquence en Hz")
ylabel("Densité spectrale de puissance (dB)")
legend('Densité de puissance estimée','Densitee de puissance théorique');
title("Comparaison de la DSP estimée et théorique du modulateur 2");
%exportgraphics(gcf,'./images/DSP_mod2.png','Resolution',400);

figure(44)
plot(f,10*log10(fftshift(DSP_2/max(DSP_2))),"g"); grid on 
xlabel("Fréquence en Hz")
ylabel("Densité spectrale de puissance (dB)")
title("La DSP estimée du modulateur 2");
%exportgraphics(gcf,'./images/DSP_solomod2.png','Resolution',400);

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
plot(f,10*log10(S_x3/max(S_x3)));
xlabel("Fréquence en Hz")
ylabel("Densité spectrale de puissance (dB)")
legend('Densité de puissance estimée','Densitee de puissance théorique');
title("Comparaison de la DSP estimée et théorique du modulateur 3");
%exportgraphics(gcf,'./images/DSP_mod3.png','Resolution',400);

figure(44)
plot(f,10*log10(fftshift(DSP_3/max(DSP_3))),"g"); grid on 
xlabel("Fréquence en Hz")
ylabel("Densité spectrale de puissance (dB)")
title("La DSP estimée du modulateur 3");
%exportgraphics(gcf,'./images/DSP_solomod3.png','Resolution',400);

figure(42) 
plot(f,10*log10(fftshift(DSP_3/max(DSP_3)))); grid on
hold on;
plot(f,10*log10(fftshift(DSP_2/max(DSP_2))),"r");
plot(f,10*log10(fftshift(DSP_1/max(DSP_1))),"g");
xlabel("Fréquence en Hz")
ylabel("Densité spectrale de puissance (dB)")
legend('Densité de puissance modulateur3','Densitee de puissance modulateur2','Densité de puissance modulateur1');
title("Comparaison des DSP 1, 2 et 3");
%exportgraphics(gcf,'./images/DSP_123.png','Resolution',400);