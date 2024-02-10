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
%exportgraphics(gcf,'./images/3-1-sortie-filtre.png','Resolution',400);

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
 
g_impul = filter(porte,1,filter(porte,1,Signal_dirac));
subplot(2,1,2)
echel = 0:Te:2*Ts;
plot(g_impul);grid on;
xlabel("temps en seconde");
ylabel("g(t)");
title("reponse impulsionnelle de g");
%exportgraphics(gcf,'./images/3-1-rep-impulsionnelle.png','Resolution',400);


% échantillonage du Signal. n0 + mNs
n0 = 3;
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
%exportgraphics(gcf,'./images/3-1-echantillonage_n03.png','Resolution',400);

%Signal en sortie 
bits_sortie = echantillon > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 


% Diagramme de l'oeil
figure(8);
plot(reshape(conv_reception_Signal,Ns,length(conv_reception_Signal)/Ns))
ylabel("Diagramme de l'oeil");
%exportgraphics(gcf,'./images/3-1-diagrammeoeil.png','Resolution',400);

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
pb_freq = fft(passe_bas); 
Hc = abs(fftshift(pb_freq));
plot(linspace(-Fc,Fc,length(pb_freq)),Hc) % la Réponse en frequence du filtre canal.
xlabel("Fréquence en Hz")
ylabel("pbfreq(f)");
title("Passe-bas en fréquence");
%exportgraphics(gcf,'./images/3-2-passe-basBW1.png','Resolution',400);


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
%exportgraphics(gcf,'./images/3-2-sortieBW1.png','Resolution',400);

% Diagramme de l'oeil
figure(11);
plot(reshape(conv_reception_Signal,Ns,length(conv_reception_Signal)/Ns))
ylabel("Diagramme de l'oeil");
%exportgraphics(gcf,'./images/3-2-diagramme-oeilBW1.png','Resolution',400);


% H(f)
f = linspace(-Fc,Fc,length(pb_freq));
H = (Ts)*sinc(Ts*f);
H_abs =  abs(fftshift(H));
figure(12)
plot(linspace(-Fc,Fc,length(pb_freq)),H_abs) % la Réponse en frequence du filtre canal.
xlabel("Fréquence en Hz")
ylabel("|H(f)|");
title("Passe-bas en fréquence");
%exportgraphics(gcf,'./images/3-2-passe-basH(f)BW1.png','Resolution',400);

% H(f)Hr(f)


HHr_abs = abs(H).^2;
figure(13)
semilogy(linspace(-Fc,Fc,length(HHr_abs)),HHr_abs/max(HHr_abs)) 
hold on
semilogy(linspace(-Fc,Fc,length(Hc)),Hc/max(Hc),'r') % la Réponse en frequence du filtre canal.
legend('|H(f)Hr(f)|','|Hc(f)|' )
xlabel("Fréquence en Hz")
ylabel("Réponse fréquentielle|");
title("comparaison de courbe");
%exportgraphics(gcf,'./images/3-2-courbes-superposeesBW1.png','Resolution',400);
hold off

% Tracé de la reponse impulsionnelle
figure(14)
stem(conv(porte,conv(porte,passe_bas)));

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
title("Échantillonnage du Signal")
%exportgraphics(gcf,'./images/3-2-echantillonageBW1.png','Resolution',400);
hold off

%Signal en sortie 
bits_sortie = echantillon > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux = sum(bits_sortie ~= transmettre );
taux_err_binaire = nb_bits_faux/length(transmettre); 
%%
BW2 = 1000 %Hz
% passe bas : canal
ordre = 61;
t = (-(ordre-1)/2:(ordre-1)/2)*Te;
Fc = BW2;

passe_bas = 2*Fc/Fe*sinc(2*Fc*t);
Fc = BW1;

figure(9)
N = numel(passe_bas);
pb_freq = fft(passe_bas); 
Hc = abs(fftshift(pb_freq));
plot(linspace(-Fc,Fc,length(pb_freq)),Hc) % la Réponse en frequence du filtre canal.
xlabel("Fréquence en Hz")
ylabel("pbfreq(f)");
title("Passe-bas en fréquence");
%exportgraphics(gcf,'./images/3-2-passe-basBW2.png','Resolution',400);


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
%exportgraphics(gcf,'./images/3-2-sortieBW2.png','Resolution',400);

% Diagramme de l'oeil
figure(11);
plot(reshape(conv_reception_Signal,Ns,length(conv_reception_Signal)/Ns))
ylabel("Diagramme de l'oeil");
%exportgraphics(gcf,'./images/3-2-diagramme-oeilBW2.png','Resolution',400);


% H(f)
f = linspace(-Fc,Fc,length(pb_freq));
H = (Ts)*sinc(Ts*f);
H_abs =  abs(fftshift(H));
figure(12)
plot(linspace(-Fc,Fc,length(pb_freq)),H_abs) % la Réponse en frequence du filtre canal.
xlabel("Fréquence en Hz")
ylabel("|H(f)|");
title("Passe-bas en fréquence");
%exportgraphics(gcf,'./images/3-2-passe-basH(f)BW2.png','Resolution',400);

% H(f)Hr(f)


HHr_abs = abs(H).^2;
figure(13)
semilogy(linspace(-Fc,Fc,length(HHr_abs)),HHr_abs/max(HHr_abs)) 
hold on
semilogy(linspace(-Fc,Fc,length(HHr_abs)),Hc/max(Hc),'r') % la Réponse en frequence du filtre canal.
legend('|H(f)Hr(f)|','|Hc(f)|' )
xlabel("Fréquence en Hz")
ylabel("Réponse fréquentielle|");
title("comparaison de courbe");
%exportgraphics(gcf,'./images/3-2-courbes-superposeesBW2.png','Resolution',400);
hold off

% Tracé de la reponse impulsionnelle
figure(14)
stem(conv(porte,conv(porte,passe_bas)));

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
title("Échantillonnage du Signal")
%exportgraphics(gcf,'./images/3-2-echantillonageBW2.png','Resolution',400);
hold off

%Signal en sortie 
bits_sortie = echantillon > 0; % Si c'est positif c'est un 1, si c'est négatif c'est un 0 

%Calcul taux d'erreur binaire
nb_bits_faux = sum(bits_sortie ~= transmettre );
taux_err_binaire = nb_bits_faux/length(transmettre); 
