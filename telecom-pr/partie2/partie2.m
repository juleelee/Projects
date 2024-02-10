%% Données 
clear all 
close all
% Fréquence d'echantillonnage
Fe = 24000; % Hz
Te = 1/Fe;

% Fréquence porteuse
fp = 2000;

alpha = 0.35; % roll off



% nombre de symbole
M = 4; % ordre de modulation
nb_sym = log2(M);

% Débit binaire 
Rb = 3000; % bits/s

Tb = 1/Rb; 

% la durée symbole
Ts = nb_sym*Tb; 

% Facteur de suréchantillonage 
Ns = Ts/Te;

L = 2*Tb/Te;

%% 
% une transmission au format DVB-S
% avec mapping QPSK  (Quadrature Phase Shift Keying)
% filtre de mise en forme : racine de cosinus surélevé de roll off 0.35
% transposition de fréquence


% Construction du signal modulé
% Signal a transmettre 
taille = 50; %le nombre de bits qu'on ve transmettre
transmettre = randi(0:1,1,taille); %génére aléatoirement taille bits

echelle = (0:Te:taille*Ts-Te);


% - Mapping : symboles 4-aires `a moyenne nulle.
% – Filtre de mise en forme : rectangulaire de hauteur 1 et de duree egale a la duree symbole.

% Symboles avec le mapping de Gray 
V_00 = exp(1i*5*pi/4); 
V_01 = exp(1i*3*pi/4);
V_11 = exp(1i*pi/4);
V_10 = exp(1i*7*pi/4);

xe = reshape(transmettre,2,[])'; % groupe les bits 2 par 2 
index = bi2de(xe)';  % convertir ligne par ligne de binaire en décimal [ 3 2 2 1 1 0 0 0 1 3...]

lut = [V_00, V_01, V_10, V_11];  
xe = lut(index +1); % en decimal en peut le voir comme des indices [ -3 -1 1 ...]
dk = xe;

mat = zeros(1,Ns); 
mat(1)= 1; 
xe = kron(xe, mat ); % une forme de peigne [-3 0 0 0 0 -1 0 0 0 0 1 0 0 ...]

%Filtre de mise en forme
%racine de cosinus sur eleve
h = rcosdesign(alpha,L,Ns,'sqrt');
ordre = length(h);
xe = [xe zeros(1,(ordre-1)/2)];
x = filter(h,1,xe); % la convolution avec la porte
x = x((ordre+1)/2 :end);

I=real(x);
Q=imag(x);
%%
t = (0:length(I)-1)*Te; % échelle de temps
figure(42);
subplot(2,1,1)
plot(t, I, 'b');
title('Signal généré sur la voie en phase I');
xlabel('Temps (s)');
ylabel('Amplitude');
subplot(2,1,2)
plot(t, Q, 'r');
xlabel('Temps (s)');
ylabel('Amplitude');
title('Signal généré sur la voie en quadrature Q');

%%  Ajout du Bruit 

%Signal a bruit par bit souhaité a l entrée du récepteur Eb/N0
dB = 0;
Eb_N0 = 10^(dB/10);

P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence

P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit

sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x));

signal_bruite = x + bruit;

x_recu = x;

%% Trac ́e du signal transmis sur fr ́equence porteuse avec une  ́echelle temporelle correcte.
% Modulation sur porteuse

% Signal modulé en fréquence
t = (0:length(x_recu)-1)*Te; % échelle de temps
porteuse = exp(1j*2*pi*fp*t);

signal_port = real(porteuse.*x_recu);

% Tracé du signal transmis sur fréquence porteuse
figure(2);
plot(t, signal_port);
xlabel('Temps (s)');
ylabel('Amplitude');
title('Signal transmis sur fréquence porteuse');

%% Démodulation 

% Retour en bande de base (demodulation cohérente)
t = (0:length(x_recu)-1)*Te; % échelle de temps
cos_signal = 2*cos(2*pi*fp*t).*signal_port;
sin_signal = 2*sin(2*pi*fp*t).*signal_port;

%filtrage par un passe bas 
ordre = 61;
t = (-(ordre-1)/2:(ordre-1)/2)*Te;
Fc = 2500;
passe_bas = 2*Fc/Fe*sinc(2*Fc*t);
% echelle freq
pb_freq = fft(passe_bas); 

% prise en compte du retard 

cos_filt = filter(passe_bas,1,[cos_signal zeros(1, (ordre-1)/2)]);
cos_filt = cos_filt((ordre+1)/2:end);

sin_filt = filter(passe_bas,1,[sin_signal zeros(1, (ordre-1)/2)]);
sin_filt = sin_filt((ordre+1)/2:end);

% signal obtenu 

sig_recu = cos_filt - sin_filt*1i;

% Filtre de reception :  filtre en racine de cosinus surélevé de roll-off 0.35
%Cela permet de respecter le critère de Nyquist et de limiter l'interférence intersymbole. 
%Il est recommandé d'utiliser un filtre adapté à la forme du signal transmis pour obtenir les meilleures performances de démodulation.
hr = rcosdesign(alpha, L, Ns,'sqrt');

ordre = length(hr);
sig_recu = [sig_recu zeros(1,(ordre-1)/2)];
filt_recep_signal = filter(hr,1,sig_recu); % la convolution avec la porte
filt_recep_signal = filt_recep_signal((ordre+1)/2 :end);


% echantillonnage 
n0 = 1;
echantillon_signal = filt_recep_signal(n0:Ns:end); 
plot(echantillon_signal)

%% Decision sur les bits 
symboles_decides= zeros(1,length(echantillon_signal));
% Detecteur à seuil
for i = 1 : length(echantillon_signal)
    if (real(echantillon_signal(i)) <= 0 && imag(echantillon_signal(i)) <= 0)
        symboles_decides(i) = -1 - 1i;
        
    elseif (real(echantillon_signal(i)) >= 0 && imag(echantillon_signal(i)) >= 0)
        symboles_decides(i) = 1 + 1i;
        
    elseif (real(echantillon_signal(i)) <= 0 && imag(echantillon_signal(i)) >= 0)
        symboles_decides(i) = -1 + 1i;
        
    elseif (real(echantillon_signal(i)) >= 0 && imag(echantillon_signal(i)) <= 0)
        symboles_decides(i) = 1 - 1i;
    end
end

% Calcul du TES
TES = length(find(symboles_decides ~= dk)) / (2 * length(dk));

% Calcul du TEB
TEB = TES / log2(4)


% echelle = [0 : Te : ((taille*Ts))-Te]; 
% 
% figure(2)
% subplot(3,1,1);
% stem(echelle,xe);grid on;
% xlabel("Temps en secondes");
% ylabel("e(t)");
% title("Signal en entrée en peigne");
% 
% 
% figure(2); 
% subplot(3,1,2)
% stem(echelle,I);grid on;
% xlabel("Temps en secondes");
% ylabel("s(t)");
% title("Signal en sortie avec le modulateur 2");
% figure(2); 
% subplot(3,1,3)
% stem(echelle,Q);grid on;
% xlabel("Temps en secondes");
% ylabel("s(t)");
% title("Signal en sortie avec le modulateur 2");
% exportgraphics(gcf,'./images/2-1et2.png','Resolution',400);
% 
% signal_cmplx = exp(2*1i*pi*fp*echelle);
% transpo_freq = real(xe.*signal_cmplx);
% 
% figure(3)
% plot(echelle,transpo_freq);
% 
% figure(4)
% DSP_2=pwelch(I,[],[],[],Fe,'twosided');
% DSP_3=pwelch(Q,[],[],[],Fe,'twosided'); 
% DSP_1=pwelch(transpo_freq,[],[],[],Fe,'twosided'); 
% f = linspace(-Fe/2,Fe/2, length(DSP_1));
% plot(f,10*log10(fftshift(DSP_1/max(DSP_1))),"g"); 
% hold on;
% plot(f,10*log10(fftshift(DSP_2/max(DSP_3))),"b"); 
% hold on;
% plot(f,10*log10(fftshift(DSP_3/max(DSP_3))),"r"); 
% legend('DSP de I(t)','DSP de Q(t)','DSP de x_e(t)')
% hold off;
% xlabel("Temps en secondes");
% ylabel("DSPs");
% title("Comparaison de DSP");
% exportgraphics(gcf,'./images/2-3et4.png','Resolution',400);
% 
% %Signal a bruit par bit souhaité a l entrée du récepteur Eb/N0
% dB = 0;
% Eb_N0 = 10^(dB/10);
% P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
% 
% x_canal = xe + P_b;