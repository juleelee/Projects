
%% Comparaison du modulateur DVS-S avec un des modulateurs propos ́es par le DVB-S2

clear all 
close all

% Données 

% Fréquence d'echantillonnage
Fe = 6000; % Hz
Te = 1/Fe;

% Fréquence porteuse
fp = 2000;

alpha = 0.2; % roll off



% nombre de symbole
M = 8; % ordre de modulation
nb_sym = log2(M);

% Débit binaire 
Rb = 3000; % bits/s

Tb = 1/Rb; 

% la durée symbole
Ts = nb_sym*Tb; 

% Facteur de suréchantillonage 
Ns = Ts/Te;

L = 8 ;

%% 
% une transmission au format DVB-S2
% avec mapping 8-PSK  (Quadrature Phase Shift Keying)
% filtre de mise en forme : racine de cosinus surélevé de roll off 0.2



% Construction du signal modulé
% Signal a transmettre 
taille = 10000; %le nombre de bits qu'on ve transmettre
transmettre = randi([0, M - 1], 1, taille); %génére aléatoirement taille bits

%echelle = (0:Te:taille*Ts-Te);

% Mapping : dk = ak +- i*bk

% Mapping permettant d'obtenir dk
dk = pskmod(transmettre, M, pi / M);
ak = real(dk);
bk = imag(dk);

plot(dk)
mat = zeros(1,Ns); 
mat(1)= 1; 

ak_diracs = kron(ak, mat);
bk_diracs = kron(bk, mat);

%Filtre de mise en forme
%racine de cosinus sur eleve en prenant en compte le retard

h = rcosdesign(alpha, L, Ns, 'sqrt');

ordre = length(h);
ak_diracs = [ak_diracs zeros(1,(ordre-1)/2)]; % on prend on compte le retard
I = filter(h,1,ak_diracs); % la convolution avec la porte
I = I((ordre+1)/2 :end);


bk_diracs = [bk_diracs zeros(1,(ordre-1)/2)];
Q = filter(h,1,bk_diracs); % la convolution avec la porte
Q = Q((ordre+1)/2 :end);

% Le signal transmis sur fréquence porteuse
x =  I + 1i * Q;


%% Tracé des signaux en phase et en quadrature
t = (0:length(I)-1)*Te; % échelle de temps
% Les constellations en sortie du mapping et de l’échantillonneur
figure(1);
plot(ak, bk, 'b*');
xlim([-1.5 1.5]);
ylim([-1.5 1.5]);
title('Les constellations en sortie du mapping')
xlabel('I');
ylabel('Q');


figure(2);
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
dB = 90;
Eb_N0 = 10^(dB/10);

P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence

P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit

sigma = sqrt(P_b);
bruit = sigma*randn(1,length(x)) + 1i * (sigma * randn(1, length(x)));

signal_bruite = x + bruit;

x_recu = x;


% Trac ́e de la densité spectrale de puissance des signaux g ́en ́er ́es sur les voies en phase et en quadrature.


% DSP pratique
DSP_1=pwelch(x_recu,[],[],[],Fe,'twosided'); 
DSP_2=pwelch(I,[],[],[],Fe,'twosided');
DSP_3=pwelch(Q,[],[],[],Fe,'twosided'); 


save('DSP_DVB_S2.mat', 'DSP_1')

f = linspace(-Fe/2,Fe/2, length(DSP_1));
figure(4)
plot(f,10*log10(fftshift(DSP_1)),"g"); 
hold on;
plot(f,10*log10(fftshift(DSP_2)),"b"); 
hold on;
plot(f,10*log10(fftshift(DSP_3)),"r"); 
legend('DSP de x_e(t)','DSP de I(t)','DSP de Q(t)')
hold off;
xlabel("Fréquence (Hz)");
ylabel("DSPs");
title("Comparaison de DSP");

% Tracé DSP théorique 
% TODO 

% voir dans le cours : Sx(f) = 1/4 * (Sxe(f - fp) + Sxe(-f-fp))
% avec Sxe(f) = voir cours page 38.

% % H(f)
% f = linspace(-Fc,Fc,length(pb_freq));
% H = (Ts)*sinc(Ts*f);
% H_abs =  abs(fftshift(H));
% figure(12)
% plot(linspace(-Fc,Fc,length(pb_freq)),H_abs) % la Réponse en frequence du filtre canal.
% xlabel("Fréquence en Hz")
% ylabel("|H(f)|");
% title("Passe-bas en fréquence");

%
% Démodulation 



% Filtre de reception :  filtre en racine de cosinus surélevé de roll-off 0.35
%Cela permet de respecter le critère de Nyquist et de limiter l'interférence intersymbole. 
%Il est recommandé d'utiliser un filtre adapté à la forme du signal transmis pour obtenir les meilleures performances de démodulation.
hr = rcosdesign(alpha, L, Ns,'sqrt');

ordre = length(hr);
x_recu = [x_recu zeros(1,(ordre-1)/2)];
filt_recep_signal = filter(hr,1,x_recu); % la convolution avec la porte
filt_recep_signal = filt_recep_signal((ordre+1)/2 :end);


% Le diagramme de l’oeil en sortie du filtre de réception
% Ou la partie reelle
diagramme_oeil = reshape(imag(filt_recep_signal), Ns, length(imag(filt_recep_signal)) / Ns);
figure(5);
plot(diagramme_oeil);
title("Diagramme de l'oeil sans bruit du signal reçu");
xlabel('Temps en secondes');

% echantillonnage 
n0 = 1; 
echantillon_signal = filt_recep_signal(n0:Ns:end); 
% Les constellations en sortie du mapping et de l’échantillonneur
    figure(42);
    plot(real(echantillon_signal), imag(echantillon_signal), 'r*');
    hold on;
    plot(ak, bk, 'b*');
    legend('Les constellations en sortie du mapping','Les constellations en sortie de l’échantillonneur')
    xlabel('I');
    ylabel('Q');


%% Decision sur les bits 

% Detecteur à seuil
bits_decides = pskdemod(echantillon_signal, M, pi / M);


% Calcul du TEB
TEB = length(find(bits_decides ~= transmettre)) / length(transmettre)

%% Trac ́e du taux d’erreur binaire obtenu en fonction du rapport signal `a bruit par bit `a l’entr ́ee du r ́ecepteur pour des valeurs allant de 0 `a 6 dB.
TEB_pratique = [];
TEB_theorique = [];
for db = 0:0.5:6
    Eb_N0 = 10^(db/10);
    P_x = mean(abs(x).^2); % Puissance du Signal modulé en fréquence
    P_b = (P_x*Ns)/(2*log2(M)*(Eb_N0)); % Puissance du bruit
    sigma = sqrt(P_b);
    bruit = sigma*randn(1,length(x)) + 1i * (sigma * randn(1, length(x)));
    % passage dans le canal
    x_bruite = x + bruit;
    x_choisi = x_bruite;



    % Filtre de reception :  filtre en racine de cosinus surélevé de roll-off 0.35
    %Cela permet de respecter le critère de Nyquist et de limiter l'interférence intersymbole. 
    %Il est recommandé d'utiliser un filtre adapté à la forme du signal transmis pour obtenir les meilleures performances de démodulation.
    hr = rcosdesign(alpha, L, Ns,'sqrt');

    ordre = length(hr);
    
    x_bruite = [x_bruite zeros(1,(ordre-1)/2)];
    filt_recep_signal = filter(hr,1,x_bruite); % la convolution avec la porte
    filt_recep_signal = filt_recep_signal((ordre+1)/2 :end);

    % echantillonnage 
    n0 = 1; 
    echantillon_signal = filt_recep_signal(n0:Ns:end); 
    
    % Les constellations en sortie du mapping et de l’échantillonneur
    figure;
    plot(real(echantillon_signal), imag(echantillon_signal), 'r*');
    hold on;
    plot(ak, bk, 'b*');
    legend('Les constellations en sortie du mapping','Les constellations en sortie de l’échantillonneur')
    xlabel('I');
    ylabel('Q');
    title(['dB = ' num2str(db)]);
    % Detecteur à seuil
    bits_decides = pskdemod(echantillon_signal, M, pi / M);


    % Calcul du TEB
    TES_bis = length(find(bits_decides ~= transmettre)) / length(transmettre);
    TEB_bis = TES_bis/log2(M);
    TEB_pratique = [TEB_pratique TEB_bis];
    TEB_t = (2 / log2(M)) * qfunc(sqrt(2 *log2(M)* Eb_N0) * sin(pi / M));
    TEB_theorique = [TEB_theorique TEB_t];

    

end 

figure(50)
semilogy(0:0.5:6,TEB_theorique);
hold on;
semilogy(0:0.5:6,TEB_pratique,'g');
hold off
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{theorique}","TEB_{pratique}")
title("Comparaison des TEB pratique et théorique")

save('TEB_DVB_S2.mat', 'TEB_pratique')


%%














