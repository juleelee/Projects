%% Comparaison des modulateurs DVB-S et DVB-S2




%% Comparaison du modulateur DVS-S avec un des modulateurs propos ́es par le DVB-S2

clear all 
close all



DSP_DVB_S = load("DSP_DVB_S.mat");
DSP_DVB_S = struct2array(DSP_DVB_S);
DSP_DVB_S2 = load("DSP_DVB_S2.mat");
DSP_DVB_S2 = struct2array(DSP_DVB_S2);
TEB_DVB_S = load("TEB_DVB_S.mat");
TEB_DVB_S = struct2array(TEB_DVB_S);
TEB_DVB_S2 = load("TEB_DVB_S2.mat");
TEB_DVB_S2 = struct2array(TEB_DVB_S2);


%% Efficacité Spectrale 

Fe = 24000;
f = linspace(-Fe/2,Fe/2, length(DSP_DVB_S));
figure(1)
plot(f,10*log10(fftshift(DSP_DVB_S)),"r"); 
hold on;
Fe = 6000;
f = linspace(-Fe/2,Fe/2, length(DSP_DVB_S2));
plot(f,10*log10(fftshift(DSP_DVB_S2)),"b"); 
hold off;
legend('DVB-S','DVB-S2')

xlabel("Fréquence (Hz)");
ylabel("DSPs");
title("Comparaison de DSP");




%% Efficacité en puissance

figure(2)
semilogy(0:0.5:6,TEB_DVB_S);
hold on;
semilogy(0:0.5:6,TEB_DVB_S2,'g');
hold off
xlabel("E_b/N_0")
ylabel("TEB")
legend("TEB_{DVB-S}","TEB_{DVB-S2}")
title("Comparaison des TEB pratique et théorique")









