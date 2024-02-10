clear;
close all;
clc;

% Parametres pour l'affichage des donnees :
taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);

load donnees_app;

% Donnees filtrees :
X = X_app;
Y = Y_app;

% Parametres d'affichage :
pas = 0.002;
marge = 0.005;
valeurs_carac_1 = min(min(X(:,1)))-marge:pas:max(max(X(:,1)))+marge;
valeurs_carac_2 = min(min(X(:,2)))-marge:pas:max(max(X(:,2)))+marge;
limites_affichage = [valeurs_carac_1(1) valeurs_carac_1(end) ...
                     valeurs_carac_2(1) valeurs_carac_2(end)];
nom_carac_1 = 'Compacite';
nom_carac_2 = 'Contraste';

%% Faire varier lambda et essayer de calculer le pourcentage
nb_valeur_lambda = 500;
pourcentage_classif = zeros(1, nb_valeur_lambda);
% Estimation du SVM lineaire (formulation duale) :
lambda = linspace(500, 10000, nb_valeur_lambda);   % valeur de lambda
for k = 1:nb_valeur_lambda
    [X_VS,w,c,code_retour] = SVM_2_souple(X,Y,lambda(k));
    
    % Si l'optimisation n'a pas converge :
    if code_retour ~= 1
	    return;
    end
    
    % Regle de decision du SVM :
    nb_1 = length(valeurs_carac_1);
    nb_2 = length(valeurs_carac_2);
    SVM_predict = zeros(nb_2,nb_1);
    for i = 1:nb_1
	    for j = 1:nb_2
		    x_ij = [valeurs_carac_1(i) ; valeurs_carac_2(j)];
		    SVM_predict(j,i) = sign(w'*x_ij-c);
	    end
    end
    
    % Pourcentage de bonnes classifications des donnees de test :
    load donnees_test;
    nb_donnees_test = size(X_test,1);
    nb_classif_OK = 0;
    for i = 1:nb_donnees_test
	    x_i = X_test(i,:);
	    prediction = sign(w'*x_i'-c);
	    if prediction==Y_test(i)
		    nb_classif_OK = nb_classif_OK+1;
	    end
    end
    % fprintf('Pourcentage de bonnes classifications des donnes de test : %.1f %%\n',double(nb_classif_OK/nb_donnees_test*100));
    pourcentage_classif(k) = double(nb_classif_OK/nb_donnees_test*100);
end
max(pourcentage_classif)

figure(1);
plot(lambda, pourcentage_classif);
title("Pourcentage de bonnes classifications en fonction de lambda");
xlabel("Valeurs de lambda");
ylabel("Pourcentage de bonnes classifications");