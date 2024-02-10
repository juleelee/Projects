clear;
close all;
clc;

% Parametres pour l'affichage des donnees :
taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);

load donnees_app;

% Donnees non filtrees :
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
n = 50;
pourcentage_classif = zeros(n);
% Estimation du SVM avec noyau gaussien :
lambda = linspace(500, 15000, n);  % valeur de lambda
sigma = linspace(0.00009, 0.001, n);	% Ecart-type du noyau gaussien
for l = 1:n
    for k = 1:n
        [X_VS,Y_VS,Alpha_VS,c,code_retour] = SVM_3_souple(X,Y,sigma(l),lambda(k));
        
        % Si l'optimisation n'a pas converge :
        if code_retour ~= 1
	        return;
        end
  
        % Pourcentage de bonnes classifications des donnees de test :
        load donnees_test;
        nb_donnees_test = size(X_test,1);
        nb_classif_OK = 0;
        for i = 1:nb_donnees_test
	        x_i = X_test(i,:);
	        prediction = sign(exp(-sum((X_VS-x_i).^2,2)/(2*sigma(l)^2))'*diag(Y_VS)*Alpha_VS-c);
	        if prediction==Y_test(i)
		        nb_classif_OK = nb_classif_OK+1;
	        end
        end
        %fprintf('Pourcentage de bonnes classifications des donnes de test : %.1f %%\n',double(nb_classif_OK/nb_donnees_test*100));
        
            pourcentage_classif(l,k) = double(nb_classif_OK/nb_donnees_test*100);
          
        
    end
end
%%
figure;
surf(lambda, sigma, pourcentage_classif);
xlabel('Lambda');
ylabel('Sigma');
zlabel('Pourcentage de classification');
title('Pourcentage de classification en fonction de Lambda et Sigma');
% Trouver les coordonnées du maximum de pourcentage_classif
[max_val, max_idx] = max(pourcentage_classif(:));
[max_row, max_col] = ind2sub(size(pourcentage_classif), max_idx);

% Marquer le maximum avec un point rouge
hold on;
plot3(lambda(max_col), sigma(max_row), max_val, 'ro', 'MarkerSize', 10, 'MarkerFaceColor', 'r');
hold off;

max(max(pourcentage_classif))
