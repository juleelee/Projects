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

sigmas = [];
percentages = [];
sigmas_app = [];
percentages_app = [];
borne_sup = 0.01;
borne_inf = 0.00001;
pas = 0.00001 ;

nb_classif_OK_app = 0;
for sigma = borne_inf:pas:borne_sup % Ecart-type du noyau gaussien
    % Estimation du SVM avec noyau gaussien :
    
    [X_VS,Y_VS,Alpha_VS,c,code_retour] = SVM_3(X,Y,sigma);
    
    % Si l'optimisation n'a pas converge :
    if code_retour ~= 1
	    return;
    end
    
   load donnees_app;
    nb_donnees_test_app = size(X_app,1);
    nb_classif_OK_app = 0;
    for i = 1:nb_donnees_test_app
	    x_i = X_app(i,:);
	    prediction = sign(exp(-sum((X_VS-x_i).^2,2)/(2*sigma^2))'*diag(Y_VS)*Alpha_VS-c);
	    if prediction==Y_app(i)
		    nb_classif_OK_app = nb_classif_OK_app+1;
	    end
    end
        percentage = double(nb_classif_OK_app/nb_donnees_test_app*100);
    fprintf('Pourcentage de bonnes classifications des donnes d apprentissage: %.1f %%\n',percentage);

    sigmas_app = [sigmas_app sigma];
    percentages_app = [percentages_app percentage];


    
    % Pourcentage de bonnes classifications des donnees de test :
    load donnees_test;
    nb_donnees_test = size(X_test,1);
    nb_classif_OK = 0;
    for i = 1:nb_donnees_test
	    x_i = X_test(i,:);
	    prediction = sign(exp(-sum((X_VS-x_i).^2,2)/(2*sigma^2))'*diag(Y_VS)*Alpha_VS-c);
	    if prediction==Y_test(i)
		    nb_classif_OK = nb_classif_OK+1;
	    end
    end
    percentage = double(nb_classif_OK/nb_donnees_test*100);
    fprintf('Pourcentage de bonnes classifications des donnes de test : %.1f %%\n',percentage);

    sigmas = [sigmas sigma];
    percentages = [percentages percentage];
end 
%%

plot(sigmas,percentages);
hold on; 
plot(sigmas_app,percentages_app);
xlim([borne_inf borne_sup]);
xlabel('\sigma','FontSize',15)
ylabel('Pourcentage de bonnes classifications','FontSize',15)
title("Impact de l'écart-type du noyau gaussien")
legend("Test","Apprentissage");

exportgraphics(gcf,'./images/per-sigma.png','Resolution',400)


