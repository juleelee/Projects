close all;

addpath('matlab_bgl');      %load graph libraries
addpath('matlab_tpgraphe'); %load tp ressources

Pos_Avg = table2array(readtable('Topologies-20231221/topology_avg.csv', 'NumHeaderLines',1));
Pos_Low = table2array(readtable('Topologies-20231221/topology_low.csv', 'NumHeaderLines',1));
Pos_High = table2array(readtable('Topologies-20231221/topology_high.csv', 'NumHeaderLines',1));

D_Avg = Distance(Pos_Avg(:,2:4));
D_Low = Distance(Pos_Low(:,2:4));
D_High = Distance(Pos_High(:,2:4));
%% Partie 1
% La représentation 3D des satellites
close all;

for i=1:3
    portee = 20*i;
    D = D_Low;
    Pos = Pos_Low;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    
    viz_adj_3d(A,Pos(:,2:4),"sat-"+string(Pos(:,1)' + 1), "Low-"+string(portee)+"km");

    D = D_Avg;
    Pos = Pos_Avg;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    
    viz_adj_3d(A,Pos(:,2:4),"sat-"+string(Pos(:,1)' + 1), "Average-"+string(portee)+"km");
    
    D = D_High;
    Pos = Pos_High;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    
    viz_adj_3d(A,Pos(:,2:4),"sat-"+string(Pos(:,1)' + 1), "High-"+string(portee)+"km");
end


%% Partie 2
%% Degré
close all;

% Degrés moyens et distibution des degrés
degre_moy_tab = zeros(3,3); % Ligne : low,avg,high  Colonne : portée 20,40,60
figure
for i=1:3
    portee = 20*i;
    
    D = D_Low;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    degre_moy = mean(sum(A,1));
    degre_moy_tab(1,i) = degre_moy;
    subplot(3,3,3*i-2);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(sum(A,1),'BinWidth',1, 'FaceColor', "black");
    title(sprintf('Densité Low, portée '+string(portee)+' km'));
    xlabel("degré");
    ylabel("Nb_{sat}");

    D = D_Avg;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    degre_moy = mean(sum(A,1));
    degre_moy_tab(2,i) = degre_moy;
    subplot(3,3,3*i-1);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(sum(A,1),'BinWidth',1, 'FaceColor', "black");
    xlabel("degré");
    ylabel("Nb_{sat}");
    title(sprintf('Densité Avg, portée '+string(portee)+' km'));

    
    D = D_High;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    degre_moy = mean(sum(A,1));
    degre_moy_tab(3,i) = degre_moy;
    subplot(3,3,3*i);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(sum(A,1),'BinWidth',1, 'FaceColor', "black");
    title(sprintf('Densité High, portée '+string(portee)+' km'));
    xlabel("degré");
    ylabel("Nb_{sat}");
end
sgtitle("Distribution des degrés")
degre_moy_tab % Pour afficher les degrés moyens
%% Degré de clustering
close all;

% Degrés de clustering moyens et distibution des degrés de clustering
degre_moy_clustering_tab = zeros(3,3); % ligne : low avg high  colonne : portee
figure
for i=1:3
    portee = 20*i;

    D = D_Low;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    clustering_coefs = clustering_coefficients(sparse(A));
    subplot(3,3,3*i-2);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(clustering_coefs, 'FaceColor', "black");
    xlabel("Coef clustering");
    ylabel("Nb_{sat}");
    title(sprintf('Densité Low, portée '+string(portee)+' km'));
    degre_moy_clustering_tab(1,i) = mean(clustering_coefs);

    D = D_Avg;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    clustering_coefs = clustering_coefficients(sparse(A));
    subplot(3,3,3*i-1);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(clustering_coefs, 'FaceColor', "black");
    xlabel("Coef clustering");
    ylabel("Nb_{sat}");
    title(sprintf('Densité Avg, portée '+string(portee)+' km'));
    degre_moy_clustering_tab(2,i) = mean(clustering_coefs);
   
    
    D = D_High;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    clustering_coefs = clustering_coefficients(sparse(A));
    subplot(3,3,3*i);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(clustering_coefs, 'FaceColor', "black");
    xlabel("Coef clustering");
    ylabel("Nb_{sat}");
    title(sprintf('Densité High, portée '+string(portee)+' km'));
    degre_moy_clustering_tab(3,i) = mean(clustering_coefs);
end
sgtitle("Distribution des coefs de clustering")
degre_moy_clustering_tab

%% Les cliques
% Remarque : Ici on ne traîte pas la portée de 60km pour des raisons de
% temps de calcul

close all;
Nb_cliques = zeros(3,3);

% Nombre de cliques et leurs ordres
figure
for i=1:2
    portee = 20*i;

    D = D_Low;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    cliques = BK_MaxClique(A);
    Nb_cliques(1,i) = size(cliques,2);
    tmp = sum(cliques,1);
    subplot(2,3,3*i-2);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(tmp, 'BinWidth',1, 'FaceColor', "black");
    xlabel("Ordre");
    ylabel("Nb_{clique}");
    title(sprintf('Densité Low, portée '+string(portee)+' km'));

    D = D_Avg;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    cliques = BK_MaxClique(A);
    Nb_cliques(2,i) = size(cliques,2);
    tmp = sum(cliques,1);
    subplot(2,3,3*i-1);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(tmp, 'BinWidth',1, 'FaceColor', "black");
    xlabel("Ordre");
    ylabel("Nb_{clique}");
    title(sprintf('Densité Avg, portée '+string(portee)+' km'));
    
    D = D_High;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    cliques = BK_MaxClique(A);
    Nb_cliques(3,i) = size(cliques,2);
    tmp = sum(cliques,1);
    subplot(2,3,3*i);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(tmp, 'BinWidth',1, 'FaceColor', "black");
    xlabel("Ordre");
    ylabel("Nb_{clique}");
    title(sprintf('Densité High, portée '+string(portee)+' km'));
end
sgtitle("Distribution des ordres des cliques maximales")
Nb_cliques
% On remarque que plus on augmente la densité plus on aura de grosses
% cliques


%% Composantes connexes
close all;

Nb_comps = zeros(3,3); % Nombre de composantes connexes pour chaque cas

figure
for i=1:3
    portee = 20*i;

    D = D_Low;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    G = graph(A);
    [~, ordre_comps] = conncomp(G);
    Nb_comps(i,1) = length(ordre_comps);
    subplot(3,3,3*i-2);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(ordre_comps, 'BinWidth',1, 'FaceColor', "black");
    xlabel("Ordre");
    ylabel("Nb_{composante}");
    title(sprintf('Densité Low, portée '+string(portee)+' km'));


    D = D_Avg;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    G = graph(A);
    [~, ordre_comps] = conncomp(G);
    Nb_comps(i,2) = length(ordre_comps);
    subplot(3,3,3*i-1);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(ordre_comps, 'BinWidth',1, 'FaceColor', "black");
    xlabel("Ordre");
    ylabel("Nb_{composante}");
    title(sprintf('Densité Avg, portée '+string(portee)+' km'));
    

    D = D_High;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    G = graph(A);
    [~, ordre_comps] = conncomp(G);
    Nb_comps(i,3) = length(ordre_comps);
    subplot(3,3,3*i);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(ordre_comps, 'BinWidth',1, 'FaceColor', "black");
    xlabel("Ordre");
    ylabel("Nb_{composante}");
    title(sprintf('Densité High, portée '+string(portee)+' km'));
end
Nb_comps
sgtitle("Distribution des ordres des composantes connexes")

%% Les plus courts chemins

close all;

moyenne_plus_court_chemin = zeros(3,3);
ecart_type_plus_court_chemin = zeros(3,3);
max_length_tab = zeros(3,1);


figure

for i=1:3
    portee = 20*i;

    D = D_Low;
    A = D <= portee*10^3; % Ici, chaque sommet est relié à lui même mais cela ne pose pas de problème dans les algos qui suivent.
    A = A - eye(size(A));
    G = graph(A);
    shortestpath = distances(G);
    shortestpath = triu(shortestpath); % On garde la matrice triangulaire sup pour ne pas compter les chemins 2 fois
    shortestpath(isinf(shortestpath)) = 0; % On enlève les chemins de longueur Inf
    shortestpath_nozero = shortestpath(shortestpath ~= 0); % On enlève les chemins de longeur 0
    subplot(3,3,3*i-2);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(shortestpath_nozero, 'FaceColor', "black");
    xlabel("Longueur du chemin");
    ylabel("Nb_{chemin}");
    title(sprintf('Densité Low, portée '+string(portee)+' km'));
    
    max_length = max(shortestpath,[], "all"); % chemin le plus long 
    if i == 3
        max_length_tab(1,1) = max_length;
    end
    % calcul des matrices A^k (pour des raisons d'optimisation)
    A_k = A;
    for j=2:max_length
        A_k = [A_k A^j];
    end
    n = length(A);
    Nb_Court_Chemins = zeros(size(A));
    for k = 1:n
        for l=k:n
            value_sp = shortestpath(k,l);
            if value_sp ~= 0
                Nb_Court_Chemins(k,l) = A_k(k,(value_sp-1)*size(A,2) + l);
            end
        end
    end
    

    moyenne_longeur_plus_court_chemin(1,i) = mean(shortestpath_nozero);
    ecart_type_longeur_plus_court_chemin(1,i) = std(shortestpath_nozero);
    moyenne_plus_court_chemin(1,i) = mean(Nb_Court_Chemins(Nb_Court_Chemins ~=0));
    ecart_type_plus_court_chemin(1,i) = std(Nb_Court_Chemins(Nb_Court_Chemins ~=0));
    

    D = D_Avg;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    G = graph(A);
    shortestpath = distances(G);
    shortestpath = triu(shortestpath); % On garde la matrice triangulaire sup pour ne pas compter les chemins 2 fois
    shortestpath(isinf(shortestpath)) = 0; % On enlève les chemins de longueur Inf
    shortestpath_nozero = shortestpath(shortestpath ~= 0); % On enlève les chemins de longeur 0
    subplot(3,3,3*i-1);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(shortestpath_nozero, 'FaceColor', "black");
    xlabel("Longueur du chemin");
    ylabel("Nb_{chemin}");
    title(sprintf('Densité Avg, portée '+string(portee)+' km'));

    max_length = max(shortestpath,[], "all"); % chemin le plus long
    if i == 3
        max_length_tab(2,1) = max_length;
    end
    % calcul des matrices A^k (pour des raisons d'optimisation)
    A_k = A;
    for j=2:max_length
        A_k = [A_k A^j];
    end
    n = length(A);
    Nb_Court_Chemins = zeros(size(A));
    for k = 1:n
        for l=k:n
            value_sp = shortestpath(k,l);
            if value_sp ~= 0
                Nb_Court_Chemins(k,l) = A_k(k,(value_sp-1)*size(A,2) + l);
            end
        end
    end
    
    moyenne_longeur_plus_court_chemin(2,i) = mean(shortestpath_nozero);
    ecart_type_longeur_plus_court_chemin(2,i) = std(shortestpath_nozero);
    moyenne_plus_court_chemin(2,i) = mean(Nb_Court_Chemins(Nb_Court_Chemins ~=0));
    ecart_type_plus_court_chemin(2,i) = std(Nb_Court_Chemins(Nb_Court_Chemins ~=0));


    D = D_High;
    A = D <= portee*10^3;
    A = A - eye(size(A));
    G = graph(A);
    shortestpath = distances(G);
    shortestpath = triu(shortestpath); % On garde la matrice triangulaire sup pour ne pas compter les chemins 2 fois
    shortestpath(isinf(shortestpath)) = 0; % On enlève les chemins de longueur Inf
    shortestpath_nozero = shortestpath(shortestpath ~= 0); % On enlève les chemins de longeur 0
    subplot(3,3,3*i);  %plot the figure in a new vignette of a 9 vignette plot
    histogram(shortestpath_nozero, 'FaceColor', "black");
    xlabel("Longueur du chemin");
    ylabel("Nb_{chemin}");
    title(sprintf('Densité High, portée '+string(portee)+' km'));
    
    max_length = max(shortestpath,[], "all"); % chemin le plus long
    if i == 3
        max_length_tab(3,1) = max_length;
    end
    % calcul des matrices A^k (pour des raisons d'optimisation)
    A_k = A;
    for j=2:max_length
        A_k = [A_k A^j];
    end
    n = length(A);
    Nb_Court_Chemins = zeros(size(A));
    for k = 1:n
        for l=k:n
            value_sp = shortestpath(k,l);
            if value_sp ~= 0
                Nb_Court_Chemins(k,l) = A_k(k,(value_sp-1)*size(A,2) + l);
            end
        end
    end

    moyenne_longeur_plus_court_chemin(3,i) = mean(shortestpath_nozero);
    ecart_type_longeur_plus_court_chemin(3,i) = std(shortestpath_nozero);
    moyenne_plus_court_chemin(3,i) = mean(Nb_Court_Chemins(Nb_Court_Chemins ~=0));
    ecart_type_plus_court_chemin(3,i) = std(Nb_Court_Chemins(Nb_Court_Chemins ~=0));
end
sgtitle("Distribution des longueurs des plus courts chemins")

moyenne_longeur_plus_court_chemin
ecart_type_longeur_plus_court_chemin;
moyenne_plus_court_chemin
ecart_type_plus_court_chemin;

save("max_lengths-60km","max_length_tab");

%% Graphes Valués


close all;

moyenne_longeur_plus_court_chemin = zeros(3,1);

max_length_tab = load("max_lengths-60km");
max_length_tab = cell2mat(struct2cell(max_length_tab));

figure

portee = 60;

D = D_Low/10^3;
A = D <= portee; % Ici, chaque sommet est relié à lui même mais cela ne pose pas de problème dans les algos qui suivent.
A = A - eye(size(A));
A = A .* (D.^2);
G = graph(A);
shortestpath = distances(G);
shortestpath = triu(shortestpath); % On garde la matrice triangulaire sup pour ne pas compter les chemins 2 fois
shortestpath(isinf(shortestpath)) = 0; % On enlève les chemins de longueur Inf
shortestpath_nozero = shortestpath(shortestpath ~= 0); % On enlève les chemins de longeur 0
subplot(1,3,1);  %plot the figure in a new vignette of a 9 vignette plot
histogram(shortestpath_nozero, 'FaceColor', "black");
xlabel("Longueur du chemin (km)");
ylabel("Nb_{chemin}");
title(sprintf('Densité Low, portée '+string(portee)+' km'));
moyenne_longeur_plus_court_chemin(1) = mean(shortestpath_nozero);


D = D_Avg/10^3;
A = D <= portee;
A = A - eye(size(A));
A = A .* (D.^2);
G = graph(A);
shortestpath = distances(G);
shortestpath = triu(shortestpath); % On garde la matrice triangulaire sup pour ne pas compter les chemins 2 fois
shortestpath(isinf(shortestpath)) = 0; % On enlève les chemins de longueur Inf
shortestpath_nozero = shortestpath(shortestpath ~= 0); % On enlève les chemins de longeur 0
subplot(1,3,2);  %plot the figure in a new vignette of a 9 vignette plot
histogram(shortestpath_nozero, 'FaceColor', "black");
xlabel("Longueur du chemin (km)");
ylabel("Nb_{chemin}");
title(sprintf('Densité Avg, portée '+string(portee)+' km'));
moyenne_longeur_plus_court_chemin(2) = mean(shortestpath_nozero);


D = D_High/10^3;
A = D <= portee;
A = A - eye(size(A));
A = A .* (D.^2);
G = graph(A);
shortestpath = distances(G);
shortestpath = triu(shortestpath); % On garde la matrice triangulaire sup pour ne pas compter les chemins 2 fois
shortestpath(isinf(shortestpath)) = 0; % On enlève les chemins de longueur Inf
shortestpath_nozero = shortestpath(shortestpath ~= 0); % On enlève les chemins de longeur 0
subplot(1,3,3);  %plot the figure in a new vignette of a 9 vignette plot
histogram(shortestpath_nozero, 'FaceColor', "black");
xlabel("Longueur du chemin (km)");
ylabel("Nb_{chemin}");
title(sprintf('Densité High, portée '+string(portee)+' km'));

moyenne_longeur_plus_court_chemin(3) = mean(shortestpath_nozero);


sgtitle("Distribution des longueurs des plus courts chemins graphe pondéré")

moyenne_longeur_plus_court_chemin;







