function [X_VS, w, c, code_retour] = SVM_2_souple(X, Y,lambda)

% On introduit le paramètre lambda
% lambda = 11; % min
%

% On calcule la matrice de Gram
K = X * X';

% On résout le problème d'optimisation duale
H = (Y * Y') .* K;
f = -ones(size(Y));
Aeq = Y';
beq = 0;
inf = zeros(size(Y));
sup = lambda * ones(size(Y));
[alpha, ~, exitflag] = quadprog(H, f, [], [], Aeq, beq, inf, sup);

% On trouve les vecteurs de support
tolerance = 1e-6;
X_VS_index = find(alpha >= tolerance & alpha < lambda);
X_VS_index_w = find(alpha >= tolerance);
X_VS = X(X_VS_index, :);
Y_VS = Y(X_VS_index, :);
alpha_VS = alpha(X_VS_index_w);

% On calcule le vecteur de poids w
w = X(X_VS_index_w, :)'*(alpha_VS .* Y(X_VS_index_w));

% On calcule le biais c
c = -1 / Y_VS(1) + X_VS(1, :) * w;

code_retour = exitflag;

end