function [X_VS, Y_VS, alpha_VS, c, code_retour] = SVM_3_souple(X, Y, sigma, lambda)

% On introduit le paramètre lambda
% lambda = 10000;

% On calcule la matrice de Gram
n = size(X, 1);
K = zeros(n, n);

for i=1:n
    for j=1:n
        K(i, j) = exp(-norm(X(i, :) - X(j, :))^2 / (2 * sigma^2));
    end
end

% On résout le problème
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
X_VS = X(X_VS_index, :);
Y_VS = Y(X_VS_index);
alpha_VS = alpha(X_VS_index);

% On calcule le biais c
c_sum = 0;

for i=1:length(X_VS_index)
    c_sum = c_sum + sum(alpha .* Y .* K(:, X_VS_index(i))) - Y(X_VS_index(i));
end

c = c_sum / length(X_VS_index);

code_retour = exitflag;

end
