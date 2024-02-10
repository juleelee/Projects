function [X_VS,Y_VS,Alpha_VS,c,code_retour] = SVM_3(X,Y,sigma)
    % Calcul de la matrice de Gram G
    G = zeros(length(X), length(X));
    for i = 1:length(X)
        for j = 1:length(X)
            % G(i, j) = K(x_i, x_j)
            G(i, j) = exp(-norm(X(i,:) - X(j,:), 2) / (2 * sigma^2));
        end
    end
    %% Calcul de alpha
    f = -ones(length(X), 1);
    beq = 0;
    Aeq = Y';
    lb = zeros(length(X), 1);
    % Application de la matrice de Gram pour le calcul de alpha
    H = diag(Y) * G * diag(Y);
    [alpha, ~, code_retour] = quadprog(H, f, [], [], Aeq, beq, lb, []);
    Y_VS = Y(alpha > 1e-6, :);
    Alpha_VS = alpha(alpha > 1e-6, :);
    X_VS = X(alpha > 1e-6, :);
    % Calcul de la condition d'optimalité c
    c = sum(alpha .* Y .* G(:, 3)) - Y(3);
end


