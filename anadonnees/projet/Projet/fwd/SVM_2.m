function [X_VS,w,c,code_retour] = SVM_2(X,Y)
    f = -ones(length(X), 1);
    beq = 0;
    Aeq = Y';
    lb = zeros(length(X), 1);
    H = diag(Y) * (X * X') * diag(Y);
    [alpha, ~, code_retour] = quadprog(H, f, [], [], Aeq, beq, lb, []);
    Y_VS = Y(alpha > 1e-6, :);
    alpha_VS = alpha(alpha > 1e-6, :);
    X_VS = X(alpha > 1e-6, :);
    w = X_VS' * diag(Y_VS) * alpha_VS;
    c = X_VS(1, :) * w - 1 / Y_VS(1); 
end