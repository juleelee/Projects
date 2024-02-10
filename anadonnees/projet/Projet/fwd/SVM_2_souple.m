function [X_VS,w,c,code_retour] = SVM_2_souple(X,Y,lambda)
    f = -ones(length(X), 1);
    beq = 0;
    Aeq = Y';
    lb = zeros(length(X), 1);
    ub = lambda * ones(length(X), 1);
    H = diag(Y) * (X * X') * diag(Y);
    [alpha, ~, code_retour] = quadprog(H, f, [], [], Aeq, beq, lb, ub);
    vect_bool_w = (alpha > 1e-6);
    Y_VS = Y(vect_bool_w, :);
    alpha_VS = alpha(vect_bool_w, :);
    X_VS = X(vect_bool_w, :);
    w = X_VS' * diag(Y_VS) * alpha_VS;
    vect_bool_c = (alpha > 1e-6) & (alpha < lambda);
    Y_VS = Y(vect_bool_c, :);
    X_VS = X(vect_bool_c, :);
    c = X_VS(1, :) * w - 1 / Y_VS(1); 
    X_VS = X(vect_bool_w, :);

end