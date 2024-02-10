function [X_VS,w,c,code_retour] = SVM_1(X,Y)
    H = [1 0 0 ; 0 1 0 ; 0 0 0];
    A = -Y .* [X -ones(length(X), 1)];
    [w_tild, ~, code_retour] = quadprog(H, zeros(length(H), 1), A, -ones(length(A), 1));
    w = w_tild(1:length(w_tild) - 1);
    c = w_tild(length(w_tild));
    X_VS = X(Y .* (X * w - c) - 1 <= 1e-6, :);
end