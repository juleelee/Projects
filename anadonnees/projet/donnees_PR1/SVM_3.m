function [X_VS,Y_VS,Alpha_VS,c,code_retour] = SVM_3(X,Y,sigma)
f=-ones(length(X),1);
for i=1:length(X)
    for j=1:length(X)
        K(i,j)=exp(-norm(X(i,:)-X(j,:))^2/(2*sigma^2));
    end
end
Hf = diag(Y)*K*diag(Y);
Aeq=Y';
beq=0;
lb = zeros(length(X),1);
[alpha,~,code_retour] = quadprog(Hf,f,[],[],Aeq,beq,lb,[]);
comp = alpha>=1e-6;
X_VS = X(comp,:);
Y_VS  = Y(comp);
Alpha_VS = alpha(comp);
xi = X_VS(1,:);
yi = Y_VS(1);

K1 = zeros(length(X_VS),1);
for i=1:length(X_VS)
    K1(i)=exp(-norm(X_VS(i,:)-xi)^2/(2*sigma^2));
end

c = (Alpha_VS.*Y_VS)'*K1-yi;


end
