function D = Distance(Pos)
%DISTANCE Calcule la distance entre les points
n = length(Pos);
D = zeros(n,n);
for i =1:n
    for j = (i+1):n
        D(i,j) = pdist([Pos(i,:);Pos(j,:)], 'euclidean');
        D(j,i) = D(i,j);
    end
end

