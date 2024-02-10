function viz_adj_3d(A,pos,nodename,ttl)

%# of cities
n=size(nodename,2);

%PLOT GRAPH(first create new figure)
figure;

%DISPLAY CITIES NAME ON THE GRAPH
plot3(pos(:,1),pos(:,2),pos(:,3),'r.');hold on;  %display a red point for each city
for i=1:n
   %text(pos(i,1)+5,pos(i,2), [nodename(i) '(' int2str(i) ')']); %with the name of the city
   text(pos(i,1)+5,pos(i,2),pos(i,3), sprintf('%s \n',nodename{i}));
end   

%DISPLAY SPT
gplot3(sparse(A),pos);
title(ttl);