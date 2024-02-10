clear;
close all;

taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);

load donnees;
beta_0 = bord_inf(1,1);
gamma_0 = bord_sup(1,1);

% Degre des courbes de Bezier (testez plusieurs valeurs) :
d = 5;

% Modelisation de la silhouette par deux courbes de Bezier couplees :
figure('Name','Modelisation de la silhouette par deux courbes de Bezier couplees',...
	'Position',[0.4*L,0.05*H,0.6*L,0.7*H]);
[p,n] = size(bord_inf);
x = transpose(0:1/(p-1):1);
liste_parametres = zeros(2*d-1,n);
for j = 1:n
	parametres = moindres_carres_paire(d,x,bord_inf(:,j),bord_sup(:,j),beta_0,gamma_0);
	y_inf = bezier(x,[beta_0;parametres(1:d-1);parametres(end)]);
	y_sup = bezier(x,[gamma_0;parametres(d:end)]);

	plot(x,bord_inf(:,j),'k','LineWidth',2);
	hold on;
	plot(x,y_inf,'r','LineWidth',3);
	plot(x,bord_sup(:,j),'k','LineWidth',2);
	plot(x,y_sup,'r','LineWidth',3);
	axis([0,1.01,60,150]);
	axis ij;
	set(gca,'FontSize',20);
	xlabel('$x$','FontSize',30,'Interpreter','Latex');
	ylabel('$y$','FontSize',30,'Interpreter','Latex','Rotation',0);
	legend(' Donnees',' Modele');

	pause(0.5);
	hold off;
	liste_parametres(:,j) = parametres;
    %liste_parametres=parametres;
end

save points_de_controle liste_parametres;

function  [reel]=poly(d,k,xi)
    reel = nchoosek(d,k)*xi.^k.*(1-xi).^(d-k);
end

function [beta]=moindres_carres(d,x,bord_inf,beta_0)
    p=length(bord_inf);
    for j=1:p
        for i=1:d
            A(j,i)=poly(d,i,x(j));
        end
    end
    B(1:p)=bord_inf-beta_0*poly(d,0,x);
    beta=A\B';
end

function [beta]=moindres_carres_paire(d,x,bord_inf,bord_sup,beta_0,gamma_0)
    p=length(bord_inf);
    for j=1:p
        for i=1:d-1
            blocpetit(j,i)=poly(d,i,x(j));
        end
    end
    blocnul=zeros(d-1,p);
    minibloc=poly(d,d,x);
    FullA = cat(1,cat(2,blocpetit,blocnul',minibloc),cat(2,blocnul',blocpetit,minibloc));

    B(1:p)=bord_inf-beta_0*poly(d,0,x);
    Binf=B(1:p-1);
    FullB = cat(2,B,B);
    size(FullB')
    size(FullA)
    beta = FullA\FullB';
end

%tab = [gamma_0;gamma]
function y=bezier(x,tab)
    somme = zeros(length(x), 1);
    for i=1:length(tab)-1
        somme = somme + tab(i+1)*poly(length(tab)-1,i,x);
    end
    y = tab(1)*poly(length(tab)-1,0,x) + somme;
end
