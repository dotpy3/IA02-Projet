%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Projet IA02 P15         %
%      Chicago Stock Exchange      %
%  Eric Gourlaouen & Marie Kromwel %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Création du plateau de jeu initial%%
createPlateau(Plateau) :-
		Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2],
        ReserveJ1 = [],
        ReserveJ2 = [],
        random(Y),
        U is Y * 8,
        PositionT is round(U),
        generateB(Bourse),
        generateL(ListeM),
		createMarchandises(ListeM,ListeMDeux),
        createMarchandises2(ListeMDeux,Marchandises),!.



%%Cours initial de la bourse%
generateB(B):-
B = [[ble,7],[mais,6],[cacao,6],[sucre,6],[cafe,6],[riz,6]].

%%Quantité de chaque ressource%%
generateL(ListeM):-
ListeM = [[ble,6],[mais,6],[cacao,6],[sucre,6],[cafe,6],[riz,6]].

%%Fonction de répartition des marchandises sur le plateau%%
%%createMarchandises va générer une répartition aléatoire des ressources %%
%%tandis que createMarchandises2 va les parser entre les piles %%
createMarchandises([[ble,0],[mais,0],[cacao,0],[sucre,0],[cafe,0],[riz,0]],[]).
        
createMarchandises(U,M) :-
        repeat,
        generateRandom(Nombre),
        nth(Nombre,U,Ressource),
        checkRessource(Ressource),
        decrementationListe(U,NouveauU,Nombre),
        createMarchandises(NouveauU,M1), M = [Res|M1],
        nth(1,Ressource, Res).
        % on en prend un au hasard
        % on vérifie p.e. avec un checkRessource que la ressource n'est pas à 0
        % on décrémente
        % on remplit la pile
        % on rappelle createMarchandises
        %generateRessource(Res)
		
createMarchandises2([],[]).
		
createMarchandises2([X,Y,Z,T|Q],[[X,Y,Z,T]|M]) :-
	createMarchandises2(Q,M).
        
%%Fonction qui génère un nombre aléatoire compris entre 1 et 6 qui permet la génération de ressource aléatoirement%%
generateRandom(M) :- random(Y), U is Y * 5+1, M is round(U).

%%Fonction qui verifie si la ressource est bien disponible (chaque ressource n'étant disponible que 6 fois)%%
checkRessource(R) :- nth(2,R,N), N > 0.

%%Fonction qui décrémente le nombre de ressources restantes en fonction de celle choisie aléatoirement%%
decrementationListe(Ancienne,Nouvelle,Nombre) :-
        nth(Nombre, Ancienne, T),
        nth(2, T, U),
        nth(1, T, X),
        V is U-1,
        remplacer(Ancienne,Nombre, [X,V], Nouvelle).
		


        
%%%% FONCTIONS DE SERVICE %%%%

remplacer([_|Q],1,R,[R|Q]).
remplacer([T|Q],X,R,[T|Q2]):-
Y is X -1,
remplacer(Q,Y,R,Q2).

