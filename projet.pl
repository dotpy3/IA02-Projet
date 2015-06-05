%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Projet IA02 P15         %
%      Chicago Stock Exchange      %
%  Eric Gourlaouen & Marie Kromwel %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Création du plateau de jeu initial%%
plateauDepart(Plateau) :-
		Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
        ReserveJ1 = [],
        ReserveJ2 = [],
        random(Y),
        U is Y * 8,
        PositionT is round(U),
        generateB(Bourse),
        generateL(ListeM),
		randomJoueur(Joueur),
		createMarchandises(ListeM,ListeMDeux),
        createMarchandises2(ListeMDeux,Marchandises),!.
		
coupPossible(Plateau,Coup) :-
		checkJoueur(Plateau,Coup),
		checkDeplacement(Coup),
		Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
		newPositionTrader(Coup,PositionT,NewPositionT),
		determinateJetonsObtenus(Marchandises, NewPositionT, JetonsObtenus),
		verifJetons(Coup,JetonsObtenus).
		
jouer_coup(PlateauInitial, Coup, NouveauPlateau) :-
		Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
		NouveauPlateau = [NMarchandises,NBourse,NPositionT,NReserveJ1,NReserveJ2,NJoueur],
		changePlayer(Joueur,NJoueur),
		changeReserve(Joueur,ReserveJ1,ReserveJ2,NReserveJ1,NReserveJ2, Coup),
		changeValeur(Coup,Bourse,NBourse),
		newPositionTrader(Coup,PositionT,NPositionT),
		changeMarchandises(Marchandises,NPositionT,NMarchandises).

changeMarchandises(Marchandises,NouvellePosition,NouvellesMarch) :-
		posAvant(NouvellePosition,PosSuppr1),
		posApres(NouvellePosition,PosSuppr2),
		suppMarchandise(Marchandises,PosSuppr1,MarchTempo),
		suppMarchandise(MarchTempo,PosSuppr2,NouvellesMarch).

pop([T|Q],T,Q).
		
suppMarchandise([T|Q],1,[T1,Q]) :- pop(T,M,T1),!.

suppMarchandise([T1|Q1],X,[T1|Q2]) :- X > 1, Y is X - 1, suppMarchandise(Q1,Y,Q2).
		
		
changeValeur(_,[],[]).
		
changeValeur([_,_,_,Intitule],[[Intitule,T2]|Q],[[Intitule,T3]|Q]) :-
		T3 is T2 - 1,!.

changeValeur(Coup,[T1|Q1],[T1|Q2]) :-
	changeValeur(Coup,Q1,Q2).
		
changeReserve('j1', RJ1,RJ2,NRJ1,RJ2,[_,_,Q,_]) :-
		append(RJ1,Q,NRJ1).

changeReserve('j2', RJ1,RJ2,RJ1,NRJ2,[_,_,Q,_]) :-
		append(RJ2,Q,NRJ2).
		
checkJoueur(Plateau,[JoueurCoup|_]) :-
		Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
		Joueur = JoueurCoup, !.
		
checkDeplacement(Coup) :-
		nth(2,Coup, N), integer(N), N > 0, N <4, !.
		
newPositionTrader(Coup, PosT, NewPosT) :-
	nth(2,Coup, N),
	PosModulo is PosT mod 8,
	NewPosT is PosModulo + N.
	
changePlayer('j1','j2').
changePlayer('j2','j1').
	
posAvant(1,8) :- !.
posAvant(Y,Z) :- Z = Y - 1.

posApres(8,1) :- !.
posApres(Y,Z) :- Z = Y + 1.
	
determinateJetonsObtenus(Marchandises, PositionTrader, Jetons) :-
		posAvant(PositionTrader,PosTrader1),
		posApres(PositionTrader,PosTrader2),
		Jetons = [PosAvant,PosApres].
		
verifJetons([_,_,Jeton1,Jeton2],[JetonO1,JetonO2]) :-
		Jeton1 = JetonO1,
		Jeton2 = JetonO2.
		
verifJetons([_,_,Jeton1,Jeton2],[JetonO1,JetonO2]) :-
		Jeton1 = JetonO2,
		Jeton2 = JetonO1.
		
affichePlateau(Plateau) :-
		Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
		print('///////////////////'), nl,
		write('/ ETAT DU PLATEAU /'), nl,
		write('///////////////////'), nl,
		write('Pile 1 : '), nth(1, Marchandises, Pile1), nth(1, Pile1, ResultatPile1), write(ResultatPile1), writePositionTrader(PositionT,1), nl,
		write('Pile 2 : '), nth(2, Marchandises, Pile2), nth(1, Pile2, ResultatPile2), write(ResultatPile2), writePositionTrader(PositionT,2), nl,
		write('Pile 3 : '), nth(3, Marchandises, Pile3), nth(1, Pile3, ResultatPile3), write(ResultatPile3), writePositionTrader(PositionT,3), nl,
		write('Pile 4 : '), nth(4, Marchandises, Pile4), nth(1, Pile4, ResultatPile4), write(ResultatPile4), writePositionTrader(PositionT,4), nl,
		write('Pile 5 : '), nth(5, Marchandises, Pile5), nth(1, Pile5, ResultatPile5), write(ResultatPile5), writePositionTrader(PositionT,5), nl,
		write('Pile 6 : '), nth(6, Marchandises, Pile6), nth(1, Pile6, ResultatPile6), write(ResultatPile6), writePositionTrader(PositionT,6), nl,
		write('Pile 7 : '), nth(7, Marchandises, Pile7), nth(1, Pile7, ResultatPile7), write(ResultatPile7), writePositionTrader(PositionT,7), nl,
		write('Pile 8 : '), nth(8, Marchandises, Pile8), nth(1, Pile8, ResultatPile8), write(ResultatPile8), writePositionTrader(PositionT,8), nl,
		nl,
		print('/////////////////////'), nl,
		write('/ ETAT DE LA BOURSE /'), nl,
		write('/////////////////////'), nl,
		nth(1,Bourse,MarchEtu1), nth(1,MarchEtu1,Nom1), nth(2,MarchEtu1,Qte1), write(Nom1), write(' : '), write(Qte1), nl,
		nth(2,Bourse,MarchEtu2), nth(1,MarchEtu2,Nom2), nth(2,MarchEtu2,Qte2), write(Nom2), write(' : '), write(Qte2), nl,
		nth(3,Bourse,MarchEtu3), nth(1,MarchEtu3,Nom3), nth(2,MarchEtu3,Qte3), write(Nom3), write(' : '), write(Qte3), nl,
		nth(4,Bourse,MarchEtu4), nth(1,MarchEtu4,Nom4), nth(2,MarchEtu4,Qte4), write(Nom4), write(' : '), write(Qte4), nl,
		nth(5,Bourse,MarchEtu5), nth(1,MarchEtu5,Nom5), nth(2,MarchEtu5,Qte5), write(Nom5), write(' : '), write(Qte5), nl,
		nth(6,Bourse,MarchEtu6), nth(1,MarchEtu6,Nom6), nth(2,MarchEtu6,Qte6), write(Nom6), write(' : '), write(Qte6), nl,
		nl,
		print('C\'est à '), write(Joueur), print(' de jouer.'),
		!.
		
writePositionTrader(PositionTrader,PositionTrader) :-
	write(' <= TRADER'), !.

writePositionTrader(PositionTrader,NumPile) :-	!.
		
		
%%Cours initial de la bourse%
generateB(B):-
B = [[ble,7],[mais,6],[cacao,6],[sucre,6],[cafe,6],[riz,6]].

%%Quantité de chaque ressource%%
generateL(ListeM):-
ListeM = [[ble,6],[mais,6],[cacao,6],[sucre,6],[cafe,6],[riz,6]].

%% Fonction de génération d'un joueur, qui prend une valeur 'j1' ou 'j2'. %%
randomJoueur(J) :-
	random(Nombre),
	NombreArrondi is round(Nombre),
	attributeJoueur(NombreArrondi, J), !.

attributeJoueur(0, J) :- J = 'j1'.
attributeJoueur(1, J) :- J = 'j2'.

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

