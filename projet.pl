%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Projet IA02 P15         %
%      Chicago Stock Exchange      %
%  Eric Gourlaouen & Marie Kromwel %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

beginGame :-
	introductionText(ListeChoix),
	waitUserInput(R,ListeChoix),
	beginSpecifiedGame(R).

introductionText(ListeChoix) :-
	write('/////////////////////////////'), nl,
	write('CHICAGO STOCK EXCHANGE'), nl,
	write('/////////////////////////////'), nl,
	nl, nl,
	ListeChoix = [0,1],
	write('(0) : ARRÊTER DE JOUER'), nl,
	write('(1) : JOUER JOUEUR CONTRE JOUEUR'), nl.

waitUserInput(R, ListeChoix) :-
	repeat,
	read(R),
	element(R, ListeChoix).

beginSpecifiedGame(0) :- write('Merci d\'avoir joué !'), !.

beginSpecifiedGame(1) :-
	%% JOUEUR CONTRE JOUEUR
	beginJoueurContreJoueur.

beginJoueurContreJoueur :-
	plateauDepart(P),
	playJoueurContreJoueur(P).

%% playJoueurContreJoueur :
%% - vérifie que la partie n est pas finie, sinon lance finPartie(P)
%% - détermine quel joueur doit jouer
%% lui affiche le plateau et lui demande de jouer
%% demande le nombre de cases
%% puis demande de sélectionner les marchandises gardées ou jetées
%% vérifie que le coup est possible
%% traite le coup
%% puis renvoie le nouveau plateau vers playJoueurContreJoueur

playJoueurContreJoueur(P) :-
	P = [[_,_],Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	endGame(P), !.

playJoueurContreJoueur(Plateau) :-
	Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	affichePlateau(Plateau),
	askCoup(Plateau,Coup), %% se charge de vérifier que le coup est possible
	jouer_coup(Plateau,Coup,NPlateau),
	playJoueurContreJoueur(NPlateau).

endGame(P) :-
	countPoints(P, ReserveJ1,PointsJ1),
	countPoints(P, ReserveJ2,PointsJ2),
	determineWinner(PointsJ1,PointsJ2,Winner),
	write('Félicitations au(x) gagnant(s) : '), write(Winner),nl,
	write('//////////////////////'),beginGame.

countPoints(P, [],0).
countPoints(Plateau, [T|Q], P) :-
	Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	countPoints(Plateau, Q,AnciensPoints),
	pointsAccordingTo(Bourse, T, PointsAccordes),
	P is AnciensPoints + PointsAccordes.

pointsAccordingTo([[R,Points]|S],R, Points) :- !.

pointsAccordingTo([R|Q],T,Pts) :- pointsAccordingTo(Q,T,Pts).
	
determinateJetonsObtenus(Marchandises, PositionTrader, Jetons) :-
		posAvant(Marchandises,PositionTrader,PosTrader1),
		posApres(Marchandises,PositionTrader,PosTrader2),
		nth(PosTrader1,Marchandises,PosAvantDonnee),
		nth(PosTrader2,Marchandises,PosApresDonnee),
		PosAvantDonnee = [T1|_],
		PosApresDonnee = [T2|_],
		Jetons = [T1,T2].

determineWinner(S, J, 'Joueur 1') :- S > J.
determineWinner(S, J, 'Joueur 2') :- S < J.
determineWinner(S, J, 'les deux joueurs ex-aequo') :- S = J.

askCoup(P,Coup) :-
	P = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	Coup = [Joueur,NbCases,Garde,Jete],
	repeat,
	write('Combien de cases souhaitez-vous sauter ?'), nl,
	read(NbCases),
	newPositionTrader(Marchandises, Coup, PositionT, NewPosT),
	determinateJetonsObtenus(Marchandises, NewPosT, JetonsObtenus),
	JetonsObtenus = [T,Q],
	write('Quel jeton souhaitez-vous jeter ? L autre jeton sera gardé.'), nl,
	write('Vous etes en position '),write(NewPosT),nl,
	write('Vous pouvez écrire : '),write(T),write(' et '),write(Q),nl,
	read(Jete),
	element(Jete, JetonsObtenus),
	other(Jete, JetonsObtenus, Garde),
	coupPossible(P,Coup).

other(T, [T,Q], Q).
other(T, [Q,T], Q).

%%Création du plateau de jeu initial%%
plateauDepart(Plateau) :-
	
		Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
        ReserveJ1 = [],
        ReserveJ2 = [],
        random(Y),
        U is Y * 8,
		V is U + 1,
        PositionT is round(V),
        generateB(Bourse),
        generateL(ListeM),
		randomJoueur(Joueur),
		createMarchandises(ListeM,ListeMDeux),
        createMarchandises2(ListeMDeux,Marchandises),!.
		
coupPossible(Plateau,Coup) :-
		checkJoueur(Plateau,Coup),
		checkDeplacement(Coup),
		Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
		newPositionTrader(Marchandises, Coup,PositionT,NewPositionT),
		determinateJetonsObtenus(Marchandises, NewPositionT, JetonsObtenus),
		verifJetons(Coup,JetonsObtenus).
		
jouer_coup(PlateauInitial, Coup, NouveauPlateau) :-
		PlateauInitial = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
		NouveauPlateau = [NMarchandises,NBourse,NPositionT2,NReserveJ1,NReserveJ2,NJoueur],
		changePlayer(Joueur,NJoueur),
		changeReserve(Joueur,ReserveJ1,ReserveJ2,NReserveJ1,NReserveJ2, Coup),
		changeValeur(Coup,Bourse,NBourse),
		newPositionTrader(Marchandises, Coup,PositionT,NPositionT),
		changeMarchandises(Marchandises,NPositionT,NMarchandises),
		checkPositionTrader(NMarchandises,NPositionT,NPositionT2).
		
checkPositionTrader(Marchandises,NPositionT,1) :-
		length(Marchandises,LengthM),
		LengthM < NPositionT, !.

checkPositionTrader(Marchandises,NPositionT,NPositionT).

%% changeMarchandises traite les piles de marchandises
%% puis supprime les piles vide

changeMarchandises(Marchandises,NouvellePosition,NouvellesMarch) :-
		posAvant(Marchandises,NouvellePosition,PosSuppr1),
		posApres(Marchandises,NouvellePosition,PosSuppr2),
		suppMarchandise(Marchandises,PosSuppr1,MarchTempo),
		suppMarchandise(MarchTempo,PosSuppr2,NouvellesMarchTemp),
		suppPilesVides(NouvellesMarchTemp,NouvellesMarch).

suppPilesVides([],[]):- !.

suppPilesVides([[]|Q1],Q2) :- suppPilesVides(Q1,Q2).

suppPilesVides([T|Q1],[T|Q2]) :- suppPilesVides(Q1,Q2).
		
nbMarchandises(Marchandises,Nb) :- length(Marchandises,Nb).
		
suppMarchandise([[T|Q1]|Q],1,[Q1|Q]) :- !.

suppMarchandise([T1|Q1],X,[T1|Q2]) :- X > 1, Y is X - 1, suppMarchandise(Q1,Y,Q2).
	
posAvant(Marchandises,1,PosDAvant) :- 
nbMarchandises(Marchandises,PosDAvant),!.
posAvant(_,Y,Z) :- Z is Y - 1.

posApres(Marchandises,Q,1) :- 
nbMarchandises(Marchandises,Q),!.
posApres(_,Y,Z) :- Z is Y + 1.

suppPilesVides([],[]).

suppPilesVides([[]|Q],Q) :- !.

suppPilesVides([T|Q1],[T|Q2]) :- suppPilesVides(Q1,Q2).
		
		
changeValeur(_,[],[]) :- !.

changeValeur([_,_,_,Intitule],[[Intitule,T2]|Q],[[Intitule,T3]|Q]) :-
		T3 is T2 - 1,!.
		
		

changeValeur(Coup,[T1|Q1],[T1|Q2]) :-
	changeValeur(Coup,Q1,Q2).
		
changeReserve('j1', RJ1,RJ2,NRJ1,RJ2,[_,_,Q,_]) :-
		append(RJ1,[Q],NRJ1).

changeReserve('j2', RJ1,RJ2,RJ1,NRJ2,[_,_,Q,_]) :-
		append(RJ2,[Q],NRJ2).
		
checkJoueur(Plateau,[JoueurCoup|_]) :-
		Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
		Joueur = JoueurCoup, !.
		
checkDeplacement(Coup) :-
		nth(2,Coup, N), integer(N), N > 0, N <4, !.
		
newPositionTrader(Marchandises, Coup, PosT, NewPosT) :-
	length(Marchandises,NbMarchandises),
	nth(2,Coup, N),
	PosTemp is N + PosT,
	changeModulo(PosTemp,NewPosT,NbMarchandises).
	
changeModulo(P,Q,Max) :-
	P > Max, !,
	Q is P - Max.

changeModulo(P,P,Max).
	
changePlayer('j1','j2').
changePlayer('j2','j1').
		
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
		length(Marchandises,NbMarchandises),
		printAllPile(NbMarchandises,Marchandises,PositionT),
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

printAllPile(1,Marchandises,PositionT) :- !,
		printPile(1,Marchandises,PositionT).

printAllPile(X,Marchandises,PositionT) :- X > 0,
		Y is X - 1,
		printAllPile(Y,Marchandises,PositionT),
		printPile(X,Marchandises,PositionT).
		
printPile(I,Marchandises,PositionT) :-
		nth(I, Marchandises, Pile), !, write('Pile '),write(I),write(' : '), nth(1, Pile, ResultatPile), write(ResultatPile), writePositionTrader(PositionT,I), nl.
	
		
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

element(Q, []) :- fail.
element(Q, [Q|_]).
element(Q, [T|R]) :- element(Q,R).

pop([T|Q],T,Q).

compter([],0) :- !.
compter([_|Q],X) :- compter(Q,Y), X is Y + 1.
