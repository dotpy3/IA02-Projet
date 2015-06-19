%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Projet IA02 P15         %
%      Chicago Stock Exchange      %
%  Eric Gourlaouen & Marie Kromwel %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%% Lancement du jeu %%%%%%%%%%%
beginGame :-
	introductionText(ListeChoix),
	waitUserInput(R,ListeChoix),
	beginSpecifiedGame(R).

introductionText(ListeChoix) :-
	write('/////////////////////////////'), nl,
	write('   CHICAGO STOCK EXCHANGE'), nl,
	write('/////////////////////////////'), nl,
	nl, nl,
	ListeChoix = [0,1,2,3],
	write('(0) : Arreter de jouer'), nl,
	write('(1) : Commencer une partie Joueur VS Joueur'), nl,
	write('(2) : Commencer une partie Joueur VS Ordinateur'), nl,
	write('(3) : Simuler une partie Ordinateur VS Ordinateur'), nl.

%%% Recupere le choix de l'utilisateur %%%
waitUserInput(R, ListeChoix) :-
	repeat,
	read(R),
	element(R, ListeChoix).

%%% Arret du jeu %%%
beginSpecifiedGame(0) :- write('Merci d\'avoir joué !'), !.

%%% Lancement partie J VS J %%%
beginSpecifiedGame(1) :-
	%% JOUEUR CONTRE JOUEUR
	beginJoueurContreJoueur.
	
%%% Lancement partie J VS IA %%%
beginSpecifiedGame(2) :-
	%% JOUEUR CONTRE JOUEUR
	beginJoueurContreIA.
	
%%% Lancement partie IA VS IA %%%	
beginSpecifiedGame(3) :-
	%% JOUEUR CONTRE JOUEUR
	beginIAContreIA.
	
%%% Determine le gagnant, affiche la fin de jeu %%%
endGame(P) :-
	P  = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	countPoints(P, ReserveJ1,PointsJ1),
	countPoints(P, ReserveJ2,PointsJ2),
	determineWinner(PointsJ1,PointsJ2,Winner),
	write('Félicitations au(x) gagnant(s) : '), write(Winner),nl,
	write('////////////////////////////////////////////'),beginGame.

%%% Compte les points des joueurs en fonction du plateau passé en paramètre %%%
countPoints(P, [],0).
countPoints(Plateau, [T|Q], P) :-
	Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	countPoints(Plateau, Q,AnciensPoints),
	pointsAccordingTo(Bourse, T, PointsAccordes),
	P is AnciensPoints + PointsAccordes.

%%% Associe des points à une ressource en fonction de sa valeur dans la bourse %%%
pointsAccordingTo([[R,Points]|S],R, Points) :- !.
pointsAccordingTo([R|Q],T,Pts) :- pointsAccordingTo(Q,T,Pts).

%%% Determine les jetons obtenus par un joueur en fonction de la position du trader et du contenu des piles %%%	
determinateJetonsObtenus(Marchandises, PositionTrader, Jetons) :-
		posAvant(Marchandises,PositionTrader,PosTrader1),
		posApres(Marchandises,PositionTrader,PosTrader2),
		nth(PosTrader1,Marchandises,PosAvantDonnee),
		nth(PosTrader2,Marchandises,PosApresDonnee),
		PosAvantDonnee = [T1|_],
		PosApresDonnee = [T2|_],
		Jetons = [T1,T2].

%%% Determine et affiche le gagnant en fonction des points de chacun %%%
determineWinner(S, J, 'Joueur 1') :- S > J.
determineWinner(S, J, 'Joueur 2') :- S < J.
determineWinner(S, J, 'les deux joueurs ex-aequo') :- S = J.

%%% Demande a l'utilisateur le coup qu'il souhaite realise %%%
askCoup(P,Coup) :-
	P = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	Coup = [Joueur,NbCases,Garde,Jete],
	repeat,
	write('Combien de cases souhaitez-vous sauter ?'), nl,
	read(NbCases),
	newPositionTrader(Marchandises, Coup, PositionT, NewPosT), % determine la nouvelle position du trader en fonction du coup
	determinateJetonsObtenus(Marchandises, NewPosT, JetonsObtenus), % recupere les jetons obtenus en fonction du coup
	JetonsObtenus = [T,Q],
	write('Quel jeton souhaitez-vous jeter ? L autre jeton sera gardé.'), nl,
	write('Vous etes en position '),write(NewPosT),nl, %% Affiche le n° de position du trader
	write('Vous pouvez écrire : '),write(T),write(' et '),write(Q),nl, %% Donne les choix possible de ressources
	read(Jete), %% Lit le jeton jete
	element(Jete, JetonsObtenus),
	other(Jete, JetonsObtenus, Garde),
	coupPossible(P,Coup). %% verifie que le coup est possible

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
	
%%% Verifie qu'un coup est possible %%%
coupPossible(Plateau,Coup) :-
	checkJoueur(Plateau,Coup),
	checkDeplacement(Coup),
	Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	newPositionTrader(Marchandises, Coup,PositionT,NewPositionT),
	determinateJetonsObtenus(Marchandises, NewPositionT, JetonsObtenus),
	verifJetons(Coup,JetonsObtenus).
		
%%% Joue un coup et renvoie le plateau apres ce coup %%%
jouer_coup(PlateauInitial, Coup, NouveauPlateau) :-
	PlateauInitial = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	NouveauPlateau = [NMarchandises,NBourse,NPositionT2,NReserveJ1,NReserveJ2,NJoueur],
	changePlayer(Joueur,NJoueur),
	changeReserve(Joueur,ReserveJ1,ReserveJ2,NReserveJ1,NReserveJ2, Coup),
	changeValeur(Coup,Bourse,NBourse),
	newPositionTrader(Marchandises, Coup,PositionT,NPositionT),
	changeMarchandises(Marchandises,NPositionT,NMarchandises),
	checkPositionTrader(NMarchandises,NPositionT,NPositionT2).

%%% Verifie que la position du trader est correcte %%%		
checkPositionTrader(Marchandises,NPositionT,1) :-
	length(Marchandises,LengthM),
	LengthM < NPositionT, !.
checkPositionTrader(Marchandises,NPositionT,NPositionT).

%%% Supprime les marchandises en fonction de la position du trader puis supprime les piles vides %%%
changeMarchandises(Marchandises,NouvellePosition,NouvellesMarch) :-
	posAvant(Marchandises,NouvellePosition,PosSuppr1),
	posApres(Marchandises,NouvellePosition,PosSuppr2),
	suppMarchandise(Marchandises,PosSuppr1,MarchTempo),
	suppMarchandise(MarchTempo,PosSuppr2,NouvellesMarchTemp),
	suppPilesVides(NouvellesMarchTemp,NouvellesMarch).

%%% Supprime les piles vides %%%
suppPilesVides([],[]):- !.
suppPilesVides([[]|Q1],Q2) :- suppPilesVides(Q1,Q2).
suppPilesVides([T|Q1],[T|Q2]) :- suppPilesVides(Q1,Q2).

%%% Redefinition du predicat length %%%	
nbMarchandises(Marchandises,Nb) :- length(Marchandises,Nb).
		
%%% Supprime une marchandise dont la position est placee en parametre %%%
suppMarchandise([[T|Q1]|Q],1,[Q1|Q]) :- !.
suppMarchandise([T1|Q1],X,[T1|Q2]) :- X > 1, Y is X - 1, suppMarchandise(Q1,Y,Q2).

%%% Donne la position qui précède une position donnee en fonction de la taille de la liste de marchandise %%%	
posAvant(Marchandises,1,PosDAvant) :- 
nbMarchandises(Marchandises,PosDAvant),!.
posAvant(_,Y,Z) :- Z is Y - 1.

%%% Donne la position qui suit une position donnee en fonction de la taille de la liste de marchandise %%%	
posApres(Marchandises,Q,1) :- 
nbMarchandises(Marchandises,Q),!.
posApres(_,Y,Z) :- Z is Y + 1.

%%% Modifie une valeur dans une liste %%%
changeValeur(_,[],[]) :- !.
changeValeur([_,_,_,Intitule],[[Intitule,T2]|Q],[[Intitule,T3]|Q]) :-
	T3 is T2 - 1,!.
changeValeur(Coup,[T1|Q1],[T1|Q2]) :-
	changeValeur(Coup,Q1,Q2).

%%% Ajoute une ressource a la reserve d'un joueur donne %%%
changeReserve('j1', RJ1,RJ2,NRJ1,RJ2,[_,_,Q,_]) :-
		append(RJ1,[Q],NRJ1).
changeReserve('j2', RJ1,RJ2,RJ1,NRJ2,[_,_,Q,_]) :-
		append(RJ2,[Q],NRJ2).

%%% Verifie que le coup correspond bien au joueur dont c'est le tour %%%		
checkJoueur(Plateau,[JoueurCoup|_]) :-
		Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
		Joueur = JoueurCoup, !.

%%% Verifie qu'un deplacement est possible (compris entre 1 et 3 (inclus)) %%%
checkDeplacement(Coup) :-
		nth(2,Coup, N), integer(N), N > 0, N <4, !.

%%% Donne la nouvelle position du trader apres un coup donne %%%
newPositionTrader(Marchandises, Coup, PosT, NewPosT) :-
	length(Marchandises,NbMarchandises),
	nth(2,Coup, N),
	PosTemp is N + PosT,
	changeModulo(PosTemp,NewPosT,NbMarchandises).

%%% Verifie que la position est bien inferieure ou egale au nombre de pile, donne un nombre coherent sinon %%%	
changeModulo(P,Q,Max) :-
	P > Max, !,
	Q is P - Max.
changeModulo(P,P,Max).

%%% Change le joueur dont c'est le tour %%%	
changePlayer('j1','j2').
changePlayer('j2','j1').

%%% Verifie que les jetons d'un coup sont bien les jetons obtenus %%%		
verifJetons([_,_,Jeton1,Jeton2],[JetonO1,JetonO2]) :-
	Jeton1 = JetonO1,
	Jeton2 = JetonO2.
verifJetons([_,_,Jeton1,Jeton2],[JetonO1,JetonO2]) :-
	Jeton1 = JetonO2,
	Jeton2 = JetonO1.

%%% Affiche le plateau %%%	
affichePlateau(Plateau) :-
	Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	print('/////////////////////'), nl,
	write('// ETAT DU PLATEAU //'), nl,
	write('/////////////////////'), nl,
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
	print('C\'est a '), write(Joueur), print(' de jouer.'),
	!.

%%% Fait appel a la fonction printPile sur chacune des piles %%%
printAllPile(1,Marchandises,PositionT) :- !,
	printPile(1,Marchandises,PositionT).
printAllPile(X,Marchandises,PositionT) :- X > 0,
	Y is X - 1,
	printAllPile(Y,Marchandises,PositionT),
	printPile(X,Marchandises,PositionT).

%%% Affiche le 1er element d'une pile et le trader si telle est sa position %%%
printPile(I,Marchandises,PositionT) :-
	nth(I, Marchandises, Pile), 
	!, 
	write('Pile '),
	write(I),
	write(' : '), 
	nth(1, Pile, ResultatPile), % 1er element
	write(ResultatPile), 
	writePositionTrader(PositionT,I), nl.

%%% Affiche trader si sa position correspond a la position passee en parametre %%%	
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


%%% Genere une répartition aléatoire des ressources %%%
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
        % on vérifie avec checkRessource que la ressource n'est pas à 0
        % on décrémente
        % on remplit la pile
        % on rappelle createMarchandises

%%% Reparti les ressources generes aleatoirement sur les différentes piles %%%	
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

%%%%%%%%%%%%%%%% Intelligence Artificielle %%%%%%%%%%%%%%%%

%%% Calcul le meilleur coup en fonction d'un plateau donne %%% 
meilleur_coup(P,j1,C) :-
	%Determiner coups possibles
	coupsPossibles(P,ListeCoupsP),
	ListeCoupsP=[C1,C2,C3,C4,C5,C6],
	%Genere la liste des plateaux correspondants aux coups
	genereListPlateau(P,ListeCoupsP,PP),
	PP=[P1,P2,P3,P4,P5,P6],
	P1 = [_,_,_,ReserveJ11,_,_],
	P2 = [_,_,_,ReserveJ12,_,_],
	P3 = [_,_,_,ReserveJ13,_,_],
	P4 = [_,_,_,ReserveJ14,_,_],
	P5 = [_,_,_,ReserveJ15,_,_],
	P6 = [_,_,_,ReserveJ16,_,_],
	countPoints(P1, ReserveJ11,Score1),
	countPoints(P2, ReserveJ12,Score2),
	countPoints(P3, ReserveJ13,Score3),	
	countPoints(P4, ReserveJ14,Score4),
	countPoints(P5, ReserveJ15,Score5),
	countPoints(P6, ReserveJ16,Score6),
	Scores=[Score1,Score2,Score3,Score4,Score5,Score6],
	max(Scores,M),
	nth(X,Scores,M),
	nth(X,ListeCoupsP,C).	 	

meilleur_coup(P,j2,C) :-
	%Determiner coups possibles
	coupsPossibles(P,ListeCoupsP),
	ListeCoupsP=[C1,C2,C3,C4,C5,C6],
	genereListPlateau(P,ListeCoupsP,PP),
	PP=[P1,P2,P3,P4,P5,P6],
	P1 = [_,_,_,_,ReserveJ21,_],
	P2 = [_,_,_,_,ReserveJ22,_],
	P3 = [_,_,_,_,ReserveJ23,_],
	P4 = [_,_,_,_,ReserveJ24,_],
	P5 = [_,_,_,_,ReserveJ25,_],
	P6 = [_,_,_,_,ReserveJ26,_],
	countPoints(P1, ReserveJ21,Score1),
	countPoints(P2, ReserveJ22,Score2),
	countPoints(P3, ReserveJ23,Score3),	
	countPoints(P4, ReserveJ24,Score4),
	countPoints(P5, ReserveJ25,Score5),
	countPoints(P6, ReserveJ26,Score6),
	Scores=[Score1,Score2,Score3,Score4,Score5,Score6],
	max(Scores,M),
	nth(X,Scores,M),
	nth(X,ListeCoupsP,C).	

%%% Genere une liste de plateaux possibles en fonction d une liste de coups possibles %%% 
genereListPlateau(P,[],[]):-!.
genereListPlateau(P,ListeCP,[P1|ListePP]):-
	ListeCP = [T|Q],
	jouer_coup(P,T,P1), 
	genereListPlateau(P,Q,ListePP).

%%% Calculs les coups possibles en fonction de l'etat d'un plateau donne %%%
coupsPossibles(P,CoupsP):-
	P = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	%cherche tous les déplacements depuis la positions actuelle
	findDeplacement(Marchandises,PositionT,ListeDep),
	%creer des coups en fonction des positions trouvées et des jetons dispo
	genereCoups(P,ListeDep,CoupsP).

%%% Recupere les deplacements possibles en fonction d'une position donnee %%%
findDeplacement(Marchandises,PosA,Res):-
	%retourne liste des deplacement possibles
	Res = [P1,P2,P3],
	length(Marchandises,NbMarchandises),
	P1T is PosA + 1,
	changeModulo(P1T,P1,NbMarchandises),
	P2T is PosA + 2,
	changeModulo(P2T,P2,NbMarchandises),
	P3T is PosA + 3,
	changeModulo(P3T,P3,NbMarchandises).

%%% Genere des coups en fonction des plateaux possibles %%%
genereCoups(P,[P1,P2,P3],[Coup1A,Coup1B,Coup2A,Coup2B,Coup3A,Coup3B]):-
	P = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	determinateJetonsObtenus(Marchandises, P1, JetonsObtenus1),
	JetonsObtenus1 = [Jete1,Garde1],
	Coup1A=[Joueur,1,Garde1,Jete1],
	Coup1B=[Joueur,1,Jete1,Garde1],
	determinateJetonsObtenus(Marchandises, P2, JetonsObtenus2),
	JetonsObtenus2 = [Jete2,Garde2],
	Coup2A=[Joueur,2,Garde2,Jete2],
	Coup2B=[Joueur,2,Jete2,Garde2],
	determinateJetonsObtenus(Marchandises, P3, JetonsObtenus3),
	JetonsObtenus3 = [Jete3,Garde3],	
	Coup3A=[Joueur,3,Garde3,Jete3],
	Coup3B=[Joueur,3,Jete3,Garde3].

%%% Calcul le score d'un joueur donne en fonction d'un plateau donne %%%
score(P,ReserveJ1,ReserveJ2,'j1',ScoreR) :-
	countPoints(P, ReserveJ1,PointsJ1),
	countPoints(P, ReserveJ2,PointsJ2),
	ScoreR is PointsJ1 - PointsJ2.
score(P,ReserveJ1,ReserveJ2,'j2',Score) :-
	countPoints(P, ReserveJ1,PointsJ1),
	countPoints(P, ReserveJ2,PointsJ2),
	Score is PointsJ2 - PointsJ1.

%%%%%%%%%%%%%%%% Joueur Contre Joueur %%%%%%%%%%%%%%%%
beginJoueurContreJoueur :-
	plateauDepart(P), %Initialise le plateau
	playJoueurContreJoueur(P). %Lance le jeu

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
	jouer_coup(Plateau,Coup,NPlateau), %% joue le coup et recupere le nouveau plateau
	playJoueurContreJoueur(NPlateau). %% appel récursif

%%%%%%%%%%%%%%%% Joueur Contre IA %%%%%%%%%%%%%%%%
beginJoueurContreIA :-
	plateauDepart(P),
	playJoueurContreIA(P).

playJoueurContreIA(P) :-
	P = [[_,_],Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	endGame(P), !.

playJoueurContreIA([Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,j1]) :-
	Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,j1],
	affichePlateau(Plateau),
	askCoup(Plateau,Coup), 
	jouer_coup(Plateau,Coup,NPlateau),
	playJoueurContreIA(NPlateau).

playJoueurContreIA([Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,j2]) :-
	Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,j2],
	meilleur_coup(Plateau,j2,Coup),
	jouer_coup(Plateau,Coup,NPlateau),
	playJoueurContreIA(NPlateau).
	
%%%%%%%%%%%%%%%% IA Contre IA %%%%%%%%%%%%%%%%
beginIAContreIA :-
	plateauDepart(P),
	playIAContreIA(P).

playIAContreIA(P) :-
	P = [[_,_],Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	endGame(P), !.

playIAContreIA(Plateau) :-
	Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	affichePlateau(Plateau),
	meilleur_coup(Plateau,Joueur,Coup),
	jouer_coup(Plateau,Coup,NPlateau),
	playIAContreIA(NPlateau).

%%%% FONCTIONS DE SERVICE %%%%

%%% Calcul le maximum d'une liste %%%
max([X],X).
max([X|L],X) :- max(L,M), X > M.
max([X|L],M) :- max(L,M), X =< M.

%%% Recupere l element non choisi dans une liste a deux elements %%%
other(T, [T,Q], Q). 
other(T, [Q,T], Q).

%%% Remplace un element dans une liste %%%
remplacer([_|Q],1,R,[R|Q]).
remplacer([T|Q],X,R,[T|Q2]):-
Y is X -1,
remplacer(Q,Y,R,Q2).

%%% Verifie si un element est dans une liste %%%
element(Q, []) :- fail.
element(Q, [Q|_]).
element(Q, [T|R]) :- element(Q,R).

%%% Retire le premier element d une liste %%%
pop([T|Q],T,Q).

	
%%%%%%%%%%%%%%%% Implementation Minimax %%%%%%%%%%%%%%%%
minimax(Plateau, _, _, Retour, JoueurMinimax) :-
	Plateau = [[P,Q],Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	!,
	score(Plateau,ReserveJ1,ReserveJ2,JoueurMinimax,Retour).
	
minimax(Plateau, 0, _, Retour, JoueurMinimax) :- !,
	Plateau = [Marchandises,Bourse,PositionT,ReserveJ1,ReserveJ2,Joueur],
	score(Plateau,ReserveJ1,ReserveJ2,JoueurMinimax,Retour).

minimax(Plateau, X, 'true', Retour, JoueurMinimax) :-
	coupsPossibles(Plateau,ListeCoups),
	Y is X - 1,
	getMaxListeCoups(Plateau, ListeCoups,Y,Retour,JoueurMinimax,'true').

minimax(Plateau, X, 'false', Retour, JoueurMinimax) :-
	coupsPossibles(Plateau,ListeCoups),
	Y is X - 1,
	getMinListeCoups(Plateau, ListeCoups,Y,Retour,JoueurMinimax,'false').

getMaxListeCoups(Plateau, ListeCoups,Y,Retour,Joueur,maximizingPlayer) :-
	transferValue(ListeCoups,Y,ListeCoupsValuee,Joueur,Plateau),
	getMaxOfList(ListeCoupsValuee,Retour).

getMinListeCoups(Plateau, ListeCoups,Y,Retour,Joueur,maximizingPlayer) :-
	transferValue(ListeCoups,Y,ListeCoupsValuee,Joueur,Plateau),
	getMinOfList(ListeCoupsValuee,Retour).

changeMaximizing('true','false').
changeMaximizing('false','true').

transferValue([Q|[]],Y,[Q,ScoreDonne],Joueur,maximizingPlayer,Plateau) :- !,
	changeMaximizing(maximizingPlayer,newMaximizing),
	jouer_coup(Plateau,Q,NouveauPlateau),
	minimax(NouveauPlateau,Y,ScoreDonne,newMaximizing).

transferValue([Q|T],Y,[[Q,ScoreDonne]|AutresCoupsValues],Joueur,maximizingPlayer,Plateau) :- 
	transferValue(T,Y,AutresCoupsValues,Joueur,maximizingPlayer,Plateau),
	changeMaximizing(maximizingPlayer,newMaximizing),
	jouer_coup(Plateau,Q,NouveauPlateau),
	minimax(NouveauPlateau,Y,ScoreDonne,newMaximizing).
	
getMaxOfList([Q|[]],Q) :- !. %% FONCTIONNE

getMaxOfList([[Intitule,Valeur]|Q], [Intitule,Valeur]) :- %% FONCTIONNE
	getMaxOfList(Q,[IntitulePrecedent,ValeurPrecedente]),
	Valeur > ValeurPrecedente, !.

getMinOfList([_|Q], ValeurPrecedente) :- %% FONCTIONNE
	getMinOfList(Q,ValeurPrecedente).
	
getMinOfList([Q|[]],Q) :- !. %% FONCTIONNE

getMinOfList([[Intitule,Valeur]|Q], [Intitule,Valeur]) :- %% FONCTIONNE
	getMinOfList(Q,[IntitulePrecedent,ValeurPrecedente]),
	Valeur < ValeurPrecedente, !.

getMinOfList([_|Q], ValeurPrecedente) :- %% FONCTIONNE
	getMinOfList(Q,ValeurPrecedente).
        
