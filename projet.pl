vble([ble,7]).
vcafe([cafe,6]).
vcacao([cacao,6]).
vmais([mais,6]).
vsucre([sucre,6]).
vriz([riz,6]).

plateau(marchandises(X), bourse(Y), positiontrader(Y), reservejoueur1(Y), reservejoueur2(Y)) :- X=7,Y=6.
marchandises([ble,cafe,cacao,mais,sucre,riz]).
bourse([vble(X),vcafe(X),vcacao(X),vmais(X),vsucre(X),vriz(X)]).
positiontrader(pile1).
reservejoueur1([ble]).
reservejoueur2([ble]).

afficheTableau(X) :- plateau(X), print(X).
