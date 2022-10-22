:- module(tda_pixhex_21090869_CastroVenegas, [pixhex/5,
                                              esHexmap/1,
                                              obtCoordPixhex/3, obtColorPixhex/2,
                                             obtProfundidadPixhex/2]).


% Descripci�n: Predicado que define como es un pixhex
% Dominio: int X int X string X int X variable (list)
% Recorrido: pixhex
% Tipo: Constructor
pixhex(CoordX, CoordY, Hex, Profundidad, [CoordX, CoordY, Hex, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0, string(Hex), integer(Profundidad), Profundidad >= 0.

% Descripci�n: Consulta si los pixeles de una imagen son pixhex
% Dominio: Pixel (list)
% Recorrido: Boleano
% Tipo: Pertenencia
% Tipo de recursi�n: Cola, da resultado sin estados pendientes
esHexmap([]).
esHexmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4, pixhex(X,Y,H,D, Cabeza), pixhex(X,Y,H,D, P),
    P \== false -> esHexmap(Cola); false.


% Descripci�n: Predicado que obtiene la coordenadas (X,Y) de un pixhex
% Dominio: pixhex X variable (int) X variable (int)
% Recorrido: int y int
% Tipo: Selector
obtCoordPixhex(Pixhex , X, Y):-
    pixhex(X,Y,_,_, Pixhex).


% Descripci�n: Predicado que obtiene el string hexadecimal de un pixhex
% Dominio: pixhex X variable (int)
% Recorrido: string
% Tipo: Selector
obtColorPixhex(Pixhex , StringHex):-
    pixhex(_,_,StringHex,_, Pixhex).

% Descripci�n: Predicado que obtiene la profundidad de un pixhex
% Dominio: pixhex X variable (int)
% Recorrido: int
% Tipo: Selector
obtProfundidadPixhex(Pixhex , Profundidad):-
    pixhex(_,_,_,Profundidad, Pixhex).
