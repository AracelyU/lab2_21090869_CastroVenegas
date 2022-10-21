:- module(tda_pixhex_21090869_CastroVenegas, [pixhex/5,
                                              pixhex_comprimido/5,
                                              esHexmap/1,
                                              esHexmapComprimido/1,
                                              obtCoordPixhex/3, obtColorPixhex/2,
                                             obtProfundidadPixhex/2]).


% Descripción: Predicado que define como es un pixhex
% Dominio: int X int X string X int X variable (list)
% Recorrido: pixhex
% Tipo: Constructor
pixhex(CoordX, CoordY, Hex, Profundidad, [CoordX, CoordY, Hex, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0, string(Hex), integer(Profundidad), Profundidad >= 0.

% Descripción: Predicado que define como es un pixhex_comprimido
% Dominio: int X int X list X int X variable (list)
% Recorrido: pixhex_comprimido
% Tipo: Constructor
pixhex_comprimido(CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad, [CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255, integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255, integer(Profundidad), Profundidad >= 0.

% Descripción: Consulta si los pixeles de una imagen son pixhex
% Dominio: Pixel (list)
% Recorrido: Boleano
% Tipo: Pertenencia
% Tipo de recursión: Cola, da resultado sin estados pendientes
esHexmap([]).
esHexmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4, pixhex(X,Y,H,D, Cabeza), pixhex(X,Y,H,D, P),
    P \== false -> esHexmap(Cola); false.

% Descripción: Consulta si los pixeles de una imagen son
% pixhex_comprimido
% Dominio: Pixel_comprimido (list)
% Recorrido(Boleano)
% Tipo: Pertenencia
% Tipo de recursión: Cola, da resultado sin estados pendientes
esHexmapComprimido([]):- !, false.
esHexmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4,
    pixhex_comprimido(X,Y,H,D, Cabeza), pixhex_comprimido(X,Y,H,D, P),
    P \== false -> true; esHexmapComprimido(Cola).


% Descripción: Predicado que obtiene la coordenadas (X,Y) de un pixhex
% Dominio: pixhex X variable (int) X variable (int)
% Recorrido: int y int
% Tipo: Selector
obtCoordPixhex(Pixhex , X, Y):-
    pixhex(X,Y,_,_, Pixhex).


% Descripción: Predicado que obtiene el string hexadecimal de un pixhex
% Dominio: pixhex X variable (int)
% Recorrido: string
% Tipo: Selector
obtColorPixhex(Pixhex , StringHex):-
    pixhex(_,_,StringHex,_, Pixhex).

% Descripción: Predicado que obtiene la profundidad de un pixhex
% Dominio: pixhex X variable (int)
% Recorrido: int
% Tipo: Selector
obtProfundidadPixhex(Pixhex , Profundidad):-
    pixhex(_,_,_,Profundidad, Pixhex).
