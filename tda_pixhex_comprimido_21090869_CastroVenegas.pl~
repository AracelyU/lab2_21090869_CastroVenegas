:- module(tda_pixhex_comprimido_21090869_CastroVenegas, [
                                              pixhex_comprimido/5,
                                              esHexmapComprimido/1,
                                              obtCoordPixhexC/3, obtColorPixhexC/2,
                                             obtProfundidadPixhexC/2]).


% Descripción: Predicado que define como es un pixhex_comprimido
% Dominio: int X int X list X int X variable (list)
% Recorrido: pixhex_comprimido
% Tipo: Constructor
pixhex_comprimido(CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad, [CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255, integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255, integer(Profundidad), Profundidad >= 0.

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

% Descripción: Predicado que obtiene la coordenadas (X,Y) de un
% pixhex_comprimido
% Dominio: pixhex_comprimido X variable (int) X variable (int)
% Recorrido: int y int Tipo: Selector
obtCoordPixhexC(PixhexC , X, Y):-
    pixhex_comprimido(X,Y,_,_, PixhexC).


% Descripción: Predicado que obtiene el string hexadecimal de un
% pixhex_comprimido
% Dominio: pixhex_comprimido X variable (int)
% Recorrido: string
% Tipo: Selector
obtColorPixhexC(PixhexC , StringHex):-
    pixhex_comprimido(_,_,StringHex,_, PixhexC).

% Descripción: Predicado que obtiene la profundidad de un
% pixhex_comprimido
% Dominio: pixhex_comprimido X variable (int)
% Recorrido: int
% Tipo: Selector
obtProfundidadPixhexC(PixhexC , Profundidad):-
    pixhex_comprimido(_,_,_,Profundidad, PixhexC).
