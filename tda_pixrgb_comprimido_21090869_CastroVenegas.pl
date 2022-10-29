:- module(tda_pixrgb_comprimido_21090869_CastroVenegas, [pixrgb_comprimido/7,
                                              esPixmapComprimido/1,
                                              obtCoordPixrgbC/3,
                                              obtColorPixrgbC/4,
                                              obtProfundidadPixrgbC/2]).


% Descripción: Predicado que define como es un pixrgb_comprimido
% Dominio: int X int X string X string X string X int X variable (list)
% Recorrido: pixrgb_comprimido
% Tipo: Constructor
pixrgb_comprimido(CoordX, CoordY, HexR, HexG, HexB, Profundidad, [CoordX, CoordY, HexR, HexG, HexB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0, string(HexR), string(HexG), string(HexB), integer(Profundidad), Profundidad >= 0.

% Descripción: Predicado que verifica si los pixeles son
% pixrgb_comprimido
% Dominio: Pixel (list)
% Recorrido: Boleano
% Tipo: Pertenencia
% Tipo de recusión: Cola, da resultado sin estados
% pendientes
esPixmapComprimido([]):- !, false.
esPixmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6,
    pixrgb_comprimido(X,Y,R,G,B,D, Cabeza), pixrgb_comprimido(X,Y,R,G,B,D, P),
    P \== false -> true; esPixmapComprimido(Cola).

% Descripción: Predicado que obtiene la coordenadas (X,Y) de un
% pixrgb_comprimido
% Dominio: pixrgb_comprimido X variable (int) X variable (int)
% Recorrido: int y int Tipo: Selector
obtCoordPixrgbC(PixrgbC , X, Y):-
    pixrgb_comprimido(X,Y,_,_,_,_, PixrgbC).

% Descripción: Predicado que obtiene el string hexadecimal de un
% pixrgb_comprimido
% Dominio: pixrgb_comprimido X variable (int) X variable (int) X
% variable (int)
% Recorrido: int y int y int
% Tipo: Selector
obtColorPixrgbC(PixrgbC , ColorR, ColorG, ColorB):-
    pixrgb_comprimido(_,_,ColorR,ColorG,ColorB,_, PixrgbC).

% Descripción: Predicado que obtiene la profundidad de un
% pixrgb_comprimido
% Dominio: pixrgb_comprimido X variable (int)
% Recorrido: int
% Tipo: Selector
obtProfundidadPixrgbC(PixrgbC , Profundidad):-
    pixrgb_comprimido(_,_,_,_,_,Profundidad, PixrgbC).
