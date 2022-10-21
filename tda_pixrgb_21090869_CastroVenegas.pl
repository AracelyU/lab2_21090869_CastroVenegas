:- module(tda_pixrgb_21090869_CastroVenegas, [pixrgb/7, pixrgb_comprimido/7,
                                             esPixmap/1, esPixmapComprimido/1,
                                             obtCoordPixrgb/3, obtColorPixrgb/4,
                                             obtProfundidadPixrgb/2]).


% DOMINIO
% CoordX = n�mero
% CoordY = n�mero
% ColorR = 0 >= n�mero >= 255
% ColorG = 0 >= n�mero >= 255
% ColorB = 0 >= n�mero >= 255
% Profundidad = n�mero
% HexR = string
% HexG = string
% HexB = string
% Pixel = pixrgb | pixbit | pixhex
%
% PREDICADOS
% pixrgb = {CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad}
% aridad = 6
% pixrgb_comprimido = {CoordX, CoordY, HexR, HexG, HexB, Profundidad}
% Profundidad} aridad = 6 esPixmap = {[Pixel]} aridad = 1
% esPixmapComprimido = {[Pixel]} aridad 1
%
%
% METAS
% Primaria
% pixrgb = (CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad)
%
% Secundaria
% pixrgb_comprimido = (CoordX, CoordY, ColorR, ColorG, Profundidad)
%
%
% CLAUSURAS
%
%
%
% REGLAS
% esPixmap([Pixel])
% esPixmapComprimido([Pixel])



% Descripci�n: Predicado que define como es un pixrgb
% Dominio: int X int X int X int X int X int X variable (list)
% Recorrido: pixrgb
% Tipo: Constructor
pixrgb(CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad, [CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255, integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255, integer(Profundidad), Profundidad >= 0.

% Descripci�n: Predicado que define como es un pixrgb_comprimido
% Dominio: int X int X string X string X string X int X variable (list)
% Recorrido: pixrgb_comprimido
% Tipo: Constructor
pixrgb_comprimido(CoordX, CoordY, HexR, HexG, HexB, Profundidad, [CoordX, CoordY, HexR, HexG, HexB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0, string(HexR), string(HexG), string(HexB), integer(Profundidad), Profundidad >= 0.

% Descripci�n: Predicado que verifica si los pixeles son pixrgb
% Dominio: Pixel (list)
% Recorrido: Boleano
% Tipo: Pertenencia
% Tipo de recusi�n: Cola, da resultado sin estados pendientes
esPixmap([]).
esPixmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6, pixrgb(X,Y,R,G,B,D, Cabeza), pixrgb(X,Y,R,G,B,D, P),
    P \== false -> esPixmap(Cola); false.

% Descripci�n: Predicado que verifica si los pixeles son
% pixrgb_comprimido
% Dominio: Pixel (list)
% Recorrido: Boleano
% Tipo: Pertenencia
% Tipo de recusi�n: Cola, da resultado sin estados
% pendientes
esPixmapComprimido([]):- !, false.
esPixmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6,
    pixrgb_comprimido(X,Y,R,G,B,D, Cabeza), pixrgb_comprimido(X,Y,R,G,B,D, P),
    P \== false -> true; esPixmapComprimido(Cola).

% Descripci�n: Predicado que obtiene la coordenadas (X,Y) de un pixrgb
% Dominio: pixrgb X variable (int) X variable (int)
% Recorrido: int y int
% Tipo: Selector
obtCoordPixrgb(Pixrgb , X, Y):-
    pixrgb(X,Y,_,_,_,_, Pixrgb).

% Descripci�n: Predicado que obtiene el string hexadecimal de un pixrgb
% Dominio: pixrgb X variable (int) X variable (int) X variable (int)
% Recorrido: int y int y int
% Tipo: Selector
obtColorPixrgb(Pixrgb , ColorR, ColorG, ColorB):-
    pixrgb(_,_,ColorR,ColorG,ColorB,_, Pixrgb).

% Descripci�n: Predicado que obtiene la profundidad de un pixhex
% Dominio: pixhex X variable (int)
% Recorrido: int
% Tipo: Selector
obtProfundidadPixrgb(Pixrgb , Profundidad):-
    pixrgb(_,_,_,_,_,Profundidad, Pixrgb).
