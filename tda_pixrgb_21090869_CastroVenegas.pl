:- module(tda_pixrgb_21090869_CastroVenegas, [pixrgb/7,
                                             esPixmap/1, obtCoordPixrgb/3, obtColorPixrgb/4,
                                             obtProfundidadPixrgb/2,
                                             colorProfundidadPixrgb/2]).


% DOMINIO
% CoordX, CoordY, Profundidad = integer >= 0
% ColorR = 0 >= integer >= 255
% ColorG = 0 >= integer >= 255
% ColorB = 0 >= integer >= 255
% Pixel = pixrgb | pixbit | pixhex
%
% PREDICADOS
% pixrgb = {CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad}
% aridad = 6
%
%
% METAS
% Primaria
% pixrgb = (CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad)
%
% Secundaria
%
%
%
% CLAUSURAS
%
%
%
% REGLAS
% esPixmap([Pixel])
% esPixmapComprimido([Pixel])



% Descripción: Predicado que define como es un pixrgb
% Dominio: int X int X int X int X int X int X variable (list)
% Recorrido: pixrgb
% Tipo: Constructor
pixrgb(CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad, [CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255, integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255, integer(Profundidad), Profundidad >= 0.

% Descripción: Predicado que verifica si los pixeles son pixrgb
% Dominio: Pixel (list)
% Recorrido: Boleano
% Tipo: Pertenencia
% Tipo de recusión: Cola, da resultado sin estados pendientes
esPixmap([]).
esPixmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6, pixrgb(X,Y,R,G,B,D, Cabeza), pixrgb(X,Y,R,G,B,D, P),
    P \== false -> esPixmap(Cola); false.


% Descripción: Predicado que obtiene la coordenadas (X,Y) de un pixrgb
% Dominio: pixrgb X variable (int) X variable (int)
% Recorrido: int y int
% Tipo: Selector
obtCoordPixrgb(Pixrgb , X, Y):-
    pixrgb(X,Y,_,_,_,_, Pixrgb).

% Descripción: Predicado que obtiene el string hexadecimal de un pixrgb
% Dominio: pixrgb X variable (int) X variable (int) X variable (int)
% Recorrido: int y int y int
% Tipo: Selector
obtColorPixrgb(Pixrgb , ColorR, ColorG, ColorB):-
    pixrgb(_,_,ColorR,ColorG,ColorB,_, Pixrgb).

% Descripción: Predicado que obtiene la profundidad de un pixhex
% Dominio: pixhex X variable (int)
% Recorrido: int
% Tipo: Selector
obtProfundidadPixrgb(Pixrgb , Profundidad):-
    pixrgb(_,_,_,_,_,Profundidad, Pixrgb).

% Descripción: Predicado que forma una string con colores y profundidad
% de un pixrgb
% Dominio: pixrgb X variable (String)
% Recorrido: String
% Tipo: Selector
colorProfundidadPixrgb(Pixrgb, String):-
    esPixmap([Pixrgb]),
    obtProfundidadPixrgb(Pixrgb, D), obtColorPixrgb(Pixrgb, R,G,B),
    string_concat("[", R, S), string_concat(S, " ", S2), string_concat(S2, G, S3),
    string_concat(S3, " ", S4), string_concat(S4, B, S5), string_concat(S5, " ", S6),
    string_concat(S6, D, S7), string_concat(S7, "]", String).
