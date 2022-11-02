:- module(tda_pixrgb_21090869_CastroVenegas, [pixrgb/7,
                                             esPixmap/1, obtCoordPixrgb/3, obtColorPixrgb/4,
                                             obtProfundidadPixrgb/2,
                                             colorProfundidadPixrgb/2]).

% Clausuras
% Dominio
% CoordX, CoordY, Profundidad = integer >= 0
% ColorR = 0 <= integer <= 255
% ColorG = 0 <= integer <= 255
% ColorB = 0 <= integer <= 255
% Pixel = pixrgb | pixbit | pixhex
% Pixrgb = pixrgb
%
% Predicados
% pixrgb{CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad}
% (aridad = 6)
% esPixmap{[Pixel]} (aridad = 1)
% obtCoordPixrgb{Pixrgb , CoordX, CoordY} (aridad = 3)
% obtColorPixrgb{Pixrgb , ColorR, ColorG, ColorB} (aridad = 4)
% obtProfundidadPixrgb{Pixrgb , Profundidad} (aridad = 2)
% colorProfundidadPixrgb{Pixrgb, String} (aridad = 2)
%
%
% Metas primarias
% pixrgb{CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad}
% esPixmap{[Pixel]}
% obtCoordPixrgb{Pixrgb , CoordX, CoordY}
% obtColorPixrgb{Pixrgb , ColorR, ColorG, ColorB}
% obtProfundidadPixrgb{Pixrgb , Profundidad}
% colorProfundidadPixrgb{Pixrgb, String}
%
% Metas secundarias: -
%
% ----------------------REPRESENTACIÓN--------------------------------
% El pixrgb se representa como una lista con
% (int X int X int X int X int X int) el cual contiene CoordX,
% CoordY, ColorR, ColorG, ColorB y Profundidad
% --------------------------------------------------------------------
%
% Reglas
%
% -------------------CONSTRUCTOR Y MODIFICADOR---------------------------
%
% Descripción: Predicado que define como es un pixrgb
% Dominio: int X int X int X int X int X int X variable (list)
% Recorrido: pixrgb
% Tipo: Constructor y modificador
pixrgb(CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad, [CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255, integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255, integer(Profundidad), Profundidad >= 0.

%----------------------------------PERTENENCIA--------------------------

% Descripción: Predicado que verifica si los pixeles son pixrgb
% Dominio: Pixel (list)
% Recorrido: Boleano, da false si la lista no es de Pixrgb
% Tipo: Pertenencia
% Tipo de recusión: Cola, da resultado sin estados pendientes
esPixmap([]).
esPixmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6,
    pixrgb(CoordX,CoordY,ColorR,ColorG,ColorB,Profundidad, Cabeza),
    pixrgb(CoordX,CoordY,ColorR,ColorG,ColorB,Profundidad, Pixrgb),
    Pixrgb \== false -> esPixmap(Cola); false.

%---------------------------------SELECTORES---------------------------

% Descripción: Predicado que obtiene la coordenadas (X,Y) de un pixrgb
% Dominio: Pixrgb X int X int
% Tipo: Selector
obtCoordPixrgb(Pixrgb , CoordX, CoordY):-
    pixrgb(CoordX,CoordY,_,_,_,_, Pixrgb).

% Descripción: Predicado que obtiene el string hexadecimal de un pixrgb
% Dominio: Pixrgb X int X int X int
% Tipo: Selector
obtColorPixrgb(Pixrgb , ColorR, ColorG, ColorB):-
    pixrgb(_,_,ColorR,ColorG,ColorB,_, Pixrgb).

% Descripción: Predicado que obtiene la profundidad de un pixrgb
% Dominio: Pixrgb X int
% Tipo: Selector
obtProfundidadPixrgb(Pixrgb , Profundidad):-
    pixrgb(_,_,_,_,_,Profundidad, Pixrgb).

% Descripción: Predicado que forma una string con colores y profundidad
% de un pixrgb
% Dominio: Pixrgb X string
% Tipo: Selector
colorProfundidadPixrgb(Pixrgb, String):-
    esPixmap([Pixrgb]),
    obtProfundidadPixrgb(Pixrgb, D), obtColorPixrgb(Pixrgb, R,G,B),
    string_concat("[", R, S), string_concat(S, " ", S2), string_concat(S2, G, S3),
    string_concat(S3, " ", S4), string_concat(S4, B, S5), string_concat(S5, " ", S6),
    string_concat(S6, D, S7), string_concat(S7, "]", String).
