

:- module(tda_pixrgb_21090869_CastroVenegas, [pixrgb/7, pixrgb_comprimido/7,
                                             esPixmap/1, esPixmapComprimido/1]).

% Descripción: Define como es un pixrgb_d
% Dominio: CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad, Letra
% Recorrido: pixrgb_d
pixrgb(CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad, [CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255, integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255, integer(Profundidad), Profundidad >= 0.

pixrgb_comprimido(CoordX, CoordY, HexR, HexG, HexB, Profundidad, [CoordX, CoordY, HexR, HexG, HexB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0, string(HexR), string(HexG), string(HexB), integer(Profundidad), Profundidad >= 0.


% Descripción: Consulta si los pixeles de una imagen son pixrgb_d
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recusión: Cola, llama sin estados pendientes
esPixmap([]).
esPixmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6, pixrgb(X,Y,R,G,B,D, Cabeza), pixrgb(X,Y,R,G,B,D, P),
    P \== false -> esPixmap(Cola); false.


% Descripción: Consulta si los pixeles de una imagen son pixrgb_d comprimido
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recusión: Cola, llama sin estados pendientes
esPixmapComprimido([]):- !, false.
esPixmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6,
    pixrgb_comprimido(X,Y,R,G,B,D, Cabeza), pixrgb_comprimido(X,Y,R,G,B,D, P),
    P \== false -> true; esPixmapComprimido(Cola).


