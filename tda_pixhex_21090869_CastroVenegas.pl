:- module(tda_pixhex_21090869_CastroVenegas, [pixhex/5,
                                              pixhex_comprimido/5,
                                              esHexmap/1,
                                              esHexmapComprimido/1,
                                              listHEX_RGB/2]).


% Descripción: Define como es un pixhex_d
% Dominio: CoordX, CoordY, Hex, Profundidad, Letra
% Recorrido: pixhex_d
% comentarios: los string son "", no ''
pixhex(CoordX, CoordY, Hex, Profundidad, [CoordX, CoordY, Hex, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0, string(Hex), integer(Profundidad), Profundidad >= 0.

pixhex_comprimido(CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad, [CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255, integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255, integer(Profundidad), Profundidad >= 0.

% Descripción: Consulta si los pixeles de una imagen son pixhex_d
% Dominio: Lista de pixeles
% Recorrido(Boleano)
% Tipo de recursión: Cola, llama sin estados pendientes
esHexmap([]).
esHexmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4, pixhex(X,Y,H,D, Cabeza), pixhex(X,Y,H,D, P),
    P \== false -> esHexmap(Cola); false.

% Descripción: Consulta si los pixeles de una imagen son pixhex_d comprimido
% Dominio: Lista de pixeles
% Recorrido(Boleano)
% Tipo de recursión: Cola, llama sin estados pendientes
esHexmapComprimido([]):- !, false.
esHexmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4,
    pixhex_comprimido(X,Y,H,D, Cabeza), pixhex_comprimido(X,Y,H,D, P),
    P \== false -> true; esHexmapComprimido(Cola).

% Descripción: Predicado que convierte un string pixhex en una lista de
% valores rgb
% Dominio: string X variable (list)
% Recorrido: list
% Tipo: Otras funciones
listHEX_RGB(String, [R, G, B]):-
    sub_string(String,1,_,4,S1),
    hexRGB(S1,R),
    sub_string(String,3,_,2,S2),
    hexRGB(S2,G),
    sub_string(String,5,_,0,S3),
    hexRGB(S3,B).

