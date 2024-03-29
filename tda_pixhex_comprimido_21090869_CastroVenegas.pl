
% crear el módulo
:- module(tda_pixhex_comprimido_21090869_CastroVenegas, [
                                              pixhex_comprimido/5,
                                              esHexmapComprimido/1,
                                              obtCoordPixhexC/3, obtColorPixhexC/2,
                                             obtProfundidadPixhexC/2]).

% Clausuras
% Dominio:
% CoordX, CoordY, Profundidad: integer >= 0
% HexL: list [ColorR, ColorG, ColorB]
% PixhexC: pixhex_comprimido
% Pixel = pixrgb | pixbit | pixhex | PixhexC
% ColorR, ColorG, ColorB = 0 <= integer <= 255
%
%
% Predicados:
% pixhex{CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad, [CoordX,
% CoordY, [ColorR, ColorG, ColorB], Profundidad]} (aridad = 5)
% esHexmapComprimido{[Pixel]} (aridad = 1)
% obtCoordHexmapC{Pixhex ,CoordX, CoordY} (aridad = 3)
% obtColorHexmapC{Pixhex , HexL} (aridad = 2)
% obtProfundidadHexmapC{Pixhex , Profundidad} (aridad = 2)
%
% Metas primarias: -
%
% Metas secundarias:
% pixhex{CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad, [CoordX,
% CoordY, [ColorR, ColorG, ColorB], Profundidad]}
% esHexmapComprimido{[Pixel]}
% obtCoordHexmapC{Pixhex ,CoordX, CoordY}
% obtColorHexmapC{Pixhex , HexL}
% obtProfundidadHexmapC{Pixhex , Profundidad}
%
% ----------------------REPRESENTACIÓN--------------------------------
% El pixhex_comprimido se representa como una lista con
% (int X int X list X int) el cual contiene CoordX,
% CoordY, HexL y Profundidad
% --------------------------------------------------------------------
% clausuras
% Reglas
%
% ------------------ CONSTRUCTOR Y MODIFICADOR------------------------

% Descripción: Predicado que define como es un pixhex_comprimido
% Dominio: int X int X list X int X PixhexC
pixhex_comprimido(CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad, [CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255,
    integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255,
    integer(Profundidad), Profundidad >= 0.

% Descripción: Consulta si los pixeles de una imagen son
% pixhex_comprimido
% Dominio: Pixel (list)
% Recorrido: Boleano, da false si no hay un pixel PixhexC
% Tipo de recursión: Cola, da resultado sin estados pendientes
% meta secundaria: pixhex_comprimido
esHexmapComprimido([]):- !, false.
esHexmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4,
    pixhex_comprimido(CoordX,CoordY,HexL,Profundidad, Cabeza),
    pixhex_comprimido(CoordX,CoordY,HexL,Profundidad, Pixel),
    Pixel \== false -> true; esHexmapComprimido(Cola).

% Descripción: Predicado que obtiene la coordenadas (X,Y) de un
% pixhex_comprimido
% Dominio: PixhexC X int X int
% meta secundaria: pixhex_comprimido
obtCoordPixhexC(PixhexC , CoordX, CoordY):-
    pixhex_comprimido(CoordX,CoordY,_,_, PixhexC).


% Descripción: Predicado que obtiene el hexL de un
% pixhex_comprimido
% Dominio: PixhexC X list
% meta secundaria: pixhex_comprimido
obtColorPixhexC(PixhexC , HexL):-
    pixhex_comprimido(_,_,HexL,_, PixhexC).

% Descripción: Predicado que obtiene la profundidad de un
% pixhex_comprimido
% Dominio: PixhexC X int
% meta secundaria: pixhex_comprimido
obtProfundidadPixhexC(PixhexC , Profundidad):-
    pixhex_comprimido(_,_,_,Profundidad, PixhexC).
