

% crear el módulo
:- module(tda_pixrgb_comprimido_21090869_CastroVenegas, [pixrgb_comprimido/7,
                                              esPixmapComprimido/1,
                                              obtCoordPixrgbC/3,
                                              obtColorPixrgbC/4,
                                              obtProfundidadPixrgbC/2]).

% Clausuras
% Dominio:
% CoordX, CoordY, Profundidad: integer >= 0
% HexR, HexG, HexB: string
% PixrgbC: pixrgb_comprimido
% Pixel = pixrgb | pixbit | pixhex
%
% Predicados:
% pixrgb_comprimido{CoordX, CoordY, HexR, HexG, HexB, Profundidad,
% [CoordX, CoordY, HexR, HexG, HexB, Profundidad]} (aridad = 7)
% esPixmapComprimido{[Pixel]} (aridad = 1)
% obtCoordPixrgbC{PixrgbC , CoordX, CoordY} (aridad = 3)
% obtColorPixrgbC{PixrgbC , HexR, HexG, HexB}  (aridad = 4)
% obtProfundidadPixrgbC{PixrgbC , Profundidad} (aridad = 2)
%
%
% Metas primarias: -
%
% Metas secundarias:
% pixrgb_comprimido{CoordX, CoordY, HexR, HexG, HexB, Profundidad,
% [CoordX, CoordY, HexR, HexG, HexB, Profundidad]}
% esPixmapComprimido{[Pixel]}
% obtCoordPixrgbC{PixrgbC , CoordX, Coord}
% obtColorPixrgbC{PixrgbC , HexR, HexG, HexB}
% obtProfundidadPixrgbC{PixrgbC , Profundidad}
%
%
%
% ----------------------REPRESENTACIÓN--------------------------------
% El pixrgb_comprimido se representa como una lista con
% (int X int X string X string X string X int) el cual contiene CoordX,
% CoordY, HexR, HexG, HexB y Profundidad
% --------------------------------------------------------------------
% clausuras
% Reglas
% ----------CONSTRUCTOR Y MODIFICADOR-------------------------------

% Descripción: Predicado que define como es un pixrgb_comprimido
% Dominio: int X int X string X string X string X int X PixrgbC
pixrgb_comprimido(CoordX, CoordY, HexR, HexG, HexB, Profundidad, [CoordX, CoordY, HexR, HexG, HexB, Profundidad]):-
    integer(CoordX), CoordX >= 0,
    integer(CoordY), CoordY >= 0,
    string(HexR), string(HexG), string(HexB),
    integer(Profundidad), Profundidad >= 0.


%------------------------------PERTENENCIA----------------------------
% Descripción: Predicado que verifica si los pixeles son
% pixrgb_comprimido
% Dominio: Pixel (list)
% Recorrido: Boleano, false si no hay un PixrgbC en la lista
% Tipo de recusión: Cola, da resultado sin estados
% pendientes
% meta secundaria: pixrgb_comprimido
esPixmapComprimido([]):- !, false.
esPixmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6,
    pixrgb_comprimido(CoordX,CoordY,HexR,HexG,HexB,Profundidad, Cabeza),
    pixrgb_comprimido(CoordX,CoordY,HexR,HexG,HexB,Profundidad, PixrgbC),
    PixrgbC \== false -> true; esPixmapComprimido(Cola).

%------------------------------SELECTORES-----------------------------
% Descripción: Predicado que obtiene la coordenadas (X,Y) de un
% PixrgbC
% Dominio: PixrgbC X int X int
% meta secundaria: pixrgb_comprimido
obtCoordPixrgbC(PixrgbC , CoordX, CoordY):-
    pixrgb_comprimido(CoordX,CoordY,_,_,_,_, PixrgbC).

% Descripción: Predicado que obtiene el string hexadecimal de un
% PixrgbC
% Dominio: PixrgbC X int X int X int
% meta secundaria: pixrgb_comprimido
obtColorPixrgbC(PixrgbC , HexR, HexG, HexB):-
    pixrgb_comprimido(_,_,HexR,HexG,HexB,_, PixrgbC).

% Descripción: Predicado que obtiene la profundidad de un
% PixrgbC
% Dominio: PixrgbC X int
% meta secundaria: pixrgb_comprimido
obtProfundidadPixrgbC(PixrgbC , Profundidad):-
    pixrgb_comprimido(_,_,_,_,_,Profundidad, PixrgbC).



