
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
%
% Predicados:
% pixrgb_comprimido{CoordX, CoordY, HexR, HexG, HexB,
% Profundidad,[CoordX, CoordY, HexR, HexG, HexB, Profundidad]} (aridad =
% 7)
% esPixmapComprimido{[Pixel]} (aridad = 1)
% obtCoordPixrgbC{PixrgbC, CoordX, CoordY} (aridad = 3)
% obtColorPixrgbC{PixrgbC, HexR, HexG, Hexb} (aridad = 4)
% obtProfundidadPixrgbC{PixrgbC, Profundidad} (aridad = 2)
%
%?????????????????????????????????????
% Metas primarias: pixrgb_comprimido, esPixmapComprimido,
% obtCoordPixrgbC, obtColorPixrgbC, obtProfundidadPixrgbC
%
% Metas secundarias:
%
%
% ----------------------REPRESENTACI�N--------------------------------
% El pixrgb_comprimido se representa como una lista con
% (int X int X string X string X string X int) el c�al contiene CoordX,
% CoordY, HexR, HexG, HexB y Profundidad
% --------------------------------------------------------------------

%---------------------------CONSTRUCTOR-------------------------------

% Descripci�n: Predicado que define como es un pixrgb_comprimido
% Dominio: int X int X string X string X string X int X PixrgbC
% Tipo: Constructor
pixrgb_comprimido(CoordX, CoordY, HexR, HexG, HexB, Profundidad, [CoordX, CoordY, HexR, HexG, HexB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0, string(HexR), string(HexG), string(HexB), integer(Profundidad), Profundidad >= 0.


%------------------------------PERTENENCIA----------------------------
% Descripci�n: Predicado que verifica si los pixeles son
% pixrgb_comprimido
% Dominio: Pixel (list)
% Recorrido: Boleano, false si no hay un PixrgbC en la lista
% Tipo de recusi�n: Cola, da resultado sin estados
% pendientes
% Tipo: Pertenencia
esPixmapComprimido([]):- !, false.
esPixmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6,
    pixrgb_comprimido(CoordX,CoordY,HexR,HexG,HexB,Profundidad, Cabeza),
    pixrgb_comprimido(CoordX,CoordY,HexR,HexG,HexB,Profundidad, PixrgbC),
    PixrgbC \== false -> true; esPixmapComprimido(Cola).

%------------------------------SELECTORES-----------------------------
% Descripci�n: Predicado que obtiene la coordenadas (X,Y) de un
% PixrgbC
% Dominio: PixrgbC X int X int
% Tipo: Selector
obtCoordPixrgbC(PixrgbC , CoordX, CoordY):-
    pixrgb_comprimido(CoordX,CoordY,_,_,_,_, PixrgbC).

% Descripci�n: Predicado que obtiene el string hexadecimal de un
% PixrgbC
% Dominio: PixrgbC X int X int X int
% Tipo: Selector
obtColorPixrgbC(PixrgbC , HexR, HexG, HexB):-
    pixrgb_comprimido(_,_,HexR,HexG,HexB,_, PixrgbC).

% Descripci�n: Predicado que obtiene la profundidad de un
% PixrgbC
% Dominio: PixrgbC X int
% Tipo: Selector
obtProfundidadPixrgbC(PixrgbC , Profundidad):-
    pixrgb_comprimido(_,_,_,_,_,Profundidad, PixrgbC).



