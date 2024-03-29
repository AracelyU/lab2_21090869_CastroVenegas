
% crear el módulo
:- module(tda_pixhex_21090869_CastroVenegas, [pixhex/5,
                                              esHexmap/1,
                                              obtCoordPixhex/3, obtColorPixhex/2,
                                             obtProfundidadPixhex/2]).


% Clausuras
% Dominio:
% CoordX, CoordY, Profundidad: integer >= 0
% Hex: string
% Pixhex: pixhex
% Pixel = pixrgb | pixbit | pixhex
%
% Predicados:
% pixhex{CoordX, CoordY, Hex, Profundidad, [CoordX, CoordY, Hex,
% Profundidad]} (aridad = 5)
% esHexmap{[Pixel]} (aridad = 1)
% obtCoordHexmap{Pixhex , CoordX, CoordY} (aridad = 3)
% obtColorHexmap{Pixhex , Hex} (aridad = 2)
% obtProfundidadHexmap{Pixhex , Profundidad} (aridad = 2)
%
% Metas primarias:
% pixhex{CoordX, CoordY, Hex, Profundidad, [CoordX, CoordY, Hex,
% Profundidad]}
%
% Metas secundarias:
% esHexmap{[Pixel]}
% obtCoordPixhex{Pixhex , CoordX, CoordY}
% obtColorPixhex{Pixhex , Hex}
% obtProfundidadPixhex{Pixhex, Profundidad}
%
%
% ----------------------REPRESENTACIÓN--------------------------------
% El pixhex se representa como una lista con
% (int X int X string X int) el cual contiene CoordX,
% CoordY, Hex y Profundidad
% --------------------------------------------------------------------
% clausuras
% Reglas
%
% ------------------ CONSTRUCTOR Y MODIFICADOR------------------------
%
% Descripción: Predicado que define como es un pixhex
% Dominio: int X int X string X int X Pixhex
pixhex(CoordX, CoordY, Hex, Profundidad, [CoordX, CoordY, Hex, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0, string(Hex), integer(Profundidad), Profundidad >= 0.

%-----------------------PERTENENCIA-------------------------------------
% Descripción: Consulta si los pixeles de una imagen son pixhex
% Dominio: Pixel (list)
% Recorrido: Boleano, da false si no son pixeles Pixhex
% Tipo: Pertenencia
% Tipo de recursión: Cola, da resultado sin estados pendientes
% meta secundaria: pixhex
esHexmap([]).
esHexmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4, pixhex(CoordX,CoordY,Hex,Profundidad, Cabeza),
    pixhex(CoordX,CoordY,Hex,Profundidad, Pixhex),
    Pixhex \== false -> esHexmap(Cola); false.

% --------------------------SELECTORES--------------------------------------
% Descripción: Predicado que obtiene la coordenadas (X,Y) de un pixhex
% Dominio: Pixhex X int X int
% meta secundaria: pixhex
obtCoordPixhex(Pixhex , CoordX, CoordY):-
    pixhex(CoordX,CoordY,_,_, Pixhex).

% Descripción: Predicado que obtiene el string hexadecimal de un pixhex
% Dominio: Pixhex X string
% meta secundaria: pixhex
obtColorPixhex(Pixhex , Hex):-
    pixhex(_,_,Hex,_, Pixhex).

% Descripción: Predicado que obtiene la profundidad de un pixhex
% Dominio: Pixhex X int
% meta secundaria: pixhex
obtProfundidadPixhex(Pixhex , Profundidad):-
    pixhex(_,_,_,Profundidad, Pixhex).


