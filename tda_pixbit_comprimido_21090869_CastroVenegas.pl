
% crear el módulo
:- module(tda_pixbit_comprimido_21090869_CastroVenegas, [pixbit_comprimido/5,
                                             esBitmapComprimido/1,
                                             obtCoordPixbitC/3, obtColorPixbitC/2,
                                             obtProfundidadPixbitC/2]).

% Clausuras
% Dominio:
% CoordX, CoordY, Profundidad: integer >= 0
% BitL: list
% Bit: integer = 0 | integer = 1
% PixbitC: pixbit_comprimido
% Pixel = pixrgb | pixbit | pixhex | PixbitC
%
% Predicados:
% pixbit_comprimido{CoordX, CoordY, BitL, Profundidad, [CoordX, CoordY,
% BitL, Profundidad]} (aridad = 5)
% esBitmapComprimido{[Pixel]} (aridad = 1)
% obtCoordPixbitC{PixbitC , CoordX, CoordY} (aridad = 3)
% obtColorPixbitC{PixbitC , Bit} (aridad = 2)
% obtProfundidadPixbitC{PixbitC , Profundidad} (aridad = 2)
%
% Metas primarias: -
%
% Metas secundarias:
% pixbit_comprimido{CoordX, CoordY, BitL, Profundidad, [CoordX, CoordY,
% BitL, Profundidad]}
% esBitmapComprimido{[Pixel]}
% obtCoordPixbit{PixbitC , CoordX, CoordY}
% obtColorPixbit{PixbitC , Bit}
% obtProfundidadPixbit{PixbitC , Profundidad}
%
%
% ----------------------REPRESENTACIÓN--------------------------------
% El pixbit_comprimido se representa como una lista con
% (int X int X list [-1, Bit] X int) el cual contiene CoordX,
% CoordY, BitL y Profundidad
% --------------------------------------------------------------------
% Reglas
%
% ------------------ CONSTRUCTOR Y MODIFICADOR------------------------
% Descripción: Predicado que define como es un pixbit_comprimido
% Dominio: int X int X list [-1, Bit] X int X PixbitC
pixbit_comprimido(CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit, Profundidad]):-
    integer(CoordX),CoordX >= 0, integer(CoordY),CoordY >= 0, is_list(Bit), integer(Profundidad),Profundidad >= 0.

%-------------------PERTENENCIA---------------------------------------
% Descripción: Consulta si los pixeles de una imagen son
% pixbit_comprimido
% Dominio: Pixel (list)
% Recorrido: Boleano, da false si no hay un PixbitC
% Tipo: Pertenencia
% Tipo de recursión: Cola, da resultado sin estados
% pendientes.
% meta secundaria: pixbit_comprimido
esBitmapComprimido([]):- !, false.
esBitmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4,
    pixbit_comprimido(CoordX,CoordY,BitL,Profundidad, Cabeza),
    pixbit_comprimido(CoordX,CoordY,BitL,Profundidad, PixbitC),
    PixbitC \== false -> true; esBitmapComprimido(Cola).

% -------------------------------SELECTORES-----------------------------
% Descripción: Predicado que obtiene la coordenadas (X,Y) de un
% pixbit_comprimido
% Dominio: PixbitC X int X int
% meta secundaria: pixbit_comprimido
obtCoordPixbitC(PixbitC , CoordX, CoordY):-
    pixbit_comprimido(CoordX,CoordY,_,_, PixbitC).


% Descripción: Predicado que obtiene la lista BitL de un
% pixbit_comprimido
% Dominio: PixbitC X list
% meta secundaria: pixbit_comprimido
obtColorPixbitC(PixbitC , BitL):-
    pixbit_comprimido(_,_,BitL,_, PixbitC).

% Descripción: Predicado que obtiene la profundidad de un pixbit
% comprimido
% Dominio: PixbitC X int
% meta secundaria: pixbit_comprimido
obtProfundidadPixbitC(PixbitC , Profundidad):-
    pixbit_comprimido(_,_,_,Profundidad, PixbitC).
