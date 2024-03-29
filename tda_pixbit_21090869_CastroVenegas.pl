
% crear módulo
:- module(tda_pixbit_21090869_CastroVenegas, [pixbit/5,
                                             esBitmap/1,
                                             obtCoordPixbit/3, obtColorPixbit/2,
                                             obtProfundidadPixbit/2]).

% Dominio:
% CoordX, CoordY, Profundidad: integer >= 0
% Bit: integer = 0 | integer = 1
% Pixbit: pixbit
% Pixel = pixrgb | pixbit | pixhex
%
% Predicados:
% pixbit{CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit,
% Profundidad]} (aridad = 5)
% esBitmap{[Pixel]} (aridad = 1)
% obtCoordPixbit{Pixbit , CoordX, CoordY} (aridad = 3)
% obtColorPixbit{Pixbit , Bit} (aridad = 2)
% obtProfundidadPixbit{Pixbit , Profundidad} (aridad = 2)
%
% Metas primarias:
% pixbit{CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit,
% Profundidad]}
%
% Metas secundarias:
% esBitmap{[Pixel]}
% obtCoordPixbit{Pixbit , CoordX, CoordY}
% obtColorPixbit{Pixbit , Bit}
% obtProfundidadPixbit{Pixbit , Profundidad}
%
%
% ----------------------REPRESENTACIÓN--------------------------------
% El pixbit se representa como una lista con
% (int X int X int (0|1) X int) el cual contiene CoordX,
% CoordY, Bit y Profundidad
% --------------------------------------------------------------------
%
% clausuras
% Reglas
%
% ------------------ CONSTRUCTOR Y MODIFICADOR------------------------
% Descripción: Predicado que define como es un pixbit
% Dominio: int X int X int (0 | 1) X int X Pixbit
pixbit(CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit, Profundidad]):-
    integer(CoordX),CoordX >= 0, integer(CoordY),CoordY >= 0, integer(Bit),Bit = 0;Bit = 1, integer(Profundidad),Profundidad >= 0.

%---------------------------PERTENENCIA--------------------------------
% Descripción: Consulta si los pixeles de una imagen son pixbit
% Dominio: Pixel (list)
% Recorrido: Boleano, false si no es una lista de Pixbit
% Tipo de recursión: Cola, da resultado sin estados pendientes.
% meta secundaria: pixbit
esBitmap([]).
esBitmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4, pixbit(CoordX,CoordY,Bit,Profundidad, Cabeza),
    pixbit(CoordX,CoordY,Bit,Profundidad, Pixbit),
    Pixbit \== false -> esBitmap(Cola); false.

% -------------------------SELECTORES---------------------------------------
% Descripción: Predicado que obtiene la coordenadas (X,Y) de un pixbit
% Dominio: Pixbit X int X int
% meta secundaria: pixbit
obtCoordPixbit(Pixbit , CoordX, CoordY):-
    pixbit(CoordX,CoordY,_,_, Pixbit).

% Descripción: Predicado que obtiene el bit de un pixbit
% Dominio: Pixbit X int
% meta secundaria: pixbit
obtColorPixbit(Pixbit , Bit):-
    pixbit(_,_,Bit,_, Pixbit).

% Descripción: Predicado que obtiene la profundidad de un pixbit
% Dominio: Pixbit X int
% meta secundaria: pixbit
obtProfundidadPixbit(Pixbit , Profundidad):-
    pixbit(_,_,_,Profundidad, Pixbit).

