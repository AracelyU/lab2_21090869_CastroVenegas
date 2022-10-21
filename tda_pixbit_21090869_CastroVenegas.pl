:- module(tda_pixbit_21090869_CastroVenegas, [pixbit/5, pixbit_comprimido/5,
                                             esBitmap/1, esBitmapComprimido/1,
                                             obtCoordPixbit/3, obtColorPixbit/2,
                                             obtProfundidadPixbit/2]).

% Descripción: Predicado que define como es un pixbit
% Dominio: int X int X int (0 | 1) X int X variable (list)
% Recorrido: pixbit
% Tipo: Constructor
pixbit(CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit, Profundidad]):-
    integer(CoordX),CoordX >= 0, integer(CoordY),CoordY >= 0, integer(Bit),Bit = 0;Bit = 1, integer(Profundidad),Profundidad >= 0.

% Descripción: Predicado que define como es un pixbit_comprimido
% Dominio: int X int X list X int X variable (list)
% Recorrido: pixbit_comprimido
% Tipo: Constructor
pixbit_comprimido(CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit, Profundidad]):-
    integer(CoordX),CoordX >= 0, integer(CoordY),CoordY >= 0, is_list(Bit), integer(Profundidad),Profundidad >= 0.

% Descripción: Consulta si los pixeles de una imagen son pixbit
% Dominio: Pixel (list)
% Recorrido: Boleano
% Tipo: Pertenencia
% Tipo de recursión: Cola, da resultado sin estados pendientes.
esBitmap([]).
esBitmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4, pixbit(X,Y,B,D, Cabeza), pixbit(X,Y,B,D, P),
    P \== false -> esBitmap(Cola); false.

% Descripción: Consulta si los pixeles de una imagen son pixbit_d comprimido
% Dominio: Pixel (list)
% Recorrido: Boleano
% Tipo: Pertenencia
% Tipo de recursión: Cola, da resultado sin estados pendientes.
esBitmapComprimido([]):- !, false.
esBitmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4,
    pixbit_comprimido(X,Y,B,D, Cabeza), pixbit_comprimido(X,Y,B,D, P),
    P \== false -> true; esBitmapComprimido(Cola).


% Descripción: Predicado que obtiene la coordenadas (X,Y) de un pixbit
% Dominio: pixbit X variable (int) X variable (int)
% Recorrido: int y int
% Tipo: Selector
obtCoordPixbit(Pixbit , X, Y):-
    pixbit(X,Y,_,_, Pixbit).


% Descripción: Predicado que obtiene el bit de un pixbit
% Dominio: pixbit X variable (int)
% Recorrido: int
% Tipo: Selector
obtColorPixbit(Pixbit , Bit):-
    pixbit(_,_,Bit,_, Pixbit).

% Descripción: Predicado que obtiene la profundidad de un pixbit
% Dominio: pixbit X variable (int)
% Recorrido: int
% Tipo: Selector
obtProfundidadPixbit(Pixbit , Profundidad):-
    pixbit(_,_,_,Profundidad, Pixbit).
