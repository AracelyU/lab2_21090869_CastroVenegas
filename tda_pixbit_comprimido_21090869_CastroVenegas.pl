:- module(tda_pixbit_comprimido_21090869_CastroVenegas, [pixbit_comprimido/5,
                                             esBitmapComprimido/1,
                                             obtCoordPixbitC/3, obtColorPixbitC/2,
                                             obtProfundidadPixbitC/2]).


% Descripción: Predicado que define como es un pixbit_comprimido
% Dominio: int X int X list X int X variable (list)
% Recorrido: pixbit_comprimido
% Tipo: Constructor
pixbit_comprimido(CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit, Profundidad]):-
    integer(CoordX),CoordX >= 0, integer(CoordY),CoordY >= 0, is_list(Bit), integer(Profundidad),Profundidad >= 0.


% Descripción: Consulta si los pixeles de una imagen son
% pixbit_comprimido
% Dominio: Pixel (list)
% Recorrido: Boleano
% Tipo: Pertenencia
% Tipo de recursión: Cola, da resultado sin estados
% pendientes.
esBitmapComprimido([]):- !, false.
esBitmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4,
    pixbit_comprimido(X,Y,B,D, Cabeza), pixbit_comprimido(X,Y,B,D, P),
    P \== false -> true; esBitmapComprimido(Cola).

% Descripción: Predicado que obtiene la coordenadas (X,Y) de un
% pixbit_comprimido
% Dominio: pixbit_comprimido X variable (int) X variable (int)
% Recorrido: int y int Tipo: Selector
obtCoordPixbitC(Pixbit , X, Y):-
    pixbit_comprimido(X,Y,_,_, Pixbit).


% Descripción: Predicado que obtiene el bit de un pixbit_comprimido
% Dominio: pixbit_comprimido X variable (int)
% Recorrido: int
% Tipo: Selector
obtColorPixbitC(PixbitC , Bit):-
    pixbit_comprimido(_,_,Bit,_, PixbitC).

% Descripción: Predicado que obtiene la profundidad de un pixbit
% comprimido
% Dominio: pixbit_comprimido X variable (int)
% Recorrido: int
% Tipo: Selector
obtProfundidadPixbitC(PixbitC , Profundidad):-
    pixbit_comprimido(_,_,_,Profundidad, PixbitC).
