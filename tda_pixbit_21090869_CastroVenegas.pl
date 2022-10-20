:- module(tda_pixbit_21090869_CastroVenegas, [pixbit/5, pixbit_comprimido/5,
                                             esBitmap/1, esBitmapComprimido/1]).

%Descripción: Define como es un pixbit_d
%Dominio: CoordX, CoordY, Bit, Profundidad, Letra
%Recorrido: pixbit_d
pixbit(CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit, Profundidad]):-
    integer(CoordX),CoordX >= 0, integer(CoordY),CoordY >= 0, integer(Bit),Bit = 0;Bit = 1, integer(Profundidad),Profundidad >= 0.

pixbit_comprimido(CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit, Profundidad]):-
    integer(CoordX),CoordX >= 0, integer(CoordY),CoordY >= 0, is_list(Bit), integer(Profundidad),Profundidad >= 0.

% Descripción: Consulta si los pixeles de una imagen son pixbit_d
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recursión: Cola, llama sin estados pendientes.
esBitmap([]).
esBitmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4, pixbit(X,Y,B,D, Cabeza), pixbit(X,Y,B,D, P),
    P \== false -> esBitmap(Cola); false.

% Descripción: Consulta si los pixeles de una imagen son pixbit_d comprimido
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recursión: Cola, llama sin estados pendientes.
esBitmapComprimido([]):- !, false.
esBitmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4,
    pixbit_comprimido(X,Y,B,D, Cabeza), pixbit_comprimido(X,Y,B,D, P),
    P \== false -> true; esBitmapComprimido(Cola).


