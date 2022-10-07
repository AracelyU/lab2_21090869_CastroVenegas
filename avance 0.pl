%Dominios -> nombre = {argumentos} = salida
%image = {Ancho, Largo, Pixeles} = image
%pixbit-d = {CoordX, CoordY, Bit, Profundidad} = pixbit-d

%Predicados -> nombre(argumentos) aridad = X
%image(Ancho, Largo, Pixeles) aridad = 3

%Metas
%Primarias -> nombre(argumentos) de los que tienen palabras rojas
%departamento(PF,VS,PD,VD)
%Secundarias -> nombre(argumentos) de los que tienen palabras negras


%Clausulas de Horn -> nombre(argumentos) de los que no tiene :-
%Hechos, axiomas, verdades, base de conocimiento, base de datos
image_vacia([]).

%Reglas -> nombre(argumentos) de los que tienen :-
%

%Descripción: Define como es un pixbit_d
%Dominio: CoordX, CoordY, Bit, Profundidad, Letra
%Recorrido: pixbit_d
pixbit_d(CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit, Profundidad]):-
    integer(CoordX),CoordX >= 0,
    integer(CoordY),CoordY >= 0,
    integer(Bit),Bit = 0;Bit = 1,
    integer(Profundidad),Profundidad >= 0.

% Descripción: Define como es un pixrgb_d
% Dominio: CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad, Letra
% Recorrido: pixrgb_d
pixrgb_d(CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad,
        [CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad]):-
    integer(CoordX), CoordX >= 0,
    integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255,
    integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255,
    integer(Profundidad), Profundidad >= 0.


% Descripción: Define como es un pixhex_d
% Dominio: CoordX, CoordY, Hex, Profundidad, Letra
% Recorrido: pixhex_d
pixhex_d(CoordX, CoordY, Hex, Profundidad):-
    integer(CoordX), CoordX >= 0,
    integer(coordY), CoordY >= 0,
    string(Hex),
    integer(Profundidad), Profundidad >= 0.

% Descripción: Define como es una imagen
% Dominio: Ancho, Largo, Pixeles, Letra
% Recorrido: imagen
image(Ancho, Largo, Pixel, [Ancho, Largo, Pixel]):-
    integer(Ancho),Ancho >= 0,
    integer(Largo), Largo >= 0,
    is_list(Pixel).

% Descripción: Consulta si los pixeles de una imagen son pixbit_d
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recursión: Cola, llama sin estados pendientes.
esBitmap([]).
esBitmap([Cabeza | Cola]):-
    length(Cabeza, N), N \== 4 -> false;
    pixbit_d(X,_,_,_, Cabeza),
    pixbit_d(_,Y,_,_, Cabeza),
    pixbit_d(_,_,B,_, Cabeza),
    pixbit_d(_,_,_,D, Cabeza),
    pixbit_d(X,Y,B,D, Cabeza) \== false ->
    esBitmap(Cola);
    false.

% Descripción: Consulta si los pixeles de una imagen son pixrgb_d
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recusión: Cola, llama sin estados pendientes
esPixmap([]).
esPixmap([Cabeza | Cola]):-
    length(Cabeza, N), N \== 6 -> false;
        pixrgb_d(_,_,_,_,_,_,Cabeza) \== false ->
           esPixmap(Cola);
           false.

% Descripción: Consulta si los pixeles de una imagen son pixhex_d
% Dominio: Lista de pixeles
% Recorrido(Boleano)
% Tipo de recursión: Cola, llama sin estados pendientes
esHexmap([]).
esHexmap([Cabeza | Cola]):-
    length(Cabeza, N), N \== 4 -> false;
    pixhex_d(X,_,_,_, Cabeza),
    pixhex_d(_,Y,_,_, Cabeza),
    pixhex_d(_,_,H,_, Cabeza),
    pixhex_d(_,_,_,D, Cabeza),
    pixhex_d(X,Y,H,D, Cabeza) \== false ->
    esHexmap(Cola);
    false.


condicional(A):-
    A = false ->
    write('es uno') -> true;
    write('no es uno') -> false.

% Descripción: Predicado que verifica si una imagen es Bitmap
% Dominio: image
% Recorrido: Boleano
imageIsBitmap(Imagen):-
    image(_,_,P, Imagen), esBitmap(P) \== false ->
    write('#t');
    write('#f').

% Descripión: Predicado que verifica si una imagen es Pixmap
% Dominio: image
% Recorrido: Boleano
imageIsPixmap(Imagen):-
    image(_,_,P, Imagen), esPixmap(P) \== false ->
    write('#t');
    write('#f').

% Descripción: Predicado que verifica si una imagen es Hexmap
% Dominio: image
% Recorrido: Boleano
imageIsHexmap(Imagen):-
    image(_,_,P, Imagen), esHexmap(P) \== false ->
    write('#t');
    write('#f').








