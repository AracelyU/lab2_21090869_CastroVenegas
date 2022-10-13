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
    integer(CoordX),CoordX >= 0, integer(CoordY),CoordY >= 0,
    integer(Bit),Bit = 0;Bit = 1, integer(Profundidad),Profundidad >= 0.

% Descripción: Define como es un pixrgb_d
% Dominio: CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad, Letra
% Recorrido: pixrgb_d
pixrgb_d(CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad,
        [CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255,
    integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255,
    integer(Profundidad), Profundidad >= 0.


% Descripción: Define como es un pixhex_d
% Dominio: CoordX, CoordY, Hex, Profundidad, Letra
% Recorrido: pixhex_d
% comentarios: los string son "", no ''
pixhex_d(CoordX, CoordY, Hex, Profundidad, [CoordX, CoordY, Hex, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    string(Hex), integer(Profundidad), Profundidad >= 0.

% Descripción: Define como es una imagen
% Dominio: Ancho, Largo, Pixeles, Letra
% Recorrido: imagen
image(Ancho, Largo, Pixel, [Ancho, Largo, Pixel]):-
    integer(Ancho),Ancho >= 0, integer(Largo), Largo >= 0, is_list(Pixel).

% Descripción: Consulta si los pixeles de una imagen son pixbit_d
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recursión: Cola, llama sin estados pendientes.
esBitmap([]).
esBitmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4,
    pixbit_d(X,Y,B,D, Cabeza), pixbit_d(X,Y,B,D, P),
    P \== false ->
    esBitmap(Cola);
    false.

% Descripción: Consulta si los pixeles de una imagen son pixrgb_d
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recusión: Cola, llama sin estados pendientes
esPixmap([]).
esPixmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6,
    pixrgb_d(X,Y,R,G,B,D, Cabeza), pixrgb_d(X,Y,R,G,B,D, P),
    P \== false ->
    esPixmap(Cola);
    false.

% Descripción: Consulta si los pixeles de una imagen son pixhex_d
% Dominio: Lista de pixeles
% Recorrido(Boleano)
% Tipo de recursión: Cola, llama sin estados pendientes
esHexmap([]).
esHexmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4,
    pixhex_d(X,Y,H,D, Cabeza), pixhex_d(X,Y,H,D, P),
    P \== false ->
    esHexmap(Cola);
    false.

% Descripción: Predicado que verifica si una imagen es Bitmap
% Dominio: image
% Recorrido: Boleano
imageIsBitmap(Imagen):-
    image(_,_,P, Imagen),
    esBitmap(P) ->
    true;
    false.

% Descripión: Predicado que verifica si una imagen es Pixmap
% Dominio: image
% Recorrido: Boleano
imageIsPixmap(Imagen):-
    image(_,_,P, Imagen),
    esPixmap(P) ->
    true;
    false.

% Descripción: Predicado que verifica si una imagen es Hexmap
% Dominio: image
% Recorrido: Boleano
imageIsHexmap(Imagen):-
    image(_,_,P, Imagen),
    esHexmap(P) ->
    true;
    false.

% Descripción: Predicado que verifica existencia de (x,y) en Pixel
% Dominio: Pixel, int, int
% Recorrido: Boleano
existeCoord(Cabeza, X, Y):-
    esBitmap([Cabeza]) ->
        pixbit_d(CoordX, CoordY, _, _, Cabeza), CoordX = X, CoordY = Y  ->
        true; false
    ;
    esHexmap([Cabeza]) ->
         pixhex_d(CoordX, CoordY,_,_, Cabeza), CoordX = X, CoordY = Y ->
         true; false
    ;
    esPixmap([Cabeza]) ->
          pixrgb_d(CoordX, CoordY,_,_,_,_, Cabeza), CoordX = X, CoordY = Y ->
          true; false.


% Descripción: Predicado que verifica si existe una coordenada (X,Y)
% especifica en la lista de pixeles de una imagen
% Dominio: Pixel x entero positivo x entero positivo
% Recorrido: Boleano
existeCoordXY([Cabeza | Cola], X, Y):-
     existeCoord(Cabeza, X, Y) ->
         true;
         existeCoordXY(Cola, X, Y).

% Descripción: Predicado que verifica si es una imagen de un solo tipo
% de pixel
% Dominio: imagen
% Recorrido: Boleano
esImage(Imagen):-
    imageIsBitmap(Imagen) -> true;
    imageIsHexmap(Imagen) -> true;
    imageIsPixmap(Imagen) -> true;
    false.


% Descripción: Predicado que modifica CoordX y CoordY de un Pixel
% Dominio: Pixel X int X int X variable
% Recorrido: Pixel
cambiarCoordXY(Pixel,X, Y, P):-
    integer(Y), Y >= 0, integer(X), X >= 0,
    esBitmap([Pixel]) ->
        pixbit_d(_,_,B,D, Pixel), pixbit_d(X,Y,B,D, P);
    esHexmap([Pixel]) ->
        pixhex_d(_,_,H,D, Pixel), pixhex_d(X,Y,H,D, P);
    esPixmap([Pixel]) ->
        pixrgb_d(_,_,R,G,B,D, Pixel), pixrgb_d(X,Y,R,G,B,D, P).


% Descripción: Predicado que modifica CoordX de un pixel


% Descripción: Predicado que voltea los pixeles horizontalmente
% Dominio: list X list X int X int X variable
% Recorrido: list
flipH_formato(_, [], _, _, _, []).
flipH_formato([CabezaC | ColaC], [Cabeza | Cola], CoordY_final, CoordX, Contador, [NuevaCabeza | Cola2]):-
         existeCoordXY([CabezaC | ColaC], CoordX, Contador) ->
               R is CoordY_final-Contador,
               R1 is Contador+1,
               cambiarCoordXY(Cabeza, CoordX, R, NuevaCabeza),
               flipH_formato([CabezaC | ColaC], Cola, CoordY_final, CoordX, R1, Cola2)
               ;
               (Contador > CoordY_final) ->
                     R2 is CoordX+1,
                     flipH_formato([CabezaC | ColaC], [Cabeza | Cola], CoordY_final, R2, 0, Cola2)
                     ;
                      R3 is Contador+1,
                     flipH_formato([CabezaC | ColaC], [Cabeza | Cola], CoordY_final, CoordX, R3, Cola2).



% Descripción: Predicado que voltea la imagen horizontalmente
% Dominio: imagen X variable
% Recorrido: imagen
imageFlipH(Imagen, Imagen2):-
    esImage(Imagen),
    image(CoordX, CoordY, Pixeles, Imagen),
    CoordY_final is CoordY-1,
    flipH_formato(Pixeles, Pixeles, CoordY_final, 0, 0, PixelesH),
    sort(PixelesH, Pixeles2), % provisionalmente uso sort, mejor intenta crear un predicado para ordenar
    image(CoordX, CoordY, Pixeles2, Imagen2).



% Descripción: Predicado que voltea los pixeles verticalmente
% Dominio: list X list X int X int X int X int X variable
% Recorrido: list
flipV_formato(_ ,[], _, _, _,_,[]).
flipV_formato([CabezaC | ColaC], [Cabeza | Cola], CoordX_final, CoordY, CoordY_final, Contador, [NuevaCabeza | Cola2]):-
    existeCoordXY([CabezaC | ColaC], Contador, CoordY) ->
        R is CoordX_final-Contador,
        R1 is CoordY+1,
        cambiarCoordXY(Cabeza, R, CoordY, NuevaCabeza),
        flipV_formato([CabezaC | ColaC], Cola, CoordX_final, R1, CoordY_final, Contador, Cola2)
        ;
        (CoordY > CoordY_final) ->
            R2 is Contador+1,
            flipV_formato([CabezaC | ColaC], [Cabeza | Cola], CoordX_final, 0, CoordY_final, R2, Cola2)
            ;
            R3 is CoordY+1,
            flipV_formato([CabezaC | ColaC], [Cabeza | Cola], CoordX_final, R3, CoordY_final, Contador, Cola2).



% Descripción: Predicado que voltea la imagen verticalmente
% Dominio: imagen X variable
% Recorrido: imagen
imageFlipV(Imagen, Imagen2):-
    esImage(Imagen),
    image(CoordX, CoordY, Pixeles, Imagen),
    CoordX_final is CoordX-1, CoordY_final is CoordY-1,
    flipV_formato(Pixeles, Pixeles, CoordX_final, 0, CoordY_final, 0, PixelesV),
    sort(PixelesV, Pixeles2), % provisionalmente uso sort, mejor intenta crear un predicado para ordenar
    image(CoordX, CoordY, Pixeles2, Imagen2).

% Descripción: Predicado que verifica si el Pixel esta dentro del rango
% dado por crop
% Dominio: Pixel X int X int X int X int
rangoXY(Pixel, X1,X2,Y1,Y2):-
    esBitmap([Pixel]) ->
        pixbit_d(X,Y,_,_, Pixel),
        ((X >= X1), (X =< X2), (Y >= Y1), (Y =< Y2)) ->
        true; false;
    esHexmap([Pixel]) ->
        pixhex_d(X,Y,_,_, Pixel),
        ((X >= X1), (X =< X2), (Y >= Y1), (Y =< Y2)) ->
        true; false;
    esPixmap([Pixel]) ->
        pixrgb_d(X,Y,_,_,_,_, Pixel),
        pixhex_d(X,Y,_,_, Pixel),
        ((X >= X1), (X =< X2), (Y >= Y1), (Y =< Y2)) ->
        true; false.


crop_formato([], _,_, _, _, []).
crop_formato([Cabeza | Cola], X1, X2, Y1, Y2, [Cabeza2 | Cola2]):-
     rangoXY(Cabeza, X1, X2, Y1, Y2) ->
            append(Cabeza, [], Cabeza2),
            crop_formato(Cola, X1, X2, Y1, Y2, Cola2)
            ;
            crop_formato(Cola, X1, X2, Y1, Y2, Cola2).


mayor(A,B,C):- A >= B -> C is A ; C is B.
menor(A,B,C):- A =< B -> C is A ; C is B.


imageCrop(Imagen, X1,X2,Y1,Y2,Imagen2):-
    esImage(Imagen),
    image(CoordX, CoordY, Pixeles, Imagen),
    menor(X1,X2,X_menor), mayor(X1,X2,X_mayor),
    menor(Y1,Y2,Y_menor), mayor(Y1,Y2,Y_mayor),
    crop_formato(Pixeles, X_menor, X_mayor, Y_menor, Y_mayor, Pixeles2),
    image(CoordX, CoordY, Pixeles2, Imagen2).



% Descripción: Predicado que entrega el equivalente a string de un
% número entero
% Dominio: int X variable
% Recorrido: string
numeroString(Num, String):-
    (Num = 0), String = "0"; (Num = 1), String = "1"; (Num = 2), String = "2"; (Num = 3), String = "3";
    (Num = 4), String = "4"; (Num = 5), String = "5"; (Num = 6), String = "6"; (Num = 7), String = "7";
    (Num = 8), String = "8"; (Num = 9), String = "9"; (Num = 10), String = "A"; (Num = 11), String = "B";
    (Num = 12), String = "C"; (Num = 13), String = "D"; (Num = 14), String = "E"; (Num = 15), String = "F"; false.


% Descripción: Predicado que convierte un pixmap a hexmap
% Dominio: pixel x variable
% Recorrido: pixel
stringRGB(Pixel, PixelH):-
    pixrgb_d(X,Y,R,G,B,D,Pixel),
    EnteroR is R // 16, RestoR is R mod 16, EnteroG is G // 16, RestoG is G mod 16, EnteroB is B // 16, RestoB is B mod 16,
    numeroString(EnteroR, R1), numeroString(RestoR, R2), numeroString(EnteroG, G1), numeroString(RestoG, G2),
    numeroString(EnteroB, B1), numeroString(RestoB, B2),
    string_concat(R1, R2, R_hex), string_concat(G1, G2, G_hex), string_concat(B1, B2, B_hex),
    string_concat(R_hex, G_hex, ColorParcial), string_concat(ColorParcial, B_hex, ColorHex),
    pixhex_d(X,Y,ColorHex, D, PixelH).


% Descripción: Predicado que cambia el formato de pixeles pixmap a
% hexmap
% Dominio: list X variable
% Recorrido: list
formatoRGB_HEX([], []).
formatoRGB_HEX([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    stringRGB(Cabeza, NuevaCabeza),
    formatoRGB_HEX(Cola, Cola2).

% Descripción: Predicado que convierte una imagen pixmap a hexmap
% Dominio: imagen X variable
% Recorrido: imagen
imageRGBToHex(Imagen, Imagen2):-
    esImage(Imagen),
    image(CoordX, CoordY, Pixeles, Imagen),
    formatoRGB_HEX(Pixeles, Pixeles2),
    image(CoordX, CoordY, Pixeles2, Imagen2).



contar(_,[],0).
contar(X,[X|L],C):- !,contar(X,L,C1), C is C1+1.
contar(X,[_|L],C):- contar(X,L,C).



lista1([],[]).
lista1([Cabeza | Cola], [Cabeza2 | Cola2]):-
    not(Cabeza = 2) ->
    append([Cabeza], [], Cabeza2),
    lista1(Cola, Cola2);
    lista1(Cola, Cola2).






% Descripción: Predicado que elimina de una lista un elemento
% Dominio: list X elemento X variable
% Recorrido: list
eliminar([],_,[]).
eliminar([Cabeza | Cola], Elemento, [Cabeza2 | Cola2]):-
    Cabeza \== Elemento ->
        Cabeza2 is Cabeza,
        eliminar(Cola, Elemento, Cola2);
        eliminar(Cola, Elemento, Cola2).

agregarElemento([H|T], E, [H | T2]):-
    agregarElemento(T,E,T2).



inserta([],X,[X]).
inserta([H|T], N, [H|R]) :- inserta(T, N, R).

% Descripción: Predicado que modifica el formato de pixeles de modo que
% esten invertidos horizontalmente
% Dominio: Pixeles
% Recorrido: Pixeles
agregar(X, L, [X|L]).

%concatena([Cabeza|Resto],Lista,[Cabeza|RestoConcatenado]):-
%concatena(Resto,Lista,RestoConcatenado).

resta(A,B,R):- R is A-B.
suma(A,B,R):- R is A+B.


listar(L, Cabeza):- Cola = [Cabeza], L=[[], Cola].


existeLista([]).
existeLista([Cabeza | Cola], Elemento):-
    Cabeza = Elemento ->
    write('true');
    existeLista(Cola, Elemento).


condicional(A):-
    integer(A) ->
      A=1 -> true;
      A=2 -> false;
    string(A) -> true.
