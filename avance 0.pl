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


% Descripción: Predicado que modifica CoordX y CoordY de un Pixel
% Dominio: Pixel x entero positivo X variable
% Recorrido: Pixel
cambiarCoordY(Pixel, Y, P):-
    integer(Y), Y >= 0,
    esBitmap([Pixel]) ->
        pixbit_d(X,_,B,D, Pixel), pixbit_d(X,Y,B,D, P);
    esHexmap([Pixel]) ->
        pixhex_d(X,_,H,D, Pixel), pixhex_d(X,Y,H,D, P);
    esPixmap([Pixel]) ->
        pixrgb_d(X,_,R,G,B,D, Pixel), pixrgb_d(X,Y,R,G,B,D, P).


% Descripción: Predicado que voltea los pixeles horizontalmente
% Dominio: list X list X int X int X variable
% Recorrido: list
flipH_formato(_, [], _, _, _, []).
flipH_formato([CabezaC | ColaC], [Cabeza | Cola], CoordY_final, CoordX, Contador, [NuevaCabeza | Cola2]):-
         existeCoordXY([CabezaC | ColaC], CoordX, Contador) ->
               R is CoordY_final-Contador,
               R1 is Contador+1,
               cambiarCoordY(Cabeza, R, NuevaCabeza),
               flipH_formato([CabezaC | ColaC], Cola, CoordY_final, CoordX, R1, Cola2)
               ;
               (Contador > CoordY_final) ->
                     R2 is CoordX+1,
                     flipH_formato([CabezaC | ColaC], [Cabeza | Cola], CoordY_final, R2, 0, Cola2)
                     ;
                      R3 is Contador+1,
                     flipH_formato([CabezaC | ColaC], [Cabeza | Cola], CoordY_final, CoordX, R3, Cola2).



% Descripción: Predicado que voltea la imagen horizontalmente
% Dominio: imagen
% Recorrido: imagen
imageFlipH(Imagen, Imagen2):-   % Pienso que puede mejorar si verifica si la imagen es de un tipo de pixel
                                % puesto que este predicado no diferencia entre un pixel y otro
    image(CoordX, CoordY, Pixeles, Imagen),
    CoordY_final is CoordY-1,
    flipH_formato(Pixeles, Pixeles, CoordY_final, 0, 0, Pixeles2),
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


crop(Imagen, X1,X2,Y1,Y2,Imagen2):-    % tiene el mismo problema que flipH, hay que verificar que el formato de imagenes este bien
     write('entro'),

    image(CoordX, CoordY, Pixeles, Imagen),
    menor(X1,X2,X_menor), mayor(X1,X2,X_mayor),
    menor(Y1,Y2,Y_menor), mayor(Y1,Y2,Y_mayor),
    crop_formato(Pixeles, X_menor, X_mayor, Y_menor, Y_mayor, Pixeles2),
    write('\nPixeles2 = '), write(Pixeles2),
    image(CoordX, CoordY, Pixeles2, Imagen2).












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
