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
% comentarios: los string son "", no ''
pixhex_d(CoordX, CoordY, Hex, Profundidad, [CoordX, CoordY, Hex, Profundidad]):-
    integer(CoordX), CoordX >= 0,
    integer(CoordY), CoordY >= 0,
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
    is_list(Cabeza),
    length(Cabeza, N), N == 4,
    pixbit_d(X,Y,B,D, Cabeza),
    pixbit_d(X,Y,B,D, P),
    P \== false ->
    esBitmap(Cola);
    false.

% Descripción: Consulta si los pixeles de una imagen son pixrgb_d
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recusión: Cola, llama sin estados pendientes
esPixmap([]).
esPixmap([Cabeza | Cola]):-
    is_list(Cabeza),
    length(Cabeza, N), N == 6,
    pixrgb_d(X,Y,R,G,B,D, Cabeza),
    pixrgb_d(X,Y,R,G,B,D, P),
    P \== false ->
    esPixmap(Cola);
    false.

% Descripción: Consulta si los pixeles de una imagen son pixhex_d
% Dominio: Lista de pixeles
% Recorrido(Boleano)
% Tipo de recursión: Cola, llama sin estados pendientes
esHexmap([]).
esHexmap([Cabeza | Cola]):-
    is_list(Cabeza),
    length(Cabeza, N), N == 4,
    pixhex_d(X,Y,H,D, Cabeza),
    pixhex_d(X,Y,H,D, P),
    P \== false ->
    esHexmap(Cola);
    false.


condicional(A):-
    integer(A) ->
      A=1 -> true;
      A=2 -> false;
    string(A) -> true.

% Descripción: Predicado que verifica si una imagen es Bitmap
% Dominio: image
% Recorrido: Boleano
imageIsBitmap(Imagen):-
    image(_,_,P, Imagen),
    esBitmap(P) ->
    write('#t');
    write('#f').

% Descripión: Predicado que verifica si una imagen es Pixmap
% Dominio: image
% Recorrido: Boleano
imageIsPixmap(Imagen):-
    image(_,_,P, Imagen),
    esPixmap(P) ->
    write('#t');
    write('#f').

% Descripción: Predicado que verifica si una imagen es Hexmap
% Dominio: image
% Recorrido: Boleano
imageIsHexmap(Imagen):-
    image(_,_,P, Imagen),
    esHexmap(P) ->
    write('#t');
    write('#f').


% Descripción: Predicado que verifica si un pixel tiene las coordenadas
% (x,y)
% Dominio: Pixel, int, int
% Recorrido: Boleano
existeCoord(Cabeza, X, Y):-
    esBitmap([Cabeza]) ->
        pixbit_d(CoordX, CoordY, _, _, Cabeza),
        CoordX = X, CoordY = Y  ->
        true;
        false
    ;
    esHexmap([Cabeza]) ->
         pixhex_d(CoordX, CoordY,_,_, Cabeza),
         CoordX = X, CoordY = Y ->
         true;
         false
    ;
    esPixmap([Cabeza]) ->
          pixrgb_d(CoordX, CoordY,_,_,_,_, Cabeza),
          CoordX = X, CoordY = Y ->
          true;
          false.


% Descripción: Predicado que verifica si existe una coordenada (X,Y)
% especifica en la lista de pixeles de una imagen
% Dominio: Pixel x entero positivo x entero positivo
% Recorrido: Boleano
existeCoordXY([Cabeza | Cola], X, Y):-
     existeCoord(Cabeza, X, Y) ->
         true;
         existeCoordXY(Cola, X, Y).


existeLista([]).
existeLista([Cabeza | Cola], Elemento):-
    Cabeza = Elemento ->
    write('true');
    existeLista(Cola, Elemento).


% Descripción: Predicado que modifica CoordX y CoordY de un Pixel por X
% e y
% Dominio: Pixel x entero positivo Recorrido: Pixel
cambiarCoordY(Pixel, Y, P):-
    integer(Y), Y >= 0,
    esBitmap([Pixel]) ->
        pixbit_d(X,_,B,D, Pixel),
        pixbit_d(X,Y,B,D, P);
    esHexmap([Pixel]) ->
        pixhex_d(X,_,H,D, Pixel),
        pixhex_d(X,Y,H,D, P);
    esPixmap([Pixel]) ->
        pixrgb_d(X,_,R,G,B,D, Pixel),
        pixrgb_d(X,Y,R,G,B,D, P).



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


flipH_formato([], _, _, _, _, []).
flipH_formato([Cabeza | Cola], Ancho, PosicionX, PosicionY, Contador, [NuevaCabeza | Cola2]):-
        not(is_list(Cabeza)) -> false;
        write('\nentro1'),

        existeCoordXY([Cabeza | Cola], PosicionX, PosicionY) ->

               write('\nentro4'),
               write('\nCabeza|Cola = '),
               write([Cabeza | Cola]),
               write('\nContador ='),
               write(Contador),
               write('\nAncho = '),
               write(Ancho),

               resta(Ancho, Contador, R), cambiarCoordY(Cabeza, R, NuevaCabeza),
               suma(PosicionY, 1, R1), suma(Contador, 1, R2),
               flipH_formato(Cola, Ancho, PosicionX, R1, R2, Cola2)

               ;
               write('\nentro3'),
               write('\nCabeza|cola = '),
               write([Cabeza | Cola]),
               write('\nPosicionX = '),
               write(PosicionX),
               write('\nContador = '),
               write(Contador),
               write('\nAncho = '),
               write(Ancho),

               (Ancho \== Contador) ->
                     write('\nentro5'),
                     suma(PosicionX, 1, R5), flipH_formato([Cabeza | Cola], Ancho, R5, 0, 0, Cola2)
                     ;
                     write('\nentro6'),
                     suma(PosicionY, 1, R6), suma(Contador, 1, R7),
                     flipH_formato([Cabeza | Cola], Ancho, PosicionX, R6, R7, Cola2).





/*


flipH_formato([], _, _, _, _, []).
flipH_formato([Cabeza | Cola], Ancho, PosicionX, PosicionY, Contador, [N | Cola2]):-
    esBitmap([Cabeza | Cola]) ->
        write('\nentro1'),
        existeCoordXY([Cabeza | Cola], PosicionX, PosicionY) ->
            write('\nentro2'),

            Contador =< Ancho ->
                write('\nentro4'),
                resta(Ancho, Contador, R),
                write('\nR= '),
                write(R),
                cambiarCoordY(Cabeza, R, NuevaCabeza),
                suma(PosicionY, 1, R1), suma(Contador, 1, R2),

                append(NuevaCabeza, [], N),
                write('\nN= '),
                write(N),
                flipH_formato(Cola, Ancho, PosicionX, R1, R2, Cola2)

               ;
                write('entro3'),
                suma(PosicionX, 1, R3),
                flipH_formato([Cabeza | Cola], Ancho, R3, 0, 0, Cola2)


            ;

            Contador =< Ancho ->
                write('entro6'),
                suma(PosicionY, 1, R4), suma(Contador, 1, R5),
                flipH_formato([Cabeza | Cola], Ancho, PosicionX, R4, R5, Cola2)

                ;
                write('entro5'),
                flipH_formato([Cabeza | Cola], Ancho, (PosicionX+1), 0,0, Cola2)

         ;

     write('no es bitmap').
*/
