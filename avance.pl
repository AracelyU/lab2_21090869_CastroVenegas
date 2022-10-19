:-module(TDA_image_21090869_CastroVenegas, [pixrgb/7, pixbit/5]).

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
pixbit(CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit, Profundidad]):-
    integer(CoordX),CoordX >= 0, integer(CoordY),CoordY >= 0, integer(Bit),Bit = 0;Bit = 1, integer(Profundidad),Profundidad >= 0.

pixbit_comprimido(CoordX, CoordY, Bit, Profundidad, [CoordX, CoordY, Bit, Profundidad]):-
    integer(CoordX),CoordX >= 0, integer(CoordY),CoordY >= 0, is_list(Bit), integer(Profundidad),Profundidad >= 0.

% Descripción: Define como es un pixrgb_d
% Dominio: CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad, Letra
% Recorrido: pixrgb_d
pixrgb(CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad, [CoordX, CoordY, ColorR, ColorG, ColorB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255, integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255, integer(Profundidad), Profundidad >= 0.

pixrgb_comprimido(CoordX, CoordY, HexR, HexG, HexB, Profundidad, [CoordX, CoordY, HexR, HexG, HexB, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0, string(HexR), string(HexG), string(HexB), integer(Profundidad), Profundidad >= 0.

% Descripción: Define como es un pixhex_d
% Dominio: CoordX, CoordY, Hex, Profundidad, Letra
% Recorrido: pixhex_d
% comentarios: los string son "", no ''
pixhex(CoordX, CoordY, Hex, Profundidad, [CoordX, CoordY, Hex, Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0, string(Hex), integer(Profundidad), Profundidad >= 0.

pixhex_comprimido(CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad, [CoordX, CoordY, [ColorR, ColorG, ColorB], Profundidad]):-
    integer(CoordX), CoordX >= 0, integer(CoordY), CoordY >= 0,
    integer(ColorR), ColorR >= 0; ColorR =< 255, integer(ColorG), ColorG >= 0; ColorG =< 255,
    integer(ColorB), ColorB >= 0; ColorB =< 255, integer(Profundidad), Profundidad >= 0.


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


% Descripción: Consulta si los pixeles de una imagen son pixrgb_d
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recusión: Cola, llama sin estados pendientes
esPixmap([]).
esPixmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6, pixrgb(X,Y,R,G,B,D, Cabeza), pixrgb(X,Y,R,G,B,D, P),
    P \== false -> esPixmap(Cola); false.


% Descripción: Consulta si los pixeles de una imagen son pixrgb_d comprimido
% Dominio: Lista de pixeles
% Recorrido: Boleano
% Tipo de recusión: Cola, llama sin estados pendientes
esPixmapComprimido([]):- !, false.
esPixmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 6,
    pixrgb_comprimido(X,Y,R,G,B,D, Cabeza), pixrgb_comprimido(X,Y,R,G,B,D, P),
    P \== false -> true; esPixmapComprimido(Cola).


% Descripción: Consulta si los pixeles de una imagen son pixhex_d
% Dominio: Lista de pixeles
% Recorrido(Boleano)
% Tipo de recursión: Cola, llama sin estados pendientes
esHexmap([]).
esHexmap([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4, pixhex(X,Y,H,D, Cabeza), pixhex(X,Y,H,D, P),
    P \== false -> esHexmap(Cola); false.

% Descripción: Consulta si los pixeles de una imagen son pixhex_d comprimido
% Dominio: Lista de pixeles
% Recorrido(Boleano)
% Tipo de recursión: Cola, llama sin estados pendientes
esHexmapComprimido([]):- !, false.
esHexmapComprimido([Cabeza | Cola]):-
    is_list(Cabeza), length(Cabeza, N), N == 4,
    pixhex_comprimido(X,Y,H,D, Cabeza), pixhex_comprimido(X,Y,H,D, P),
    P \== false -> true; esHexmapComprimido(Cola).


% Descripción: Predicado que verifica si una imagen es Bitmap
% Dominio: image
% Recorrido: Boleano
imageIsBitmap(Imagen):- image(_,_,P, Imagen), esBitmap(P) -> true; false.

% Descripión: Predicado que verifica si una imagen es Pixmap
% Dominio: image
% Recorrido: Boleano
imageIsPixmap(Imagen):- image(_,_,P, Imagen), esPixmap(P) -> true; false.

% Descripción: Predicado que verifica si una imagen es Hexmap
% Dominio: image
% Recorrido: Boleano
imageIsHexmap(Imagen):- image(_,_,P, Imagen), esHexmap(P) -> true; false.

% Descripción: Predicado que verifica si una imagen fue comprimida
% Dominio: image
% Recorrido: Boleano
imageIsCompress(Imagen):-
    image(_,_,P, Imagen),
    esHexmapComprimido(P) -> true;
    image(_,_,P, Imagen),
    esPixmapComprimido(P) -> true;
    image(_,_,P, Imagen),
    esBitmapComprimido(P) -> true; false.

% Descripción: Predicado que verifica existencia de (x,y) en Pixel
% Dominio: Pixel, int, int
% Recorrido: Boleano
existeCoord(Cabeza, X, Y):-
    esBitmap([Cabeza]) -> pixbit(CoordX, CoordY, _, _, Cabeza), CoordX = X, CoordY = Y  -> true; false
    ;
    esHexmap([Cabeza]) -> pixhex(CoordX, CoordY,_,_, Cabeza), CoordX = X, CoordY = Y -> true; false
    ;
    esPixmap([Cabeza]) -> pixrgb(CoordX, CoordY,_,_,_,_, Cabeza), CoordX = X, CoordY = Y -> true; false.

% Descripción: Predicado que verifica si existe una coordenada (X,Y)
% especifica en la lista de pixeles de una imagen
% Dominio: Pixel x entero positivo x entero positivo
% Recorrido: Boleano
existeCoordXY([Cabeza | Cola], X, Y):- existeCoord(Cabeza, X, Y) -> true; existeCoordXY(Cola, X, Y).

% Descripción: Predicado que verifica si es una imagen de un solo tipo
% de pixel
% Dominio: imagen
% Recorrido: Boleano
esImage(Imagen):-
    imageIsBitmap(Imagen) -> true; imageIsHexmap(Imagen) -> true; imageIsPixmap(Imagen) -> true; false.


% Descripción: Predicado que modifica CoordX y CoordY de un Pixel
% Dominio: Pixel X int X int X variable
% Recorrido: Pixel
cambiarCoordXY(Pixel,X, Y, P):-
    integer(Y), Y >= 0, integer(X), X >= 0,
    esBitmap([Pixel]) -> pixbit(_,_,B,D, Pixel), pixbit(X,Y,B,D, P);
    esHexmap([Pixel]) -> pixhex(_,_,H,D, Pixel), pixhex(X,Y,H,D, P);
    esPixmap([Pixel]) -> pixrgb(_,_,R,G,B,D, Pixel), pixrgb(X,Y,R,G,B,D, P).

% Descripción: Predicado que eliminar un tipo de elemento de una lista
% Dominio: elemento X list X variable. Recorrido: list
removerElemento(_, [], []).
removerElemento(Y, [Y|Xs], Zs):-
          removerElemento(Y, Xs, Zs), !.
removerElemento(X, [Y|Xs], [Y|Zs]):-
          removerElemento(X, Xs, Zs).


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

/*

% Descripción: Predicado que voltea la imagen horizontalmente
% Dominio: imagen X variable
% Recorrido: imagen
imageFlipH(Imagen, Imagen2):-
    esImage(Imagen),
    image(CoordX, CoordY, Pixeles, Imagen),
    CoordY_final is CoordY-1,
    flipH_formato(Pixeles, Pixeles, CoordY_final, 0, 0, PixelesH),
    sort(PixelesH, PixelesH2),
    removerElemento([], PixelesH2, Pixeles2),
    image(CoordX, CoordY, Pixeles2, Imagen2).
*/

% Descripción: Predicado que voltea la imagen horizontalmente
% Dominio: imagen X variable
% Recorrido: imagen
imageFlipH(Imagen, Imagen2):-
    imageIsCompress(Imagen) ->
        imageDecompress(Imagen, ImagenD),
        image(CoordX, CoordY, Pixeles, ImagenD),
        CoordY_final is CoordY-1,
        flipH_formato(Pixeles, Pixeles, CoordY_final, 0, 0, PixelesH),
        sort(PixelesH, PixelesH2),
        removerElemento([], PixelesH2, Pixeles2),
        image(CoordX, CoordY, Pixeles2, Imagen2)
        ;

        esImage(Imagen),
        image(CoordX, CoordY, Pixeles, Imagen),
        CoordY_final is CoordY-1,
        flipH_formato(Pixeles, Pixeles, CoordY_final, 0, 0, PixelesH),
        sort(PixelesH, PixelesH2),
        removerElemento([], PixelesH2, Pixeles2),
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
    sort(PixelesV, PixelesV2),
    removerElemento([], PixelesV2, Pixeles2),
    image(CoordX, CoordY, Pixeles2, Imagen2).

% Descripción: Predicado que verifica si el Pixel esta dentro del rango
% dado por crop
% Dominio: Pixel X int X int X int X int
rangoXY(Pixel, X1,X2,Y1,Y2):-
    esBitmap([Pixel]) -> pixbit(X,Y,_,_, Pixel),
        ((X >= X1), (X =< X2), (Y >= Y1), (Y =< Y2)) -> true; false;
    esHexmap([Pixel]) -> pixhex(X,Y,_,_, Pixel),
        ((X >= X1), (X =< X2), (Y >= Y1), (Y =< Y2)) -> true; false;
    esPixmap([Pixel]) -> pixrgb(X,Y,_,_,_,_, Pixel),
        ((X >= X1), (X =< X2), (Y >= Y1), (Y =< Y2)) -> true; false.

% Descripción: Predicado que crea eliminar los pixeles que no esten en
% el rango establecido por imageCrop
% Dominio: list X int X int x int X int X variable
% Recorrido: list
% Tipo de recursión: Natural, pues crea una lista con base a estados
% pendientes
crop_formato([], _,_, _, _, []).
crop_formato([Cabeza | Cola], X1, X2, Y1, Y2, [Cabeza2 | Cola2]):-
     rangoXY(Cabeza, X1, X2, Y1, Y2) ->
            append(Cabeza, [], Cabeza2),
            crop_formato(Cola, X1, X2, Y1, Y2, Cola2)
            ;
            crop_formato(Cola, X1, X2, Y1, Y2, Cola2).


% Descripción: Predicado que devuelve el mayor entre dos números.
% Dominio: int X int X variable. Recorrido: int
mayor(A,B,C):- A >= B -> C is A ; C is B.

% Descripción: Predicado que devuelve el menor entre dos números
% Dominio: int X int X variable. Recorrido: int
menor(A,B,C):- A =< B -> C is A ; C is B.


% Descripción: Predicado que elimina los pixeles de una imagen que no
% esten en el cuadrante definido
% Dominio: Imagen X int X int X int X int X variable
% Recorrido: Imagen
imageCrop(Imagen, X1,X2,Y1,Y2,Imagen2):-
    esImage(Imagen),
    image(CoordX, CoordY, Pixeles, Imagen),
    menor(X1,X2,X_menor), mayor(X1,X2,X_mayor),
    menor(Y1,Y2,Y_menor), mayor(Y1,Y2,Y_mayor),
    crop_formato(Pixeles, X_menor, X_mayor, Y_menor, Y_mayor, PixelesC),
    removerElemento([], PixelesC, Pixeles2), image(CoordX, CoordY, Pixeles2, Imagen2).


% Descripción: Predicado que entrega el equivalente a string de un
% número entero
% Dominio: int X variable
% Recorrido: string
numeroString(Num, String):-
    (Num = 0), String = "0"; (Num = 1), String = "1"; (Num = 2), String = "2"; (Num = 3), String = "3";
    (Num = 4), String = "4"; (Num = 5), String = "5"; (Num = 6), String = "6"; (Num = 7), String = "7";
    (Num = 8), String = "8"; (Num = 9), String = "9"; (Num = 10), String = "A"; (Num = 11), String = "B";
    (Num = 12), String = "C"; (Num = 13), String = "D"; (Num = 14), String = "E"; (Num = 15), String = "F"; false.

stringNumero(String, Num):-
    (String = "0"), Num = 0; (String = "1"), Num = 1; (String = "2"), Num = 2; (String = "3"), Num = 3;
    (String = "4"), Num = 4; (String = "5"), Num = 5; (String = "6"), Num = 6; (String = "7"), Num = 7;
    (String = "8"), Num = 8; (String = "9"), Num = 9; (String = "A"), Num = 10; (String = "B"), Num = 11;
    (String = "C"), Num = 12; (String = "D"), Num = 13, (String = "E"), Num = 14; (String = "F"), Num = 15; false.

listHEX_RGB(String, [R, G, B]):-
    sub_string(String,1,_,4,S1),
    hexRGB(S1,R),
    sub_string(String,3,_,2,S2),
    hexRGB(S2,G),
    sub_string(String,5,_,0,S3),
    hexRGB(S3,B).

% Descripción: Predicado que convierte una lista de 3 valores pixmap a string hexmap
% Dominio: list X variable
% Recorrido: string
listRGB_HEX([R, G, B], ColorHex):-
    rgbHex(R, R_hex), rgbHex(G, G_hex), rgbHex(B, B_hex),
    string_concat("#", R_hex, ColorParcial), string_concat(G_hex, B_hex, ColorParcial2),
    string_concat(ColorParcial, ColorParcial2, ColorHex).

rgbHex(Num, String):-
    Entero is Num // 16, Resto is Num mod 16,
    numeroString(Entero, N1), numeroString(Resto, N2),
    string_concat(N1,N2, String).

hexRGB(String, Color):-
    sub_string(String,0,_,1,S1),
    (S1 = "0") ->
        sub_string(String,1,_,0,S2), stringNumero(S2,N2),
        Color is N2
        ;
        sub_string(String,1,_,0,S2),
        (S2 = "0") ->
            sub_string(String,0,_,1,S1), stringNumero(S1,N1),
            Color is N1 * 16
            ;
            sub_string(String,0,_,1,S1), sub_string(String,1,_,0,S2),
            stringNumero(S1,N1), stringNumero(S2,N2),
            Color is N1 + N2 * 16.


% Descripción: Predicado que convierte un pixmap a hexmap
% Dominio: pixel x variable
% Recorrido: pixel
stringRGB(Pixel, PixelH):-
    pixrgb(X,Y,R,G,B,D,Pixel),
    listRGB_HEX([R,G,B], ColorHex),
    pixhex(X,Y,ColorHex, D, PixelH).

% Descripción: Predicado que cambia el formato de pixeles pixmap a
% hexmap
% Dominio: list X variable
% Recorrido: list
% Tipo de recursión: Natural pues crea una nueva lista con base a
% estados pendientes
formatoRGB_HEX([], []).
formatoRGB_HEX([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    stringRGB(Cabeza, NuevaCabeza),
    formatoRGB_HEX(Cola, Cola2).

% Descripción: Predicado que convierte una imagen pixmap a hexmap
% Dominio: imagen X variable
% Recorrido: imagen
imageRGBToHex(Imagen, Imagen2):-
    image(CoordX, CoordY, Pixeles, Imagen),
    esPixmap(Pixeles),
    formatoRGB_HEX(Pixeles, Pixeles2),
    image(CoordX, CoordY, Pixeles2, Imagen2).

% Descripción: Predicado que cambia un pixrgb_d por su color opuesto
% Dominio: pixrgb_d X variable
% Recorrido: pixrgb_d
imageInvertColorRGB(Pixel, Pixel2):-
    pixrgb(X,Y,R,G,B,D, Pixel),
    R_invert is 255 - R, G_invert is 255 - G, B_invert is 255 - B,
    pixrgb(X,Y,R_invert, G_invert, B_invert, D, Pixel2).

% Descripción: Predicado que verifica si dos pixbit tienen mismo
% bit, dos pixhex tiene el mismo hex y dos pixrgb tienen el mismo RGB
% Dominio: pixel X pixel
% Recorrido: Boleano
igualColor(Pixel, Pixel2):-
    esBitmap([Pixel, Pixel2]) ->
        pixbit(_,_,B,_, Pixel), pixbit(_,_,B,_, Pixel2) -> true; false
   ;
    esHexmap([Pixel, Pixel2]) ->
        pixhex(_,_,H,_, Pixel), pixhex(_,_,H,_, Pixel2) -> true; false
   ;
    esPixmap([Pixel, Pixel2]) ->
        pixrgb(_,_,R,G,B,_, Pixel), pixrgb(_,_,R,G,B,_, Pixel2) -> true; false
   ;
   false.

% Descripción: Predicado que eliminar un color de una lista de pixeles
% Dominio: list X pixel X variable
% Recorrido: list
% Tipo de recursión: Natural, pues crea una nueva lista con base a
% estados pendientes
eliminarColor([], _, []).
eliminarColor([Cabeza | Cola], Pixel, [NuevaCabeza | Cola2]):-
    igualColor(Cabeza, Pixel) ->
        eliminarColor(Cola, Pixel, Cola2);
        append(Cabeza, [], NuevaCabeza),
        eliminarColor(Cola, Pixel, Cola2).

esPixel(Elemento):-
    esBitmap([Elemento]) -> true;
    esPixmap([Elemento]) -> true;
    esHexmap([Elemento]) -> true; false.

% Predicado que obtiene la cabeza de pixeles
% Dominio: list X variable
% Recorrido: pixel
cabezaPixel([],[]).
cabezaPixel([Cabeza | Cola], NuevaCabeza):-
    esPixel(Cabeza) -> append(Cabeza, [], NuevaCabeza);
    cabezaPixel(Cola, NuevaCabeza).

% Descripción: Predicado que suma los elementos iguales de pixeles
% Dominio: list X pixel X variable
% Recorrio: int
% Tipo de recursión: Natural, pues cuenta con base a estados pendientes
contarColor([], _, 0).
contarColor([Cabeza | Cola], Pixel, C):-
    igualColor(Cabeza, Pixel) ->
        !, contarColor(Cola, Pixel, C1), C is C1+1;
        contarColor(Cola, Pixel, C).


% Descripción: Predicado que recopila la información de color de los pixeles
% Dominio: list X variable
% Recorrido: list
histograma_formato([], _).
histograma_formato([Cabeza | Cola], [[Cantidad , Elemento] | Cola2]):-
        colorPixel(Cabeza, Elemento),
        contarColor([Cabeza | Cola], Cabeza, Cantidad),
        eliminarColor([Cabeza | Cola], Cabeza, NuevaLista),
        removerElemento([], NuevaLista, NuevaLista2),
        histograma_formato(NuevaLista2, Cola2).

% Descripción: Predicado que obtiene el histograma de una imagen
% Dominio: imagen X variable
% Recorrido: list x variable
% imageToHistogram
imageToHistogram(Imagen, Histograma):-
    image(_,_,Pixeles, Imagen), histograma_formato(Pixeles, H),
    removerElemento([], H, Histograma).


% Descripción: Predicado que modifica los pixeles de una imagen rotando 90° a la derecha
% Dominio: list X list X int X int X int X int X variable
% Recorrido: list
rotate90_formato(_,[],_,_,_,_,_).
rotate90_formato([CabezaC | ColaC], [Cabeza | Cola], CoordX, Contador, CoordX_final, CoordY_final, [NuevaCabeza | Cola2]):-
    existeCoordXY([CabezaC | ColaC], CoordX, Contador) ->
        R1 is Contador+1,
        cambiarCoordXY(Cabeza, Contador, CoordX_final, NuevaCabeza),
        rotate90_formato([CabezaC | ColaC], Cola, CoordX, R1, CoordX_final, CoordY_final, Cola2)
        ;
        (Contador > CoordY_final) ->
            R3 is CoordX+1,
            R4 is CoordX_final-1,
            rotate90_formato([CabezaC | ColaC], [Cabeza | Cola], R3, 0, R4, CoordY_final, Cola2)
            ;
            R3 is Contador+1,
            rotate90_formato([CabezaC | ColaC], [Cabeza | Cola], CoordX, R3, CoordX_final, CoordY_final, Cola2).


% Descripción: Predicado que rota una imagen 90° a la derecha
% Dominio: imagen X variable
% Recorrido: imagen
imageRotate90(Imagen, Imagen2):-
    esImage(Imagen),
    image(CoordX, CoordY, Pixeles, Imagen),
    CoordX_final is CoordY-1, CoordY_final is CoordX-1,
    write("\nX="), write(CoordX_final), write("  Y="), write(CoordY_final),
    rotate90_formato(Pixeles, Pixeles, 0, 0, CoordX_final, CoordY_final, Pixeles2),
    %write("\nsalio"),
    %write("\nsalio2"),
    image(CoordY, CoordX, Pixeles2, Imagen2).

% Descripción: Predicado que obtiene el pixel más repetido
% Dominio: list x variable
% Recorrido: pixel
pixelMasRepetido([], _, E, Elemento):- Elemento = E.
pixelMasRepetido([Cabeza | Cola], Cant, Elem, Elemento):-
    [Cantidad , _] = Cabeza,
    (Cantidad > Cant) ->
        [Cantidad, E] = Cabeza,
        pixelMasRepetido(Cola, Cantidad, E, Elemento);
        pixelMasRepetido(Cola, Cant, Elem, Elemento).


% Descripción: Predicado que comprime pixeles
% Dominio: list X color X variable
% Recorrido: list
comprimirPixeles([],_,[]).
comprimirPixeles([Cabeza | Cola], Color, [NuevaCabeza | Cola2]):-
    colorPixel(Cabeza, C),
    (C = Color) ->
       comprimirPixel(Cabeza, NuevaCabeza),
       comprimirPixeles(Cola, Color, Cola2);
       NuevaCabeza = Cabeza,
       comprimirPixeles(Cola, Color, Cola2).

comprimirPixel(Pixel, Pixel2):-
    esBitmap([Pixel]) ->
        pixbit(X,Y,B,D,Pixel), pixbit_comprimido(X,Y,[-1, B],D,Pixel2);
    esHexmap([Pixel]) ->
        pixhex(X,Y,S,D,Pixel), listHEX_RGB(S, H), pixhex_comprimido(X,Y,H,D,Pixel2);
    esPixmap([Pixel]) ->
        pixrgb(X,Y,R,G,B,D,Pixel),
        rgbHex(R, HexR),
        rgbHex(G, HexG),
        rgbHex(B, HexB), pixrgb_comprimido(X,Y,HexR,HexG,HexB,D,Pixel2);
    false.

imageCompress(Imagen, Imagen2):-
    esImage(Imagen),
    image(X,Y,Pixeles, Imagen),
    imageToHistogram(Imagen, H),
    pixelMasRepetido(H, -1, _, PixelMasRepetido),
    comprimirPixeles(Pixeles, PixelMasRepetido, PixelesC),
    sort(PixelesC, PixelesC2),
    removerElemento([], PixelesC2, Pixeles2),
    image(X,Y,Pixeles2, Imagen2).

% Descripción: Predicado que verifica si dos pixeles tienen la misma
% coordenada (x,y)
% Dominio: pixel X pixel
% Recorrido: Boleano
igualCoordXY(Pixel, Pixel2):-
    esPixmap([Pixel , Pixel2]) ->
        pixrgb(X,Y,_,_,_,_, Pixel), pixrgb(X,Y,_,_,_,_, Pixel2) -> true; false
    ;
    esBitmap([Pixel , Pixel2]) ->
         pixbit(X,Y,_,_, Pixel), pixbit(X,Y,_,_, Pixel2) -> true; false
    ;
    esHexmap([Pixel , Pixel2]) ->
         pixhex(X,Y,_,_, Pixel), pixhex(X,Y,_,_, Pixel2) -> true; false
    ;
    false.


% Descripción: Predicado que reemplaza un pixel de pixeles
% Dominio: list X Pixel X variable
% Recorrido: list
changePixel([], _, []).
changePixel([Cabeza | Cola], Pixel, [NuevaCabeza | Cola2]):-
    igualCoordXY(Cabeza, Pixel) ->
            append(Pixel, [], NuevaCabeza),
            changePixel(Cola, Pixel, Cola2)
            ;
            append(Cabeza, [], NuevaCabeza),
            changePixel(Cola, Pixel, Cola2).


% Descripción: Predicado que reemplaza un pixel de la imagen
% Dominio: Imagen X pixel X variable
% Recorrido: Imagen
imageChangePixel(Imagen, Pixel, Imagen2):-
    esImage(Imagen),
    image(CoordX, CoordY, Pixeles, Imagen),
    changePixel(Pixeles, Pixel, Pixeles2),
    image(CoordX, CoordY, Pixeles2, Imagen2).



colorPixel(Pixel, Color):-
    esBitmap([Pixel]) ->  pixbit(_,_,Color, _, Pixel);
    esHexmap([Pixel]) -> pixhex(_,_, Color, _, Pixel);
    esPixmap([Pixel]) -> pixrgb(_,_,R,G,B, _, Pixel), Color = [R,G,B].


%espacioRelleno(Pixel, String):-
%    esBitmap([Pixel]) -> string_concat("", "", String);
%    esHexmap([Pixel]) -> string_concat("", "", String);
%    esPixmap([Pixel]) -> string_concat("", "\t\t\t\t", String).

imageString_formato(_, [],_,_, _, ["\n"]).
imageString_formato([CabezaC| ColaC], [Cabeza| Cola], CoordY, CoordX, CoordY_final, [String | Cola2]):-
    existeCoordXY([CabezaC | ColaC], CoordX, CoordY) ->

              R is CoordY + 1,
              colorPixel(Cabeza, Color),
              string_concat(Color, "\t", String),
              imageString_formato([CabezaC | ColaC], Cola, R, CoordX, CoordY_final, Cola2)
        ;
              CoordY > CoordY_final ->
                       R3 is CoordX + 1,
                       string_concat("\n", "", String),
                       imageString_formato([CabezaC | ColaC], [Cabeza | Cola], 0, R3, CoordY_final, Cola2)
                       ;
                       R2 is CoordY + 1,
                       %espacioRelleno(CabezaC, Relleno),
                       string_concat("", "\t", String),
                       imageString_formato([CabezaC | ColaC], [Cabeza | Cola], R2, CoordX, CoordY_final, Cola2).
/*
% Descripción: Predicado que crea una cadena de string de la imagen
% Dominio: image X variable
% Recorrido: string
imageToString(Imagen, Cadena):-
    esImage(Imagen), image(_,CoordY,Pixeles, Imagen), CoordY_final is CoordY-1,
    imageString_formato(Pixeles, Pixeles, 0, 0, CoordY_final, String),
    atomics_to_string(String, Cadena).
*/

% Descripción: Predicado que crea una cadena de string de la imagen
% Dominio: image X variable
% Recorrido: string
imageToString(Imagen, Cadena):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       image(_,CoordY,Pixeles, ImagenD),
       CoordY_final is CoordY-1,
       imageString_formato(Pixeles, Pixeles, 0, 0, CoordY_final, String),
       atomics_to_string(String, Cadena)
       ;
       esImage(Imagen), image(_,CoordY,Pixeles, Imagen),
       CoordY_final is CoordY-1,
       imageString_formato(Pixeles, Pixeles, 0, 0, CoordY_final, String),
       atomics_to_string(String, Cadena).


igualProfundidad(Pixel, Pixel2):-
    esBitmap([Pixel]) ->
        pixbit(_,_,_,D, Pixel), pixbit(_,_,_,D, Pixel2) ->  true; false;
    esPixmap([Pixel]) ->
        pixrgb(_,_,_,_,_,D, Pixel), pixrgb(_,_,_,_,_,D, Pixel2) ->  true; false;
    esHexmap([Pixel]) ->
        pixhex(_,_,_,D, Pixel), pixhex(_,_,_,D, Pixel2) ->  true; false.

eliminarProfundidad([], _, []).
eliminarProfundidad([Cabeza | Cola], Pixel, [NuevaCabeza | Cola2]):-
    igualProfundidad(Cabeza, Pixel) ->
        eliminarProfundidad(Cola, Pixel, Cola2);
        NuevaCabeza = Cabeza,
        eliminarProfundidad(Cola, Pixel, Cola2).

obtenerProfundidad(Pixel, D):-
    esPixmap([Pixel]) ->  pixrgb(_,_,_,_,_,D, Pixel);
    esHexmap([Pixel]) ->  pixhex(_,_,_,D, Pixel);
    esBitmap([Pixel]) ->  pixbit(_,_,_,D, Pixel).

listaProfundidad([],[]).
listaProfundidad([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    obtenerProfundidad(Cabeza, NuevaCabeza),
    eliminarProfundidad([Cabeza | Cola], Cabeza, NuevaLista),
    removerElemento([], NuevaLista, NuevaLista2),
    listaProfundidad(NuevaLista2, Cola2).

reemplazarPixelProfundidad(Pixel, Pixel2):-
    esBitmap([Pixel]) ->  pixbit(X,Y,_,D, Pixel), pixbit(X,Y,1,D, Pixel2);
    esPixmap([Pixel]) ->  pixrgb(X,Y,_,_,_,D, Pixel), pixrgb(X,Y,255,255,255,D, Pixel2);
    esHexmap([Pixel]) ->  pixhex(X,Y,_,D, Pixel), pixhex(X,Y,"#FFFFFF",D, Pixel2).

pixelesIgualProfundidad(_,[],[]).
pixelesIgualProfundidad(Profundidad, [Cabeza | Cola], [NuevaCabeza | Cola2]):-
    obtenerProfundidad(Cabeza, D),
    (D = Profundidad) ->
        NuevaCabeza = Cabeza,
        pixelesIgualProfundidad(Profundidad, Cola, Cola2);
        reemplazarPixelProfundidad(Cabeza, NuevaCabeza),
        pixelesIgualProfundidad(Profundidad, Cola, Cola2).

profundidad_formato(_,[],_).
profundidad_formato(Imagen, [Profundidad | ColaD], [NuevaCabeza | Cola2]):-
    write("1 "),
    image(CoordX, CoordY, Pixeles, Imagen),
    write("2 "),
    pixelesIgualProfundidad(Profundidad, Pixeles, PixelesD),
    write("3 "),
    image(CoordX, CoordY, PixelesD, NuevaCabeza),
    write("4 "),
    profundidad_formato(Imagen, ColaD, Cola2).


pixelDescomprimido(Pixel, Pixel2):-
    esPixmapComprimido([Pixel]) ->
        pixrgb_comprimido(X,Y,R_hex,G_hex,B_hex,D,Pixel),
        hexRGB(R_hex,R), hexRGB(G_hex,G), hexRGB(B_hex, B),
        pixrgb(X,Y,R,G,B,D,Pixel2);
    esHexmapComprimido([Pixel]) ->
        pixhex_comprimido(X,Y,H_list,D, Pixel),
        listRGB_HEX(H_list, H), pixhex(X,Y,H,D, Pixel2);
    esBitmapComprimido([Pixel])->
        pixbit_comprimido(X,Y,B_list,D, Pixel),
        [_, B] = B_list, pixbit(X,Y,B,D, Pixel2).


pixelesDescomprimidos([], []).
pixelesDescomprimidos([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    esPixel(Cabeza) ->
        NuevaCabeza = Cabeza,
        pixelesDescomprimidos(Cola, Cola2)
        ;
        pixelDescomprimido(Cabeza, NuevaCabeza), pixelesDescomprimidos(Cola,Cola2).

% Descripción: Predicado que descomprime una imagen
% Dominio:
% Recorrido
imageDecompress(Imagen, Imagen2):-
    imageIsCompress(Imagen),
    image(CoordX, CoordY, Pixeles, Imagen),
    pixelesDescomprimidos(Pixeles, Pixeles2),
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
        eliminar(Cola, Elemento, Cola2),!.

agregarElemento([H|T], E, [H | T2]):-
    agregarElemento(T,E,T2).


pixelLista([Cabeza | Cola], L):-
    esPixel(Cabeza) ->  append([Cabeza], Cola, L);
                        append(Cola, [], L).

cabezaLista([Cabeza | _], P):- append(Cabeza, [], P).

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

