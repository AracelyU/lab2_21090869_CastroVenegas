:- use_module(tda_pixbit_21090869_CastroVenegas).
:- use_module(tda_pixhex_21090869_CastroVenegas).
:- use_module(tda_pixrgb_21090869_CastroVenegas).


% Descripci�n: Predicado que define como es una imagen
% Dominio: int X int X list X variable (list)
% Recorrido: image
% Tipo: Constructor
image(Ancho, Largo, Pixel, [Ancho, Largo, Pixel]):-
    integer(Ancho),Ancho >= 0, integer(Largo), Largo >= 0, is_list(Pixel).

% Descripci�n: Predicado que verifica si una imagen es Bitmap
% Dominio: image
% Recorrido: Boleano
% Tipo: Pertenencia
imageIsBitmap(Imagen):- image(_,_,P, Imagen), esBitmap(P) -> true; false.

% Descripi�n: Predicado que verifica si una imagen es Pixmap
% Dominio: image
% Recorrido: Boleano
% Tipo: Pertenencia
imageIsPixmap(Imagen):- image(_,_,P, Imagen), esPixmap(P) -> true; false.

% Descripci�n: Predicado que verifica si una imagen es Hexmap
% Dominio: image
% Recorrido: Boleano
% Tipo: Pertenencia
imageIsHexmap(Imagen):- image(_,_,P, Imagen), esHexmap(P) -> true; false.

% Descripci�n: Predicado que verifica si una imagen fue comprimida
% Dominio: image
% Recorrido: Boleano
% Tipo: Pertenencia
imageIsCompress(Imagen):-
    obtenerPixeles(Imagen, P),
    esHexmapComprimido(P) -> true;
    obtenerPixeles(Imagen, P),
    esPixmapComprimido(P) -> true;
    obtenerPixeles(Imagen, P),
    esBitmapComprimido(P) -> true; false.

% Descripci�n: Predicado que verifica si la entrada es una imagen
% Dominio: image
% Recorrido: Boleano
% Tipo: Pertenencia
esImage(Imagen):-
    imageIsBitmap(Imagen) -> true; imageIsHexmap(Imagen) -> true; imageIsPixmap(Imagen) -> true; false.


% Descripci�n: Predicado que obtiene los pixeles de una imagen
% Dominio: image X variable (list)
% Recorrido: list
% Tipo: Selector
obtenerPixeles(Imagen , Pixeles):-
    image(_,_, Pixeles, Imagen).

% Descripci�n: Predicado que verifica existencia de (x,y) en Pixel
% cuando se cambie el formato de pixeles en la imagen
% Dominio: Pixel, int, int Recorrido: Boleano Tipo: Pertenencia,
% apoya a predicado existeCoordXY
% Tipo: Otras funciones
existeCoord(Cabeza, X, Y):-
    esBitmap([Cabeza]) -> obtCoordPixbit(Cabeza, CoordX, CoordY), CoordX = X, CoordY = Y  -> true; false
    ;
    esHexmap([Cabeza]) -> obtCoordPixhex(Cabeza, CoordX, CoordY), CoordX = X, CoordY = Y -> true; false
    ;
    esPixmap([Cabeza]) -> obtCoordPixrgb(Cabeza, CoordX, CoordY), CoordX = X, CoordY = Y -> true; false.

% Descripci�n: Predicado que verifica si existe una coordenada (X,Y) en
% especifica en la lista de pixeles de una imagen, para hacer flipH,
% flipV entre otros en una imagen crop
% Dominio: Pixel x int x int
% Recorrido: Boleano Tipo: Pertenencia
% Tipo: Otras funciones
% Tipo de recursi�n: Cola, da resultado inmediato
existeCoordXY([Cabeza | Cola], X, Y):- existeCoord(Cabeza, X, Y) -> true; existeCoordXY(Cola, X, Y).

% Descripci�n: Predicado que verifica si el Pixel esta dentro del rango
% dado por crop
% Dominio: Pixel X int X int X int X int
% Recorrido: Boleano
% Tipo: Otras funciones
rangoXY(Pixel, X1,X2,Y1,Y2):-
    esBitmap([Pixel]) -> obtCoordPixbit(Pixel, X, Y),
        ((X >= X1), (X =< X2), (Y >= Y1), (Y =< Y2)) -> true; false;
    esHexmap([Pixel]) -> obtCoordPixhex(Pixel, X, Y),
        ((X >= X1), (X =< X2), (Y >= Y1), (Y =< Y2)) -> true; false;
    esPixmap([Pixel]) -> obtCoordPixrgb(Pixel, X, Y),
        ((X >= X1), (X =< X2), (Y >= Y1), (Y =< Y2)) -> true; false.

% Descripci�n: Predicado que modifica CoordX y CoordY de un Pixel para
% flipH, flipV entre otros
% Dominio: Pixel X int X int X variable
% (Pixel) Recorrido: Pixel
% Tipo: Otras funciones
cambiarCoordXY(Pixel,X, Y, P):-
    integer(Y), Y >= 0, integer(X), X >= 0,
    esBitmap([Pixel]) -> obtProfundidadPixbit(Pixel, D), obtColorPixbit(Pixel, B), pixbit(X,Y,B,D, P);
    esHexmap([Pixel]) -> obtProfundidadPixhex(Pixel, D), obtColorPixhex(Pixel, H), pixhex(X,Y,H,D, P);
    esPixmap([Pixel]) -> obtProfundidadPixhex(Pixel, D), obtColorPixrgb(Pixel, R,G,B), pixrgb(X,Y,R,G,B,D, P).

% Descripci�n: Predicado que eliminar las variables sin valor asignado
% de la lista de pixeles tras flipH, flipV, entre otros
% Dominio: elemento X list X variable (list).
% Recorrido: list
% Tipo: Otras funciones
removerElemento(_, [], []).
removerElemento(Y, [Y|Xs], Zs):-
          removerElemento(Y, Xs, Zs), !.
removerElemento(X, [Y|Xs], [Y|Zs]):-
          removerElemento(X, Xs, Zs).


% Descripci�n: Predicado que voltea los pixeles horizontalmente, apoya a
% imageflipH
% Dominio: list X list X int X int X int X variable (list)
% Recorrido: list Tipo: Otras funciones
% Tipo de recursi�n: Natural, pues crea una lista con base a estados
% pendientes
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


% Descripci�n: Predicado que voltea la imagen horizontalmente
% Dominio: image X variable (image)
% Recorrido: image
% Tipo: Modificador
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

% Descripci�n: Predicado que voltea los pixeles verticalmente ,apoya a
% imageFlipV
% Dominio: list X list X int X int X int X int X variable(list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, pues crea una lista con base a estados
% pendientes
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

% Descripci�n: Predicado que voltea la imagen verticalmente
% Dominio: image X variable (image)
% Recorrido: image
% Tipo: Modificador
imageFlipV(Imagen, Imagen2):-
    imageIsCompress(Imagen) ->
        imageDecompress(Imagen, ImagenD),
        image(CoordX, CoordY, Pixeles, ImagenD),
        CoordX_final is CoordX-1, CoordY_final is CoordY-1,
        flipV_formato(Pixeles, Pixeles, CoordX_final, 0, 0, CoordY_final, PixelesV),
        sort(PixelesV, PixelesV2),
        removerElemento([], PixelesV2, Pixeles2),
        image(CoordX, CoordY, Pixeles2, Imagen2)
       ;
       esImage(Imagen),
       image(CoordX, CoordY, Pixeles, Imagen),
       CoordX_final is CoordX-1,
       CoordY_final is CoordY-1,
       flipV_formato(Pixeles, Pixeles, CoordX_final, 0, 0, CoordY_final, PixelesV),
       sort(PixelesV, PixelesV2),
       removerElemento([], PixelesV2, Pixeles2),
       image(CoordX, CoordY, Pixeles2, Imagen2).


% Descripci�n: Predicado que crea eliminar los pixeles que no esten en
% el rango establecido por imageCrop
% Dominio: list X int X int x int X int X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, pues crea una lista con base a estados
% pendientes
crop_formato([], _,_, _, _, []).
crop_formato([Cabeza | Cola], X1, X2, Y1, Y2, [Cabeza2 | Cola2]):-
     rangoXY(Cabeza, X1, X2, Y1, Y2) ->
            append(Cabeza, [], Cabeza2),
            crop_formato(Cola, X1, X2, Y1, Y2, Cola2)
            ;
            crop_formato(Cola, X1, X2, Y1, Y2, Cola2).


% Descripci�n: Predicado que devuelve el mayor entre dos n�meros, apoya
% a imageCrop
% Dominio: int X int X variable (int). Recorrido: int
% Tipo: Otras funciones
mayor(A,B,C):- A >= B -> C is A ; C is B.

% Descripci�n: Predicado que devuelve el menor entre dos n�meros, apoya
% a imageCrop
% Dominio: int X int X variable (int). Recorrido: int
% Tipo: Otras funciones
menor(A,B,C):- A =< B -> C is A ; C is B.


% Descripci�n: Predicado que elimina los pixeles de una imagen que no
% esten en el cuadrante definido
% Dominio: image X int X int X int X int X variable (image)
% Recorrido: image
% Tipo: Modificador
imageCrop(Imagen, X1,X2,Y1,Y2,Imagen2):-
   imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       image(CoordX, CoordY, Pixeles, ImagenD),
       menor(X1,X2,X_menor), mayor(X1,X2,X_mayor),
       menor(Y1,Y2,Y_menor), mayor(Y1,Y2,Y_mayor),
       crop_formato(Pixeles, X_menor, X_mayor, Y_menor, Y_mayor, PixelesC),
       removerElemento([], PixelesC, Pixeles2), image(CoordX, CoordY, Pixeles2, Imagen2)
       ;
       esImage(Imagen),
       image(CoordX, CoordY, Pixeles, Imagen),
       menor(X1,X2,X_menor), mayor(X1,X2,X_mayor),
       menor(Y1,Y2,Y_menor), mayor(Y1,Y2,Y_mayor),
       crop_formato(Pixeles, X_menor, X_mayor, Y_menor, Y_mayor, PixelesC),
       removerElemento([], PixelesC, Pixeles2), image(CoordX, CoordY, Pixeles2, Imagen2).

% Descripci�n: Predicado que entrega el equivalente a entero de un
% string, apoya en imageCompress
% Dominio: string X variable (int)
% Recorrido: int
% Tipo: Otras funciones
stringNumero(String, Num):-
    (String = "0"), Num = 0; (String = "1"), Num = 1; (String = "2"), Num = 2; (String = "3"), Num = 3;
    (String = "4"), Num = 4; (String = "5"), Num = 5; (String = "6"), Num = 6; (String = "7"), Num = 7;
    (String = "8"), Num = 8; (String = "9"), Num = 9; (String = "A"), Num = 10; (String = "B"), Num = 11;
    (String = "C"), Num = 12; (String = "D"), Num = 13, (String = "E"), Num = 14; (String = "F"), Num = 15.


% Descripci�n: Predicado que entrega el equivalente a string de un
% n�mero entero, apoya a imageRGBToHex
% Dominio: int X variable (string)
% Recorrido: string
% Tipo: Otras funciones
numeroString(Num, String):-
    (Num = 0), String = "0"; (Num = 1), String = "1"; (Num = 2), String = "2"; (Num = 3), String = "3";
    (Num = 4), String = "4"; (Num = 5), String = "5"; (Num = 6), String = "6"; (Num = 7), String = "7";
    (Num = 8), String = "8"; (Num = 9), String = "9"; (Num = 10), String = "A"; (Num = 11), String = "B";
    (Num = 12), String = "C"; (Num = 13), String = "D"; (Num = 14), String = "E"; (Num = 15), String = "F".

% Descripci�n: Predicado que convierte un string pixhex en una lista de
% tres valores enteros
% Dominio: string X variable (int)
% Recorrido: int
% Tipo: Otras funciones
listHEX_RGB(String, [R, G, B]):-
    sub_string(String,1,_,4,S1),
    hexRGB(S1,R),
    sub_string(String,3,_,2,S2),
    hexRGB(S2,G),
    sub_string(String,5,_,0,S3),
    hexRGB(S3,B).

% Descripci�n: Predicado que convierte una lista de 3 valores pixmap a string hexmap
% Dominio: list X variable (string)
% Recorrido: string
% Tipo: Otras funciones
listRGB_HEX([R, G, B], ColorHex):-
    rgbHex(R, R_hex), rgbHex(G, G_hex), rgbHex(B, B_hex),
    string_concat("#", R_hex, ColorParcial), string_concat(G_hex, B_hex, ColorParcial2),
    string_concat(ColorParcial, ColorParcial2, ColorHex).

% Descripci�n: Predicado que transforma un n�mero con unidad y decena a
% su equivalente string
% Dominio: int X variable (String)
% Recorrido: String
% Tipo: Otras funciones
rgbHex(Num, String):-
    Entero is Num // 16, Resto is Num mod 16,
    numeroString(Entero, N1), numeroString(Resto, N2),
    string_concat(N1,N2, String).

% Descripci�n: Predicado que transforma un string con dos letras a su
% equivalente a n�mero, apoya a imageCompress
% Dominio: string X variable (int)
% Recorrido: int
% Tipo: Otras funciones
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


% Descripci�n: Predicado que convierte un pixmap a hexmap
% Dominio: pixrgb x variable (pixhex)
% Recorrido: pixhex
% Tipo: Otras funciones
stringRGB(Pixel, PixelH):-
    obtProfundidadPixrgb(Pixel, D), obtColorPixrgb(Pixel, R,G,B), obtCoordPixrgb(Pixel, X,Y),
    listRGB_HEX([R,G,B], ColorHex),
    pixhex(X,Y,ColorHex, D, PixelH).

% Descripci�n: Predicado que cambia el formato de pixeles pixmap a
% hexmap
% Dominio: list X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural pues crea una nueva lista con base a
% estados pendientes
formatoRGB_HEX([], []).
formatoRGB_HEX([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    stringRGB(Cabeza, NuevaCabeza),
    formatoRGB_HEX(Cola, Cola2).

% Descripci�n: Predicado que convierte una imagen pixmap a hexmap
% Dominio: image X variable (image)
% Recorrido: image
% Tipo: Modificador
imageRGBToHex(Imagen, Imagen2):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       image(CoordX, CoordY, Pixeles, ImagenD),
       esPixmap(Pixeles),
       formatoRGB_HEX(Pixeles, Pixeles2),
       image(CoordX, CoordY, Pixeles2, Imagen2)
       ;
       image(CoordX, CoordY, Pixeles, Imagen),
       esPixmap(Pixeles),
       formatoRGB_HEX(Pixeles, Pixeles2),
       image(CoordX, CoordY, Pixeles2, Imagen2).

% Descripci�n: Predicado que cambia un pixrgb por su color opuesto
% Dominio: pixrgb X variable (pixrgb)
% Recorrido: pixrgb
% Tipo: Otras funciones
imageInvertColorRGB(Pixel, Pixel2):-
    obtProfundidadPixrgb(Pixel, D), obtColorPixrgb(Pixel, R,G,B), obtCoordPixrgb(Pixel, X,Y),
    R_invert is 255 - R, G_invert is 255 - G, B_invert is 255 - B,
    pixrgb(X,Y,R_invert, G_invert, B_invert, D, Pixel2).

% Descripci�n: Predicado que verifica si dos pixbit tienen mismo
% bit, dos Pixhex tiene el mismo hex y dos pixrgb tienen el mismo RGB
% Dominio: Pixel X Pixel
% Recorrido: Boleano
% Tipo: Otras funciones
igualColor(Pixel, Pixel2):-
    esBitmap([Pixel, Pixel2]) ->
        obtColorPixbit(Pixel, B), obtColorPixbit(Pixel2,B) -> true; false
   ;
    esHexmap([Pixel, Pixel2]) ->
        obtColorPixhex(Pixel, B), obtColorPixhex(Pixel2,B) -> true; false
   ;
    esPixmap([Pixel, Pixel2]) ->
        obtColorPixrgb(Pixel, R,G,B), obtColorPixrgb(Pixel2, R,G,B) -> true; false
   ;
   false.

% Descripci�n: Predicado que eliminar un color de una lista de pixeles
% Dominio: list X Pixel X variable (list)
% Recorrido: list
% Tipo de recursi�n: Natural, pues crea una nueva lista con base a
% estados pendientes
% Tipo: Otras funciones
eliminarColor([], _, []).
eliminarColor([Cabeza | Cola], Pixel, [NuevaCabeza | Cola2]):-
    igualColor(Cabeza, Pixel) ->
        eliminarColor(Cola, Pixel, Cola2);
        append(Cabeza, [], NuevaCabeza),
        eliminarColor(Cola, Pixel, Cola2).


% Descripci�n: Predicado que verifica si es un pixrgb, pixhex o pixbit
% Dominio: Pixel
% Recorrido: Boleano
% Tipo: Otras funciones
esPixel(Elemento):-
    esBitmapComprimido([Elemento]) -> false;
    esPixmapComprimido([Elemento]) -> false;
    esHexmapComprimido([Elemento]) -> false;
    esBitmap([Elemento]) -> true;
    esPixmap([Elemento]) -> true;
    esHexmap([Elemento]) -> true; false.

% Descripci�n: Predicado que suma los elementos iguales de pixeles
% Dominio: list X Pixel X variable (int)
% Recorrio: int
% Tipo: Selector
% Tipo de recursi�n: Natural, pues cuenta con base a estados pendientes
contarColor([], _, 0).
contarColor([Cabeza | Cola], Pixel, C):-
    igualColor(Cabeza, Pixel) ->
        !, contarColor(Cola, Pixel, C1), C is C1+1;
        contarColor(Cola, Pixel, C).


% Descripci�n: Predicado que recopila la informaci�n de color de los pixeles
% Dominio: list X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, pues crea la lista con base a estados
% pendientes
histograma_formato([], _).
histograma_formato([Cabeza | Cola], [[Cantidad , Elemento] | Cola2]):-
        colorPixel(Cabeza, Elemento),
        contarColor([Cabeza | Cola], Cabeza, Cantidad),
        eliminarColor([Cabeza | Cola], Cabeza, NuevaLista),
        removerElemento([], NuevaLista, NuevaLista2),
        histograma_formato(NuevaLista2, Cola2).

% Descripci�n: Predicado que obtiene el histograma de una imagen
% Dominio: image X variable (image)
% Recorrido: list
% Tipo: Otras funciones
imageToHistogram(Imagen, Histograma):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       image(_, _, Pixeles, ImagenD),
       histograma_formato(Pixeles, H), removerElemento([], H, Histograma)
       ;
       esImage(Imagen),
       image(_,_,Pixeles, Imagen), histograma_formato(Pixeles, H),
       removerElemento([], H, Histograma).

% Descripci�n: Predicado que modifica los pixeles de una imagen rotando 90� a la derecha
% Dominio: list X list X int X int X int X int X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, crea la lista con base a estados
% pendientes
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

% Descripci�n: Predicado que rota una imagen 90� a la derecha
% Dominio: image X variable (image)
% Recorrido: image
% Tipo: Modificador
imageRotate90(Imagen, Imagen2):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       image(CoordX, CoordY, Pixeles, ImagenD),
       CoordX_final is CoordY-1, CoordY_final is CoordX-1,
       rotate90_formato(Pixeles, Pixeles, 0,0, CoordX_final, CoordY_final, PixelesR),
       removerElemento([], PixelesR, PixelesR2), sort(PixelesR2, Pixeles2),
       image(CoordY, CoordX, Pixeles2, Imagen2)
       ;
       esImage(Imagen),
       image(CoordX, CoordY, Pixeles, Imagen),
       CoordX_final is CoordY-1, CoordY_final is CoordX-1,
       rotate90_formato(Pixeles, Pixeles, 0,0, CoordX_final, CoordY_final, PixelesR),
       removerElemento([], PixelesR, PixelesR2), sort(PixelesR2, Pixeles2),
       image(CoordY, CoordX, Pixeles2, Imagen2).

% Descripci�n: Predicado que obtiene el pixel m�s repetido
% Dominio: list x variable (Pixel)
% Recorrido: Pixel
% Tipo: Otras funciones
% Tipo de recursi�n: Cola, entrega resultado inmediato
pixelMasRepetido([], _, E, Elemento):- Elemento = E.
pixelMasRepetido([Cabeza | Cola], Cant, Elem, Elemento):-
    [Cantidad , _] = Cabeza,
    (Cantidad > Cant) ->
        [Cantidad, E] = Cabeza,
        pixelMasRepetido(Cola, Cantidad, E, Elemento);
        pixelMasRepetido(Cola, Cant, Elem, Elemento).


% Descripci�n: Predicado que comprime un pixel
% Dominio: Pixel X variable (Pixel_comprimido)
% Recorrido: Pixel_comprimido
comprimirPixel(Pixel, Pixel2):-
    esBitmap([Pixel]) ->
        obtProfundidadPixbit(Pixel, D), obtCoordPixbit(Pixel, X,Y), obtColorPixbit(Pixel, B),
        pixbit_comprimido(X,Y,[-1, B],D,Pixel2);
    esHexmap([Pixel]) ->
        obtProfundidadPixhex(Pixel, D), obtCoordPixhex(Pixel, X,Y), obtColorPixhex(Pixel, S),
        listHEX_RGB(S, H), pixhex_comprimido(X,Y,H,D,Pixel2);
    esPixmap([Pixel]) ->
        obtProfundidadPixrgb(Pixel, D), obtCoordPixrgb(Pixel, X,Y), obtColorPixrgb(Pixel, R,G,B),
        rgbHex(R, HexR),
        rgbHex(G, HexG),
        rgbHex(B, HexB), pixrgb_comprimido(X,Y,HexR,HexG,HexB,D,Pixel2);
    false.

% Descripci�n: Predicado que comprime pixeles
% Dominio: list X color X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, crea la lista con base a estados
% pendientes
comprimirPixeles([],_,[]).
comprimirPixeles([Cabeza | Cola], Color, [NuevaCabeza | Cola2]):-
    colorPixel(Cabeza, C),
    (C = Color) ->
       comprimirPixel(Cabeza, NuevaCabeza),
       comprimirPixeles(Cola, Color, Cola2);
       NuevaCabeza = Cabeza,
       comprimirPixeles(Cola, Color, Cola2).

% Descripci�n: Predicado que comprime una image por el color m�s
% repetido
% Dominio: image X variable (image)
% Recorrido: image
% Tipo: Modificador
imageCompress(Imagen, Imagen2):-
    esImage(Imagen),
    image(X,Y,Pixeles, Imagen),
    imageToHistogram(Imagen, H),
    pixelMasRepetido(H, -1, _, PixelMasRepetido),
    comprimirPixeles(Pixeles, PixelMasRepetido, PixelesC),
    sort(PixelesC, PixelesC2),
    removerElemento([], PixelesC2, Pixeles2),
    image(X,Y,Pixeles2, Imagen2).

% Descripci�n: Predicado que verifica si dos pixeles tienen la misma
% coordenada (x,y)
% Dominio: Pixel X Pixel
% Recorrido: Boleano
% Tipo: Otras funciones
igualCoordXY(Pixel, Pixel2):-
    esPixmap([Pixel , Pixel2]) ->
        obtCoordPixrgb(Pixel, X,Y), obtCoordPixrgb(Pixel2, X,Y) -> true; false
    ;
    esBitmap([Pixel , Pixel2]) ->
        obtCoordPixbit(Pixel, X,Y), obtCoordPixbit(Pixel2, X,Y) -> true; false
    ;
    esHexmap([Pixel , Pixel2]) ->
        obtCoordPixhex(Pixel, X,Y), obtCoordPixhex(Pixel2, X,Y) -> true; false
    ;
    false.


% Descripci�n: Predicado que reemplaza un pixel de pixeles
% Dominio: list X Pixel X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, crea la lista con base a estados
% pendientes
changePixel([], _, []).
changePixel([Cabeza | Cola], Pixel, [NuevaCabeza | Cola2]):-
    igualCoordXY(Cabeza, Pixel) ->
            append(Pixel, [], NuevaCabeza),
            changePixel(Cola, Pixel, Cola2)
            ;
            append(Cabeza, [], NuevaCabeza),
            changePixel(Cola, Pixel, Cola2).


% Descripci�n: Predicado que reemplaza un pixel de la imagen
% Dominio: image X Pixel X variable (image)
% Recorrido: image
% Tipo: Modificador
imageChangePixel(Imagen, Pixel, Imagen2):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       image(CoordX, CoordY, Pixeles, ImagenD),
       changePixel(Pixeles, Pixel, Pixeles2),
       image(CoordX, CoordY, Pixeles2, Imagen2)
       ;
       esImage(Imagen),
       image(CoordX, CoordY, Pixeles, Imagen),
       changePixel(Pixeles, Pixel, Pixeles2),
       image(CoordX, CoordY, Pixeles2, Imagen2).

% Descripci�n: Predicado que obtiene el color de un pixel
% Dominio: Pixel X variable (string | int | list)
% Recorrido: string | int | list
% Tipo: Otras funciones
colorPixel(Pixel, Color):-
    esBitmap([Pixel]) -> obtColorPixbit(Pixel, Color);
    esHexmap([Pixel]) -> obtColorPixhex(Pixel, Color);
    esPixmap([Pixel]) -> obtColorPixrgb(Pixel, R,G,B), Color = [R,G,B].


% Descripci�n: Predicado que crea la cadena de pixeles
% Dominio: list X list X int X int X int X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, crea la lista con base a estados
% pendientes
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
                       string_concat("", "\t", String),
                       imageString_formato([CabezaC | ColaC], [Cabeza | Cola], R2, CoordX, CoordY_final, Cola2).


% Descripci�n: Predicado que crea una cadena de string de la imagen
% Dominio: image X variable (string)
% Recorrido: string
% Tipo: Otras funciones
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

% Descripci�n: Predicado que verifica si dos pixel tiene igual
% profundidad
% Dominio: Pixel X Pixel
% Recorrido: Boleano
% Tipo: Otras funciones
igualProfundidad(Pixel, Pixel2):-
    esBitmap([Pixel]) ->
        obtProfundidadPixbit(Pixel, D), obtProfundidadPixbit(Pixel2, D) ->  true; false;
    esPixmap([Pixel]) ->
        obtProfundidadPixrgb(Pixel, D), obtProfundidadPixrgb(Pixel2, D) -> true; false;
    esHexmap([Pixel]) ->
        obtProfundidadPixhex(Pixel, D), obtProfundidadPixhex(Pixel2, D) ->  true; false.

% Descripci�n: Predicado que eliminar una profundidad de pixeles
% Dominio: list X Pixel X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, crea la lista con base a estados
% pendientes
eliminarProfundidad([], _, []).
eliminarProfundidad([Cabeza | Cola], Pixel, [NuevaCabeza | Cola2]):-
    igualProfundidad(Cabeza, Pixel) ->
        eliminarProfundidad(Cola, Pixel, Cola2);
        NuevaCabeza = Cabeza,
        eliminarProfundidad(Cola, Pixel, Cola2).


% Descripci�n: Predicado que obtiene la profundidad de un Pixel
% Dominio: Pixel X variable (int)
% Recorrido: int
% Tipo: Otras funciones
obtenerProfundidad(Pixel, D):-
    esPixmap([Pixel]) ->  obtProfundidadPixrgb(Pixel, D);
    esHexmap([Pixel]) ->  obtProfundidadPixhex(Pixel, D);
    esBitmap([Pixel]) ->  obtProfundidadPixbit(Pixel, D).

% Descripci�n: Predicado que crea una lista con profundidades
% Dominio: list X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, crea la lista con base a estados
% pendientes
listaProfundidad([],[]).
listaProfundidad([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    obtenerProfundidad(Cabeza, NuevaCabeza),
    eliminarProfundidad([Cabeza | Cola], Cabeza, NuevaLista),
    removerElemento([], NuevaLista, NuevaLista2),
    listaProfundidad(NuevaLista2, Cola2).

% Descripci�n: Predicado que modifica el color de un Pixel a blanco
% Dominio: Pixel X variable (Pixel)
% Recorrido: Pixel
% Tipo: Otras funciones
reemplazarPixelProfundidad(Pixel, Pixel2):-
    esBitmap([Pixel]) ->  obtProfundidadPixbit(Pixel, D), obtCoordPixbit(Pixel, X,Y), pixbit(X,Y,1,D, Pixel2);
    esPixmap([Pixel]) ->  obtProfundidadPixrgb(Pixel, D), obtCoordPixrgb(Pixel, X,Y), pixrgb(X,Y,255,255,255,D, Pixel2);
    esHexmap([Pixel]) ->  obtProfundidadPixhex(Pixel, D), obtCoordPixhex(Pixel, X,Y), pixhex(X,Y,"#FFFFFF",D, Pixel2).

% Descripci�n: Funci�n que crea una lista de pixeles con misma
% profundidad y los que no se rellenan con blanco
% Dominio: int X list X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, crea la lista con base a estados
% pendientes
pixelesIgualProfundidad(_,[],[]).
pixelesIgualProfundidad(Profundidad, [Cabeza | Cola], [NuevaCabeza | Cola2]):-
    obtenerProfundidad(Cabeza, D),
    (D = Profundidad) ->
        NuevaCabeza = Cabeza,
        pixelesIgualProfundidad(Profundidad, Cola, Cola2);
        reemplazarPixelProfundidad(Cabeza, NuevaCabeza),
        pixelesIgualProfundidad(Profundidad, Cola, Cola2).

% Descripci�n: Predicado que crea la lista de imagenes diferenciadas por
% profundidad
% Dominio: image X list X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, crea la lista con base a estados
% pendientes
profundidad_formato(_,[],_).
profundidad_formato(Imagen, [Profundidad | ColaD], [NuevaCabeza | Cola2]):-
    image(CoordX, CoordY, Pixeles, Imagen),
    pixelesIgualProfundidad(Profundidad, Pixeles, PixelesD),
    image(CoordX, CoordY, PixelesD, NuevaCabeza),
    profundidad_formato(Imagen, ColaD, Cola2).

% Descripci�n: Predicado que permite separar una im�gen en capas en base a la profundidad en que se sit�an los pixeles
% Dominio: image X variable (list)
% Recorrido: list
% Tipo: Otras funciones
imageDepthLayers(Imagen, Lista):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       image(_,_,Pixeles, ImagenD),
       listaProfundidad(Pixeles, Profundidades),
       profundidad_formato(ImagenD, Profundidades, ListaP),
       removerElemento([], ListaP, Lista);
    esImage(Imagen),
       image(_,_,Pixeles, Imagen),
       listaProfundidad(Pixeles, Profundidades),
       profundidad_formato(Imagen,Profundidades, ListaP),
       removerElemento([], ListaP, Lista).

% Descripci�n: Predicado que descomprime un pixel comprimido
% Dominio: Pixel_comprimido X variable (Pixel)
% Recorrido: Pixel
% Tipo: Otras funciones
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

% Descripci�n: Predicado que descomprime pixeles de formato de pixeles
% comprimidos
% Dominio: list X variable (list)
% Recorrido: list
% Tipo: Otras funciones
% Tipo de recursi�n: Natural, crea la lista con base a estados
% pendientes
pixelesDescomprimidos([], []).
pixelesDescomprimidos([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    esPixel(Cabeza) ->
        NuevaCabeza = Cabeza,
        pixelesDescomprimidos(Cola, Cola2)
        ;
        pixelDescomprimido(Cabeza, NuevaCabeza), pixelesDescomprimidos(Cola,Cola2).

% Descripci�n: Predicado que descomprime una imagen comprimida
% Dominio: image X variable (image)
% Recorrido: image
% Tipo: Modificador
imageDecompress(Imagen, Imagen2):-
    imageIsCompress(Imagen),
    image(CoordX, CoordY, Pixeles, Imagen),
    pixelesDescomprimidos(Pixeles, Pixeles2),
    image(CoordX, CoordY, Pixeles2, Imagen2).
