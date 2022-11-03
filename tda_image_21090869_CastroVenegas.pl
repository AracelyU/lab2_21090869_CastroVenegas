:- module(tda_image_21090869_castrovenegas,[image/4,
                                            imageIsBitmap/1,
                                            imageIsHexmap/1,
                                            imageIsPixmap/1,
                                            imageIsCompress/1,
                                            imageFlipH/2,
                                            imageFlipV/2,
                                            imageCrop/6,
                                            imageRGBToHex/2,
                                            imageInvertColorRGB/2,
                                            imageToHistogram/2,
                                            imageRotate90/2,
                                            imageCompress/2,
                                            imageChangePixel/3,
                                            imageToString/2,
                                            imageDepthLayers/2,
                                            imageDecompress/2]).


% Módulos utilizados
:- use_module(tda_pixbit_21090869_CastroVenegas).
:- use_module(tda_pixhex_21090869_CastroVenegas).
:- use_module(tda_pixrgb_21090869_CastroVenegas).
:- use_module(tda_pixbit_comprimido_21090869_CastroVenegas).
:- use_module(tda_pixhex_comprimido_21090869_CastroVenegas).
:- use_module(tda_pixrgb_comprimido_21090869_CastroVenegas).

% Dominio
% Imagen = image
% ImagenD = image descomprimida
% Ancho, largo, CoordX, CoordY, Profundidad = int >= 0
% Pixeles = list [pixbit | pixhex | pixrgb]
% Pixel = [pixbit | pixhex | pixrgb]
% Contador, Num = int
% ColorR, ColorG, ColorB = 0 <= integer <= 255
% Bit = integer = 0 | integer = 1
% Hex = string
% ColorHex = string
% Color = Bit | Hex | [ColorR, ColorG, ColorB]
% ColorN = Color no igual
%
%
% Predicados
% image{Ancho, Largo, Pixeles, [Ancho, Largo, Pixeles]} (aridad = 4)
% imageIsBitmap{Imagen} (aridad = 1)
% imageIsHexmap{Imagen} (aridad = 1)
% imageIsPixmap{Imagen} (aridad = 1)
% imageIsCompress{Imagen} (aridad = 1)
% esImage{Imagen} (aridad = 1)
% obtPixelesImage{Imagen , Pixeles} (aridad = 2)
% obtCoordImage{Imagen , Ancho, Largo} (aridad = 3)
% cambiarCoordXY{Pixel, CoordX, CoordY, Pixel2} (aridad = 4)
% flipH_formato{[Cabeza | Cola], CoordY_final, CoordX, Contador,
% [NuevaCabeza | Cola2]} (aridad = 5)
% imageFlipH{Imagen, Imagen2} (aridad = 2)
% flipV_formato{[Cabeza | Cola], CoordX_final, CoordX, CoordY,
% CoordY_final, [NuevaCabeza | Cola2]} (aridad = 6)
% imageFlipV{Imagen, Imagen2} (aridad = 2)
% rangoXY{Pixel, X1,X2,Y1,Y2} (aridad = 5)
% crop_filtro{[Cabeza | Cola], X1,X2,Y1,Y2, [NuevaCabeza | Cola2]}
% (aridad = 6)
% crop_formato{[Cabeza | Cola], CoordX, CoordY, CoordY_final,
% [NuevaCabeza | Cola2]} (aridad = 5)
% mayor{Num1,Num2,Mayor} (aridad = 3)
% menor{Num1,Num2,Menor} (aridad = 3)
% imageCrop{Imagen, X1,Y1,X2,Y2,Imagen2} (aridad = 6)
% numeroString{Num, String} (aridad = 2)
% rgbHex{Num, String} (aridad = 2)
% listRGB_HEX{[ColorR, ColorG, ColorB], ColorHex} (aridad = 2)
% stringRGB{Pixel, PixelH} (aridad = 2)
% formatoRGB_HEX{[Cabeza | Cola], [NuevaCabeza | Cola2]} (aridad = 2)
% imageRGBToHex{Imagen, Imagen2} (aridad = 2)
% imageInvertColorRGB{Pixel, Pixel2} (aridad = 2)
% igualColor{Pixel, Pixel2} (aridad = 2)
% eliminarElemento(Distinto, [[Elemento , Cabeza] | Cola], [Cabeza |
% Cola2]) (aridad = 3)
% listaEliminarColor{[Cabeza | Cola], [NuevaCabeza | Cola2]} (aridad =
% 2)
% contarColor{[Cabeza | Cola], Pixel, Contador} (aridad = 3)
% colorPixel{Pixel, Color} (aridad = 2)
% histograma_formato{[Cabeza | Cola], [[Cantidad , Elemento] | Cola2]}
% (aridad = 2)
% imageToHistogram{Imagen, Imagen2} (aridad = 2)
% rotate90_formato{[Cabeza | Cola], Contador, CoordX_final,
% CoordY_final, [NuevaCabeza | Cola2]} (aridad = 5)
% imageRotate90{Imagen, Imagen2} (aridad = 2)
% stringNumero{String, Num} (aridad = 2)
% hexRGB{String, Color} (aridad = 2)
% listHEX_RGB{String, [ColorR, ColorG, ColorB]} (aridad = 2)
% comprimirPixel{Pixel, Pixel2} (aridad = 2)
% comprimirPixeles{[Cabeza | Cola], Color, [NuevaCabeza | Cola2]}
% (aridad = 3)
% pixelMasRepetido{[Cabeza | Cola], Cant, Elem, Elemento} (aridad = 4)
% imageCompress{Imagen, Imagen2} (aridad = 2)
% igualCoordXY{Pixel, Pixel2} (aridad = 2)
% changePixel{[Cabeza | Cola], Pixel, [NuevaCabeza | Cola2]} (aridad =
% 3)
% imageChangePixel{Imagen, Pixel, Imagen2} (aridad = 3)
% imageString_formato{[Cabeza| Cola], CoordY, CoordX, CoordY_final,
% [String | Cola2]} (aridad = 5)
% imageToString{Imagen, Cadena} (aridad = 2)
% obtProfundidad{Pixel, Profundidad} (aridad = 2)
% listaEliminarProfundidad{[Cabeza | Cola], [NuevaCabeza | Cola2]}
% (aridad = 2)
% listaProfundidad{[Cabeza | Cola], [NuevaCabeza | Cola2]} (aridad = 2)
% reemplazarPixelProfundidadBlanco{Pixel, Pixel2, Profundidad} (aridad
% = 3)
% pixelesIgualProfundidad{Profundidad, [Cabeza | Cola], [NuevaCabeza |
% Cola2]} (aridad = 3)
% profundidad_formato{Imagen, [Profundidad | ColaD], [NuevaCabeza |
% Cola2]} (aridad = 3)
% imageDepthLayers{Imagen, Lista} (aridad = 2)
% pixelDescomprimido{Pixel, Pixel2} (aridad = 2)
% esPixel{Pixel} (aridad = 1)
% pixelesDescomprimidos{[Cabeza | Cola], [NuevaCabeza | Cola2]} (aridad
% = 2)
% imageDecompress{Imagen, Imagen2} (aridad = 2)
%
% Metas primarias
% image
% imageIsBitmap
% imageIsHexmap
% imageIsPixmap
% imageIsCompress
% imageFlipH
% imageFlipV
% imageCrop
% imageRGBToHex
% imageInvertColorRGB
% histograma_formato
% imageRotate90
% imageCompress
% imageToString
% imageDepthLayers
% imageDecompress
%
% Metas secundarias
% esImage
% obtPixelesImage
% obtCoordImage
% cambiarCoordXY
% flipH_formato
% flipV_formato
% rangoXY
% crop_filtro
% crop_formato
% mayor
% menor
% numeroString
% rgbHex
% listRGB_HEX
% stringRGB
% formatoRGB_HEX
% igualColor
% eliminarElemento
% listaEliminarColor
% contarColor
% colorPixel
% rotate90_formato
% stringNumero
% hexRGB
% listHEX_RGB
% comprimirPixel
% comprimirPixeles
% pixelMasRepetido
% igualCoordXY
% changePixel
% imageChangePixel
% imageString_formato
% obtProfundidad
% listaEliminarProfundidad
% listaProfundidad
% reemplazarPixelProfundidadBlanco
% pixelesIgualProfundidad
% profundidad_formato
% pixelDescomprimido
% pixelesDescomprimidos
%
%
% ----------------------REPRESENTACIÓN---------------------------------
% Una imagen se representa como una lista con (int X int X list) el
% cual contiene Ancho, Largo y Pixeles
% ---------------------------------------------------------------------
%
% Regla
% -------------CONSTRUCTOR y MODIFICADOR -------------------------
%
% Descripción: Predicado que define como es una imagen
% Dominio: int X int X list X image
% Tipo: Constructor y modificador
image(Ancho, Largo, Pixeles, [Ancho, Largo, Pixeles]):-
    integer(Ancho),Ancho >= 0, integer(Largo), Largo >= 0, is_list(Pixeles).

%-----------------PERTENENCIA------------------------------------
% Descripción: Predicado que verifica si una imagen es Bitmap
% Dominio: image
% Recorrido: Boleano, false si no corresponde a una imagen bitmap
% Tipo: Pertenencia
imageIsBitmap(Imagen):- obtPixelesImage(Imagen, Pixeles), esBitmap(Pixeles) -> true; false.

% Descripión: Predicado que verifica si una imagen es Pixmap
% Dominio: image
% Recorrido: Boleano, false si no corresponde a una imagen Pixmap
% Tipo: Pertenencia
imageIsPixmap(Imagen):- obtPixelesImage(Imagen, Pixeles), esPixmap(Pixeles) -> true; false.

% Descripción: Predicado que verifica si una imagen es Hexmap
% Dominio: image
% Recorrido: Boleano, false si no corresponde a una imagen Hexmap
% Tipo: Pertenencia
imageIsHexmap(Imagen):- obtPixelesImage(Imagen, Pixeles), esHexmap(Pixeles) -> true; false.

% Descripción: Predicado que verifica si una imagen fue comprimida
% Dominio: image
% Recorrido: Boleano, false si no corresponde a una imagen comprimida
% Tipo: Pertenencia
imageIsCompress(Imagen):-
    obtPixelesImage(Imagen, Pixeles), esHexmapComprimido(Pixeles) -> true;
    obtPixelesImage(Imagen, Pixeles), esPixmapComprimido(Pixeles) -> true;
    obtPixelesImage(Imagen, Pixeles), esBitmapComprimido(Pixeles) -> true; false.

% Descripción: Predicado que verifica si la entrada es una imagen
% Dominio: image
% Recorrido: Boleano, false si no corresponde a una imagen Pixmap, Hexmap
% o Bitmap
% Tipo: Pertenencia
esImage(Imagen):-
    imageIsBitmap(Imagen) -> true; imageIsHexmap(Imagen) -> true; imageIsPixmap(Imagen) -> true; false.


% -------------------------------------SELECTORES----------------------
% Descripción: Predicado que obtiene los pixeles de una imagen Dominio:
% image X [pixbit | pixhex | pixrgb] (list)
% Tipo: Selector
obtPixelesImage(Imagen , Pixeles):- image(_,_, Pixeles, Imagen).

% Descripción: Predicado que obtiene las dimensiones de una imagen
% Dominio: image X int X int
% Tipo: Selector
obtCoordImage(Imagen , Ancho, Largo):- image(Ancho,Largo, _, Imagen).


% ---------OTROS MODIFICADORES Y PREDICADOS-------------------------
%
% Descripción: Predicado que modifica CoordX y CoordY de un Pixel para
% flipH, flipV, entre otros Dominio: Pixel X int X int X Pixel Tipo:
% Otras funciones
cambiarCoordXY(Pixel, CoordX, CoordY, Pixel2):-
    integer(CoordY), CoordY >= 0, integer(CoordX), CoordX >= 0,
    esBitmap([Pixel]) ->
        obtProfundidadPixbit(Pixel, Profundidad), obtColorPixbit(Pixel, Bit),
        pixbit(CoordX,CoordY,Bit,Profundidad, Pixel2);
    esHexmap([Pixel]) ->
        obtProfundidadPixhex(Pixel, Profundidad), obtColorPixhex(Pixel, Hex),
        pixhex(CoordX,CoordY,Hex,Profundidad, Pixel2);
    esPixmap([Pixel]) ->
        obtProfundidadPixrgb(Pixel, Profundidad), obtColorPixrgb(Pixel, ColorR,ColorG,ColorB),
        pixrgb(CoordX,CoordY,ColorR,ColorG,ColorB,Profundidad, Pixel2).

% Descripción: Predicado que voltea los pixeles horizontalmente, apoya a
% imageflipH
% Dominio: Pixeles X int X int X int X Pixeles
% Tipo de recursión: Natural, pues crea una lista con base a estados
% pendientes
% Tipo: Otras funciones
flipH_formato([], _, _, _, []).
flipH_formato([Cabeza | Cola], CoordY_final, CoordX, Contador, [NuevaCabeza | Cola2]):-
    (Contador =< CoordY_final) ->
           R is CoordY_final-Contador,
           R1 is Contador+1,
           cambiarCoordXY(Cabeza, CoordX, R, NuevaCabeza),
           flipH_formato(Cola, CoordY_final, CoordX, R1, Cola2)
           ;
           R2 is CoordX+1,
           flipH_formato([Cabeza | Cola], CoordY_final, R2, 0, [NuevaCabeza | Cola2]).

% Descripción: Predicado que voltea la imagen horizontalmente
% Dominio: image X image
% Tipo: Modificador
imageFlipH(Imagen, Imagen2):-
    imageIsCompress(Imagen) ->
        imageDecompress(Imagen, ImagenD),
        obtCoordImage(ImagenD, Ancho, Largo), obtPixelesImage(ImagenD, Pixeles),
        CoordY_final is Largo-1,
        flipH_formato(Pixeles, CoordY_final, 0, 0, PixelesH),
        sort(PixelesH, Pixeles2),
        image(Ancho, Largo, Pixeles2, Imagen2)
        ;
        esImage(Imagen),
        obtCoordImage(Imagen, Ancho, Largo), obtPixelesImage(Imagen, Pixeles),
        CoordY_final is Largo-1,
        flipH_formato(Pixeles, CoordY_final, 0, 0, PixelesH),
        sort(PixelesH, Pixeles2),
        image(Ancho, Largo, Pixeles2, Imagen2).


% Descripción: Predicado que voltea los pixeles verticalmente ,apoya a
% imageFlipV
% Dominio: Pixeles X int X int X int X int X Pixeles
% Tipo de recursión: Natural, pues crea una lista con base a estados
% pendientes
% Tipo: Otras funciones
flipV_formato([], _, _, _,_,[]).
flipV_formato([Cabeza | Cola], CoordX_final, CoordX, CoordY, CoordY_final, [NuevaCabeza | Cola2]):-
     (CoordY =< CoordY_final) ->
        R is CoordX_final-CoordX,
        R1 is CoordY+1,
        cambiarCoordXY(Cabeza, R, CoordY, NuevaCabeza),
        flipV_formato(Cola, CoordX_final, CoordX, R1, CoordY_final, Cola2)
        ;
        R2 is CoordX+1,
        flipV_formato([Cabeza | Cola], CoordX_final, R2, 0, CoordY_final, [NuevaCabeza | Cola2]).


% Descripción: Predicado que voltea la imagen verticalmente
% Dominio: image X image
% Tipo: Modificador
imageFlipV(Imagen, Imagen2):-
    imageIsCompress(Imagen) ->
        imageDecompress(Imagen, ImagenD),
        obtCoordImage(ImagenD, Ancho, Largo), obtPixelesImage(ImagenD, Pixeles),
        CoordX_final is Ancho-1, CoordY_final is Largo-1,
        flipV_formato(Pixeles, CoordX_final, 0, 0, CoordY_final, PixelesV),
        sort(PixelesV, Pixeles2),
        image(Ancho, Largo, Pixeles2, Imagen2)
        ;
        esImage(Imagen),
        obtCoordImage(Imagen, Ancho, Largo), obtPixelesImage(Imagen, Pixeles),
        CoordX_final is Ancho-1,
        CoordY_final is Largo-1,
        flipV_formato(Pixeles, CoordX_final, 0, 0, CoordY_final, PixelesV),
        sort(PixelesV, Pixeles2),
        image(Ancho, Largo, Pixeles2, Imagen2).

% Descripción: Predicado que verifica si el Pixel esta dentro del rango
% dado por crop
% Dominio: Pixel X int X int X int X int
% Recorrido: Boleano, false si no esta dentro del rango establecido
% Tipo: Otras funciones
rangoXY(Pixel, X1,X2,Y1,Y2):-
    esBitmap([Pixel]) ->
         obtCoordPixbit(Pixel, CoordX, CoordY),
        ((CoordX >= X1), (CoordX =< X2), (CoordY >= Y1), (CoordY =< Y2)) -> true; false;
    esHexmap([Pixel]) ->
        obtCoordPixhex(Pixel, CoordX, CoordY),
        ((CoordX >= X1), (CoordX =< X2), (CoordY >= Y1), (CoordY =< Y2)) -> true; false;
    esPixmap([Pixel]) ->
        obtCoordPixrgb(Pixel, CoordX, CoordY),
        ((CoordX >= X1), (CoordX =< X2), (CoordY >= Y1), (CoordY =< Y2)) -> true; false.

% Descripción: Predicado que filtra los pixeles que no esten en
% el rango establecido por imageCrop
% Dominio: Pixeles X int X int x int X int X Pixeles
% Tipo de recursión: Natural, pues crea una lista con base a estados
% pendientes
% Tipo: Otras funciones
crop_filtro([], _,_,_,_,[]).
crop_filtro([Cabeza | Cola], X1,X2,Y1,Y2, [NuevaCabeza | Cola2]):-
      rangoXY(Cabeza, X1,X2,Y1,Y2) ->
          NuevaCabeza = [1, Cabeza],
          crop_filtro(Cola, X1,X2,Y1,Y2, Cola2);
          NuevaCabeza = [0, Cabeza],
          crop_filtro(Cola, X1,X2,Y1,Y2, Cola2).

% Descripción: Predicado que modifica las coordenadas de los pixeles
% tras filtrar para la nueva imagen recortada
% Dominio: Pixeles, int X int X int X Pixeles
% Tipo de recursión: Natural, pues crea una lista con base a estados
% pendientes
% Tipo: Otras funciones
crop_formato([], _,_,_,[]).
crop_formato([Cabeza | Cola], CoordX, CoordY, CoordY_final, [NuevaCabeza | Cola2]):-
    (CoordY =< CoordY_final) ->
        R is CoordY+1,
        cambiarCoordXY(Cabeza, CoordX, CoordY, NuevaCabeza),
        crop_formato(Cola, CoordX, R, CoordY_final, Cola2);

        R1 is CoordX+1,
        crop_formato([Cabeza | Cola], R1, 0, CoordY_final, [NuevaCabeza | Cola2]).

% Descripción: Predicado que devuelve el mayor entre dos números, apoya
% a imageCrop
% Dominio: int X int X int
% Tipo: Otras funciones
mayor(Num1,Num2,Mayor):- Num1 >= Num2 -> Mayor is Num1 ; Mayor is Num2.

% Descripción: Predicado que devuelve el menor entre dos números, apoya
% a imageCrop
% Dominio: int X int X int
% Tipo: Otras funciones
menor(Num1,Num2,Menor):- Num1 =< Num2 -> Menor is Num1 ; Menor is Num2.

% Descripción: Predicado que crea una imagen nueva con los pixeles
% que esten en el cuadrante definido
% Dominio: image X int X int X int X int X image
% Tipo: Modificador
imageCrop(Imagen, X1,Y1,X2,Y2,Imagen2):-
   imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD), obtPixelesImage(ImagenD, Pixeles),
       menor(X1,X2,X_menor), mayor(X1,X2,X_mayor), menor(Y1,Y2,Y_menor), mayor(Y1,Y2,Y_mayor),
       crop_filtro(Pixeles, X_menor, X_mayor, Y_menor, Y_mayor, PixelesC),
       eliminarElemento(0, PixelesC, PixelesC2),
       CoordY_final is Y_mayor - Y_menor,
       crop_formato(PixelesC2, 0,0, CoordY_final, Pixeles2),
       length(Pixeles2, N),
       ((N \== 0) ->
           CoordX is X_mayor - X_menor,
           CoordXNuevo is CoordX + 1,
           CoordYNuevo is CoordY_final + 1,
           image(CoordXNuevo, CoordYNuevo, Pixeles2, Imagen2);
           image(0,0,Pixeles2, Imagen2) )
        ;
       esImage(Imagen),
       image(_, _, Pixeles, Imagen),
       menor(X1,X2,X_menor), mayor(X1,X2,X_mayor), menor(Y1,Y2,Y_menor), mayor(Y1,Y2,Y_mayor),
       crop_filtro(Pixeles, X_menor, X_mayor, Y_menor, Y_mayor, PixelesC),
       eliminarElemento(0, PixelesC, PixelesC2),
       CoordY_final is Y_mayor - Y_menor,
       crop_formato(PixelesC2, 0,0, CoordY_final, Pixeles2),
       length(Pixeles2, N),
       ((N \== 0) ->
           CoordX is X_mayor - X_menor,
           CoordXNuevo is CoordX + 1,
           CoordYNuevo is CoordY_final + 1,
           image(CoordXNuevo, CoordYNuevo, Pixeles2, Imagen2);
           image(0,0, Pixeles2, Imagen2)).


% Descripción: Predicado que entrega el equivalente a string de un
% número entero, apoya a imageRGBToHex
% Dominio: int X string
% Tipo: Otras funciones
numeroString(Num, String):-
    (Num = 0), String = "0"; (Num = 1), String = "1"; (Num = 2), String = "2"; (Num = 3), String = "3";
    (Num = 4), String = "4"; (Num = 5), String = "5"; (Num = 6), String = "6"; (Num = 7), String = "7";
    (Num = 8), String = "8"; (Num = 9), String = "9"; (Num = 10), String = "A"; (Num = 11), String = "B";
    (Num = 12), String = "C"; (Num = 13), String = "D"; (Num = 14), String = "E"; (Num = 15), String = "F".

% Descripción: Predicado que transforma un número con unidad y decena a
% su equivalente string
% Dominio: int X string
% Tipo: Otras funciones
rgbHex(Num, String):-
    Entero is Num // 16, Resto is Num mod 16,
    numeroString(Entero, N1), numeroString(Resto, N2),
    string_concat(N1,N2, String).

% Descripción: Predicado que convierte una lista de 3 valores pixmap a string hexmap
% Dominio: list X string
% Tipo: Otras funciones
listRGB_HEX([ColorR, ColorG, ColorB], ColorHex):-
    rgbHex(ColorR, R_hex), rgbHex(ColorG, G_hex), rgbHex(ColorB, B_hex),
    string_concat("#", R_hex, ColorParcial), string_concat(G_hex, B_hex, ColorParcial2),
    string_concat(ColorParcial, ColorParcial2, ColorHex).

% Descripción: Predicado que convierte un pixmap a hexmap
% Dominio: pixrgb x pixhex
% Tipo: Otras funciones
stringRGB(Pixel, PixelH):-
    obtProfundidadPixrgb(Pixel, Profundidad), obtColorPixrgb(Pixel, ColorR,ColorG,ColorB),
    obtCoordPixrgb(Pixel, CoordX,CoordY),
    listRGB_HEX([ColorR,ColorG,ColorB], ColorHex),
    pixhex(CoordX,CoordY,ColorHex, Profundidad, PixelH).

% Descripción: Predicado que cambia el formato de pixeles pixmap a
% hexmap
% Dominio: Pixeles X Pixeles
% Tipo de recursión: Natural, pues crea una nueva lista con base a
% estados pendientes
% Tipo: Otras funciones
formatoRGB_HEX([], []).
formatoRGB_HEX([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    stringRGB(Cabeza, NuevaCabeza),
    formatoRGB_HEX(Cola, Cola2).

% Descripción: Predicado que convierte una imagen Pixmap a Hexmap
% Dominio: image X image
% Tipo: Modificador
imageRGBToHex(Imagen, Imagen2):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       obtCoordImage(ImagenD, Ancho, Largo), obtPixelesImage(Imagen, Pixeles),
       esPixmap(Pixeles),
       formatoRGB_HEX(Pixeles, Pixeles2),
       image(Ancho, Largo, Pixeles2, Imagen2)
       ;
       obtCoordImage(Imagen, Ancho, Largo), obtPixelesImage(Imagen, Pixeles),
       esPixmap(Pixeles),
       formatoRGB_HEX(Pixeles, Pixeles2),
       image(Ancho, Largo, Pixeles2, Imagen2).

% Descripción: Predicado que cambia un pixrgb por su color opuesto
% Dominio: pixrgb X pixrgb
% Tipo: Otras funciones
imageInvertColorRGB(Pixel, Pixel2):-
    obtProfundidadPixrgb(Pixel, Profundidad), obtColorPixrgb(Pixel, ColorR,ColorG,ColorB), obtCoordPixrgb(Pixel, CoordX,CoordY),
    R_invert is 255 - ColorR, G_invert is 255 - ColorG, B_invert is 255 - ColorB,
    pixrgb(CoordX,CoordY,R_invert, G_invert, B_invert, Profundidad, Pixel2).

% Descripción: Predicado que verifica si dos pixbit tienen mismo
% bit, dos Pixhex tiene el mismo hex y dos pixrgb tienen el mismo color
% RGB
% Dominio: Pixel X Pixel
% Recorrido: Boleano, false si no tienen el mismo color
% Tipo: Otras funciones
igualColor(Pixel, Pixel2):-
    esBitmap([Pixel, Pixel2]) ->
        obtColorPixbit(Pixel, Bit), obtColorPixbit(Pixel2,Bit) -> true; false;
    esHexmap([Pixel, Pixel2]) ->
        obtColorPixhex(Pixel, Hex), obtColorPixhex(Pixel2,Hex) -> true; false;
    esPixmap([Pixel, Pixel2]) ->
        obtColorPixrgb(Pixel, ColorR,ColorG,ColorB), obtColorPixrgb(Pixel2, ColorR,ColorG,ColorB) -> true; false; false.

% Descripción: Predicado que filtra pixeles según elemento
% Dominio: (int | string | list) X [ (int | string | list) , Pixel] X
% Pixeles
% Tipo de recursión: Natural, pues crea una lista con base a estados
% pendientes
% Tipo: Otras funciones
eliminarElemento(_,[],[]).
eliminarElemento(Elemento, [[Elemento , _] | Cola], Cola2):-
    eliminarElemento(Elemento, Cola, Cola2), !.
eliminarElemento(Distinto, [[_ , Cabeza] | Cola], [Cabeza | Cola2]):-
    eliminarElemento(Distinto, Cola, Cola2).

% Descripción: Predicado que modifica los pixeles para eliminarElemento
% por color
% Dominio: Pixeles X list
% Tipo de recursión: Natural, pues crea una lista con base a estados
% pendientes
% Tipo: Otras funciones
listaEliminarColor([], []).
listaEliminarColor([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    colorPixel(Cabeza, Color),
    NuevaCabeza = [Color, Cabeza],
    listaEliminarColor(Cola, Cola2).

% Descripción: Predicado que suma los elementos iguales de pixeles
% Dominio: list X Pixel X int
% Tipo de recursión: Natural, pues cuenta con base a estados pendientes
% Tipo: Selector
contarColor([], _, 0).
contarColor([Cabeza | Cola], Pixel, Contador):-
    igualColor(Cabeza, Pixel) ->
        !, contarColor(Cola, Pixel, Contador1), Contador is Contador1+1;
        contarColor(Cola, Pixel, Contador).

% Descripción: Predicado que obtiene el color de un pixel
% Dominio: Pixel X (string | int | list)
% Tipo: Otras funciones
colorPixel(Pixel, Color):-
    esBitmap([Pixel]) -> obtColorPixbit(Pixel, Color);
    esHexmap([Pixel]) -> obtColorPixhex(Pixel, Color);
    esPixmap([Pixel]) -> obtColorPixrgb(Pixel, ColorR,ColorG,ColorB), Color = [ColorR,ColorG,ColorB].

% Descripción: Predicado que recopila la información de color de los pixeles
% Dominio: Pixeles X list
% Tipo de recursión: Natural, pues crea la lista con base a estados
% pendientes
% Tipo: Otras funciones
histograma_formato([], []).
histograma_formato([Cabeza | Cola], [[Cantidad , Elemento] | Cola2]):-
        colorPixel(Cabeza, Elemento),
        contarColor([Cabeza | Cola], Cabeza, Cantidad),
        listaEliminarColor([Cabeza | Cola], ListaAEliminar),
        eliminarElemento(Elemento, ListaAEliminar, NuevaLista),
        histograma_formato(NuevaLista, Cola2).

% Descripción: Predicado que obtiene el histograma de una imagen
% Dominio: image X list
% Tipo: Otras funciones
imageToHistogram(Imagen, Histograma):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       obtPixelesImage(ImagenD, Pixeles),
       histograma_formato(Pixeles, Histograma)
       ;
       esImage(Imagen),
       obtPixelesImage(Imagen, Pixeles),
       histograma_formato(Pixeles, Histograma).

% Descripción: Predicado que modifica los pixeles de una imagen rotando 90° a la derecha
% Dominio: Pixeles X list X int X int X int X int X Pixeles
% Tipo de recursión: Natural, crea la lista con base a estados
% pendientes
% Tipo: Otras funciones
rotate90_formato([],_,_,_,[]).
rotate90_formato([Cabeza | Cola], Contador, CoordX_final, CoordY_final, [NuevaCabeza | Cola2]):-
    (Contador =< CoordY_final) ->
        R is Contador+1,
        cambiarCoordXY(Cabeza, Contador, CoordX_final, NuevaCabeza),
        rotate90_formato(Cola, R, CoordX_final, CoordY_final, Cola2)
        ;
        R1 is CoordX_final-1,
        rotate90_formato([Cabeza | Cola], 0, R1, CoordY_final, [NuevaCabeza | Cola2]).

% Descripción: Predicado que rota una imagen 90° a la derecha
% Dominio: image X image
% Tipo: Modificador
imageRotate90(Imagen, Imagen2):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       obtCoordImage(ImagenD, Ancho, Largo), obtPixelesImage(ImagenD, Pixeles),
       CoordX_final is Ancho-1, CoordY_final is Largo-1,
       rotate90_formato(Pixeles, 0, CoordX_final, CoordY_final, PixelesR),
       sort(PixelesR, Pixeles2),
       image(Largo, Ancho, Pixeles2, Imagen2)
       ;
       esImage(Imagen),
       obtCoordImage(Imagen, Ancho, Largo), obtPixelesImage(Imagen, Pixeles),
       CoordX_final is Ancho-1, CoordY_final is Largo-1,
       rotate90_formato(Pixeles, 0, CoordX_final, CoordY_final, PixelesR),
       sort(PixelesR, Pixeles2),
       image(Largo, Ancho, Pixeles2, Imagen2).


% Descripción: Predicado que entrega el equivalente a entero de un
% string, apoya en imageCompress
% Dominio: string X int
% Tipo: Otras funciones
stringNumero(String, Num):-
    (String = "0"), Num = 0; (String = "1"), Num = 1; (String = "2"), Num = 2; (String = "3"), Num = 3;
    (String = "4"), Num = 4; (String = "5"), Num = 5; (String = "6"), Num = 6; (String = "7"), Num = 7;
    (String = "8"), Num = 8; (String = "9"), Num = 9; (String = "A"), Num = 10; (String = "B"), Num = 11;
    (String = "C"), Num = 12; (String = "D"), Num = 13, (String = "E"), Num = 14; (String = "F"), Num = 15.

% Descripción: Predicado que transforma un string con dos letras a su
% equivalente a número, apoya a imageCompress
% Dominio: string X int
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

% Descripción: Predicado que convierte un string pixhex en una lista de
% tres valores enteros
% Dominio: string X int
% Tipo: Otras funciones
listHEX_RGB(String, [ColorR, ColorG, ColorB]):-
    sub_string(String,1,_,4,S1),
    hexRGB(S1,ColorR),
    sub_string(String,3,_,2,S2),
    hexRGB(S2,ColorG),
    sub_string(String,5,_,0,S3),
    hexRGB(S3,ColorB).

% Descripción: Predicado que comprime un pixel
% Dominio: Pixel X Pixel_comprimido
% Tipo: Otras funciones
comprimirPixel(Pixel, Pixel2):-
    esBitmap([Pixel]) ->
        obtProfundidadPixbit(Pixel, Profundidad), obtCoordPixbit(Pixel, CoordX,CoordY),
        obtColorPixbit(Pixel, Bit), pixbit_comprimido(CoordX,CoordY,[-1, Bit],Profundidad,Pixel2);
    esHexmap([Pixel]) ->
        obtProfundidadPixhex(Pixel, Profundidad), obtCoordPixhex(Pixel, CoordX,CoordY),
        obtColorPixhex(Pixel, Hex), listHEX_RGB(Hex, HexL), pixhex_comprimido(CoordX,CoordY,HexL,Profundidad,Pixel2);
    esPixmap([Pixel]) ->
        obtProfundidadPixrgb(Pixel, Profundidad), obtCoordPixrgb(Pixel, CoordX,CoordY),
        obtColorPixrgb(Pixel, ColorR,ColorG,ColorB),
        rgbHex(ColorR, HexR), rgbHex(ColorG, HexG), rgbHex(ColorB, HexB),
        pixrgb_comprimido(CoordX,CoordY,HexR,HexG,HexB,Profundidad,Pixel2).

% Descripción: Predicado que comprime pixeles
% Dominio: Pixeles X color X PixelesC
% Tipo de recursión: Natural, crea la lista con base a estados
% pendientes
% Tipo: Otras funciones
comprimirPixeles([],_,[]).
comprimirPixeles([Cabeza | Cola], Color, [NuevaCabeza | Cola2]):-
    colorPixel(Cabeza, C),
    (C = Color) ->
       comprimirPixel(Cabeza, NuevaCabeza),
       comprimirPixeles(Cola, Color, Cola2);
       NuevaCabeza = Cabeza,
       comprimirPixeles(Cola, Color, Cola2).


% Descripción: Predicado que obtiene el pixel más repetido
% Dominio: Pixeles x Pixel
% Tipo de recursión: Cola, entrega resultado inmediato
% Tipo: Otras funciones
pixelMasRepetido([], _, E, Elemento):- Elemento = E.
pixelMasRepetido([Cabeza | Cola], Cant, Elem, Elemento):-
    [Cantidad , _] = Cabeza,
    (Cantidad > Cant) ->
        [Cantidad, E] = Cabeza,
        pixelMasRepetido(Cola, Cantidad, E, Elemento);
        pixelMasRepetido(Cola, Cant, Elem, Elemento).

% Descripción: Predicado que comprime una image por el color más
% repetido
% Dominio: image X image
% Tipo: Modificador
imageCompress(Imagen, Imagen2):-
    esImage(Imagen),
    obtCoordImage(Imagen, Ancho, Largo), obtPixelesImage(Imagen, Pixeles),
    imageToHistogram(Imagen, H),
    pixelMasRepetido(H, -1, _, PixelMasRepetido),
    comprimirPixeles(Pixeles, PixelMasRepetido, PixelesC),
    sort(PixelesC, Pixeles2),
    image(Ancho,Largo,Pixeles2, Imagen2).

% Descripción: Predicado que verifica si dos pixeles tienen la misma
% coordenada (x,y)
% Dominio: Pixel X Pixel
% Recorrido: Boleano, false si no tienen la misma coordenada (x,y)
% Tipo: Otras funciones
igualCoordXY(Pixel, Pixel2):-
    esPixmap([Pixel , Pixel2]) ->
        obtCoordPixrgb(Pixel, CoordX,CoordY), obtCoordPixrgb(Pixel2, CoordX,CoordY) -> true; false;
    esBitmap([Pixel , Pixel2]) ->
        obtCoordPixbit(Pixel, CoordX,CoordY), obtCoordPixbit(Pixel2, CoordX,CoordY) -> true; false;
    esHexmap([Pixel , Pixel2]) ->
        obtCoordPixhex(Pixel, CoordX,CoordY), obtCoordPixhex(Pixel2, CoordX,CoordY) -> true; false; false.

% Descripción: Predicado que reemplaza un pixel de pixeles
% Dominio: Pixeles X Pixel X Pixeles
% Tipo de recursión: Natural, crea la lista con base a estados
% pendientes
% Tipo: Otras funciones
changePixel([], _, []).
changePixel([Cabeza | Cola], Pixel, [NuevaCabeza | Cola2]):-
    igualCoordXY(Cabeza, Pixel) ->
            append(Pixel, [], NuevaCabeza),
            changePixel(Cola, Pixel, Cola2)
            ;
            append(Cabeza, [], NuevaCabeza),
            changePixel(Cola, Pixel, Cola2).

% Descripción: Predicado que reemplaza un pixel de la imagen
% Dominio: image X Pixel X image
% Tipo: Modificador
imageChangePixel(Imagen, Pixel, Imagen2):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       obtCoordImage(ImagenD, Ancho, Largo), obtPixelesImage(ImagenD, Pixeles),
       changePixel(Pixeles, Pixel, Pixeles2),
       image(Ancho, Largo, Pixeles2, Imagen2)
       ;
       esImage(Imagen),
       obtCoordImage(Imagen, Ancho, Largo), obtPixelesImage(Imagen, Pixeles),
       changePixel(Pixeles, Pixel, Pixeles2),
       image(Ancho, Largo, Pixeles2, Imagen2).


% Descripción: Predicado que crea la cadena de pixeles
% Dominio: Pixeles X list X int X int X int X list
% Tipo de recursión: Natural, crea la lista con base a estados
% pendientes
% Tipo: Otras funciones
imageString_formato([],_,_, _, ["\n"]).
imageString_formato([Cabeza| Cola], CoordY, CoordX, CoordY_final, [String | Cola2]):-
    (CoordY =< CoordY_final) ->
              (esPixmap([Cabeza]) ->
                  R is CoordY + 1,
                  colorProfundidadPixrgb(Cabeza, StringMedio),
                  string_concat(StringMedio, "\t", String),
                  imageString_formato(Cola, R, CoordX, CoordY_final, Cola2)
                  ;
                  R is CoordY + 1, colorPixel(Cabeza, Color),
                  string_concat(Color, "\t", String),
                  imageString_formato(Cola, R, CoordX, CoordY_final, Cola2))

              ;
              R3 is CoordX + 1,
              string_concat("\n", "", String),
              imageString_formato([Cabeza | Cola], 0, R3, CoordY_final, Cola2).


% Descripción: Predicado que crea una cadena de string de la imagen
% Dominio: image X string
% Tipo: Otras funciones
imageToString(Imagen, Cadena):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       obtCoordImage(ImagenD, _, Largo), obtPixelesImage(ImagenD, Pixeles),
       CoordY_final is Largo-1,
       imageString_formato(Pixeles, 0, 0, CoordY_final, String),
       atomics_to_string(String, Cadena)
       ;
       esImage(Imagen),
       obtCoordImage(Imagen, _, Largo), obtPixelesImage(Imagen, Pixeles),
       CoordY_final is Largo-1,
       imageString_formato(Pixeles, 0, 0, CoordY_final, String),
       atomics_to_string(String, Cadena).

% Descripción: Predicado que obtiene la profundidad de un Pixel
% Dominio: Pixel X int
% Tipo: Otras funciones
obtenerProfundidad(Pixel, Profundidad):-
    esPixmap([Pixel]) ->  obtProfundidadPixrgb(Pixel, Profundidad);
    esHexmap([Pixel]) ->  obtProfundidadPixhex(Pixel, Profundidad);
    esBitmap([Pixel]) ->  obtProfundidadPixbit(Pixel, Profundidad).

% Descripción: Predicado que modifica los pixeles para eliminarElemento
% por profundidad
% Dominio: Pixeles X list
% Tipo de recursión: Natural, pues crea una lista con base a estados
% pendientes
% Tipo: Otras funciones
listaEliminarProfundidad([],[]).
listaEliminarProfundidad([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    obtenerProfundidad(Cabeza, D),
    NuevaCabeza = [D, Cabeza],
    listaEliminarProfundidad(Cola, Cola2).

% Descripción: Predicado que crea una lista de profundidades
% Dominio: Pixeles X list
% Tipo de recursión: Natural, crea la lista con base a estados
% pendientes
% Tipo: Otras funciones
listaProfundidad([],[]).
listaProfundidad([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    obtenerProfundidad(Cabeza, NuevaCabeza),
    listaEliminarProfundidad([Cabeza | Cola], ListaAEliminar),
    eliminarElemento(NuevaCabeza, ListaAEliminar, NuevaLista),
    listaProfundidad(NuevaLista, Cola2).

% Descripción: Predicado que modifica el color de un Pixel a blanco
% Dominio: Pixel X Pixel
% Tipo: Otras funciones
reemplazarPixelBlanco(Pixel, Pixel2, Profundidad):-
    esBitmap([Pixel]) ->
        obtCoordPixbit(Pixel, CoordX,CoordY), pixbit(CoordX,CoordY,1,Profundidad, Pixel2);
    esPixmap([Pixel]) ->
        obtCoordPixrgb(Pixel, CoordX,CoordY), pixrgb(CoordX,CoordY,255,255,255,Profundidad, Pixel2);
    esHexmap([Pixel]) ->  obtCoordPixhex(Pixel, CoordX,CoordY), pixhex(CoordX,CoordY,"#FFFFFF",Profundidad, Pixel2).

% Descripción: Función que crea una lista de pixeles con misma
% profundidad y los que no se rellenan con blanco
% Dominio: int X Pixeles X Pixeles
% Tipo de recursión: Natural, crea la lista con base a estados
% pendientes
% Tipo: Otras funciones
pixelesIgualProfundidad(_,[],[]).
pixelesIgualProfundidad(Profundidad, [Cabeza | Cola], [NuevaCabeza | Cola2]):-
    obtenerProfundidad(Cabeza, D),
    (D = Profundidad) ->
        NuevaCabeza = Cabeza,
        pixelesIgualProfundidad(Profundidad, Cola, Cola2);
        reemplazarPixelBlanco(Cabeza, NuevaCabeza, Profundidad),
        pixelesIgualProfundidad(Profundidad, Cola, Cola2).

% Descripción: Predicado que crea la lista de imagenes diferenciadas por
% profundidad
% Dominio: image X list X Pixeles
% Tipo de recursión: Natural, crea la lista con base a estados
% pendientes
% Tipo: Otras funciones
profundidad_formato(_,[],[]).
profundidad_formato(Imagen, [Profundidad | ColaD], [NuevaCabeza | Cola2]):-
    obtCoordImage(Imagen, Ancho, Largo), obtPixelesImage(Imagen, Pixeles),
    pixelesIgualProfundidad(Profundidad, Pixeles, PixelesD),
    image(Ancho, Largo, PixelesD, NuevaCabeza),
    profundidad_formato(Imagen, ColaD, Cola2).

% Descripción: Predicado que permite separar una imágen en capas en base a la profundidad en que se sitúan los pixeles
% Dominio: image X list (image)
% Tipo: Otras funciones
imageDepthLayers(Imagen, Lista):-
    imageIsCompress(Imagen) ->
       imageDecompress(Imagen, ImagenD),
       obtPixelesImage(ImagenD, Pixeles),
       listaProfundidad(Pixeles, Profundidades),
       profundidad_formato(ImagenD, Profundidades, Lista);
    esImage(Imagen),
       obtPixelesImage(Imagen, Pixeles),
       listaProfundidad(Pixeles, Profundidades),
       profundidad_formato(Imagen,Profundidades, Lista).

% Descripción: Predicado que descomprime un pixel comprimido
% Dominio: Pixel_comprimido X Pixel
% Tipo: Otras funciones
pixelDescomprimido(Pixel, Pixel2):-
    esPixmapComprimido([Pixel]) ->
        obtCoordPixrgbC(Pixel, CoordX, CoordY), obtColorPixrgbC(Pixel, R_hex, G_hex, B_hex),
        obtProfundidadPixrgbC(Pixel, Profundidad),
        hexRGB(R_hex,ColorR), hexRGB(G_hex, ColorG), hexRGB(B_hex, ColorB),
        pixrgb(CoordX,CoordY,ColorR,ColorG,ColorB,Profundidad,Pixel2);
    esHexmapComprimido([Pixel]) ->
        obtCoordPixhexC(Pixel, CoordX, CoordY), obtColorPixhexC(Pixel, H_list),
        obtProfundidadPixhexC(Pixel, Profundidad),
        listRGB_HEX(H_list, Hex),
        pixhex(CoordX,CoordY,Hex,Profundidad, Pixel2);
    esBitmapComprimido([Pixel])->
        obtCoordPixbitC(Pixel, CoordX, CoordY), obtColorPixbitC(Pixel, B_list),
        obtProfundidadPixbitC(Pixel, Profundidad),
        [_, Bit] = B_list, pixbit(CoordX,CoordY,Bit,Profundidad, Pixel2).

% Descripción: Predicado que verifica si es un pixrgb, pixhex o pixbit
% Dominio: Pixel
% Recorrido: Boleano, false si no es un Pixel pixbit, pixhex o pixrgb
% Tipo: Otras funciones
esPixel(Elemento):-
    esBitmapComprimido([Elemento]) -> false;
    esPixmapComprimido([Elemento]) -> false;
    esHexmapComprimido([Elemento]) -> false;
    esBitmap([Elemento]) -> true;
    esPixmap([Elemento]) -> true;
    esHexmap([Elemento]) -> true; false.

% Descripción: Predicado que descomprime pixeles de formato de pixeles
% comprimidos
% Dominio: Pixeles X Pixeles
% Tipo de recursión: Natural, crea la lista con base a estados
% pendientes
% Tipo: Otras funciones
pixelesDescomprimidos([], []).
pixelesDescomprimidos([Cabeza | Cola], [NuevaCabeza | Cola2]):-
    esPixel(Cabeza) ->
        NuevaCabeza = Cabeza,
        pixelesDescomprimidos(Cola, Cola2)
        ;
        pixelDescomprimido(Cabeza, NuevaCabeza),
        pixelesDescomprimidos(Cola,Cola2).

% Descripción: Predicado que descomprime una imagen comprimida
% Dominio: image X image
% Tipo: Modificador
imageDecompress(Imagen, Imagen2):-
    imageIsCompress(Imagen),
    obtCoordImage(Imagen, Ancho, Largo), obtPixelesImage(Imagen, Pixeles),
    pixelesDescomprimidos(Pixeles, Pixeles2),
    image(Ancho, Largo, Pixeles2, Imagen2).
