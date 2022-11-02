:- use_module(tda_image_21090869_castrovenegas).
:- use_module(tda_pixbit_21090869_CastroVenegas).
:- use_module(tda_pixhex_21090869_CastroVenegas).
:- use_module(tda_pixrgb_21090869_CastroVenegas).
:- use_module(tda_pixbit_comprimido_21090869_CastroVenegas).
:- use_module(tda_pixhex_comprimido_21090869_CastroVenegas).
:- use_module(tda_pixrgb_comprimido_21090869_CastroVenegas).


/*
Script básico Pruebas

El código presentado a continuación contiene ejemplos que le permitirán probar todas las
funciones. No obstante, estas funciones no cubren todos los escenarios posibles. Estos
ejemplos le servirán como referencia para su autoevaluación, sin embargo se recomienda
que pueda variar los ejemplos y probar distintos escenarios. Recuerde que en su entrega
final debe ampliar este script de pruebas con al menos 3 ejemplos más por función. Esto
quiere decir, que su script de prueba (archivo principal) debe contener todos los ejemplos
listados a continuación (sin cambios) además de los suyos.

Las definiciones de imágenes enumeradas de 1 hasta N corresponden a funciones constantes. En
este caso se aplican para hacer el código más legible evitando una composición directa de
las funciones, lo que hace que cada una de las evaluaciones de funciones resulte más confusa.

Probar que se puede generar una imagen pixbit

pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30,PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD, I),imageIsBitmap(I), imageToString(I, Str),write(Str).

Probar que imageIsBitMap detecta cuando se tiene una imagen en hex o
en rgb.

pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1,
0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD],
I), imageIsBitmap( I ).

Estos casos deben dar false:

pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20,
PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD),
image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).

pixrgb( 0, 0,200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), pixrgb( 1,
0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), image(
2, 2, [PA, PB, PC, PD], I), imageIsBitmap( I ).

Probar que se puede generar una imagen pixhex

pixhex( 0, 0, “#FF0000”, 10, PA), pixhex( 0, 1, “#FF0000”, 20, PB), pixhex( 1, 0, “#0000FF”, 30, PC), pixhex( 1, 1, “#0000FF”, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageToString(I, Str),write(Str).

Probar que imageIsHexmap detecta cuando se tiene una imagen en bit o en rgb.

pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB),
pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD),
image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap( I ).

Estos casos deben dar false:

pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap( I ).

pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), pixrgb( 1, 0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap( I ).

Probar que se puede generar una imagen pixrgb

pixrgb( 0, 0, 255, 0, 0, 10, PA), pixrgb( 0, 1, 255, 0, 0, 20, PB), pixrgb( 1, 0, 0, 0, 255, 30, PC), pixrgbbit( 1, 1, 0, 0, 255, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageToString(I, Str),write(Str).

Probar que imageIsPixmap detecta cuando se tiene una imagen en hex o en bit.

pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20,
PB), pixrgb( 1, 0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190,
4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).

Estos casos deben dar false:

pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).

pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB),
pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD),
image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ).

Convierte una imagen RGB a HEX y comprueba con los predicados de pertenencia, luego convierte a string y muestra por pantalla:

pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20,
PB), pixrgb( 1, 0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190,
4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap( I ),
imageRGBToHex(I, I2), imageIsHexmap(I2), imageToString(I2, Str),
write(Str).

Comprime una imagen, luego descomprime y debe resultar la misma imagen original:

pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageCompress(I, I2), imageDecompress(I2, I3).

En el ejemplo anterior "I" debería ser igual a "I3"

Si se rota una imagen 4 veces en 90°, debería resultar la imagen original:

pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB),
pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD),
image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2),
imageRotate90(I2, I3), imageRotate90(I3, I4), imageRotate90(I4, I5).

En el ejemplo anterior "I" debería ser igual a "I5"


Si se rota una imagen en 90° que tiene el mismo color y profundidad en todos sus píxeles, entonces la imagen resultante debería ser la misma imagen original.

pixhex( 0, 0, "#FF0000", 30, PA), pixhex( 0, 1, "#FF0000", 30, PB),
pixhex( 1, 0, "#FF0000", 30, PC), pixhex( 1, 1, "#FF0000", 30, PD),
image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2).

En el ejemplo anterior "I" debería ser igual a "I2"

Si se hace imageFlipV dos veces de una imagen, debería resultar la
imagen original:

pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB),
pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD),
image( 2, 2, [PA, PB, PC, PD], I), imageFlipV(I, I2), imageFlipV(I2,I3).

En el ejemplo anterior "I" debería ser igual a "I3"

Si se hace imageFlipH dos veces de una imagen, debería resultar la imagen original:

pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB),
pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD),
image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2), imageFlipH(I2,I3).

En el ejemplo anterior "I" debería ser igual a "I3"

Si se hace imageFlipH a una imagen que tiene el mismo color y profundidad en todos sus pixeles, entonces la imagen resultante debería ser la misma imagen original.

pixhex( 0, 0, "#FF0000", 30, PA), pixhex( 0, 1, "#FF0000", 30, PB),
pixhex( 1, 0, "#FF0000", 30, PC), pixhex( 1, 1, "#FF0000", 30, PD),
image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2).

En el ejemplo anterior "I" debería ser igual a "I2"

Se crea una imagen de 3x3 pixeles y se corta en una de 2x2 con solo la
 esquina inferior izquierda:

pixhex( 0, 0, "#FF0000", 20, PA), pixhex(0, 1, "#FF0000", 20, PB),
pixhex( 0, 2, "#FF0000", 20, PC), pixhex( 1, 0, "#0000FF", 30, PD),
pixhex( 1, 1, "#0000FF", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF),
pixhex( 2, 0, "#0000FF", 4, PG), pixhex( 2, 1, "#0000FF", 4, PH),
pixhex( 2, 2, "#0000FF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF,
PG, PH, PI], I), imageCrop( I, 1, 1, 2, 2, I2), pixhex( 0, 0, "#0000FF",
4, PE2), pixhex( 0, 1, "#0000FF", 4, PF2), pixhex( 1, 0, "#0000FF", 4,
PH2), pixhex( 1, 1, "#0000FF", 4, PI2), image( 2, 2, [PE2, PF2, PH2,
PI2], I3).

En el ejemplo anterior, "I2" debería ser una imagen con los mismos
pixeles y dimensiones que "I3"

Toma el píxel de la posición (0,1) que en la imagen original tiene los valores RGB (20, 20, 20) y lo reemplaza por otro píxel con valor RGB (54, 54, 54).
pixrgb( 0, 0, 10, 10, 10, 10, P1), pixrgb( 0, 1, 20, 20, 20, 20, P2), pixrgb( 1, 0, 30, 30, 30, 30, P3), pixrgb( 1, 1, 40, 40, 40, 40, P4), image( 2, 2, [P1, P2, P3, P4], I1), pixrgb( 0, 1, 54, 54, 54, 20, P2_modificado), imageChangePixel(I1, P2_modificado, I2)

Se construye imagen de 2x2 con los primeros 2 pixeles con profundidad 10 y los otros 2 con profundidad de 30, entonces al consultar “imageDepthLayers” se debería obtener una lista con dos imágenes.

pixrgb( 0, 0, 33, 33, 33, 10, PA), pixrgb( 0, 1, 44, 44, 44, 10, PB), pixrgb( 1, 0, 55, 55, 55, 30, PC), pixrgb( 1, 1, 66, 66, 66, 30, PD), image( 2, 2, [PA, PB, PC, PD], I), imageDepthLayers(I, [PRIMERA, SEGUNDA]), pixrgb( 0, 0, 33, 33, 33, 10, PA2), pixrgb( 0, 1, 44, 44, 44, 10, PB2), pixrgb( 1, 0, 255, 255, 255, 10, PC2), pixrgb( 1, 1, 255, 255, 255, 10, PD2), image( 2, 2, [PA2, PB2, PC2, PD2], I2), pixrgb( 0, 0, 255, 255, 255, 30, PA3), pixrgb( 0, 1, 255, 255, 255, 30, PB3), pixrgb( 1, 0, 55, 55, 55, 30, PC3), pixrgb( 1, 1, 66, 66, 66, 30, PD3), image( 2, 2, [PA3, PB3, PC3, PD3], I3).

En el ejemplo anterior, “I2” debería ser una imagen con los mismos pixeles y dimensiones que “PRIMERA”. “I3” debería ser una imagen con los mismos pixeles y dimensiones que “SEGUNDA”.


*/

/*

Script básico Pruebas (Extensión propia)

En esta sección del archivo están todas las funciones del Script básico Pruebas adicionales

El código presentado a continuación contiene ejemplos que le permitirán
probar todas las funciones. No obstante, estos predicados no cubren
todos los escenarios posibles. Es para ampliar este script de pruebas
con al menos 3 ejemplos más por cada predicado.

OBS:
-> Las imagenes definidas tendrán el formato ImgX para diferenciarlas
de las imagenes anteriores con formato IX
-> Los pixeles definidos tendrán el formato A,B,C...Z para
diferenciarlas de los pixeles anteriores con formato PX


Probar el predicado image

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageToString(Img1, Str),
write(Str).

pixhex(0,0,"#A0A0A0",10,A), pixhex(0,1,"#FF30A0", 10, B),
pixhex(1,0,"#FFFF00", 25, C), pixhex(1,1,"#FF30A0", 10, D),
image(2,2,[A,B,C,D], Img2), imageToString(Img2, Str),
write(Str).

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), imageToString(Img3, Str), write(Str).

Probar el predicado imageIsBitmap

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageIsBitmap(Img1).

Estos casos dan false

pixhex(0,0,"#A0A0A0",10,A), pixhex(0,1,"#FF30A0", 10, B),
pixhex(1,0,"#FFFF00", 25, C), pixhex(1,1,"#FF30A0", 10, D),
image(2,2,[A,B,C,D], Img2), imageIsBitmap(Img2).

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), imageIsBitmap(Img3).

Probar el predicado imageIsHexmap

pixhex(0,0,"#A0A0A0",10,A), pixhex(0,1,"#FF30A0", 10, B),
pixhex(1,0,"#FFFF00", 25, C), pixhex(1,1,"#FF30A0", 10, D),
image(2,2,[A,B,C,D], Img2), imageIsHexmap(Img2).

Estos casos dan false

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageIsHexmap(Img1).

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), imageIsHexmap(Img3).

Probar el predicado imageIsPixmap

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), imageIsPixmap(Img3).

Estos casos dan false

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageIsPixmap(Img1).

pixhex(0,0,"#A0A0A0",10,A), pixhex(0,1,"#FF30A0", 10, B),
pixhex(1,0,"#FFFF00", 25, C), pixhex(1,1,"#FF30A0", 10, D),
image(2,2,[A,B,C,D], Img2), imageIsPixmap(Img2).

Probar el predicado imageIsCompress

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageCompress(Img1, Img1A),
imageIsCompress(Img1A).

pixhex(0,0,"#A0A0A0",10,A), pixhex(0,1,"#FF30A0", 10, B),
pixhex(1,0,"#FFFF00", 25, C), pixhex(1,1,"#FF30A0", 10, D),
image(2,2,[A,B,C,D], Img2), imageCompress(Img2, Img2A),
imageIsCompress(Img2A).

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3),
imageCompress(Img3, Img3A), imageIsCompress(Img3A).

Estos casos dan false

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageIsCompress(Img1).

pixhex(0,0,"#A0A0A0",10,A), pixhex(0,1,"#FF30A0", 10, B),
pixhex(1,0,"#FFFF00", 25, C), pixhex(1,1,"#FF30A0", 10, D),
image(2,2,[A,B,C,D], Img2), imageIsCompress(Img2).

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), imageIsCompress(Img3).

Probar predicado imageFlipH

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), imageFlipH(Img3, Img3A).

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(0,3,1,40,D), pixbit(0,4,1,10,E), pixbit(0,5,0,10,F),
image(1,6, [A,B,C,D,E,F], Img4), imageFlipH(Img4, Img4A).

pixhex(0,0,"#A0A0A0",10,A), pixhex(1,0,"#FF30A0", 10, B),
pixhex(2,0,"#FFFF00",25,C), pixhex(3,0,"#FF30A0", 10, D),
pixhex(4,0,"#000000",13,E), image(5, 1, [A,B,C,D,E], Img5),
imageFlipH(Img5, Img5A).

Probar el predicado imageFlipV

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), imageFlipV(Img3, Img3A).

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(0,3,1,40,D), pixbit(0,4,1,10,E), pixbit(0,5,0,10,F),
image(1,6, [A,B,C,D,E,F], Img4), imageFlipV(Img4, Img4A).

pixhex(0,0,"#A0A0A0",10,A), pixhex(1,0,"#FF30A0", 10, B),
pixhex(2,0,"#FFFF00",25,C), pixhex(3,0,"#FF30A0", 10, D),
pixhex(4,0,"#000000",13,E), image(5, 1, [A,B,C,D,E], Img5),
imageFlipV(Img5, Img5A).

Probar imageCrop

pixhex(0,0,"#A0A0A0",10,A), pixhex(0,1,"#FF30A0", 10, B),
pixhex(1,0,"#FFFF00", 25, C), pixhex(1,1,"#FF30A0", 10, D),
image(2,2,[A,B,C,D], Img2), imageCrop(Img2, 0,0,0,1,Img2A),
image(1,2,[A,B], Img2B).

En el ejemplo anterior "Img2A" = "Img2B"

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(1,0,30,30,30,30,C), pixrgb(1,1,40,40,40,40,D),
pixrgb(2,0,10,10,10,10,E), pixrgb(2,1,10,10,10,10,F),
image(3,2,[A,B,C,D,E,F], Img3), imageCrop(Img3, 1,0,2,1, Img3A),
pixrgb(0,0,30,30,30,30,A2), pixrgb(0,1,40,40,40,40,B2),
pixrgb(1,0,10,10,10,10,C2), pixrgb(1,1,10,10,10,10,D2),
image(2,2,[A2,B2,C2,D2], Img3B).

En el ejemplo anterior "Img3A" = "Img3B"

pixhex(0,0,"#A0A0A0",10,A), pixhex(1,0,"#FF30A0", 10, B),
pixhex(2,0,"#FFFF00",25,C), pixhex(3,0,"#FF30A0", 10, D),
pixhex(4,0,"#000000",13,E), image(5, 1, [A,B,C,D,E], Img5),
imageCrop(Img5,4,0,4,0,Img5A).

En el ejemplo anterior solo hay una imagen con un píxel

pixbit(0,0,1,10,A),pixbit(0,1,0,20,B), pixbit(1,0,0,20,C),
pixbit(1,1,1,10,D), image(2,2,[A,B,C,D], Img2), imageCrop(Img2, 2,
2, 2, 2, Img2A).

En el ejemplo anterior la imagen debe ser vacia

Probar imageRGBToHex

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(1,0,30,30,30,30,C), pixrgb(1,1,40,40,40,40,D),
pixrgb(2,0,10,10,10,10,E), pixrgb(2,1,10,10,10,10,F),
image(3,2,[A,B,C,D,E,F], Img3), imageToString(Img3, Str), write(Str),
imageRGBToHex(Img3, Img3A), imageToString(Img3A, Str2), write(Str2).

pixrgb(0,0,35,20,10,0,A), pixrgb(0,1,20,25,20,0,B),
pixrgb(0,2,4,32,55,10,C), pixrgb(0,3,40,255,255,10,D),
pixrgb(1,0,255,255,10,10,E), pixrgb(1,1,10,10,255,0,F),
pixrgb(1,2,45,66,100,0,G), pixrgb(1,3,100,100,100,100,H),
pixrgb(2,0,40,12,45,10,I), pixrgb(2,1,0,0,0,255,J),
pixrgb(2,2,10,10,10,10,K), pixrgb(2,3,100,10,20,50,L),
image(3,4,[A,B,C,D,E,F,G,H,I,J,K,L], Img8), imageToString(Img8, Str),
write(Str), imageRGBToHex(Img8, Img8A), imageToString(Img8A, Str2),
write(Str2).

pixrgb(0,0,10,10,10,10,A),pixrgb(1,0,30,30,30,30,B),pixrgb(2,0,10,10,10,10,C),
image(3,1,[A,B,C], Img6), imageToString(Img6, Str), write(Str),
imageRGBToHex(Img6, Img6A), imageToString(Img6A, Str2), write(Str2).

Probar imageToHistogram

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), imageToHistogram(Img3, Histograma).

pixrgb(0,0,35,20,10,0,A), pixrgb(0,1,20,25,20,0,B),
pixrgb(1,0,4,32,55,10,C), pixrgb(1,1,40,255,255,10,D),
image(2,2,[A,B,C,D], Img8), imageRGBToHex(Img8, Img8A),
imageToHistogram(Img8A, Histograma).

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageToHistogram(Img1,
Histograma).

Probar imageRotate90

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,50,50,50,50,E), pixrgb(1,2,60,60,60,60,F),
image(2,3,[A,B,C,D,E,F], Img3),imageToString(Img3, Str), write(Str),
imageRotate90(Img3, Img3A), imageToString(Img3A, Str2), write("\n"),
write(Str2).

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageToString(Img1, Str),
write(Str), imageRotate90(Img1, Img1A), imageToString(Img1A, Str2),
write("\n"), write(Str2).

pixhex(0,0,"#A0A0A0",10,A), pixhex(1,0,"#FF30A0", 10, B),
pixhex(2,0,"#FFFF00",25,C), pixhex(3,0,"#FF30A0", 10, D),
pixhex(4,0,"#000000",13,E), image(5, 1, [A,B,C,D,E], Img5),
imageToString(Img5, Str), write(Str), imageRotate90(Img5, Img5A),
imageToString(Img5A, Str2), write("\n"), write(Str2).

Probar imageCompress

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(1,0,1,30,C),
pixbit(1,1,1,40,D), image(3,3,[A,B,C,D], Img4), imageCompress(Img4,
Img4A).

pixhex(0,0,"#A0A0A0",10,A), pixhex(0,1,"#FF30A0", 10, B),
pixhex(1,0,"#FFFF00", 25, C), pixhex(1,1,"#FF30A0", 10, D),
image(2,2,[A,B,C,D], Img2), imageCompress(Img2, Img2A).

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(1,0,30,30,30,30,C), pixrgb(1,1,40,40,40,40,D),
image(2,2,[A,B,C,D], Img5), imageCompress(Img5, Img5A).

Probar imageChangePixel

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(1,0,1,30,C),
pixbit(1,1,1,40,D), image(3,3,[A,B,C,D], Img4), pixbit(1,0,0,33,C2),
imageChangePixel(Img4, C2, Img4A).

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), pixhex(0,0,"#FFFFFF", 10, A2),
imageChangePixel(Img3, A2, Img3A).

En el ejemplo anterior "Img3" = "Img3A"

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), pixrgb(0,0,255,255,255, 10, A2),
imageChangePixel(Img3, A2, Img3A).

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
image(1,3,[A,B,C], Img8), pixbit(0,2,0,44,C2), imageChangePixel(Img8,
C2, Img8A).

Probar imageToString
pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageToString(Img1, Str),
write(Str).

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(1,0,30,30,30,30,C), pixrgb(1,1,40,40,40,40,D),
pixrgb(2,0,10,10,10,10,E), pixrgb(2,1,10,10,10,10,F),
image(3,2,[A,B,C,D,E,F], Img3), imageCrop(Img3, 1,0,2,1, Img3A),
imageToString(Img3A, Str), write(Str).

pixrgb(0,0,35,20,10,0,A), pixrgb(0,1,20,25,20,0,B),
pixrgb(0,2,4,32,55,10,C), pixrgb(0,3,40,255,255,10,D),
pixrgb(1,0,255,255,10,10,E), pixrgb(1,1,10,10,255,0,F),
pixrgb(1,2,45,66,100,0,G), pixrgb(1,3,100,100,100,100,H),
pixrgb(2,0,40,12,45,10,I), pixrgb(2,1,0,0,0,255,J),
pixrgb(2,2,10,10,10,10,K), pixrgb(2,3,100,10,20,50,L),
image(3,4,[A,B,C,D,E,F,G,H,I,J,K,L], Img8),imageRGBToHex(Img8, Img8A),
imageToString(Img8A, Str), write(Str).

Probar imageDepthLayers

pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageDepthLayers(Img1, L).

pixhex(0,0,"#A0A0A0",10,A), pixhex(0,1,"#FF30A0", 10, B),
pixhex(1,0,"#FFFF00", 25, C), pixhex(1,1,"#FF30A0", 10, D),
image(2,2,[A,B,C,D], Img2), imageDepthLayers(Img2, L).

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), imageDepthLayers(Img3, L).

Probar imageDecompress
pixbit(0,0,1,10,A), pixbit(0,1,0,20,B), pixbit(0,2,1,30,C),
pixbit(1,0,1,40,D), pixbit(1,1,1,10,E), pixbit(1,2,0,10,F),
pixbit(2,0,1,30,G), pixbit(2,1,0,20,H), pixbit(2,2,1,10,I),
image(3,3,[A,B,C,D,E,F,G,H,I], Img1), imageCompress(Img1, Img1A),
imageDecompress(Img1A, Img1B), imageToString(Img1B, Str),
write(Str).

pixhex(0,0,"#A0A0A0",10,A), pixhex(0,1,"#FF30A0", 10, B),
pixhex(1,0,"#FFFF00", 25, C), pixhex(1,1,"#FF30A0", 10, D),
image(2,2,[A,B,C,D], Img2), imageCompress(Img2,
Img2A), imageDecompress(Img2A, Img2B), imageToString(Img2B, Str),
write(Str).

pixrgb(0,0,10,10,10,10,A), pixrgb(0,1,20,20,20,20,B),
pixrgb(0,2,30,30,30,30,C), pixrgb(1,0,40,40,40,40,D),
pixrgb(1,1,10,10,10,10,E), pixrgb(1,2,10,10,10,10,F),
image(2,3,[A,B,C,D,E,F], Img3), imageCompress(Img3,
Img3A), imageDecompress(Img3A, Img3B), imageToString(Img3B, Str),
write(Str).

*/
















