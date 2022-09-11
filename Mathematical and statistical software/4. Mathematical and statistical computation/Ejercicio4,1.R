########################################################
#
# Ejercicio_4.1.Clara_Benlloch_Coscollà
#
############################################
#
#
pdf(file="Gráfica.Ejercicio4,1_Clara_Benlloch.pdf", width=8.5, height=6)
#
# Habiendo determinado el directorio donde está el archivo:
load("partidas.Rdata")
#
Baraja<-data.frame(partidas.mus$baraja)
Grande<-data.frame(partidas.mus$grande)
Rondas<-data.frame(partidas.mus$rondas)
Pequena<-data.frame(partidas.mus$pequena)
Juego<-data.frame(partidas.mus$juego)
#
# 1. Porcentaje de veces que cada pareja ha tenido la mejor tirada en 'Grande'
#
Cuantas.rondas<-length(Grande$par)
maxA<-which(Grande$par.gmax=="A")
maxB<-which(Grande$par.gmax=="B")
print("Porcentaje pareja A")
(PorcentajeA<-length(maxA)/Cuantas.rondas*100)
print("Porcentaje pareja B")
(PorcentajeB<-length(maxB)/Cuantas.rondas*100)
#
# 2. Porcentaje de veces que se ha tenido la mejor tirada en 'Grande'
#   sin tener un 'Rey':
#
Gsin12.A1<-which(Grande$grande.A1<10000)
Gsin12.A2<-which(Grande$grande.A2<10000)
maxA2<-intersect(maxA,Gsin12.A1)
maxA3<-intersect(maxA2,Gsin12.A2)
print("Porcentaje pareja A")
(PorcentajeAsin12<-length(maxA3)/Cuantas.rondas*100)
#
Gsin12.B1<-which(Grande$grande.B1<10000)
Gsin12.B2<-which(Grande$grande.B2<10000)
maxB2<-intersect(maxB,Gsin12.B1)
maxB3<-intersect(maxB2,Gsin12.B2)
print("Porcentaje pareja B")
(PorcentajeBsin12<-length(maxB3)/Cuantas.rondas*100)
#
# 3. Porcentaje de veces que cada jugador ha tenido la mejor tirada en 'Pequeña'
#
minA1<-which(Pequena$jug.pmin=="A1")
print("Porcentaje jugador A1")
(PorcentajeA1<-length(minA1)/Cuantas.rondas*100)
minA2<-which(Pequena$jug.pmin=="A2")
print("Porcentaje jugador A2")
(PorcentajeA2<-length(minA2)/Cuantas.rondas*100)
minB1<-which(Pequena$jug.pmin=="B1")
print("Porcentaje jugador B1")
(PorcentajeB1<-length(minB1)/Cuantas.rondas*100)
minB2<-which(Pequena$jug.pmin=="B2")
print("Porcentaje jugador B2")
(PorcentajeB2<-length(minB2)/Cuantas.rondas*100)
#
# 4. Porcentaje de veces que cada jugador ha tenido la mejor tirada en 'Pequeña'
# sin un 'As'
#
Psin1.A1<-which(Pequena$pequena.A1>=2000)
minA1.2<-intersect(minA1,Psin1.A1)
print("Porcentaje A1")
(PA1sin1<-length(minA1.2)/Cuantas.rondas*100)

Psin1.A2<-which(Pequena$pequena.A2>=2000)
minA2.2<-intersect(minA2,Psin1.A2)
print("Porcentaje A2")
(PA2sin1<-length(minA2.2)/Cuantas.rondas*100)

Psin1.B1<-which(Pequena$pequena.B1>=2000)
minB1.2<-intersect(minB1,Psin1.B1)
print("Porcentaje B1")
(PB1sin1<-length(minB1.2)/Cuantas.rondas*100)

Psin1.B2<-which(Pequena$pequena.B2>=2000)
minB2.2<-intersect(minB2,Psin1.B2)
print("Porcentaje B2")
(PB2sin1<-length(minB2.2)/Cuantas.rondas*100)
#
# 6. Cuantas veces se ha llegado a 'Juego' en 'Juego'
#
Situacion.Juego<-which(Juego$Jmax>30)
print("Se ha llegado a 'Juego':")
(Pjuego<-length(Situacion.Juego)/length(Juego$Jmax)*100)
#
# 7. Diagramas de barras: 
#
Mejor.Jugada<-which(Juego$Jmax==31)
Num.Juego<-which(Juego$Jmax>30)
HayJuego<-as.vector(Juego$jug.jmej[Mejor.Jugada])
JuegoA1<-(length(which(HayJuego=="A1"))/length(Num.Juego)*100)
JuegoA2<-(length(which(HayJuego=="A2"))/length(Num.Juego)*100)
JuegoB1<-(length(which(HayJuego=="B1"))/length(Num.Juego)*100)
JuegoB2<-(length(which(HayJuego=="B2"))/length(Num.Juego)*100)
barplot(c(JuegoA1,JuegoA2,JuegoB1,JuegoB2),names.arg=c("JuegoA1","JuegoA2","JuegoB1","JuegoB2"),
        main="Porcentajes de mejor jugada de jugador 
        cuando hay 'juego'")
#
Mejor.Jugada2<-which(Juego$Jmax==30)
Num.Juego2<-which(Juego$Jmax<=30)
HayJuego2<-as.vector(Juego$jug.jmej[Mejor.Jugada2])
JuegoA1.2<-(length(which(HayJuego2=="A1"))/length(Num.Juego2)*100)
JuegoA2.2<-(length(which(HayJuego2=="A2"))/length(Num.Juego2)*100)
JuegoB1.2<-(length(which(HayJuego2=="B1"))/length(Num.Juego2)*100)
JuegoB2.2<-(length(which(HayJuego2=="B2"))/length(Num.Juego2)*100)
barplot(c(JuegoA1.2,JuegoA2.2,JuegoB1.2,JuegoB2.2),names.arg=c("PuntoA1","PuntoA2","PuntoB1","PuntoB2"),
        main="Porcentajes de mejor jugada de jugador 
        cuando no hay 'juego'")
par(mfrow=c(1,2))
barplot(c(JuegoA1,JuegoA2,JuegoB1,JuegoB2),names.arg=c("JuegoA1","JuegoA2","JuegoB1","JuegoB2"),
        main="% mejor jugada 
        si hay 'juego'")
barplot(c(JuegoA1.2,JuegoA2.2,JuegoB1.2,JuegoB2.2),names.arg=c("PuntoA1","PuntoA2","PuntoB1","PuntoB2"),
        main="% mejor jugada 
        si no hay 'juego'")
#
# 8. Porcentaje de veces que A1 no ha ganado con 31:
#
A1con31<-which(Juego$juego.A1==31)
JuegoA1.3<-which(Juego$jug.jmej!="A1")
NoA1<-intersect(A1con31,JuegoA1.3)
print("Porcentaje de veces que A1 no ha ganado con 31:")
(PNoA1<-length(NoA1)/length(Juego$jug.jmej)*100)
print("Esto ocurre cuando otra persona tiene también 31 y va de mano")
print("Esto ocurre en las partidas:")
NoA1
#
# 9. Porcentaje de veces que gana A1 sin tener 31 habiendo 'juego':
#
Situacion.Juego2<-which(Juego$Jmax>31)
Nombres<-as.vector(Juego$jug.jmej[Situacion.Juego2])
Asin31<-which(Nombres=="A1")
PAsin31<-length(Asin31)/length(Situacion.Juego)*100
print("El porcentaje de veces que A1 gana 'juego' sin 31 puntos:")
PAsin31
#
# 10. 
print("Mostrar gráficamente el porcentaje de veces que se ha ganado el juego o se han obtenido los máximos puntos con cada número:")
#
minJmax<-min(Juego$Jmax)
#Hacemos un vector de frecuencias:
Vfrec<-rep(0,minJmax-1)
for(i in 17:40){
  Frecuencias<-length(which(Juego$Jmax==i))/length(Juego$Jmax)*100
  Vfrec<-c(Vfrec,Frecuencias)
}
Nombres2<-c(1:40)
par(mfrow=c(1,1))
barplot(Vfrec, names.arg=c(Nombres2),
        main="Porcentaje de veces en el que cada número es 
        el máximo 'Juego'")
#
dev.off()
#
###########################################################