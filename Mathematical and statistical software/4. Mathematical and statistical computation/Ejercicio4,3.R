########################################################
#
# Ejercicio_4.3.Clara_Benlloch_Coscollà
#
############################################
# 
# Una vez designado el directorio de trabajo,
# 
pdf(file="Gráficas.Ejercicio4,3_Clara_Benlloch.pdf", width=8.5, height=6)

#
juego<-read.table("eguneanbehin.txt",header=T)
Puntuacion<-juego$Puntuak
Preguntas<-juego$Asmatutakoak
tiempo<-juego$Denbora

plot(tiempo,Puntuacion,xlim=c(20,150),ylim=c(20,100))
title(main="Puntuación dada según el tiempo transcurrido para todas las partidas")
#
#
#Relacion lineal entre Aciertos y puntos:
plot(Preguntas,Puntuacion)
title(main="Puntuación según el número de aciertos")
R.tiempo<-rep(0,length(tiempo))
#
# Observando la tabla de datossupongo la siguiente relación 
# para la puntuación que resta el tiempo.
# Dando 10 puntos por pregunta y partiendo de que 0 aciertos son 
# 10 puntos, como indica el último dato.
for(i in 1:length(tiempo)){
  R.tiempo[i]<-Preguntas[i]*10-Puntuacion[i]+10
}
#
plot(tiempo,R.tiempo,ylim=c(0,30),xlim=c(0,150))
title(main="Puntuación restada según el tiempo gastado en responder las preguntas")
#
# Ajustamos las curvas para cada puntuación con un ajuste no lineal
# ajustándolas a una función y=a+b*x^c donde "x" es el tiempo 
# e "y" la puntuación quitada por el tiempo gastado. 
y1<-R.tiempo[1:60]
x1<-tiempo[1:60]
plot(x1,y1)
title(main="Puntuación restada según el tiempo para 10 aciertos")
Parametros1<-nls(y1~a+b*x1^c,start=list(a=0,b=0.5,c=0.5))
Nuevos.par<-data.frame(x1=seq(min(x1),max(x1),len=100))
lines(Nuevos.par$x1,predict(Parametros1,newdata=Nuevos.par),col="red")
C1<-coef(Parametros1)

y2<-R.tiempo[61:110]
x2<-tiempo[61:110]
plot(x2,y2)
title(main="Puntuación restada según el tiempo para 9 aciertos")
Parametros2<-nls(y2~a+b*x2^c,start=list(a=-5,b=0.5,c=0.5))
Nuevos.par<-data.frame(x2=seq(min(x2),max(x2),len=100))
lines(Nuevos.par$x2,predict(Parametros2,newdata=Nuevos.par),col="red")
C2<-coef(Parametros2)

y3<-R.tiempo[111:167]
x3<-tiempo[111:167]
plot(x3,y3)
title(main="Puntuación restada según el tiempo para 8 aciertos")
Parametros3<-nls(y3~a+b*x3^c,start=list(a=-5,b=0.5,c=0.5))
Nuevos.par<-data.frame(x3=seq(min(x3),max(x3),len=100))
lines(Nuevos.par$x3,predict(Parametros3,newdata=Nuevos.par),col="red")
C3<-coef(Parametros3)

y4<-R.tiempo[168:219]
x4<-tiempo[168:219]
plot(x4,y4)
title(main="Puntuación restada según el tiempo para 7 aciertos")
Parametros4<-nls(y4~a+b*x4^c,start=list(a=0,b=0.5,c=0.5))
Nuevos.par<-data.frame(x4=seq(min(x4),max(x4),len=100))
lines(Nuevos.par$x4,predict(Parametros4,newdata=Nuevos.par),col="red")
C4<-coef(Parametros4)

y5<-R.tiempo[220:253]
x5<-tiempo[220:253]
plot(x5,y5)
title(main="Puntuación restada según el tiempo para 6 aciertos")
Parametros5<-nls(y5~a+b*x5^c,start=list(a=0,b=0.5,c=0.5))
Nuevos.par<-data.frame(x5=seq(min(x5),max(x5),len=100))
lines(Nuevos.par$x5,predict(Parametros5,newdata=Nuevos.par),col="red")
C5<-coef(Parametros5)

# He intentado encontrar una relación entre los parámetros
# de las curvas pero no parece haberlo
# Vector<-c(C1[2],C2[2],C3[2],C4[2],C5[2])
# Vector2<-c(C1[3],C2[3],C3[3],C4[2],C5[2])
# plot(Vector,Vector2)

print("Para cada número de preguntas acertadas, una función a+b*t^c (con diferentes parámetros a,b y c) ajusta la puntuación que será sustraida según el tiempo tardado en responder.")
print("Damos la función de puntuación para las partidas que tenemos más datos, estas son para 10,9,8,7 y 6 respuestas acertadas.")  
print("La función general de la puntuación obtenida es: Puntuacion=Preguntas.acertadas*10+10-funcion(t) , con funcion(t)=a+b*t^c con los parámetros a,b y c correspondientes.")
print("Representamos graficamente los ajustes para las puntuaciones de 10-6 utilizando esta funcion.")
plot(tiempo,Puntuacion,xlim=c(20,150),ylim=c(20,100))
title(main="Puntuación dada según el tiempo transcurrido para todas las partidas")
Tiempos<-seq(20:120)
F1<-10*10+10-C1[1]-C1[2]*Tiempos^C1[3]
lines(F1,col="red")
F2<-9*10+10-C2[1]-C2[2]*Tiempos^C2[3]
lines(F2,col="red")
F3<-8*10+10-C3[1]-C3[2]*Tiempos^C3[3]
lines(F3,col="red")
F4<-7*10+10-C4[1]-C4[2]*Tiempos^C4[3]
lines(F4,col="red")
F5<-6*10+10-C5[1]-C5[2]*Tiempos^C5[3]
lines(F5,col="red")
dev.off()
#
#####################################################################################