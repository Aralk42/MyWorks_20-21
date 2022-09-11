####################################################
#
# Ejercicio 2.3
# Clara Benlloch Coscollà
#
########################################
#
setwd("")
#Una vez designado el directorio de trabajo:
#
pdf(file="Ejercicio2,3_Clara_Benlloch.pdf", width=8.5, height=6)
#
Mu<-40 #Media
Sigma<-5 #Desviación estandard
#
x<-seq(25,55,by=0.01)
y<-dnorm(x, mean=Mu, sd=Sigma, log=F)
#
plot(x,y,type="l",col="red",lwd=2,from=25,to=55,axes=F,frame.plot=T)
axis(2, las=1)
axis(1)
#
#Texto eje x:
mtext(text=expression(mu-3*sigma, mu-2*sigma, mu-sigma, mu,mu+sigma,mu+2*sigma,mu+3*sigma),at=seq(25,55,by=5),cex=0.85,side=1, line=2, col="red")
#
#Título:
title(main="Función de densidad de probabilidad (gaussiana)",
      col.main="blue",cex.main=1,font.main="2")
#
#Líneas del gráfico:
val.x1<-which(x==30);val.x2<-which(x==35)
val.x3<-which(x==45);val.x4<-which(x==50)
segments(Mu,0,Mu,max(y), lty="dotted", col="blue")
segments(30,0,30,y[val.x1], lty="dotted", col="blue")
segments(35,0,35,y[val.x2], lty="dotted", col="blue")
segments(45,0,45,y[val.x3], lty="dotted", col="blue")
segments(50,0,50,y[val.x4], lty="dotted", col="blue")
#
#Flechas:
arrows(35, 0.03, 45, 0.03, code = 3, length = 0.08, 
       angle=35, lty="dotted", col="blue")
arrows(30, 0.008, 50, 0.008, code = 3, length = 0.08, 
       angle=35, lty="dotted", col="blue")
#
#Texto en el gráfico:
text(40,0.03,labels="0.6827",cex=0.85,col="blue")
text(40,0.008,labels="0.9545",cex=0.85,col="blue")
text(50,0.065,col="red",
     label=(expression(y==frac(1,sigma*sqrt(2*pi))*e^{paste(-frac(1,2),bgroup("(",over(x - mu,sigma),")")^2)})))
#
dev.off()
#
#
###############################################
