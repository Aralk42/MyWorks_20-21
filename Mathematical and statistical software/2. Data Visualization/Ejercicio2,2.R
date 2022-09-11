####################################################
#
# Ejercicio 2.2
# Clara Benlloch Coscollà
#
########################################
#
setwd("")
# Una vez designado el directorio de trabajo: 
#
pdf(file="Ejercicio2,2_Clara_Benlloch.pdf", width=8.5, height=6)
#
a<-3*10^8*sqrt(2)
t<-seq(0,2*pi,by=0.001)
x<- (a*cos(t))/(1+sin(t)^2)
y<- (a*sin(t)*cos(t))/(1+sin(t)^2)
plot(x,y,type="l",col="red", axes=F,ann=F,las=1, lwd=4)
title(main="Lemniscata")
#
dev.off()
#
#####################################################