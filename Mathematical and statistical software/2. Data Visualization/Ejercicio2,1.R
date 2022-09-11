####################################################
#
# Ejercicio 2.1
# Clara Benlloch Coscollà
#
########################################
#
#
setwd("")
# Una vez designado el directorio de trabajo con el archivo "9.4.DAT": 
#
pdf(file="Ejercicio2,1_Clara_Benlloch.pdf", width=8.5, height=6)
#
Precios<-scan("9.4.DAT")
#
plot(1800:1997,Precios, frame.plot=F,type="l",
     col="red", axes=F,ann=F,las=1,ylim=c(60,480))
#
box(bty="l")
#
axis(2,las=2, at = c(67,203,339,475), labels=c(67,203,339,475))
axis(1,at = seq(1800,2000,by=25), labels=seq(1800,2000,by=25))
#
mtext("Año",side=1, line=3, col="red")
mtext("Precio($)",side=2, line=0.5, las=2, col= "red")
#
title(main="Evolución del precio del cobre ($)
     en el periodo del 1800-1997",col.main="blue",
      font.main="4",cex.main=0.95,line = -1)
#
#
dev.off()
#
#################################################