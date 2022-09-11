##################################################
#
# Ejercicio 3.1. Clara Benlloch Coscollà
#
###################
# 
# Una vez designado el directorio de trabajo: 
#
pdf(file="Ejercicio3,1_Clara_Benlloch.pdf", width=7.5, height=8)
#
alpha<-seq(0,2*pi,length.out=100)
r<-10
i<-0
num<-1
x<-r*cos(alpha)
y<-r*sin(alpha)
plot(x,y, type="l",axes=F,xlab="",ylab="",lwd=3)
text(0,r-0.05,num,font=2)
#
for(i in 9:1){
  num<-num+1
  par(new=T)
  r<-r-1
  x<-r*cos(alpha)
  y<-r*sin(alpha)
  if(r==1){
    polygon(x,y, col = "red")
  }
  lines(x,y,lwd=1.5)
  text(0,r-0.05,num,font=2)
}
#
dev.off()
#
###########################################################
