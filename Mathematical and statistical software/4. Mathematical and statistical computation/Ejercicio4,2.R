########################################################
#
# Ejercicio_4.2.Clara_Benlloch_Coscollà
#
############################################
#
# Una vez designado el directorio de trabajo,
#
# Criba de Eratóstenes:
# 
# Definimos la función:
#
#
  Criba.de.Eratostenes<-function(n,tiempo){
    numeros<-c(2:n)
    i<-1
    c1<-TRUE
    if(n<2){
      stop("El número debe ser mayor o igual a 2")
      numeros<-"none"
    }
    else
    {
    if(length(numeros)>2){
  while (c1){
    multiplos<-numeros[i]
    j<-1
    w<-2
    c2<-TRUE
    while(c2){
      j<-j+1
      w<-j*numeros[i]
      multiplos<-c(multiplos,w)
      if(w >= n)c2<-FALSE
    }
    multiplos<-c(multiplos[-1])
   
    for(j in 1:length(multiplos)){
      if(j==1){
        iguales<-which(numeros==multiplos[j])
        sustraer<-iguales
      }
      else{
    iguales<-which(numeros==multiplos[j])
    sustraer<-c(sustraer,iguales)
      }
    }
    numeros<-numeros[-sustraer]
    
    i<-i+1
    if(numeros[i]^2>n)c1<-FALSE
  }
  }
    }
return(numeros)
  }
  #
  # Por ejemplo, para n=40:
  n <- 40
  Primos<-Criba.de.Eratostenes(n)
  Primos
  #
  #
  print("Vemos ahora para diferentes valores de n el tiempo que tarda en ejecutar la función:")
  n<-c(2,100,500,1000,1500,2000,3500,5000,7500,10000,11000,12000,13000,14000,15000,17500,20000)
  for(y in 1:length(n)){
    if(n[y]==2){
    Tiempo<-system.time({Criba.de.Eratostenes(n[y])})
    }
    else{
      Tiempo2<-system.time({Criba.de.Eratostenes(n[y])})
      Tiempo<-rbind(Tiempo,Tiempo2)
    }
  }
  # 
    pdf(file="Gráfica.Ejercicio4,2_Clara_Benlloch.pdf", width=8.5, height=6)
  #
  # Aproximar funcion entre n y t.
  Tiempo<-as.data.frame(Tiempo)
  Tiempo.elapsed<-Tiempo$elapsed
  plot(n,Tiempo.elapsed)
  title(main="Tiempo de ejecución para cada n")
  Funcion<-nls(Tiempo.elapsed~b*n+c*n^2,start=list(b=0,c=20))
  Ej<-data.frame(n=seq(min(n),max(n),len=100))
  lines(Ej$n,predict(Funcion,newdata=Ej),col="red")
  dev.off()
  #
  print("Haciendo un ajuste no lineal (mostrado en el gráfico) se puede estimar la relacion entre n y t como t=b*n+c*n^2, con los coeficientes b y c:")
  coef(Funcion)
  #
  #########################################################