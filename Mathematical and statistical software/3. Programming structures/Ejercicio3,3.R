###########################################
#
# Ejercicio 3.3. Clara Benlloch Coscollà
#
###################
#
# 1.
# Construimos la baraja:
Cartas.1al7<-c(1:7)
oros.10al12<-c(10,11,12)
names(oros.10al12)=c("Oros.10","Oros.11","Oros.12")
#
copas.10al12<-c(10,11,12)
names(copas.10al12)=c("Copas.10","Copas.11","Copas.12")
#
espadas.10al12<-c(10,11,12)
names(espadas.10al12)=c("Espadas.10","Espadas.11","Espadas.12")
#
bastos.10al12<-c(10,11,12)
names(bastos.10al12)=c("Bastos.10","Bastos.11","Bastos.12")
#
baraja<-c(Oros.=Cartas.1al7,oros.10al12,Copas.=Cartas.1al7,copas.10al12,Espadas.=Cartas.1al7,espadas.10al12,Bastos.=Cartas.1al7,bastos.10al12)
#
#########
# Reparto:
#
reparto<-function(baraja,semilla){
  set.seed(semilla)
  Reparto<-c(sample(baraja,16,replace=FALSE))
  #
  a1<-Reparto[1:4];b1<-Reparto[5:8];a2<-Reparto[9:12];b2<-Reparto[13:16]
  manos<-data.frame(a1,b1,a2,b2,byrow=T)
  return(manos)
  
}
#
# Para establecer el orden de cada mano:
Ordenmano<-function(i){
  if(i==1 | i%%4==1){k<-1234}
  if(i==2 | i%%4==2){k<-2341}
  if(i==3 | i%%4==3){k<-3412}
  if(i%%4==0){k<-4123}
  return(k)
}
#
# Construimos el dataframe de reparto con el orden de cada mano:
  for (i in 1:100){
    semilla<-2011+i
    Manos<-reparto(baraja,semilla)
    A1<-Manos[1,];B1<-Manos[2,];A2<-Manos[3,];B2<-Manos[4,]
    k<-Ordenmano(i)
    if (i == 1){
      k<-1234
      Repartos<-data.frame(c(A1[1],A1[2],A1[3],A1[4],B1[1],B1[2],B1[3],B1[4],A2[1],A2[2],A2[3],A2[4],B2[1],B2[2],B2[3],B2[4],k))
      names(Repartos)<-c("A1.1","A1.2","A1.3","A1.4","B1.1","B1.2","B1.3","B1.4","A2.1","A2.2","A2.3","A2.4","B2.1","B2.2","B2.3","B2.4","ordenmano")
      
    }
    else {

      Vect<-data.frame(c(A1[1],A1[2],A1[3],A1[4],B1[1],B1[2],B1[3],B1[4],A2[1],A2[2],A2[3],A2[4],B2[1],B2[2],B2[3],B2[4],k))
      names(Vect)<-c("A1.1","A1.2","A1.3","A1.4","B1.1","B1.2","B1.3","B1.4","A2.1","A2.2","A2.3","A2.4","B2.1","B2.2","B2.3","B2.4","ordenmano")
      
      Repartos<-rbind(Repartos,Vect)
    }
  }
Repartos
 # Se añade ahora la columna de juego y de quien tiene mejores cartas:
 for(i in 1:100){
    suma<-c("Suma.A1"=sum(Repartos[i,1:4]),"Suma.B1"=sum(Repartos[i,5:8]),"Suma.A2"=sum(Repartos[i,9:12]),"Suma.B2"=sum(Repartos[i,13:16]))
    if(suma[1]>=31|suma[2]>=31|suma[3]>=31|suma[4]>=31){
      Mano.juego<-data.frame("juego"=c("Hay juego"))
        if(max(suma[1],suma[3])>max(suma[2],suma[4])){
          mano.ganada<-data.frame("Mano"=c("A"))
          if(suma[1]|suma[3]==40|32 & suma[2]|suma[4]==31){
            mano.ganada<-data.frame("Mano"=c("B"))
          }
          else{
            if(suma[1]|suma[3]==40 & suma[2]|suma[4]==32){
            mano.ganada<-data.frame("Mano"=c("B"))
            }
          }
        }
      else{
        mano.ganada<-data.frame("Mano"=c("B"))
        if(suma[2]|suma[4]==40|32 & suma[1]|suma[3]==31){
          mano.ganada<-data.frame("Mano"=c("A"))
        }
        else{
          if(suma[2]|suma[4]==40 & suma[1]|suma[3]==32){
          mano.ganada<-data.frame("Mano"=c("A"))
          }
        }        
      }
      #
      # En caso de empate:
      if(max(suma[1],suma[3])==max(suma[2],suma[4])){
        if(Repartos[i,17]==1234|3412){
          mano.ganada<-data.frame("Mano"=c("A"))
        }
        else{
          mano.ganada<-data.frame("Mano"=c("B"))
        }
      }
    }
    # Si ninguna suma más de 31:
    else{
      Mano.juego<-data.frame("juego"=c("No hay juego"))
      # Decir quien tiene mejores cartas. 
      if(max(suma[1],suma[3])>max(suma[2],suma[4])){
        mano.ganada<-data.frame("Mano"=c("A"))
      }
      else{
        mano.ganada<-data.frame("Mano"=c("B"))
      }
      #
      #En caso de empate:
      if(max(suma[1],suma[3])==max(suma[2],suma[4])){
        if(Repartos[i,17]==1234|3412){
          mano.ganada<-data.frame("Mano"=c("A"))
        }
        else{
          mano.ganada<-data.frame("Mano"=c("B"))
        }
      }
    }
    if(i==1){
      juego<-Mano.juego
      Mejores.Cartas<-mano.ganada
    }
    else {
    juego<-rbind(juego,Mano.juego)
    Mejores.Cartas<-rbind(Mejores.Cartas,mano.ganada)
    }
  }
(Repartos<-cbind(Repartos,juego,Mejores.Cartas))
#############################
# 2. Lance 'Grande'
#
# Vemos qué pareja gana en el lance 'Grande':
Grande<-data.frame("A1.1"=Repartos[,1],"B1.1"=Repartos[,5],"A2.1"=Repartos[,9],"B2.1"=Repartos[,13])
Ganan<-c("Gana A1 y A2","Gana B1 y B2","Gana A1 y A2","Gana B1 y B2")
for(i in 1:100){
  maximo<-max(Grande[i,])
  j<-which(Grande[i,]==maximo)
  Orden<-order(Grande[i,],decreasing=T)
  
  if(length(j)==1){
    Pareja<-Ganan[j]
  }
else {
    # Si el número máximo está repetido en cada pareja:
    # (Si el número máximo se repite en la misma pareja, supongo que gana esta porque las dos cartas
    # tendrán la puntuación más alta)
    if(length(j)==2 & Grande[i,1]!=Grande[i,3] & Grande[i,2]!=Grande[i,4]){
      Pareja<-Ganan[Orden[3]]
    }
  else{
    j2<-j[1]
    Pareja<-Ganan[j2]
  }
  # Si hay 3 iguales y son los máximos, supongo que ganará la pareja que los tenga iguales, ya que 
  # la que tenga la carta diferente será de menor valor
  if(length(j)==3){
    Orden2<-order(c(Orden[1],Orden[2]),decreasing=T)
    if(Orden2[1]==4){
      Pareja<-Ganan[2]
    }
    else{
      Pareja<-Ganan[1]
    }
  }
  # Si los 4 valores son iguales gana quien va de mano:
  if(length(j)==4){
    Orden3<-Ordenmano(i)
    if(Orden3==1234|Orden==3412){
      Pareja<-Ganan[1]
    }
    else{
      Pareja<-Ganan[2]
    }
  }
  
}

  if(i==1){
    Pareja.Ganadora<-data.frame(Pareja)
  }
  else{
  Pareja.Ganadora<-rbind(Pareja.Ganadora,Pareja)
  }
}
(Grande<-cbind(Grande,"Grande"=Pareja.Ganadora))
################################
# 3. Lance 'Pequeño'
#
# Vemos qué pareja gana en el lance 'Pequeño':
Pequeña<-data.frame("A1.2"=Repartos[,2],"B1.2"=Repartos[,6],"A2.2"=Repartos[,10],"B2.2"=Repartos[,14])
for(i in 1:100){
  minimo<-min(Pequeña[i,])
  Orden<-order(Pequeña[i,])
  j<-which(Pequeña[i,]==minimo)
  if(length(j)==1){
    ganador<-Pequeña[i,j]
    Pareja2<-data.frame(Ganan[j])
  }
  # Si el número mínimo está repetido en cada pareja:
  # (Si el número mínimo se repite en la misma pareja, supongo que gana esta porque las dos cartas
  # tendrán la puntuación más baja)
  else {
    if(length(j)==2 & Pequeña[i,1]!=Pequeña[i,3] & Pequeña[i,2]!=Pequeña[i,4]){
      Pareja2<-Ganan[Orden[3]]
    }
    else{
      j2<-j[1]
      Pareja2<-Ganan[j2]
    }
    # Si hay 3 iguales y son los mínimos, supongo que ganará la pareja que los tenga iguales, ya que 
    # la que tenga la carta diferente será de mayor valor
    if(length(j)==3){
      Orden2<-order(c(Orden[1],Orden[2]))
      if(Orden2[1]==4){
        Pareja2<-Ganan[2]
      }
      else{
        Pareja2<-Ganan[1]
      }
    }
    # Si los 4 valores son iguales gana quien va de mano:
    if(length(j)==4){
      Orden3<-Ordenmano(i)
      if(Orden3==1234|Orden==3412){
        Pareja2<-Ganan[1]
      }
      else{
        Pareja2<-Ganan[2]
      }
    }
    
  }
  if(i==1){
    Pareja.Ganadora2<-data.frame(Pareja2)
  }
  else{
    Pareja.Ganadora2<-rbind(Pareja.Ganadora2,Pareja2)
  }
}
(Pequeña<-cbind(Pequeña,Pareja.Ganadora2))
#Unimos los lances 'Grande' y 'Pequeño':
(Total<-cbind(Grande,Pequeña))
#
################################################################