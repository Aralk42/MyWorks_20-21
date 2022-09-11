#######################################
#
# Ejercicio 3.2. Clara Benlloch Coscollà
#
###################
# 
#
# Planteamos la función ranking:
ranking<-function(conjunto,valores,preferencia){
  #
  Datos<-data.frame(conjunto,valores)
  #Ordenamos el conjunto en función de los valores:
  Datos<-Datos[order(Datos$valores, decreasing = TRUE),]
  #
  # Vemos cuantos números diferentes hay en valores 
  # (suponiendo que los valores pueden ir de 1 a 100, en caso
  # contrario, se aumentaría este rango):
  num<-intersect(1:100,valores)
  #
  # Para cada numero de valores, vemos su orden de preferencia:
  k<-1
  for (i in length(num):1){
    #
    # Buscamos cuales son las posiciones de cada numero en valores:
    posiciones<-which(valores==num[i])
    #
    # Buscamos donde está la letra del conjunto en preferencia:
    Letra<-which(preferencia==Datos[k,1])
    k<-k+1
    for (j in (length(posiciones)-1):1){
      #
      # Buscamos la siguiente letra en preferencia y comparamos 
      # las posiciones. Si la segunda letra
      # está antes que la primera, se intercambia la posición de ambas. 
      #
      Letra2<-which(preferencia==Datos[k,1])
      if(Letra2<Letra){
        guarda<-Datos[k-1,1]
        Datos[k-1,1]<-Datos[k,1]
        Datos[k,1]<-guarda
      }
      k<-k+1
    }
  }
  return(Datos[,1])
  
}
conjunto<-c(LETTERS[1:5])
valores<-c(1,1,2,1,2)
preferencia<-c("B","A","E","D","C")
Resultado<-ranking(conjunto,valores,preferencia)
Resultado
#
#En el mus:
cartas<-c("carta.jugador1","carta.jugador2","carta.jugador3","carta.jugador4")
valor<-c(1,5,1,5)
preferencias<-c("carta.jugador3","carta.jugador4","carta.jugador1","carta.jugador2")
Resultado2<-ranking(cartas,valor,preferencias)
print("Aplicado a como creo que funciona una ronda de mus, con valores: ");valor
print("Para cada carta y el siguiente orden de preferencia: ");preferencias
print("El resultado sería, de mayor a menor puntuación: ");Resultado2
#
############################################################
