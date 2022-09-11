#Clara_Benlloch.1.1.txt

#Cargar base de datos iris:
data(iris)

#1.Identificadores: 

PetalosAltura <- iris$Petal.Length
AlturaEsp<-which(1.6<PetalosAltura & PetalosAltura<4.0)
PetalosAnchura <- iris$Petal.Width
AnchuraEsp<-which(1.2<PetalosAnchura & PetalosAnchura<1.5)
(PetalosEspecific <- intersect(AlturaEsp,AnchuraEsp))

#2. ¿Cuántas son?

(length(PetalosEspecific))

#3. ¿De qué especies son?

(Especie1<-as.character(iris[PetalosEspecific[1],5]))
(Especie2<-as.character(iris[PetalosEspecific[2],5]))

#4. ¿Demás características?

Altura.Sepalo<-c(iris[PetalosEspecific[1],1],iris[PetalosEspecific[2],1])
Anchura.Sepalo<-c(iris[PetalosEspecific[1],2],iris[PetalosEspecific[2],2])
(Características<-data.frame(PetalosEspecific,Altura.Sepalo,Anchura.Sepalo))

#5.Plantas ordenadas por la anchura del pétalo.

(iris<-iris[order(iris$Petal.Width, decreasing = TRUE),]) 

#6.Ranking.

Petalo<-iris$Petal.Width
Rango<-rank(-Petalo,na.last="keep",ties.method=c("min"))
Rango2<-rank(-Petalo,na.last="keep",ties.method=c("max"))
Unicos<-unique(Rango)
Unicos2<-unique(Rango2)
Formula<-Unicos2-Unicos+1
Ranking1<-rep(1:length(Unicos),times=Formula)
RankingPetalos <- data.frame(Ranking=Ranking1)
(iris<-cbind(iris,RankingPetalos))

#7.Suma puestos

#Suma Ranking Setosa:
Especies<-as.character(iris$Species)
Petalo.Setosa<-which(Especies=="setosa")
(Suma.Setosa<-sum(Ranking1[Petalo.Setosa]))

#Suma Ranking virginica
Petalo.Virginica<-which(Especies=="virginica")
(Suma.Virginica<-sum(Ranking1[Petalo.Virginica]))

#Suma Ranking versicolor
Petalo.Versicolor<-which(Especies=="versicolor")
(Suma.Versicolor<-sum(Ranking1[Petalo.Versicolor]))

#Mayor anchura del pétalo
Vector.Sumas<-c(Setosa=Suma.Setosa,Virginica=Suma.Virginica,Versicolor=Suma.Versicolor)
Minima.Suma<-min(Vector.Sumas)
Puesto.Minima.Suma<-which(Vector.Sumas==Minima.Suma)
(Mayor.Anchura<-c(names(Puesto.Minima.Suma), " tiene, en general, mayor anchura de pétalo."))



