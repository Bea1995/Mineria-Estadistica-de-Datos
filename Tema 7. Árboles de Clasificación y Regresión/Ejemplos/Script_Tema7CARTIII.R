#############################################
#MINERIA ESTADISTICA DE DATOS               #
#ARBOLES DE REGRESION                       #
#DATOS  Hitters DE LA LIBRERIA ISLR         #
#############################################
library(rpart)
library(ISLR)
data(Hitters)
?Hitters
summary(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)  #SELECCIONAR CASOS COMPLETOS
dim(Hitters)
sum(is.na(Hitters))
boxplot(Hitters$Salary)  #MEJOR LA TRANSFORMACION
boxplot(log(Hitters$Salary))

#DIVIDIR LOS DATOS EN ENTRENAMIENTO/TEST
set.seed(1)
n=nrow(Hitters)
train=sample(1:n,  floor(n*0.8))
test=setdiff(1:n,train)


#CONSTRUIR EL ARBOL INCLUYENDO PRUNE
#EN REGRESION, EL CP ES EL AUMENTO MINIMO EN EL R2
Hit.rpart <- rpart(log(Salary) ~ ., data = Hitters[train,],cp=0.001)
Hit.rpart

plot(Hit.rpart,main="CART datos Hitters. CP=0.001",uniform=TRUE)
text(Hit.rpart,col="blue",cex=0.6)

printcp(Hit.rpart,digits=3)
plotcp(Hit.rpart,lty=2,upper="splits",col="blue")
rsq.rpart(Hit.rpart)  #INCLUYE R2


#Tabla con las estimaciones VC
cptabla<- Hit.rpart$cptable
#Regla 1-ES
CP1ES<- min(cptabla[,4])+cptabla[which.min(cptabla[,4]),5] ; CP1ES
indicp<- 1:nrow(cptabla)
cprecorte<- cptabla[indicp[cptabla[,4]<CP1ES][1],1]; cprecorte
#Recorte 
Hit.rpart2<-prune.rpart(Hit.rpart,cp=cprecorte)  
Hit.rpart2
plot(Hit.rpart2,main="CART recortado",uniform=TRUE)
text(Hit.rpart2,col="blue",cex=0.6)

#RESUMEN AMPLIO
summary(Hit.rpart)  

#IMPORTANCIA DE LAS VARIABLES
Hit.rpart$variable.importance  


#CALIDAD DE LAS PREDICCIONES EN LA ESCALA ORIGINAL
preditest=exp(predict(Hit.rpart2,Hitters[-train,]))
Ytest=Hitters[-train,"Salary"]
predient=exp(predict(Hit.rpart2))
Yent=Hitters[train,"Salary"]


Ajuste=function(y,yp,conjunto)
{
  residuos=y-yp
  RECM=sqrt(mean(residuos^2))
  R2=cor(y,yp)^2
  plot(y,yp,col="blue",xlab="Observado",ylab="Predicciones",main=conjunto)
  abline(a=0,b=1,col="red",lwd=2)
  plot(yp,residuos,col="blue",xlab="Predicciones",ylab="Residuos",main=conjunto)
  abline(h=0,col="lightgrey",lwd=2)
  return(list(RECM=RECM,R2=R2))
}
Ajuste(Ytest,preditest,"Test")
Ajuste(Yent,predient,"Entrenamiento")








