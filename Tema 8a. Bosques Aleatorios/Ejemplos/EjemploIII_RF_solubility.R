###########################################
#MÁSTER UNIVERSITARIO EN MATEMÁTICAS      #
#MINERÍA ESTADÍSTICA DE DATOS             #
#RANDOM FORESTS                           #
#DATOS  solubility DE LA LIBRERIA         # 
#AppliedPredictiveModeling                #
###########################################

################
#CART
################

#install.packages(c("AppliedPredictiveModeling","partykit"))
library(AppliedPredictiveModeling) #Datos solubility
library(rpart)
library(partykit)
library(rpart.plot)

#Cargar los conjuntos de entrenamiento y test, X e Y
#INCLUYE TRANSF. BOX-COX VARIABLES CUANTITATIVAS
data(solubility)  
dim(solTrainXtrans)   

#208 var binarias, 16 var frecuencia, 4 var cuantitativas
summary(solTrainY)  #VARIABLE TRANSF. LOGARITMICAMENTE
hist(solTrainY,br=30,col="lightgrey",
     main="Solubilidad (log)")

#VARIABLE ORIGINAL:
hist(exp(solTrainY),br=30,col="lightgrey",main="Solubilidad")

#Nube de puntos X-Y de las cuantitativas
for (i in c(209,226:228))  #Las cuantitativas
{
   plot(solTrainXtrans[,i],solTrainY,col="blue",
        xlab =colnames(solTrainXtrans)[i],
       ylab="Solubility (log)")
   legend("bottomright",
          legend=paste("r=",round(cor(solTrainXtrans[,i],solTrainY),2)))
 grid()
}

#COMO rpart USA FORMULAS, EN UN SOLO DATAFRAME SE 
#VAN A GUARDAR LAS PREDICTORAS Y LA RESPUESTA
trainData <- solTrainXtrans
trainData$y <- solTrainY

#CONSTRUIR EL ARBOL INCLUYENDO PRUNE
solub.rpart <- rpart(y ~ ., data = trainData,cp=0.0001)
solub.rpart

plot(solub.rpart,main="CART datos solubility. CP=0.001",
     uniform=TRUE)
text(solub.rpart,col="blue",cex=0.6)

printcp(solub.rpart,digits=3)
plotcp(solub.rpart,lty=2,upper="splits",col="blue")

library(DMwR)
solub.rpart2<-rt.prune(solub.rpart)
solub.rpart2

plot(solub.rpart2,main="CART recortado",uniform=TRUE,compress=TRUE)
text(solub.rpart2,col="blue",cex=0.6)
rpart.plot(solub.rpart2,fallen.leaves=FALSE,cex=0.5)

#IMPORTANCIA DE LAS VARIABLES
cbind(solub.rpart2$variable.importance)  

#CALIDAD DE LAS PREDICCIONES EN LA ESCALA ORIGINAL
preditest=exp(predict(solub.rpart2,solTestXtrans))
Ytest=exp(solTestY)
predient=exp(predict(solub.rpart2))
Yent=exp(solTrainY)

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

################
#RF
################
library(randomForest)
solub.rf <- randomForest(y ~ ., data = trainData, 
                         importance=TRUE, 
                         do.trace=TRUE,ntree=500)
solub.rf

plot(solub.rf) 
varImpPlot(solub.rf)

importancias=importance(solub.rf)
importancias[order(-importancias[,1]),]

preditestRF=exp(predict(solub.rf,solTestXtrans))
Ytest=exp(solTestY)
predientRF=exp(predict(solub.rpart2))
Ajuste(Ytest,preditestRF,"RF, Test")
Ajuste(Yent,predientRF,"RF, Entrenamiento")

#DETERMINACION DEL NUMERO DE VARIABLES A CONSIDERAR EN LA
#DIVISION DE CADA NODO:

#mtry, usar tuneRF
#por defecto nx/3=76
solub.tuneRF=tuneRF(x=trainData[,-229],y=trainData[,229])
#por defecto, stepfactor=2, prueba 76/2, 76, 76*2
solub.tuneRF

#Para probar otros valores, cambiar stepFactor
solub.tuneRF=tuneRF(x=trainData[,-229],y=trainData[,229],
                    stepFactor=1.1)
solub.tuneRF

#Con el valor de mtry se construye el modelo RF y 
#se estudia su rendimiento