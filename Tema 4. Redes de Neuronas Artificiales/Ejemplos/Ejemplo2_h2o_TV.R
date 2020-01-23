###################################################
#EJEMPLO 2 de Aprendizaje Profundo                #
###################################################


#COMO SE HAN LEIDO LOS DATOS TV
#library(e1071)
#LOS DATOS ESTAN EN FORMATO LIBSVM
#datos<- read.matrix.csr("CNN.txt")
#library(SparseM)
#PASARLOS DE FORMATO MATRICIAL SPARSE A MATRIZ
#A<- as.matrix(datos$x)
#listavx<- c(1:17,18:123,4124,4125)
#datosXY<- data.frame(datos$y,A[,listavx])
#GUARDARLOS EN FORMATO R
#save(datosXY,file="CNN.RData")

load(file="CNN.RData")
str(datosXY)
dim(datosXY)
table(datosXY[,1])  #1: anuncio, -1:no


#DIVISION CONJUNTOS ENTRENAMIENTO/TEST
n<- nrow(datosXY)
nent<- ceiling(0.7*n)
ntest<- n-nent
indin<- 1:n
indient<- sort(sample(indin,nent))
inditest<- setdiff(indin,indient)

datos_ent<- datosXY[indient,]
datos_test<- datosXY[inditest,]
dim(datos_ent)
dim(datos_test)

library(h2o)
#INICIALIZAR SERVICIO h2o. LA OPCIÓN nthreads = -1 
#PERMITE AL PROGRAMA CONFIGURAR EL NÚMERO DE HILOS
#DE PROCESAMIENTO DE ACUERDO AL SISTEMA DISPONIBLE 
localH2O = h2o.init(nthreads = -1)

#CONVERTIR LOS DATA FRAMES EN OBJETOS QUE PUEDEN SER PROCESADOS EN 
#EL ENTORNO PARALELIZADO DE CALCULO
train.hex <- as.h2o(datos_ent)
test.hex<- as.h2o(datos_test)

#CONSTRUIR UN MODELO DEEP-LEARNING
#ES UNA RED 125-200-200-2
modelo_h2o <- h2o.deeplearning(
  x = 2:126, y = 1, 
  training_frame = train.hex,
  validation_frame=test.hex,
  distribution="multinomial",
  activation = 'RectifierWithDropout',
  hidden = c(200, 200),
  hidden_dropout_ratio = c(0.5, 0.5),
  input_dropout_ratio = 0.2,
  epochs = 50,
  l1 = 1e-5,
  l2 = 1e-5,
  rho = 0.99,
  epsilon = 1e-8,
  train_samples_per_iteration = 2000)
summary(modelo_h2o)

#PARA OBTENER LAS PREDICCIONES EN EL CONJUNTO TEST POR NUESTRA CUENTA:
predic_test <- h2o.predict(modelo_h2o, newdata = test.hex)
#PASARLO A FORMATO DE data.frame
pred <- as.data.frame(predic_test)
head(pred)
tail(pred)
#LA TABLA QUE SALE EN EL modelo_h2o
tabla=table(datos_test[,1],pred[,1])
tabla
aciertos=100*diag(prop.table(tabla,1))
acierto=100*sum(diag(tabla))/sum(tabla)
acierto
cbind(tabla,Acierto_test=round(aciertos,1))

probabitest=pred$p1  #Prob. clase 1
library(ROCR)
prediobj<-prediction(probabitest, datos_test[,1])
plot(performance(prediobj, "tpr","fpr"),main="CURVA COR TEST")
lines(c(0,1:100)/100,c(0,1:100)/100,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(auc,3)))
cat("�rea bajo la curva COR Test= ",auc,"\n")

