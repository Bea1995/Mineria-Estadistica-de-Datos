##################################################
#MASTER UNIVERSITARIO EN MATEMATICAS             #
#Universidad de Sevilla                          # 
#MINERIA ESTADISTICA DE DATOS                    #
#Beatriz Coronado Sanz                           #
#TRABAJO TEMA 4                                  #
##################################################

#install.packages("nnet")
library(nnet)
set.seed(51511)

#1. Lectura de los datos
datosEnt<- read.csv("mnist_train.csv", header=FALSE ,sep=",")
datosTest<- read.csv("mnist_test.csv", header=FALSE ,sep=",")

#Transformamos las variables de 1 a 784 al intervalo [0,1]
datosEnt[,1:784]<-datosEnt[,1:784]/255
datosTest[,1:784]<-datosTest[,1:784]/255

#2. Selección de digitos y conjuntos de datos finales
#Seleccionamos 5 digitos aleatoriamente entre 0 y 9
digitos=sort(sample(0:9,5,replace=F));digitos

#Redefinimos los conjuntos de datos con esos 5 digitos
indient<-which((datosEnt[,785]%in%digitos))
inditest<-which((datosTest[,785]%in%digitos))

conjEnt<-datosEnt[indient,]
conjTest<-datosTest[inditest,]

#Extramos aleatoriamente la muestra de datos de entrenamiento y test
n=3000
ntest=1000

indmasent<-sort(sample(indient,size=n))
indmastest<-sort(sample(inditest,size=ntest))

masEnt<-datosEnt[indmasent,]
masTest<-datosTest[indmastest,]

#3. Representacion de los digitos de entrenamiento y test
#install.packages("pixmap")
library(pixmap)

#Conjunto entrenamiento
parantig<-par(mfrow=c(5,5),mar=c(0,0,0,0)+1,bg="yellow")
for (i in 1:25)
{matriz<- masEnt[i,1:784]
x<-pixmapGrey(matriz,28,28,cellres=1)
x@grey<-t(x@grey)
plot(x,main=sprintf("Ent: %d", i))
}

#Tabla resumen del conjunto de entrenamiento
masEnt[,785]<-factor(masEnt[,785])
table(masEnt[,785])

#Conjunto test
parantig<-par(mfrow=c(5,5),mar=c(0,0,0,0)+1,bg="green")
for (i in 1:25)
{matriz<- masTest[i,1:784]
x<-pixmapGrey(matriz,28,28,cellres=1)
x@grey<-t(x@grey)
plot(x, main=sprintf("Test: %d", i))
}
par(parantig)  #Restablece las opciones

#Tanla resumen del conjunto de test
masTest[,785]<-factor(masTest[,785])
table(masTest[,785])

#4. Descartar las variables (píxeles) constantes
#Descartamos las variables cuyo maximo y minimo sea el mismo (constantes)
maximo<-apply(masEnt[,1:784],2,max)
minimo<-apply(masEnt[,1:784],2,min)
indVar<-which((maximo-minimo!=0))

#Descartamos tanto en el conjunto de entrenamiento como en el de test
xEnt<-masEnt[,indVar]
yEnt<-masEnt[,785]

xTest<-masTest[,indVar]
yTest<-masTest[,785]

#5. Construir y evaluar un modelo de red

#5.1. Calculo del PCA
#Construimos PCA sin la matriz de correlaccion
pca<-princomp(xEnt,cor=F)

#Resumen de PCA
mysummary<- matrix(NA,nrow=length(pca$sdev),ncol=3)
mysummary[,1]<-  pca$sdev^2
mysummary[,2]<- 100*mysummary[,1]/sum(mysummary[,1])
mysummary[,3]<- cumsum(mysummary[,2])
colnames(mysummary)<- c("Eigenvalue","Percentage","Cumulative percent.")
round(mysummary,2)

#Vemos el número de componentes principales cuya varianza es superior a la media
numcp<-max(which((pca$sdev^2>mean(pca$sdev^2))))
numcp #(79 componentes principales con var>1, 88.64% de la varianza total)
if(numcp>60) numcp<-60 #(solo llega hasta 60, 84,75% de la varianza total)
numcp

cp_ent<- pca$scores[,1:numcp] #Valores asociados a los componentes principales

#Aplicamos al conjunto test el pca del conjunto de entrenamiento
cp_test<-predict(pca,xTest)[,1:numcp]
cp<-rbind(cp_ent,cp_test)

#Convertimos los digitos seleccionados en vectores de 5 elementos
#Un 1 indica que pertenece a esa clase y 0 si no
digito=c(yEnt,yTest)
digi_cod=class.ind(digito)
head(digi_cod)

#Red con nnetcon 15 nodos ocultos y parametro de regularizacion 0
red=nnet(cp[1:n,],digi_cod[1:n,],
         size=15, decay=0, linout=TRUE)

#Resumen de la red
summary(red)

#Predecimos para los dos conjuntos
pred_ent=apply(predict(red),1,which.max)-1
pred_test=apply(predict(red,cp[-c(1:n),]),1,which.max)-1

#Tabla de resultados y aciertos en el conjunto de entrenamiento
predAuxEnt=(pred_ent==0)*digitos[1]+(pred_ent==1)*digitos[2]+(pred_ent==2)*digitos[3]+(pred_ent==3)*digitos[4]+(pred_ent==4)*digitos[5]
tablaent=table(masEnt[,785],predAuxEnt)
tablaent
aciertos_ent=100*diag(prop.table(tablaent,1))
acierto_ent=100*sum(diag(tablaent))/sum(tablaent)
acierto_ent
cbind(tablaent,Acierto_ent=round(aciertos_ent,1))

#Tabla de resultados y acierto en el conjunto de test
predAuxTest=(pred_test==0)*digitos[1]+(pred_test==1)*digitos[2]+(pred_test==2)*digitos[3]+(pred_test==3)*digitos[4]+(pred_test==4)*digitos[5]
tablatest=table(masTest[,785],predAuxTest)
tablatest
aciertos_test=100*diag(prop.table(tablatest,1))
acierto_test=100*sum(diag(tablatest))/sum(tablatest)
acierto_test
cbind(tablatest,Acierto_test=round(aciertos_test,1))

#6. Construir y evaluar un modelo de red mediante Aprendizaje Profundo
#install.packages("h2o")
library(h2o)

#Inicializamos el servicio h2o
localH2O = h2o.init(nthreads = -1)

#Convertimos nuestros datos en objetos que procese h2o
train.hex <- as.h2o(masEnt)
test.hex<- as.h2o(masTest)

#Modelo deep-learning: red 624-200-200-10
modelo_h2o <- h2o.deeplearning(
  x = indVar, y = 785, 
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

#Resumen del modelo
summary(modelo_h2o)

#Obtenemos las predicciones en el conjunto test
predic_test <- h2o.predict(modelo_h2o, newdata = test.hex)
pred <- as.data.frame(predic_test)
head(pred)
tail(pred)

#Tabla de resultados y acierto
tabla=table(masTest[,785],pred[,1])
tabla
aciertos=100*diag(prop.table(tabla,1))
acierto=100*sum(diag(tabla))/sum(tabla)
acierto
cbind(tabla,Acierto_test=round(aciertos,1))
