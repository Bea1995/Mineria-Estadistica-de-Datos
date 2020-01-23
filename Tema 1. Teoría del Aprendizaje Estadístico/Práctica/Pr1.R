##################################################
#MASTER UNIVERSITARIO EN MATEMATICAS             #
#Universidad de Sevilla                          # 
#MINERIA ESTADISTICA DE DATOS                    #
#Beatriz Coronado Sanz                           #
#TRABAJO TEMA 1                                  #
##################################################

## BreastCancer

#1) Obtención de los datos

#install.packages("mlbench")
library("mlbench")

#Ponemos una semilla para que siempre nos salgan los mismos resultados
set.seed(123456789)

#Leemos los datos de BreastCancer
#Los escribimos en un fichero y lo volvemos a leer para indicar que son datos numéricos
#Omitimos los NA
#Dibujamos los datos y mostramos un resumen de ellos
data("BreastCancer",package="mlbench")
write.table(BreastCancer,"BreastCancer.txt",row.names=F)
BreastCancer.dat <- read.table("BreastCancer.txt", header=T, sep=" ",dec=".")
BreastCancer.dat<-na.omit(BreastCancer.dat)
plot(BreastCancer.dat)
summary(BreastCancer.dat)

#2) Partición en los conjuntos de entrenamiento y test
#Partimos el conjunto de datos en datos para entrenar (70%) y datos para testear (30%)
#Cada conjunto se obtiene de manera pseudoaleatoria
n<- nrow(BreastCancer.dat)
indices<-1:n
inditest= sample(indices,ceiling(n*0.3))
indient= setdiff(indices,inditest)

xent=BreastCancer.dat[indient,2:10]
xtest=BreastCancer.dat[inditest,2:10]

yent=BreastCancer.dat[indient,11]
ytest=BreastCancer.dat[inditest,11]

ntest=length(ytest)

#Centramos los datos del conjunto de entrenamiento para que tengan media nula y varianza 1
#Aplicamos a los datos de test la misma transformación que a los datos de entrenamiento
zent<- scale(xent,center=TRUE,scale=TRUE)
medias<- attr(zent,"scaled:center")
dt<- attr(zent,"scaled:scale")
ztest<- scale(xtest,medias,dt)

apply(zent,2,mean)
apply(ztest,2,mean)
apply(zent,2,sd)
apply(ztest,2,sd)

summary(zent)
summary(ztest)

#3) Construimos el estimador de Naive-Bayes
#install.packages("e1071")
library(e1071)
#Construimos el modelo con los datos de entrenamiento
modeloNB<- naiveBayes(Class ~ ., data = BreastCancer.dat[indient,2:11])
modeloNB      #Contiene las medias y las desviaciones tipicas en cada característica
#Predecimos lo bien que funciona el modelo para los datos de test
preditest<- predict(modeloNB,BreastCancer.dat[inditest,2:10])
#Mostramos la tabla de confusión para los datos de test
confutest<-table(BreastCancer.dat[inditest,11],preditest)
confutest
#Calculamos la tasa de acierto, la sensitividad y la especifidad del test
cat(" Tasa de acierto test= \t",
    100*(confutest[1,1]+confutest[2,2])/ntest,"\n",
    "Sensitividad test= \t",
    100*confutest[2,2]/sum(confutest[2,]),"\n",
    "Especificidad test=  \t",
    100*confutest[1,1]/sum(confutest[1,]) ,"\n")

#Calculamos el AUC sobre las predicciones que hemos realizado para saber el rendimiendo de nuestro modelo
#install.packages("ROCR")
library(ROCR)
probabi<- predict(modeloNB,BreastCancer.dat[inditest,2:10],type="raw")[,2] #Prob. Sí
prediobj<-prediction(probabi,BreastCancer.dat[inditest,11])
plot(performance(prediobj, "tpr","fpr"),
     main="COR TEST. Naive Bayes, BC",
     xlab="Tasa de falsos positivos", 
     ylab="Tasa de verdaderos positivos")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(auc,3)))
cat("AUC test= ",auc ,"\n")

#4) Realizamos Knn para BreastCancer con validacion cruzada
library(class)
library(e1071)
#Usamos tune para realizar knn con distinto valor en el número de vecinos que tenemos en cuenta
knnvc<- tune.knn(zent,yent,k=seq(1,15,2),use.all=FALSE)
knnvc
knnvc$train.ind  #Las 10 divisiones realizadas para validación cruzada
knnvc$performances    #Rendimientos en cada división
knnvc$best.parameters #El número de vecinos óptimo para nuestro problema
plot(knnvc$performances[,1:2],type="l")
str(knnvc)
#Calculamos el rendimiento en el conjunto test
preditestVC<- knn(train=xent,cl=yent,prob=TRUE,
                  test=xtest,k=knnvc$best.parameters$k,
                  use.all=FALSE)
#Mostramos la tabla de confusión, la tasa de acierto, la sensitividad y la especifidad
confutestVC<-table(ytest,preditestVC)
confutestVC
cat(" Tasa de acierto test= \t",
    100*(confutestVC[1,1]+confutestVC[2,2])/sum(confutestVC),"\n",
    "Sensitividad test= \t\t",
    100*confutestVC[2,2]/sum(confutestVC[2,]),"\n",
    "Especificidad test=  \t",
    100*confutestVC[1,1]/sum(confutestVC[1,]) ,"\n")

#Curva Operativa Característica
proportestVC= attr(preditestVC,"prob") #Prop. clase asignada
proportestVC
clasif_n=which(preditestVC=="benign")
probabitestVC_y= proportestVC   
probabitestVC_y[clasif_n]= 1-proportestVC[clasif_n] #Prob. clase asignada
head(data.frame(preditestVC,probabitestVC_y))

#Calculamos el AUC sobre las predicciones que hemos realizado para saber el rendimiendo de nuestro modelo
library(ROCR)
predVC <- prediction( probabitestVC_y, ytest) 
perfVC <- performance(predVC,"tpr","fpr") 
plot(perfVC,main="KNN-VC, datos test de breast cancer")
abline(a=0,b=1,col="blue",lty=2)
grid()
aucVC<- as.numeric(performance(predVC,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(aucVC,3)))
cat("Área bajo la curva COR Test= ",aucVC,"\n")

##Glass

#1) Obtención de los datos
library("mlbench")

#Ponemos una semilla para que siempre nos salgan los mismos resultados
set.seed(123456789)

#Leemos los datos de Glass
#Dibujamos los datos y mostramos un resumen de ellos
data("Glass",package="mlbench")
plot(Glass)
summary(Glass)

#2) Partición en los conjuntos de entrenamiento y test
#Partimos el conjunto de datos en datos para entrenar (70%) y datos para testear (30%)
#Cada conjunto se obtiene de manera pseudoaleatoria
n<- nrow(Glass)
indices<-1:n
inditest= sample(indices,ceiling(n*0.3))
indient= setdiff(indices,inditest)

xent=Glass[indient,1:9]
xtest=Glass[inditest,1:9]

yent=Glass[indient,10]
ytest=Glass[inditest,10]

ntest=length(ytest)

#Centramos los datos del conjunto de entrenamiento para que tengan media nula y varianza 1
#Aplicamos a los datos de test la misma transformación que a los datos de entrenamiento
zent<- scale(xent,center=TRUE,scale=TRUE)
medias<- attr(zent,"scaled:center")
dt<- attr(zent,"scaled:scale")
ztest<- scale(xtest,medias,dt)

apply(zent,2,mean)
apply(ztest,2,mean)
apply(zent,2,sd)
apply(ztest,2,sd)

summary(zent)
summary(ztest)

#3) Construimos el estimador de Naive-Bayes
#install.packages("e1071")
library(e1071)
#Construimos el modelo con los datos de entrenamiento
modeloNB<- naiveBayes(Type ~ ., data = Glass[indient,1:10])
modeloNB      #Medias y desviaciones típicas en cada característica
#Predecimos lo bien que funciona el modelo para los datos de test
preditest<- predict(modeloNB,Glass[inditest,1:9])
#Mostramos la tabla de confusión
confutest<-table(Glass[inditest,10],preditest)
confutest
#Calculamos la tasa de acierto del test
cat(" Tasa de acierto test= \t",
    100*(sum(diag(confutest))/ntest),"\n")
#Calculamos la precision, la sensitividad y la especifidad para cada clase
for(i in 1:length(table(ytest)))
{
  cat(" Precision de la clase",i , "=\t",
      100*(diag(confutest)[i]/sum(confutest[,i])),"\n",
      "Sensitividad de la clase",i , "=\t",
      100*(diag(confutest)[i]/sum(confutest[i,])),"\n",
      "Especificidad de la clase",i , "=\t",
      100*((sum(confutest[,i])-diag(confutest)[i])/(sum(confutest)-sum(confutest[i,]))) ,"\n")
}

#Calculamos el error bootstrap para nuestro modelo
predi<-predict(modeloNB,Glass[,1:9]) #Prediccion de todos los datos con el modelo calculado
confu<-table(Glass[,10],predi) #Tabla de confusion para los datos
B<- 2000
erroremp<- (sum(confu)-sum(diag(confu)))/sum(confu) #Error empírico del modelo
errorboot<-numeric(B)  #Error en datos para cada modelo bootstrap
errorOOB<- numeric(B) #Error OOB para cada modelo bootstrap
indin<- 1:n
for (b in 1:B)
{ if (b %% 25==0) cat("Muestra bootstrap número ",b,"\n")
  #Guardamos en indiboot la composición de la muestra bootstrap 
  #y en indiOOB, las posiciones no incluidas en indiboot
  indiboot= sample(indin,rep=TRUE) 
  indiOOB= setdiff(indin,indiboot)
  #Calculamos el modelo NB para el conjunto indiboot
  modelo.boot=naiveBayes(Type ~ ., data = Glass[indiboot,])
  #Hacemos la prediccion de ese modelo
  prediboot<- predict(modelo.boot,Glass[indiboot,1:9])
  #Tabla de confusion de la prediccion
  confuboot<-table(Glass[indiboot,10],prediboot)
  #Calculo del error bootstrap para indiboot
  errorboot[b]= (sum(confuboot)-sum(diag(confuboot)))/sum(confuboot)
  #Hacemos la prediccion del conjunto de indiOOB para el modelo
  prediOOB= predict(modelo.boot,Glass[indiOOB,1:9])
  #Tabla de confusión de la prediccion
  confuOOB<-table(Glass[indiOOB,10],prediOOB)
  #Calculo del error bootstrap para indiOOB
  errorOOB[b]= (sum(confuOOB)-sum(diag(confuOOB)))/sum(confuOOB)
}
summary(errorboot)
summary(errorOOB)

#Calculamos los distintos errores bootstrap
errorB<- mean(errorboot)
errorOOB<- mean(errorOOB)
error632B<-0.368*erroremp+0.632*errorOOB

Noinf<-0
for(i in 1:length(table(Glass$Type)))
{
  Noinf<-Noinf+table(Glass$Type)[i]/n*(1-table(predi)[i]/n)
}
Noinf

#Cálculo de tsr y w
print(tsr<- (errorOOB-erroremp)/(Noinf-erroremp))
print(w<- 0.632/(1-0.368*tsr))

error632masB<-(1-w)*erroremp+w*errorOOB

#Tabla con todos los errores bootstrap
cat(" Estimaciones del error:\n",
    "MSE Empírico=       ",erroremp ,"\n",
    "MSE Bootstrap=      ", errorB,"\n",
    "MSE OOB=            ", errorOOB,"\n",                   
    "MSE 0.632Boot=      ", error632B,"\n", 
    "MSE 0.632+Boot=     ", error632masB,"\n")

#4) Realizamos Knn para Glass con validacion cruzada
library(class)
library(e1071)
#Usamos tune para realizar knn con distinto valor en el número de vecinos que tenemos en cuenta
knnvc<- tune.knn(zent, yent,k=seq(1,15,2),use.all=FALSE)
knnvc
knnvc$train.ind  #Las 10 divisiones realizadas
knnvc$performances    #Rendimientos en cada división
knnvc$best.parameters #El número de vecinos óptimo para nuestro problema
plot(knnvc$performances[,1:2],type="l")
str(knnvc)

#Calculamos el rendimiento en el conjunto test
preditestVC<- knn(train=xent,cl=yent,prob=TRUE,
                  test=xtest,k=knnvc$best.parameters$k,
                  use.all=FALSE)
#Mostramos la tabla de confusión
confutestVC<-table(ytest,preditestVC)
confutestVC
#Calculamos la tasa de acierto del test
cat(" Tasa de acierto test= \t",
    100*(sum(diag(confutestVC))/ntest),"\n")
#Calculamos la precision, la sensitividad y la especifidad para cada clase
for(i in 1:length(table(ytest)))
{
  cat(" Precision de la clase",i , "=\t",
      100*(diag(confutestVC)[i]/sum(confutestVC[,i])),"\n",
      "Sensitividad de la clase",i , "=\t",
      100*(diag(confutestVC)[i]/sum(confutestVC[i,])),"\n",
      "Especificidad de la clase",i , "=\t",
      100*((sum(confutestVC[,i])-diag(confutestVC)[i])/(sum(confutestVC)-sum(confutestVC[i,]))) ,"\n")
}

#Calculamos el error bootstrap
#Prediccion de todos los datos con el modelo calculado
predi<-knn(train=xent,cl=yent,prob=TRUE,
                  test=Glass[,1:9],k=knnvc$best.parameters$k,
                  use.all=FALSE)
#Tabla de confusión de la prediccion
confu<-table(Glass[,10],predi)
B<- 2000
erroremp<- (sum(confu)-sum(diag(confu)))/sum(confu) #Error empírico
errorboot<-numeric(B)  #Error en datos para cada modelo bootstrap
errorOOB<- numeric(B) #Error OOB para cada modelo bootstrap
indin<- 1:n
for (b in 1:B)
{ if (b %% 25==0) cat("Muestra bootstrap número ",b,"\n")
  #Guardamos en indiboot la composición de la muestra bootstrap 
  #y en indiOOB, las posiciones no incluidas en indiboot
  indiboot= sample(indin,rep=TRUE) 
  indiOOB= setdiff(indin,indiboot)
  #Calculamos el modelo knn para el conjunto indiboot y lo predecimos para este mismo conjunto
  prediboot<- knn(train=Glass[indiboot,1:9],cl=Glass[indiboot,10],prob=TRUE,
                    test=Glass[indiboot,1:9],k=knnvc$best.parameters$k,
                    use.all=FALSE)
  #Tabla de confusión de esta predicción
  confuboot<-table(Glass[indiboot,10],prediboot)
  #Calculo del error bootstrap para indiboot
  errorboot[b]= (sum(confuboot)-sum(diag(confuboot)))/sum(confuboot)
  #Calculamos el modelo knn para el conjunto indiboot y lo predecimos para indiOOB
  prediOOB= knn(train=Glass[indiboot,1:9],cl=Glass[indiboot,10],prob=TRUE,
                test=Glass[indiOOB,1:9],k=knnvc$best.parameters$k,
                use.all=FALSE)
  #Tabla de confusión de esta predicción
  confuOOB<-table(Glass[indiOOB,10],prediOOB)
  #Calculo del error bootstrap para indiOOB
  errorOOB[b]= (sum(confuOOB)-sum(diag(confuOOB)))/sum(confuOOB)
}
summary(errorboot)
summary(errorOOB)

#Calculamos los distintos errores bootstrap
errorB<- mean(errorboot)
errorOOB<- mean(errorOOB)
error632B<-0.368*erroremp+0.632*errorOOB

Noinf<-0
for(i in 1:length(table(Glass$Type)))
{
  Noinf<-Noinf+table(Glass$Type)[i]/n*(1-table(predi)[i]/n)
}
Noinf

#Cálculo de tsr y w
print(tsr<- (errorOOB-erroremp)/(Noinf-erroremp))
print(w<- 0.632/(1-0.368*tsr))

error632masB<-(1-w)*erroremp+w*errorOOB

#Tabla con todos los errores bootstrap
cat(" Estimaciones del error:\n",
    "MSE Empírico=       ",erroremp ,"\n",
    "MSE Bootstrap=      ", errorB,"\n",
    "MSE OOB=            ", errorOOB,"\n",                   
    "MSE 0.632Boot=      ", error632B,"\n", 
    "MSE 0.632+Boot=     ", error632masB,"\n")