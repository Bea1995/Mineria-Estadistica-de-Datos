#Practica SVM

#Paquetes
library(dplyr)
library(tidyr)
library(corrplot)
library(readxl)

#Lectura de datos
datos<-read_excel("Libro1Datos.xlsx")
summary(datos)
head(datos)
plot(datos,pch=16)

#Regresion lineal
model1<-lm(Y~X,datos)
abline(model1)

datos$Y1<-predict(model1,datos)
points(datos$X,datos$Y1,col="blue")

recm<-function(error)(mean(error^2))
error<-model1$residuals
recmprediccion<-recm(error)

#Regresion con SVM
library(e1071) #En SVM, solo vale para hacer regresiones
model2<-svm(Y~X,datos)
datos$Y2<-predict(model2,datos)
points(datos$X,datos$Y2,col="red", pch=4)

error2<-datos$Y-datos$Y2
svmrecm<-recm(error2)

#Regresion con svm y penalizaciones
tunemodel<-tune(svm,Y~X,data=datos,ranges=list(epsilon=seq(0,1,0.1),cost=2^(2:9)))
print(tunemodel)
plot(tunemodel)

tuneresult<-tunemodel$best.model
datos$Y3<-predict(tuneresult,datos)
error3<-datos$Y-datos$Y3
recmtunemodel<-recm(error3)

#Resultados
library(ggplot2)
datos2 = gather(datos,val,nom,-X)
ggplot(datos2,aes(x=X,y=nom,colour=val),size=2) + 
  geom_point() + geom_line(size=1.3) + geom_line()

data.frame("MSE lineal" = recmprediccion, "MSE SVM" = svmrecm, "MSE Tune-SVM" = recmtunemodel)

#Clasificacion
heart_tidy2 <- read_excel("heart_tidy2.xlsx")
heart_tidy2[["V10"]] <- as.numeric(heart_tidy2[["V10"]]) # Se importa mal de excel y viene como char

library(caret)
set.seed(3040)

intrain<-createDataPartition(y=heart_tidy2$V14,p=0.7,list=FALSE)
test<-heart_tidy2[-intrain,]
train<-heart_tidy2[intrain,]

train[["V14"]]=factor(train[["V14"]])
tcontrol<-trainControl(method="repeatedcv",repeats=10,number=10)
set.seed(3020)

svm_lin<-train(V14~.,train,method="svmLinear",trControl=tcontrol,preProcess=c("center","scale"),tunelength=10)
test_pred<-predict(svm_lin,newdata=test)
table(test$V14,test_pred)
