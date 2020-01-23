#Practica SVM

#Paquetes
library(readxl)

#Lectura de datos
datos<-read_excel("molusco.xlsx")
summary(datos)
head(datos)
plot(datos$Longitud,datos$Diametro)

#Regresion lineal
model1<-lm(Diametro~Longitud,datos)
abline(model1)

datos$Y1<-predict(model1,datos)
points(datos$Longitud,datos$Y1,col="blue")

recm<-function(error)(mean(error^2))
error<-model1$residuals
recmprediccion<-recm(error)

#Regresion con SVM
library(e1071) #En SVM, solo vale para hacer regresiones
model2<-svm(Diametro~Longitud,datos)
datos$Y2<-predict(model2,datos)
points(datos$Longitud,datos$Y2,col="red", pch=4)

error2<-datos$Diametro-datos$Y2
svmrecm<-recm(error2)

#Regresion con svm y penalizaciones
tunemodel<-tune(svm,Diametro~Longitud,data=datos,ranges=list(epsilon=seq(0,1,0.1),cost=2^(2:9)))
print(tunemodel)
plot(tunemodel)

tuneresult<-tunemodel$best.model
datos$Y3<-predict(tuneresult,datos)
error3<-datos$Diametro-datos$Y3
recmtunemodel<-recm(error3)

plot(datos$Longitud,datos$Diametro)
points(datos$Longitud,datos$Y1,col="blue")
points(datos$Longitud,datos$Y2,col="red", pch=4)
points(datos$Longitud,datos$Y3,col="green",pch=5)

#Resultados
data.frame("MSE lineal" = recmprediccion, "MSE SVM" = svmrecm, "MSE Tune-SVM" = recmtunemodel)
