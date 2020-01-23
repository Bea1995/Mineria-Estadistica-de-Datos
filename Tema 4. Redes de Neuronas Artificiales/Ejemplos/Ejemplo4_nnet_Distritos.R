###################################################
#EJEMPLO 4. USO DE nnet, REGRESION                #
###################################################

#I) LEER LOS DATOS, DIVIDIR EN ENTRENAMIENTO-TEST
#################################################
#503 distritos muncipales de EEUU
datos=read.table("Distritos.txt",header=TRUE)
dim(datos)
summary(datos)
head(datos)
#crim: tasa criminalidad per cápita
#resid:Porcentaje zonas residenciales
#indus: %zonas industriales
#jr: 1 si el distrito es limítrofe con el río, 0 en otro caso
#nox: concentración óxido de nitrógeno
#mvh: número medio habitantes/vivienda
#antig: % viviendas anteriores a 1940
#dis: distancia media ponderada a los 5 centros de trabajo principales
#circunv: índice accesibilidad vías de circunvalación
#tax: indicador impuestos administraciÃ³n
#alprof: profesores/alumno
#ipn: indicador población negra
#porcb: % población clase baja
#medv: mediana del valor de las viviendas (VARIABLE DEPENDIENTE)

n=nrow(datos)
indices<- 1:n
inditest<- sample(indices,trunc(n*0.25)+1)
indient<-setdiff(indices,inditest)

#II) MODELO KNN EN REGRESION, USAREMOS EL PAQUETE caret
#YA QUE knn DE class ES SOLO PARA CLASIFICACION.
#########################################################
#install.packages("caret")
library(caret)
help("train")

entrenam_knn=train(medv~.,data=datos,subset=indient,
                   method="knn",
                   preProcess = c("center", "scale"),
                   tuneLength = 10,
                   trControl = trainControl(method = "cv"))


entrenam_knn
predKNN_test <- predict(entrenam_knn, newdata = datos[inditest,])
str(predKNN_test)

Ajuste<- function(y,pred,titulo)
{
  residuos=y-pred
  plot(y,pred,main=titulo,ylab=expression(hat(y)))
  abline(a=0,b=1,col="blue",lwd=2)
  grid()
  MSE= mean(residuos^2)
  RMSE= sqrt(MSE)
  R2= cor(y,pred)^2
  return(list(MSE=MSE,RMSE=RMSE,R2=R2))
}
Ajuste(datos[inditest,14],predKNN_test,"Test, KNN")

#III) MODELO nnet EN REGRESION 
#################################
x=datos[,-14]
y=datos$medv
zent=scale(x[indient,])
ztest=scale(x[inditest,],attr(zent,"scaled:center"),
            attr(zent,"scaled:scale"))
summary(zent)
summary(ztest)

redtune<- tune(nnet,zent,y[indient],
               ranges=list(size=5:15,decay=c(0,0.05,0.1)),
               maxit=100,linout=TRUE)
summary(redtune)
plot(redtune)
redselvc<- redtune$best.model
redselvc

predPM_ent=predict(redselvc)
predPM_test=predict(redselvc,newdata=ztest)

Ajuste(datos$medv[indient],predPM_ent,
       "Entrenamiento, Perceptrón Multicapas")

Ajuste(datos$medv[inditest],predPM_test,
       "Test, Perceptrón Multicapas")
Ajuste(datos[inditest,14],predKNN_test,
       "Test, KNN")


