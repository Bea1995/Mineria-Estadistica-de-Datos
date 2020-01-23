#Practica SVM

#Paquetes
library(readxl)
library(dbscan)

datos<-read_excel("molusco.xlsx")
names(datos)

datosSimp=cbind(datos[9],datos[5])
head(datosSimp)
summary(datosSimp)
plot(datosSimp)

#Podemos normalizar los datos
#Si no el radio de la esfera debe ser de la misma escala de los datos
#eps=radio
#minPts=minimo puntos en la esfera para que sea semilla
DB=dbscan(datosSimp,eps=0.2,minPts=4)
DB
