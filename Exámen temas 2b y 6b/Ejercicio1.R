library(cluster)

datos=read.csv("distancias.csv",header=TRUE,dec=".",sep=",")

agnes1=agnes(datos,diss=TRUE,metric="euclidean",stand=TRUE,method="ward") 
summary(agnes1)
plot(agnes1,which.plots=c(2),main="dendograma agnes") #c(2) es para dendograma
