#install.packages("MASS")
library(MASS)

#Número de galaxias observadas en diferentes observaciones
data("galaxies")
datos=galaxies

#EM-GMM

#install.packages("mclust")
library(mclust) #Para una dimensión

EM<-Mclust(datos,G=4) #G es el número de clusters
summary(EM,parameters=TRUE)
plot(EM,what="classification")

#K-Medias

#install.packages("cluster")
library(cluster)

data("agriculture")
agriculture

#diss=disimilaridad
#medoids=vector de centroides inicial
#stand=FALSE si medoids=NULL
#do.swap=si cambia puntos
KM=pam(agriculture,4,diss=FALSE,metric="euclidean",medoids=NULL,stand=FALSE,do.swap=TRUE)
KM
plot(KM)

#DBSCAN

#install.packages("dbscan")
library(dbscan)

#Podemos normalizar los datos
#Si no el radio de la esfera debe ser de lamisma escala de los datos
#eps=radio
#minPts=minimo puntos en la esfera para que sea semilla
DB=dbscan(agriculture,eps=5,minPts=2)
DB