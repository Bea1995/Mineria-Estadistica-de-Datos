library(cluster)
data(agriculture)
agriculture

#calcular los clusters a partir de las matrices de distancia
D=dist(agriculture) #distancia euclidea
D2=dist(agriculture,method="minkowski")
print(round(D,2)) #matriz de distancias con dos decimales
print(round(D2,2))

#Simple linkage
agrS=hclust(D,method="single") 
agrS
str(agrS)
plot(agrS) #Dendograma con los resultados

#Complete linkage
agrC=hclust(D,method="complete") 
agrC
str(agrC)
plot(agrC) #Dendograma con los resultados

#Average linkage
agrA=hclust(D,method="average") 
agrA
str(agrA)
plot(agrA) #Dendograma con los resultados

#Centroide ponderado
agrCt=hclust(D,method="centroid") 
agrCt
str(agrCt)
plot(agrCt) #Dendograma con los resultados

#Centroide mediano
agrMed=hclust(D,method="median") 
agrMed
str(agrMed)
plot(agrMed) #Dendograma con los resultados

#Ward
agrW=hclust(D,method="ward.D") 
agrW
str(agrW)
plot(agrW) #Dendograma con los resultados

#Centroide ponderado
agrQ=hclust(D,method="mcquitty") 
agrQ
str(agrQ)
plot(agrQ) #Dendograma con los resultados

#Cortamos el árbol a la altura que queramos (con un número de clusters fijo)
pertenencia=cutree(agrS,k=5) #5 clusters para linkage simple
pertenencia

pertenencia=cutree(agrCt,k=3) #3 clusters para centroide ponderado
pertenencia

#Funcion agnes
#Ventajas: permite trabajar sin crear la matriz de distancias o con ellas
#Da el coeficiente de aglomeracion
?agnes
#agnes(datos,metric=distancia,stand?,metodo)
#Con stand=true tipificamos los datos (estandarizamos los datos)
#El metodo por defecto es average linkage
agnes1=agnes(agriculture,metric="euclidean",stand=TRUE)
summary(agnes1)

agnes2=agnes(agriculture,metric="euclidean",stand=TRUE,method="ward") 
summary(agnes2)
plot(agnes2,which.plots=c(2),main="dendograma agnes") #c(2) es para dendograma
