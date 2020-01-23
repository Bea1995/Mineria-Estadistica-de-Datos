###################################################
#EJEMPLO 3. USO DE nnet,clasificación con 4 clases#
#FALTA REALIZAR tune                              #
###################################################

library(mlbench)
library(e1071)
library(nnet)
data(Vehicle)
?Vehicle
summary(Vehicle)
head(Vehicle)
n<- nrow(Vehicle)
nent<- ceiling(0.75*n)
ntest<- n-nent
indin<- 1:n
set.seed(12345)
indient<- sort(sample(indin,nent))
inditest<- setdiff(indin,indient)
#Transformaciones
medias<- apply(Vehicle[indient,-19],2,mean)
dt<- apply(Vehicle[indient,-19],2,sd)
Vehicle[indient,-19]<- scale(Vehicle[indient,-19],medias,dt)
Vehicle[inditest,-19]<- scale(Vehicle[inditest,-19],medias,dt)
apply(Vehicle[indient,-19],2,mean)
apply(Vehicle[inditest,-19],2,mean)
apply(Vehicle[indient,-19],2,sd)
apply(Vehicle[inditest,-19],2,sd)

#Se utiliza la función error log-mv-condicional
red<-nnet(Class~.,data=Vehicle[indient,],size=10,maxit=10000)
red
summary(red)
#3b
###
predict(red)[1:10,]  #Estim. probab.
predict(red,type="class")[1:10]  #Directamente clases
confuent<-table(Vehicle[indient,19], predict(red, type="class"))     
confutest<-table(Vehicle[inditest,19], predict(red, Vehicle[inditest,],type="class"))
print("Entrenamiento:")
confuent
cat("100*Tasa de acierto empírica= ",100*sum(diag(confuent))/nent,"\n")
print("Test:")
confutest
cat("100*Tasa de acierto test= ",100*sum(diag(confutest))/ntest,"\n")
for (i in 1:4)
  cat("Acierto ",rownames(confutest)[i],":",100*confutest[i,i]/sum(confutest[i,]),
      "% \n")

