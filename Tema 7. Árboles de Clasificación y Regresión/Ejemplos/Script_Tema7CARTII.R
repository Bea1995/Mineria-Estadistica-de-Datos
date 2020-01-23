##############################################################
#MINERIA ESTADISTICA DE DATOS                                #
#ARBOLES DE CLASIFICACION                                    #
#DATOS Vehicle DE LA LIBRERIA mlbench                        #
##############################################################
library(mlbench)
library(rpart)
data(Vehicle, package="mlbench")
nent<- ceiling(0.7*n)
ntest<- n-nent
indin<- 1:n
indient<- sort(sample(indin,nent))
inditest<- setdiff(indin,indient)
Vehic_ent<-Vehicle[indient,]  #Conjuntos de entrenamiento y test
Vehic_test<-Vehicle[inditest,]
Vehicle.rpart<- rpart(Class~ ., data=Vehic_ent,  method="class", cp=0.001)
plot(Vehicle.rpart,main="CART datos Vehicle. CP=0.001")
text(Vehicle.rpart,col="blue",cex=0.6)
printcp(Vehicle.rpart,digits=3)
plotcp(Vehicle.rpart)
plotcp(Vehicle.rpart,lty=2,upper="splits",col="blue")

#Tabla con las estimaciones VC
cptabla<- Vehicle.rpart$cptable
#Regla 1-ES
CP1ES<- min(cptabla[,4])+cptabla[which.min(cptabla[,4]),5] ; CP1ES
indicp<- 1:nrow(cptabla)
cprecorte<- cptabla[indicp[cptabla[,4]<CP1ES][1],1]; cprecorte
#Recorte
Vehicle.rpart2<-prune.rpart(Vehicle.rpart,cp=cprecorte)  
Vehicle.rpart2
plot(Vehicle.rpart2,main="CART sobre Vehicle, Arbol recortado")
text(Vehicle.rpart2,col="blue",cex=0.6)

ct<-table(Vehic_test$Class, predict(Vehicle.rpart2,Vehic_test,type="class"))
ct

# Porcentaje correcto por grupos
100*diag(prop.table(ct, 1))
# total porcentaje correcto
100*sum(diag(prop.table(ct)))
