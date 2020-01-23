###########################################
#MÁSTER UNIVERSITARIO EN MATEMÁTICAS      #
#MINERÍA ESTADÍSTICA DE DATOS             #
#RANDOM FORESTS                           #
#DATOS SOBRE FRAGMENTOS DE EMISION DE TV  #
#(Ver el fichero UCIML_Descrip.pdf)       #
###########################################
#CÓMO LEER LOS DATOS ORIGINALES, TAL Y COMO ESTÁN EN UCI MACHINE LEARNING REPOSITORY
#install.packages("e1071")
#install.packages("SparseM")
library(e1071)

#LA SIGUIENTE FUNCION, DE e1071, LEE UNA MATRIZ DE DATOS EN FORMATO SPARSE
datos<- read.matrix.csr("CNN.txt")
str(datos) 

#GENERA OBJETO PROPIO DE SparseM #VER PAG. 3 DE SparseM.pdf
library(SparseM)

#PASARLOS DE FORMATO MATRICIAL SPARSE A MATRIZ
A<- as.matrix(datos$x)
listavx<- c(1:17,18:123,4124,4125) 

#PREDICTORAS A USAR
datosXY<- data.frame(datos$y,A[,listavx])

#GUARDARLOS EN FORMATO R
save(datosXY,file="CNN.RData")

##########################################
#DESDE AQUÍ SE LEEN LOS DATOS EN FORMATO R
##########################################
load(file="CNN.RData")
colnames(datosXY)[1]="Y"
table(datosXY$Y)  #+1: Anuncio; -1: No Anuncio
datosXY$Y=factor(datosXY$Y)
levels(datosXY$Y)=c("P","A") #(P)ROGRAMA, (A)NUNCIO
table(datosXY$Y)

#######################################################
# HAY VARIABLES QUE SON CONSTANTES, VAMOS A ELIMINARLAS
#######################################################
recorrido=apply(datosXY[,-1],2,range)
rangos=diff(recorrido)
posic= which(rangos==0) +1  

#LA PRIMERA COLUMNA ES LA Y
posic
datos<-datosXY[,-posic]
dim(datos)

###############################
# PARTICION ENTRENAMIENTO/TEST
###############################
n<- nrow(datos)
indin<- 1:n
nent<-ceiling(0.7*n)
ntest<- n-nent

set.seed(13579)

indient<- sort(sample(indin,nent))
inditest<- setdiff(indin,indient)
datosent<- datos[indient,]
datostest<- datos[inditest,]
dim(datosent)
dim(datostest)

##############################
# CART
##############################
library(rpart)
library(rpart.plot)

anuncios.rpart<- rpart(Y~ ., data=datosent,  
                       method="class",cp=0.001)
plot(anuncios.rpart,main="CNN. CP=0.001",uniform=TRUE,
     compress=TRUE)
text(anuncios.rpart,col="blue",cex=0.5)

printcp(anuncios.rpart,digits=3)
plotcp(anuncios.rpart,lty=2,upper="splits",col="blue")

#Tabla con las estimaciones VC
cptabla<- anuncios.rpart$cptable

#Regla 1-ES
#Se puede hacer con rt.prune de la librería DMwR
CP1ES<- min(cptabla[,4])+cptabla[which.min(cptabla[,4]),5] 
CP1ES
cprecorte<- cptabla[cptabla[,4]<CP1ES,][1,1]
cprecorte

#Recorte
anuncios.rpart2<-prune.rpart(anuncios.rpart,cp=cprecorte)  
anuncios.rpart2
#summary(anuncios.rpart2)

cbind(anuncios.rpart2$variable.importance)
plot(anuncios.rpart2,main="CNN, Arbol recortado",
     uniform=TRUE,compress=TRUE)
text(anuncios.rpart2,col="blue",cex=0.5)
rpart.plot(anuncios.rpart2,
           fallen.leaves=FALSE,cex=0.5)

ct<-table(datostest$Y, 
          predict(anuncios.rpart2,datostest,type="class"))
ct

# Porcentaje correcto por grupos
100*diag(prop.table(ct, 1))
# total porcentaje correcto
100*sum(diag(prop.table(ct)))

#AUC
library(ROCR)
probabiCART<- predict(anuncios.rpart, 
                      datostest,"prob")[,1]
prediobj<-prediction(probabiCART,datostest$Y)
plot(performance(prediobj, "tpr","fpr"),
     main="CURVA COR TEST. CART")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc ,"\n")

################################################
# RANDOM FOREST
################################################
library(randomForest)

t1=proc.time()
anuncios.rf <- randomForest(Y ~ ., data=datosent, 
                          importance=TRUE, 
                          do.trace=TRUE,ntree=100)  
#Mejor 500, pero tarda

(tCPURF=proc.time()-t1)
print(anuncios.rf) 

plot(anuncios.rf) 
legend("topright",col=1:3,lty=1:3,
       legend=c("OOB",levels(datosent$Y)))

importancias=importance(anuncios.rf)
round(cbind(importancias[order(-importancias[,3]),3]),2)
datostest.pred <- predict(anuncios.rf, datostest) 

ct<- table(observado = datostest$Y, prediccion = datostest.pred)
ct

# Porcentaje correcto por grupos
100*diag(prop.table(ct, 1))
# total porcentaje correcto
100*sum(diag(prop.table(ct))) 

#COR
probabiRF<- predict(anuncios.rf, datostest,"prob")[,1]
library(ROCR)
prediobj<-prediction(probabiRF,datostest$Y)
plot(performance(prediobj, "tpr","fpr"),
     main="CURVA COR TEST. RF")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc ,"\n")

###################################
# ¿EXISTE DIFERENCIA SIGNIFICATIVA 
# ENTRE LOS AUC DE AMBOS MODELOS?
###################################
#install.packages("pROC")
library(pROC)

roc_CART<- roc(datostest$Y,probabiCART)
roc_RF<- roc(datostest$Y,probabiRF)
roc.test(roc_CART,roc_RF,lwd=2)

plot(roc_CART,col="red",lwd=2,main="COR: CART y RF")
plot(roc_RF,add=TRUE,lty=2,col="blue")
legend("bottomright",col=c("red","blue"),lty=1:2,lwd=2,
        legend=c(paste("CART:",round(roc_CART$auc,3)),
                 paste("RF:     ",round(roc_RF$auc,3))))

##################################################
#CONSTRUCCION DE RF CON PROCESAMIENTO EN PARALELO
##################################################
#install.packages("doParallel")
#install.packages("foreach")
require(doParallel)
require(foreach)
kCPU = 4

#makeCluster, de la librería parallel, crea un cluster de kCPU núcleos
#registerDoParallel construye la infraestructura de cálculo en paralelo 
#para que foreach la use
cl <- makeCluster(kCPU)
registerDoParallel(cl, cores = kCPU)

t1=proc.time()
anuncios.rfPar <- foreach(i = 1:kCPU, .combine = combine, .multicombine = TRUE, 
              .packages='randomForest') %dopar% {
                randomForest(Y ~ ., data=datosent, ntree = 25) 
                #TOTAL 100 ÁRBOLES
              }
(tCPURF_Paral=proc.time()-t1)
probabiRFPar<- predict(anuncios.rfPar, datostest,"prob")[,1]
(roc_RFPar<- roc(datostest$Y,probabiRFPar))
