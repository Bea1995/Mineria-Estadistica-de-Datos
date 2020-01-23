##################################################
#MASTER UNIVERSITARIO EN MATEMATICAS             #
#Universidad de Sevilla                          # 
#MINERIA ESTADISTICA DE DATOS                    #
#Beatriz Coronado Sanz                           #
#TRABAJO TEMAS 7 y 8                             #
##################################################

#1. Cargamos los datos
library(mlbench)
data("LetterRecognition")
datos=LetterRecognition

#Resumen de los datos
names(datos)
summary(datos)
plot(datos[,1])

#2.Elección aleatoria de dos letras
set.seed(971)

letras=sample(unique(datos[,1]),2)
letras #Las letras escogidas son la I y la F

#Obtenemos los indices de las dos letras
ind=which(datos[,1]%in%letras)
datosSimp=datos[ind,]

#Eliminamos los niveles de letras que no utilizamos
datosSimp$lettr=droplevels(datosSimp$lettr) 

#Partimos los datos en conjunto de entrenamiento y test
n<- nrow(datosSimp)
nent<- ceiling(0.7*n)
ntest<- n-nent
indin<- 1:n
indient<- sort(sample(indin,nent))
inditest<- setdiff(indin,indient)

datosent<-datosSimp[indient,]
datostest<-datosSimp[inditest,]

table(datosent$lettr)
table(datostest$lettr)

#3.Modelos CART y Random Forests

##CART
library(rpart)

#Construimos el árbol con rpart
datos.rpart1 <- rpart(lettr ~ ., data=datosent, method="class")
datos.rpart1

#Pintamos el árbol CART obtenido
plot(datos.rpart1,main="CART datos letter recognition 1",uniform=TRUE)
text(datos.rpart1,col="blue",cex=0.8)

#Vemos los resultados de este árbol sobre el conjunto test
ct<-table(datostest$lettr,predict(datos.rpart1,datostest, type="class"))
ct
#Acierto por grupos
100*diag(prop.table(ct, 1))
#Acierto total
100*sum(diag(prop.table(ct)))

#Curva COR
library(ROCR)
probabiCART<- predict(datos.rpart1,datostest,type="prob")[,2]
prediobj<-prediction(probabiCART,datostest$lettr)
plot(performance(prediobj, "tpr","fpr"),main="CURVA COR TEST Cart 1")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc ,"\n")

#Nuestro interes es reducir el valor de cp (reducir la impureza)
#Observamos el del árbol ya construido
printcp(datos.rpart1,digits=3)   

#Para ello creamos otro árbol que para cuando se alcanza cp=0.001
datos.rpart2 <- rpart(lettr ~ ., data=datosent, method="class",cp=0.001)
datos.rpart2

#Pintamos el árbol
plot(datos.rpart2,main="CART datos letter recognition 2. CP=0.001",uniform=TRUE)
text(datos.rpart2,col="blue",cex=0.8)

#Vemos como se reduce el cp hasta el límite que queríamos
printcp(datos.rpart2,digits=3)
plotcp(datos.rpart2)
plotcp(datos.rpart2,lty=2,upper="splits",col="blue")

#Vemos los resultados de este árbol sobre el conjunto test
ct<-table(datostest$lettr,predict(datos.rpart2,datostest,type="class"))
ct
#Acierto por grupos
100*diag(prop.table(ct, 1))
#Acierto total
100*sum(diag(prop.table(ct)))

#Tabla con las estimaciones VC
cptabla<- datos.rpart2$cptable; cptabla

#Calculamos la Regla 1-ES para saber por donde tenemos que cortar el árbol
CP1ES<- min(cptabla[,4])+cptabla[which.min(cptabla[,4]),5] ; CP1ES
indicp<- 1:nrow(cptabla)
cprecorte<- cptabla[indicp[cptabla[,4]<CP1ES][1],1]; cprecorte

#Calculamos otro CART con el recorte óptimo del árbol anterior
datos.rpart3<-prune.rpart(datos.rpart2,cp=cprecorte) 
datos.rpart3

#Pintamos el árbol
plot(datos.rpart3,main="CART recortado",uniform=TRUE)
text(datos.rpart3,col="blue",cex=0.8)

#Vemos los resultados de este árbol sobre el conjunto test
ct<-table(datostest$lettr,predict(datos.rpart3,datostest,type="class"))
ct
#Acierto por grupos
100*diag(prop.table(ct, 1))
#Acierto total
100*sum(diag(prop.table(ct)))

#Curva COR
probabiCART3<- predict(datos.rpart3,datostest,type="prob")[,2]
prediobj<-prediction(probabiCART3,datostest$lettr)
plot(performance(prediobj, "tpr","fpr"),main="CURVA COR TEST Cart 3")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc ,"\n")

#Generamos distintas reglas de clasificación a partir del objeto rpart
list.rules.rpart <- function(model)
{
  if (!inherits(model, "rpart")) stop("No es una objeto rpart")
  #
  #
  #
  frm     <- model$frame
  names   <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <- model$frame[1,]$n
  #
  # Print each leaf node as a rule.
  #
  numreglas=0
  for (i in 1:nrow(frm))
  {
    if (frm[i,1] == "<leaf>")  #Nodos terminales
    {
      numreglas=numreglas+1
      cat("\n")
      cat(sprintf(" Regla número: %s (nodo %s) ", numreglas,names[i]))
      cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                  ylevels[frm[i,]$yval], frm[i,]$n,
                  round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
      pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
      cat(sprintf("   %s\n", unlist(pth)[-1]), sep="")
    }
  }
}

#Observamos las diferencias entre los árboles obtenidos
list.rules.rpart(datos.rpart1)
plot(datos.rpart1,main="CART 1",uniform=TRUE)
text(datos.rpart1,col="blue",cex=0.8)

list.rules.rpart(datos.rpart2)
plot(datos.rpart2,main="CART 2",uniform=TRUE)
text(datos.rpart2,col="blue",cex=0.8)

list.rules.rpart(datos.rpart3)
plot(datos.rpart3,main="CART 3 recortado",uniform=TRUE)
text(datos.rpart3,col="blue",cex=0.8)

#Podemos ver la importancia de las variables para cada árbol CART
cbind(datos.rpart1$variable.importance)
cbind(datos.rpart2$variable.importance)
cbind(datos.rpart3$variable.importance)

##Random Forests
library(randomForest)

#Creamos el random forest
RF<- randomForest(lettr ~ ., data=datosSimp, subset=indient,
                  importance=TRUE,do.trace=TRUE)
RF

#Dibujamos una gráfica con los resultados
plot(RF)
legend("topright",col=1:3,lty=1:3, legend=c("OOB",levels(datosSimp$lettr)))
grid()

#Veamos el acierto para el conjunto test
ct<-table(datosSimp[-indient,"lettr"],
          predict(RF,newdata=datosSimp[-indient,], type="response"))
ct
#Acierto por grupos
100*diag(prop.table(ct, 1))
#Acierto total
100*sum(diag(prop.table(ct)))

#Veamos la importancia de las variables
varImpPlot(RF)
importancias=importance(RF)
head(importancias)
round(cbind(importancias[order(-importancias[,3]),3]),2)

#Curva COR
library(ROCR)
probabiRF<- predict(RF,newdata=datosSimp[-indient,],type="prob")[,2]
prediobj<-prediction(probabiRF,datostest$lettr)
plot(performance(prediobj, "tpr","fpr"),main="CURVA COR TEST RF")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc ,"\n")

#Veamos la diferencia entre CART 3 y Random Forest
library(pROC)

roc_CART<- roc(datostest$lettr,probabiCART3)
roc_RF<- roc(datostest$lettr,probabiRF)
roc.test(roc_CART,roc_RF,lwd=3)

#Dibujemos las dos curvas en una gráfica
plot(roc_CART,col="red",lwd=2,main="COR: CART y RF")
plot(roc_RF,add=TRUE,lty=2,col="blue")
legend("bottomright",col=c("red","blue"),lty=1:2,lwd=2,
       legend=c(paste("CART:",round(roc_CART$auc,3)),
                paste("RF:     ",round(roc_RF$auc,3))))

#Vamos a construir random forest con procesamiento paralelo
require(doParallel)
require(foreach)
kCPU = 4

#makeCluster crea un cluster de kCPU núcleos
#registerDoParallel construye la infraestructura de cálculo en paralelo 
#para que foreach la use
cl <- makeCluster(kCPU)
registerDoParallel(cl, cores = kCPU)

t1=proc.time()
anuncios.rfPar <- foreach(i = 1:kCPU, .combine = combine, .multicombine = TRUE, 
                          .packages='randomForest') %dopar% {
                            randomForest(lettr ~ ., data=datosent, ntree = 25) 
                            #TOTAL 100 ÁRBOLES
                          }
(tCPURF_Paral=proc.time()-t1)

#Veamos el acierto para el conjunto test
ct<-table(datosSimp[-indient,"lettr"],
          predict(anuncios.rfPar, datostest,type="response"))
ct
#Acierto por grupos
100*diag(prop.table(ct, 1))
#Acierto total
100*sum(diag(prop.table(ct)))

#Curva COR
probabiRFpar<- predict(anuncios.rfPar,newdata=datosSimp[-indient,], type="prob")[,2]
prediobj<-prediction(probabiRFpar,datosSimp[-indient,"lettr"])
plot(performance(prediobj, "tpr","fpr"),main="CURVA COR TEST, RF")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc ,"\n")
