##############################################################
#MINERIA ESTADISTICA DE DATOS                                #
#ARBOLES DE CLASIFICACION                                    #
#DATOS spam DE LA LIBRERIA kernlab                           #
##############################################################
#install.packages("kernlab")  #para acceder a los datos
#install.packages("ROCR")     #para construir la Curva Operativa Característica
library(kernlab)
library(rpart)
data(spam)
dim(spam)
help(spam)
summary(spam)
set.seed(12345)
n<- nrow(spam)
nent<- ceiling(0.7*n)
ntest<- n-nent
indin<- 1:n
indient<- sort(sample(indin,nent))
inditest<- setdiff(indin,indient)

spament<-spam[indient,]  #Conjuntos de entrenamiento y test
spamtest<-spam[inditest,]

spam.rpart <- rpart(type ~ ., data=spament, method="class")

#Se pueden definir otras probabilidades a priori y una matriz de costes
#de clasificación incorrecta
#spam.rpart <- rpart(type ~ ., data=spament, method="class",
#   parms=list(prior=c(0.8,0.2),split="information",
#   loss=matrix(c(0,5,1,0),2,2)))

plot(spam.rpart,main="CART datos spam",uniform=TRUE)
text(spam.rpart,col="blue",cex=0.7)

predictest<- predict(spam.rpart,spamtest, type="class")
ct<-table(spamtest$type,predictest)
ct
# ACIERTO POR GRUPOS
100*diag(prop.table(ct, 1))
# ACIERTO TOTAL
100*sum(diag(prop.table(ct)))

#Curva COR
library(ROCR)
probabi<- predict(spam.rpart,spamtest,type="prob")[,2] #Prob. yes
prediobj<-prediction(probabi,spamtest$type)
plot(performance(prediobj, "tpr","fpr"),main="CURVA COR TEST")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc ,"\n")

#VALIDACION CRUZADA, REGLA 1-SE
printcp(spam.rpart,digits=3)   #reducir el valor de cp:
spam.rpart2 <- rpart(type ~ ., data=spament, method="class",cp=0.001)

spam.rpart2
plot(spam.rpart2,main="CART datos spam. CP=0.001",uniform=TRUE)
text(spam.rpart2,col="blue",cex=0.6)
printcp(spam.rpart2,digits=3)
plotcp(spam.rpart2)
plotcp(spam.rpart2,lty=2,upper="splits",col="blue")
#Tabla con las estimaciones VC
cptabla<- spam.rpart2$cptable
#Regla 1-ES
CP1ES<- min(cptabla[,4])+cptabla[which.min(cptabla[,4]),5] ; CP1ES
indicp<- 1:nrow(cptabla)
cprecorte<- cptabla[indicp[cptabla[,4]<CP1ES][1],1]; cprecorte

spam.rpart3<-prune.rpart(spam.rpart2,cp=cprecorte) 
spam.rpart3
plot(spam.rpart3,main="CART recortado",uniform=TRUE)
text(spam.rpart3,col="blue",cex=0.6)

#Se puede utilizar también la función rt.prune de 
#la librería DMWR
library(DMwR)
arbolrec=rt.prune(spam.rpart2)
arbolrec
#Comprobación:
printcp(arbolrec)
printcp(spam.rpart3)

#Sobre el conjunto test
ct<-table(spamtest$type,predict(spam.rpart3,spamtest,type="class"))
ct
# Porcentaje correcto por grupos
100*diag(prop.table(ct, 1))
# total porcentaje correcto
100*sum(diag(prop.table(ct)))

#Curva COR
probabi<- predict(spam.rpart3,spamtest,type="prob")[,2] #Prob. yes
prediobj<-prediction(probabi,spamtest$type)
plot(performance(prediobj, "tpr","fpr"),main="CURVA COR TEST")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc ,"\n")


#LA SIGUIENTE FUNCION PERMITE GENERAR LAS DISTINTAS REGLAS DE CLASIFICACION
#A PARTIR DEL OBJETO RESULTANTE DE rpart

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

list.rules.rpart(spam.rpart3)
plot(spam.rpart3,main="CART recortado",uniform=TRUE)
text(spam.rpart3,col="blue",cex=0.6)

list.rules.rpart(spam.rpart)
plot(spam.rpart,main="CART recortado",uniform=TRUE)
text(spam.rpart,col="blue",cex=0.6)


#DIBUJAR EL ARBOL CON CON partykit

install.packages("partykit")
library(partykit)

rpart1 <- as.party(spam.rpart)
plot(rpart1) 
#LA NUMERACION DE LOS NODOS PUEDE DIFERIR CON rpart
#PARA ARBOLES GRANDES, COMO rpart3, NO SE APRECIA BIEN 

#IMPORTANCIA DE LAS VARIABLES
######################################
#Aparece en la salida de summary:
summary(spam.rpart)
#o bien
cbind(spam.rpart$variable.importance)

#"An overall measure of variable importance 
#is the sum of the goodness of split
#measures for each split for which it was 
#the primary variable, 
#plus goodness * (adjusted agreement) for all 
#splits in which it was a surrogate. In the printout 
#these are scaled to sum to 100 and the rounded values 
#are shown, omitting any variable whose proportion is 
#less than 1%.

