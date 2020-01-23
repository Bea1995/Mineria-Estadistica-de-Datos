###################################################
#EJEMPLO 2. USO DE nnet Y caret                   #
###################################################

#I) LECTURA DE LOS DATOS, DIVISION ENT./TEST
datos=read.table("Desplazamientos.txt",header=TRUE)
summary(datos)
str(datos)
dim(datos)
head(datos)

n<- nrow(datos)
indin<- 1:n
nent<-ceiling(0.7*n)
ntest<- n-nent
set.seed(13579)
indient<- sort(sample(indin,nent))
inditest<- setdiff(indin,indient)

#II) MODELO LDA (analisis del discriminante lineal)
library(MASS)
modeloLDA<- lda(tipo ~ ., data = datos[indient,])
modeloLDA
#A menor coste relativo, más probable elegir el autobús
#A mayor coste relativo, más probable elegir el automóvil
#Las personas con ingresos altos tienden a usar el transporte público

boxplot(predict(modeloLDA)$x~datos[indient,4])
preditest<- predict(modeloLDA,datos[inditest,])$class
confutest<-table(datos[inditest,4],preditest)
confutest

#Una función para calcular los 3 % acierto 
Calc_Aciertos=function(tabla)
{
  Acierto=100*(tabla[1,1]+tabla[2,2])/sum(tabla)
  nombres=rownames(tabla)
  Acierto1=100*tabla[1,1]/sum(tabla[1,])
  Acierto2=100*tabla[2,2]/sum(tabla[2,])
  resul=c(Acierto=Acierto,Acierto1,Acierto2)
  names(resul)[2:3]=nombres
  resul
}

Aciertos=Calc_Aciertos(confutest)
cat(" Tasa de acierto test= \t",Aciertos[1],"\n",
    "Acierto Auto test= \t",Aciertos[2],"\n",
    "Acierto Bus test= \t",Aciertos[3],"\n")

Matriz_res=Aciertos  #Para ir guardando los resultados

library(ROCR)
#Prob. Bus
probabi<- predict(modeloLDA,datos[inditest,])$posterior[,2] 
prediobj<-prediction(probabi,datos[inditest,4])
plot(performance(prediobj, "tpr","fpr"),
     main="COR TEST. LDA, Desplazamientos",
     xlab="Tasa de falsos positivos", 
     ylab="Tasa de verdaderos positivos")
lines(c(0,1:100)/100,c(0,1:100)/100,col="blue",lty=2)
aucLDA<- as.numeric(performance(prediobj,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(aucLDA,3)))
cat("AUC LDA test= ",aucLDA ,"\n")

Matriz_res=c(Aciertos,auc=aucLDA)  #Para ir guardando los resultados

#III) MODELO NAIVE-BAYES

library(e1071)
modeloNB<- naiveBayes(tipo ~ ., data = datos[indient,])
modeloNB
preditest<- predict(modeloNB,datos[inditest,-4])
confutest<-table(datos[inditest,4],preditest)
confutest
Aciertos=Calc_Aciertos(confutest)
cat(" Tasa de acierto test= \t",Aciertos[1],"\n",
    "Acierto Auto test= \t",Aciertos[2],"\n",
    "Acierto Bus test= \t",Aciertos[3],"\n")


library(ROCR)
probabi<- predict(modeloNB,datos[inditest,-4],
                  type="raw")[,2] #Prob. Bus
prediobj<-prediction(probabi,datos[inditest,4])
plot(performance(prediobj, "tpr","fpr"),
     main="COR TEST. Naive Bayes, Desplazamientos",
     xlab="Tasa de falsos positivos", 
     ylab="Tasa de verdaderos positivos")
lines(c(0,1:100)/100,c(0,1:100)/100,col="blue",lty=2)
aucNB<- as.numeric(performance(prediobj,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(aucNB,3)))
cat("AUC Naive Bayes test= ",aucNB ,"\n")
Matriz_res=rbind(Matriz_res,c(Aciertos,aucNB))

#IV) MODELO nnet
library(caret)
ctrl <- trainControl(method="cv",classProbs=TRUE,
                     summaryFunction = twoClassSummary) 
modeloPM <- train(tipo ~ ., data = datos[indient,], 
                  method = "nnet", 
                  trControl = ctrl, 
                  preProcess = c("center","scale"), 
                tuneGrid = expand.grid(size=1:10,decay=c(0,0.05,0.1)) )
summary(modeloPM)
modeloPM
preditestPM<- predict(modeloPM,datos[inditest,-4])
confutest<-table(datos[inditest,4],preditestPM)
confutest
Aciertos=Calc_Aciertos(confutest)
cat(" Tasa de acierto test= \t",Aciertos[1],"\n",
    "Acierto Auto test= \t",Aciertos[2],"\n",
    "Acierto Bus test= \t",Aciertos[3],"\n")

probabiPM<- predict(modeloPM,newdata = datos[inditest,-4] , 
                     type="prob")[,2] #Prob. bus
prediobj<-prediction(probabiPM,datos[inditest,4])
plot(performance(prediobj, "tpr","fpr"),
     main="COR TEST. PM, Desplazamientos",
     xlab="Tasa de falsos positivos", 
     ylab="Tasa de verdaderos positivos")
lines(c(0,1:100)/100,c(0,1:100)/100,col="blue",lty=2)
aucPM<- as.numeric(performance(prediobj,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(aucPM,3)))
cat("AUC test= ",aucPM ,"\n")
Matriz_res=rbind(Matriz_res,c(Aciertos,aucPM))

#Resumen de resultados
Matriz_res
rownames(Matriz_res)=c("LDA","NB","PM")
Matriz_res
plot(Matriz_res[,c(1,4)],type="n",main="Resultados")
text(Matriz_res[,c(1,4)],labels=rownames(Matriz_res),
     col="blue")
grid()
