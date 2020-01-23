#######################################
#MÁSTER UNIVERSITARIO EN MATEMÁTICAS  #
#MINERÍA ESTADÍSTICA DE DATOS         #
#RANDOM FORESTS                       #
#DATOS spam DE LA LIBRERIA kernlab    #
#######################################
#install.packages("kernlab")  
#install.packages("ROCR")
#install.packages("randomForest")

library(kernlab)
library(randomForest)

data(spam)
dim(spam)
help(spam)
summary(spam)

set.seed(12345)

n<- nrow(spam)
nent<- ceiling(0.7*n)
indin<- 1:n
indient<- sort(sample(indin,nent))

#cONSTRUCCIÓN DEL BOSQUE ALEATORIO
RF<- randomForest(type ~ ., data=spam, 
                  subset=indient,
                  importance=TRUE,do.trace=TRUE)
RF

plot(RF)
legend("topright",col=1:3,lty=1:3,
       legend=c("OOB",levels(spam$type)))
grid()

varImpPlot(RF)
importancias=importance(RF)
head(importancias)

round(cbind(importancias[order(-importancias[,3]),3]),2)

predictest<- predict(RF,newdata=spam[-indient,], 
                     type="response")
ct<-table(spam[-indient,"type"],predictest)
ct
# ACIERTO POR GRUPOS
100*diag(prop.table(ct, 1))
# ACIERTO TOTAL
100*sum(diag(prop.table(ct)))

#Curva COR
library(ROCR)
probabi<- predict(RF,newdata=spam[-indient,],
                  type="prob")[,2] #Prob.spam
prediobj<-prediction(probabi,spam[-indient,"type"])
plot(performance(prediobj, "tpr","fpr"),
     main="CURVA COR TEST, RF")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc ,"\n")
