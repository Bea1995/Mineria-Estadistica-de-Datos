##################################################
#MASTER UNIVERSITARIO EN MATEMATICAS             #
#Universidad de Sevilla                          # 
#MINERIA ESTADISTICA DE DATOS                    #
#Rafael Pino Mejías                              #
#APRENDIZAJE ESTADISTICO                         #
#KNN: USO DE LA PARTICION ENT./VAL./TEST         #
##TAMBIEN SE EFECTUA LA PARTICION ENT./TEST      #
##################################################


##I) LECTURA DE DATOS Y PARTICION ENT-VAL-TEST
##############################################

set.seed(13579)
spam<- data.frame(read.table("spam.txt",header=TRUE))
n<- nrow(spam)
summary(spam)

indices<- 1:n

#Seleccionar un 25% de valores de indices para inditest
#otro 25% para indivali, el resto para indient

inditest= sample(indices,ceiling(n*0.25))
indivali= sample(setdiff(indices,inditest),ceiling(n*0.25))
indient= setdiff(indices,union(inditest,indivali))

xent=spam[indient,1:6]; xvali=spam[indivali,1:6]; 
xtest=spam[inditest,1:6]
yent=spam[indient,7]; yvali=spam[indivali,7]; ytest=spam[inditest,7]


#Se recomienda tipificar las variables predictoras, para eliminar
#el efecto que puedan tener los difrentes rangos en las distancias
zent<- scale(xent,center=TRUE,scale=TRUE)
str(zent)
medias<- attr(zent,"scaled:center")
dt<- attr(zent,"scaled:scale")
zvali<- scale(xvali,medias,dt)
ztest<- scale(xtest,medias,dt)
apply(zent,2,mean)
apply(zvali,2,mean)
apply(ztest,2,mean)
apply(zent,2,sd)
apply(zvali,2,sd)
apply(ztest,2,sd)



##II)
######
library(class)  #Se necesita para knn
#Se va a usar el conjunto de validación para elegir K
vK<- seq(1,15,2)  #Candidatos
tasaerrorv<- numeric(length(vK))

#Para cada valor i en vk, aplicar knn
#requiere como argumentos 
##train=matrix X Ent.; test=matriz X test;
#cl=Clase de cada elemento de entrenam; k=i
#Se guarda directamente la clasificación de cada elemento de test
#Parámetro use.all: si es TRUE, todos los casos con distancias que 
#coincidan con las k menores distancias son tenidos en cuenta, por tanto
#es posible que el número efectivo de vecinos sea > k
#si es FALSE, se realiza un selección aleatoria de casos y en tal caso
#se trabaja con exactamente con K vecinos

for (i in 1:length(vK))
{
  modeloi<- knn(train=xent, test=xvali,cl=yent ,
                k = vK[i],use.all=FALSE) 
  tasaerrorv[i]<- mean(modeloi != yvali)
}

plot(vK,tasaerrorv,type="l")
kmin<- vK[which.min(tasaerrorv)]; kmin

##III) Construcción con kmin en el conjunto de entrenamiento
#y evaluación en el conjunto test
############################################################
#Para aumentar el tamaño del conjunto de entrenamiento, se
#pueden unir entrenamiento y validación
xent=rbind(xent,xvali)
yent=c(yent,yvali)

zent<- scale(xent,center=TRUE,scale=TRUE)
str(zent)
medias<- attr(zent,"scaled:center")
dt<- attr(zent,"scaled:scale")
ztest<- scale(xtest,medias,dt)
apply(zent,2,mean)
apply(ztest,2,mean)
apply(zent,2,sd)
apply(ztest,2,sd)

preditest<- knn(train=spam[indient,1:6], test=spam[inditest,1:6],
                cl=spam[indient,7], k = kmin, prob=TRUE,use.all=FALSE)
#prob=TRUE permite guardar en el atributo prob
#la proporción de vecinos que "votan" a favor de 
#la clase vencedora
str(preditest)
preditest   #Son las predicciones test

confutest<-table(spam$yesno[inditest],preditest)
confutest

#Calcular los % acierto, sensitividad y especificidad
Acierto=100*(confutest[1,1]+confutest[2,2])/sum(confutest)
Sensit= 100*confutest[2,2]/sum(confutest[2,])
Especif=100*confutest[1,1]/sum(confutest[1,])
cat(" Acierto test=       ",Acierto,"% \n",
    "Sensitividad test=  ",Sensit,"% \n",
    "Especificidad test= ",Especif,"% \n")

#Otra forma, con prop.table:
100*sum(diag(prop.table(confutest)))
diag(100*prop.table(confutest,1))

#Curva Operativa Característica
#Se necesita una estimación de la probabilidad 
#de pertenecer a la clase que la librería considera
#"positiva", que por defecto es la segunda (ordenadas)
#en este caso "y"
#Si se obtiene AUC<0.5, basta con trabajar con las 
#probabilidades estimadas de la otra clase
#si k=1 y se quieren estimar probabilidades, mejor tomar 
#use.all=TRUE para intentar evitar probabilidades estim.
#que sean 0 ó 1

proportest= attr(preditest,"prob") #Prop. clase asignada
clasif_n=which(preditest=="n")
#en probabitest_y se va a guardar la prob. estimada
#de que el correo sea spam ("y")
probabitest_y= proportest   
probabitest_y[clasif_n]= 1-proportest[clasif_n]
head(data.frame(preditest,probabitest_y))

#Usaremos el paquete ROCR para obtener AUC
#install.packages("ROCR")
library(ROCR) 
#prediction es de esta librería, realiza un 
#procesamiento previo propio del paquete
pred <- prediction( probabitest_y, ytest) 
perf <- performance(pred,"tpr","fpr") 
plot(perf,main="KNN, datos test de spam")
abline(a=0,b=1,col="blue",lty=2)
grid()
auc<- as.numeric(performance(pred,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(auc,3)))
cat("Área bajo la curva COR Test= ",auc,"\n")

#V) PARTICION ENTRENAMIENTO-TEST
###############################
inditest<- sample(indices,trunc(n*0.25)+1)
indient<- setdiff(indices,inditest)

xent=spam[indient,1:6]; xtest=spam[inditest,1:6]
yent=spam[indient,7];   ytest=spam[inditest,7]


#Se recomienda tipificar las variables predictoras, para eliminar
#el efecto que puedan tener los difrentes rangos en las distancias
zent<- scale(xent,center=TRUE,scale=TRUE)
medias<- attr(zent,"scaled:center")
dt<- attr(zent,"scaled:scale")
ztest<- scale(xtest,medias,dt)
apply(zent,2,mean)
apply(ztest,2,mean)
apply(zent,2,sd)
apply(ztest,2,sd)


#Para aplicar validación cruzada, se puede usar
#la librería e1071, función tune
#install.packages("e1071")
library(e1071)  #Para usar tune
knnvc<- tune.knn(zent,  yent,k=seq(1,15,2),use.all=FALSE)
knnvc
knnvc$train.ind  #Las 10 divisiones realizadas
knnvc$performances    #rendimientos en cada división
plot(knnvc$performances[,1:2],type="l")
str(knnvc)
knnvc$best.parameters
#Rendimiento en el conjunto test
preditestVC<- knn(train=xent,cl=yent,prob=TRUE,
                  test=xtest,k=knnvc$best.parameters$k,
                  use.all=FALSE)
confutestVC<-table(ytest,preditestVC)
confutestVC
100*sum(diag(prop.table(confutestVC)))
diag(100*prop.table(confutestVC,1))
#AUC
proportestVC= attr(preditestVC,"prob") #Prop. clase asignada
clasif_n=which(preditestVC=="n")
probabitestVC_y= proportestVC   
probabitestVC_y[clasif_n]= 1-proportestVC[clasif_n]
head(data.frame(preditestVC,probabitestVC_y))

predVC <- prediction( probabitestVC_y, ytest) 
perfVC <- performance(predVC,"tpr","fpr") 
plot(perfVC,main="KNN-VC, datos test de spam")
abline(a=0,b=1,col="blue",lty=2)
grid()
aucVC<- as.numeric(performance(predVC,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(aucVC,3)))
cat("Área bajo la curva COR Test= ",aucVC,"\n")

#VI) NAIVE-BAYES
################
#en la función naiveBayes (de e1071) se utilizan fórmulas
library(e1071)
modeloNB<- naiveBayes(yesno ~ ., data = spam[indient,])
modeloNB      #Medias y d.t. en cada grupo
preditest<- predict(modeloNB,spam[inditest,1:6])
confutest<-table(spam[inditest,7],preditest)
confutest
cat(" Tasa de acierto test= \t",
    100*(confutest[1,1]+confutest[2,2])/ntest,"\n",
    "Sensitividad test= \t",
    100*confutest[2,2]/sum(confutest[2,]),"\n",
    "Especificidad test=  \t",
    100*confutest[1,1]/sum(confutest[1,]) ,"\n")

library(ROCR)
probabi<- predict(modeloNB,spam[inditest,1:6],type="raw")[,2] #Prob. Sí
prediobj<-prediction(probabi,spam[inditest,7])
plot(performance(prediobj, "tpr","fpr"),
     main="COR TEST. Naive Bayes, SPAM",
     xlab="Tasa de falsos positivos", 
     ylab="Tasa de verdaderos positivos")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(auc,3)))
cat("AUC test= ",auc ,"\n")


#Puede ser interesante transformar para obtener
#distribuciones univariantes más acordes con la ley Normal
hist(spam$crl.tot); hist(log(spam$crl.tot))
hist(spam$dollar); hist(log(spam$dollar)) 
summary(log(spam$dollar))  #!!!Hay 0´s

##Para evitar problemas con los 0 se suma una constante
spamtr=spam
spamtr[,-7]=log(spam[,-7]+0.1)  


modeloNB<- naiveBayes(yesno ~ ., data = spamtr[indient,])
modeloNB      #Medias y d.t. en cada grupo
preditest<- predict(modeloNB,spamtr[inditest,1:6])
confutest<-table(spamtr[inditest,7],preditest)
print("Test:")
confutest
cat(" Tasa de acierto test (Tr. Log.)= \t",
    100*(confutest[1,1]+confutest[2,2])/sum(confutest),"\n",
    "Sensitividad test (Tr. Log.)= \t\t",
    100*confutest[2,2]/sum(confutest[2,]),"\n",
    "Especificidad test (Tr. Log.)=  \t",
    100*confutest[1,1]/sum(confutest[1,]) ,"\n")

library(ROCR)
probabi<- predict(modeloNB,spamtr[inditest,1:6],type="raw")[,2] #Prob. Sí
prediobj<-prediction(probabi,spamtr[inditest,7])
plot(performance(prediobj, "tpr","fpr"),
     main="COR TEST. Naive Bayes, SPAM",
     xlab="Tasa de falsos positivos", ylab="Tasa de verdaderos positivos")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
legend("bottomright",legend=paste("AUC=",round(auc,3)))
cat("AUC test= (transf.) ",auc ,"\n")



