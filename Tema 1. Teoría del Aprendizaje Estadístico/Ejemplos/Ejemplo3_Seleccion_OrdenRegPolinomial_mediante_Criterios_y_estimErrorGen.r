##################################################
#MASTER UNIVERSITARIO EN MATEMATICAS             #
#Universidad de Sevilla                          # 
#MINERIA ESTADISTICA DE DATOS                    #
#Rafael Pino Mejías                              #
#APRENDIZAJE ESTADISTICO                         #
#ILUSTRACION DE LA SELECCION DEL ORDEN DE UN     #
#MODELO DE REGRESION POLINOMIAL MEDIANTE LOS     #
#CRITERIOS DE INFORMACION Y ESTIMACION DEL       #
#ERROR DE GENERALIZACION                         #
##################################################


#I) Leer los datos
#############################################

#Leer "datos.txt", tiene cabecera, la primera
#columna son nombres de casos
datos=read.table(file="datos.txt",header=TRUE,row.names = 1)
dim(datos)
head(datos)
str(datos)
attach(datos)

plot(x,y,lwd=2,main="Conjunto de entrenamiento",
     col="blue",xlab="x",ylab="y")
grid()


#II) Construir los modelos para distintos valores de p
######################################################
#Función de ajuste, incluyendo el cálculo de los criterios 
#de información
#las funciones AIC y BIC de R, aplicadas a un modelo extraen 
#los criterios de información pero no utilizan la misma expresión, 
#usan una corrección
#también se dispone de extractAIC, pero no incluye en el 
#número de parámetros
#la varianza del error

ajustepolcrit= function(y,x,p)
{
  formula="y~1"
  if (p>0) formula2=paste("+I(","x^",1:p,")",sep="",collapse="") else
    formula2=""
  formula=paste0(formula,formula2,collapse="") #Equivale a paste(...sep="")
  formula=as.formula(formula)
  datos=data.frame(y,x)
  modelo=lm(formula,data=datos) 
  RSS= sum(residuals(modelo)^2) #Suma de cuadrados de errores
  n= nobs(modelo)
  M= p+2 #Incluye el término independiente y la varianza del error
  #RMSE: Root Mean Squared Error (RECM)
  return(list(modelo=modelo, predic=predict(modelo),
              RMSE=sqrt(mean(residuals(modelo)^2)), 
              AIC=n*log(RSS/n)+2*M,
              AICc= n*log(RSS/n)+2*M+2*(M*(M+1))/(n-M-1),
              BIC=n*log(RSS/n)+log(n)*M ))
}


p=9
criterios= matrix(NA,p+1,3)
colnames(criterios)= c("AIC","AICc","BIC")
rownames(criterios)= 0:p

#Para cada valor en 0:p ajustar el modelo
#y guardar los criterios 

for (i in 0:p)
{
  modelo= ajustepolcrit(y,x,i)
  criterios[i+1,1]= modelo$AIC
  criterios[i+1,2]= modelo$AICc
  criterios[i+1,3]= modelo$BIC
}
criterios

matplot(criterios,main="Criterios de información",type="l",
        xlab="p",col=c("red","blue","darkgreen"),lwd=2,lty=1)
legend("topright",col=c("red","blue","darkgreen"),lty=1,
       legend=c("AIC","AICc","BIC"),lwd=2)
grid()

apply(criterios,2,which.min)-1 #Fila 1: p=0


##III) Estimar el rendimiento esperado del modelo
#################################################

#Al no disponer de conjunto test, se pueden plantear dos opciones:
#A. Validación cruzada
#B. Bootstrap

#A. Validación cruzada
#######################
#install.packages("DAAG")
library(DAAG)
help(cv.lm)
#Necesitamos la fórmula
p=5  #Orden previamente seleccionado
formula2=paste("+I(","x^",1:p,")",sep="",collapse="")
formula=paste0("y~1",formula2,collapse="")
formula=as.formula(formula)
VC=cv.lm(formula,m=10,data=datos,seed=555)
#Para cada división se visualiza:
#Número de la division
#Total y composición del conjunto de observaciones test 
#(fuera de la estimación) 
#Predicciones con el modelo completo y predicciones con
#el modelo que no incluye ese subconjunto
#Residuos con ese modelo CV, y también para este modelo CV
#Suma de cuadrados del error y cuadrado medio del error
#el atributo ms se obtiene sumando todas las SCE (VC) y dividiendo
#por el total de casos
attributes(VC)
MSEVC=attr(VC,"ms"); MSEVC
sum((VC$y-VC$cvpred)^2)/100  #Coincide

#Caso m=n, jackknife
VCJ=cv.lm(formula,m=nrow(datos),data=datos)
MSEJ=attr(VCJ,"ms"); MSEJ

#Error empírico
modelo=lm(formula,datos)
mean(residuals(modelo)^2)

#B. Bootstrap
#############
#Calcular estimaciones bootstrap del MSE generalizado
B<- 2000
predi=predict(modelo)
erroremp<- mean(residuals(modelo)^2)
errorboot<-numeric(B)  #Error en datos para cada modelo bootstrap
errorOOB<- numeric(B) #Error OOB para cada modelo bootstrap
n<- nrow(datos)
indin<- 1:n
for (b in 1:B)
{ if (b %% 25==0) cat("Muestra bootstrap número ",b,"\n")
#Guardar en indiboot
#la composición de la muestra bootstrap
#en indiOOB, las posiciones no incluidas en indiboot
  indiboot= sample(indin,rep=TRUE) 
  indiOOB= setdiff(indin,indiboot)
  modelo.boot=lm(formula,datos[indiboot,])
  errorboot[b]= mean(( datos$y-predict(modelo.boot,datos) )^2)
  prediOOB= predict(modelo.boot,datos[indiOOB,])
  errorOOB[b]= mean((datos$y[indiOOB]-prediOOB)^2)
}
summary(errorboot)
summary(errorOOB)

errorB<- mean(errorboot)
errorOOB<- mean(errorOOB)

error632B<-0.368*erroremp+0.632*errorOOB

matrizL<- matrix(NA,n,n)
for (i in 1:n)
  for (j in 1:n)
  {
    matrizL[i,j]<- (datos$y[i]-predi[j])^2
  }

print(Noinf<- mean(matrizL))

#Cálculo de tsr y w
print(tsr<- (errorOOB-erroremp)/(Noinf-erroremp))
print(w<- 0.632/(1-0.368*tsr))


error632masB<-(1-w)*erroremp+w*errorOOB


cat(" Estimaciones del error:\n",
    "MSE Empírico=       ",erroremp ,"\n",
    "MSE VC-10=          ", MSEVC,"\n",
    "MSE VC-J=           ",  MSEJ,"\n",
    "MSE Bootstrap=      ", errorB,"\n",
    "MSE OOB=            ", errorOOB,"\n",                   
    "MSE 0.632Boot=      ", error632B,"\n", 
    "MSE 0.632+Boot=     ", error632masB,"\n")
    




