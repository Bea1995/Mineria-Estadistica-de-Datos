##################################################
#MASTER UNIVERSITARIO EN MATEMATICAS             #
#Universidad de Sevilla                          # 
#MINERIA ESTADISTICA DE DATOS                    #
#Rafael Pino Mejías                              #
#APRENDIZAJE ESTADISTICO                         #
#AJUSTE DE MODELOS DE REGRESION POLINOMIAL       #
#EJEMPLO DE LAS TRANSPARENCIAS                   #
##################################################


#I) Generar los datos de entrenamiento y test
#############################################
#Parámetros:
sigma= 0.01  #desv. típica del error
n= 10   #tamaño del conjunto de entrenamiento
ntest= 100  #tamaño del conjunto test
set.seed(12345789) #Semilla generador números aleatorios

#Conjunto de entrenamiento:
#generar las x, se ordenan para utilizar el
#comando lines a la hora de representar las
#predicciones para los distintos valores de p


#Generar una muestra de tamaño n de 
#la ley U(0,1) y ordenar los valores resultantes
#guardar en xent
#calcular los valores de yent a partir de xent

xent= sort(runif(n)) 
yent= sin(2*pi*xent)+rnorm(n,0,sigma) #generar las y
plot(xent,yent,lwd=2,main="Conjunto de entrenamiento",
     col="blue",xlab="x",ylab="y")
grid()

#Lo mismo para los datos test
#Conjunto test:
xtest= sort(runif(ntest))
ytest=sin(2*pi*xtest)+rnorm(ntest,0,sigma)
plot(xtest,ytest,lwd=2,main="Conjunto test",
     col="red",xlab="x",ylab="y")
grid()


#II) Construir los modelos para distintos valores de p
######################################################

#Una función para el ajuste polinomial hasta el término x^p
#En R, el comando lm(formula,datos) permite ajustar un 
#modelo de regresión lineal. 
#Se necesita una fórmula del tipo
# y~1+I(x)+I(x^2)+...+I(x^p)

ajustepol= function(y,x,p)
{
  formula="y~1"  #Término independiente
  if (p>0) formula2=paste("+I(","x^",1:p,")",sep="",collapse="") else
    formula2=""
  formula=paste0(formula,formula2,collapse="") #Equivale a paste(...sep="")
  formula=as.formula(formula)  #CoversiÃ³n a la clase formula de R
  datos=data.frame(y,x)
  modelo=lm(formula,data=datos) 
  return(list(modelo=modelo,predic=predict(modelo),
              RECM=sqrt(mean(residuals(modelo)^2))) )
}


colores= c("black","red","green","darkgrey")
vp= c(0,1,3,9)

#Sobre los datos de entrenamiento
plot(xent,yent,lwd=2,main="Conjunto de entrenamiento",
     col="blue",xlab="x",ylab="y")
grid()

#Ppara cada elemento de vp, añadir
#con lines la gráfica de las predicciones con el
#modelo Reg.Polin. con el orden y color correspondiente
for (j in 1:length(vp))
  lines(xent,ajustepol(yent,xent,vp[j])$predic,col=colores[j],lwd=2)
legend("topright",col=colores,lty=1,paste("p= ",vp),lwd=2)
grid()


#En los datos test
#Una función para obtener predicciones con otros datos
#Se le da como entrada un modelo de regresión lineal
predipol= function(modelo,x,y)  
{
  datos= data.frame(y,x)
  predic= predict(modelo,datos)
  residuos= y-predic
  return(list(predic=predic,residuos=residuos,
              RECM=sqrt(mean(residuos^2))) )
}


plot(xtest,ytest,lwd=2,main="Conjunto test",
     col="red",xlab="x",ylab="y")
grid()


#Para cada elemento de vp, añadir
#con lines la gráfica de las predicciones en los datos test
#con el #modelo Reg.Polin. con el orden y color correspondiente

for (j in 1:length(vp))
{
   modelo=ajustepol(yent,xent,vp[j])$modelo
   lines(xtest,predipol(modelo,xtest,ytest)$predic,
                       col=colores[j],lwd=2)
}
legend("topright",col=colores,lty=1,paste("p= ",vp),lwd=2)
grid()

##III) En modelos con sobreajuste el valor absoluto 
#       de los coeficientes suele ser alto
###################################################
#Construir una matriz con los coeficientes para p entre 1 y 9
p=9
matrizcoeficientes= matrix(0,nrow=p+1,ncol=p+1)
for (j in 1:p)
  matrizcoeficientes[1:(j+1),j+1]= ajustepol(yent,xent,j)$modelo$coefficients
colnames(matrizcoeficientes)= paste("Modelo",0:9,sep="")
rownames(matrizcoeficientes)= 0:9
round(matrizcoeficientes,1)  

##IV) Calcular RECM en los conjuntos de entrenamiento y test
###########################################################
#Errores de entrenamiento y test
RECM= matrix(0,nrow=p+1,ncol=2)
colnames(RECM)= c("Entrenamiento","Test")
rownames(RECM)= 0:p 


for (j in 0:p)
{ 
  RECM[j+1,1]= ajustepol(yent,xent,j)$RECM
  RECM[j+1,2]= predipol(ajustepol(yent,xent,j)$modelo,xtest,ytest)$RECM
}
round(RECM,3)  
plot(0:p,RECM[,1],type="l",col="blue",lwd=1.5,xlab="p",ylab="RECM",
     main="Errores según p")
lines(0:p,RECM[,2],type="l",main="RECM",col="red",lwd=1.5)
legend("topright",col=c("blue","red"),lty=1,legend=c("Entrenamiento","Test"),
       lwd=1.5)
grid()

