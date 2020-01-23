###################################################
#EJEMPLO 1 de Aprendizaje Profundo                #
###################################################
#install.packages("h2o")
#install.packages("pixmap") #Para representar graficamente los datos
library(h2o)
library(pixmap)
datos<-data.frame(read.table("digitosOCR.txt",header=TRUE))
dim(datos)
colnames(datos)
summary(datos)  #Cada pixel es una intensidad de gris en [-1,1]
datos[,2:257]<-((-1)*datos[,2:257]+1)/2  #de [-1,1] a [0,1];
#ya que pixmap para una imagen en escala de grises, 
#considera valores en [0,1]
#0= negro, 1=blanco
parantig<-par(mfrow=c(5,5),mar=c(0,0,0,0)+1,bg="blue")
for (i in 1:25)
{matriz<- datos[i,2:257]
 x<-pixmapGrey(matriz,16,16,cellres=1)
 x@grey<-t(x@grey)
 plot(x)
}
par(parantig)  #Restablece las opciones
datos[,1]<- factor(datos[,1])
table(datos[,1])

#COMO LAS VARIABLES TOMAN VALORES EN [0,1] 
#NO HACE FALTA TRANSFORMARLAS

#DIVISION CONJUNTOS ENTRENAMIENTO/TEST
n<- nrow(datos)
nent<- ceiling(0.7*n)
ntest<- n-nent
indin<- 1:n
indient<- sort(sample(indin,nent))
inditest<- setdiff(indin,indient)

datos_ent<- datos[indient,]
datos_test<- datos[inditest,]
dim(datos_ent)
dim(datos_test)


#INICIALIZAR SERVICIO h2o. LA OPCIÓN nthreads = -1 
#PERMITE AL PROGRAMA CONFIGURAR EL NÚMERO DE HILOS
#DE PROCESAMIENTO DE ACUERDO AL SISTEMA DISPONIBLE 
localH2O = h2o.init(nthreads = -1)

#CONVERTIR LOS DATA FRAMES EN OBJETOS QUE PUEDEN SER PROCESADOS EN 
#EL ENTORNO PARALELIZADO DE CALCULO
train.hex <- as.h2o(datos_ent)
test.hex<- as.h2o(datos_test)

#CONSTRUIR UN MODELO DEEP-LEARNING
#ES UNA RED 256-200-200-10
?h2o.deeplearning
modelo_h2o <- h2o.deeplearning(
                x = 2:257, y = 1, 
                training_frame = train.hex,
                validation_frame=test.hex,
                distribution="multinomial",
                activation = 'RectifierWithDropout',
                hidden = c(200, 200),
                hidden_dropout_ratio = c(0.5, 0.5),
                input_dropout_ratio = 0.2,
                epochs = 50,
                l1 = 1e-5,
                l2 = 1e-5,
                rho = 0.99,
                epsilon = 1e-8,
                train_samples_per_iteration = 2000)
summary(modelo_h2o)

#PARA OBTENER LAS PREDICCIONES EN EL CONJUNTO TEST POR NUESTRA CUENTA:
predic_test <- h2o.predict(modelo_h2o, newdata = test.hex)
#PASARLO A FORMATO DE data.frame
pred <- as.data.frame(predic_test)
head(pred)
tail(pred)
#LA TABLA QUE SALE EN EL modelo_h2o
tabla=table(datos_test[,1],pred[,1])
tabla
aciertos=100*diag(prop.table(tabla,1))
acierto=100*sum(diag(tabla))/sum(tabla)
acierto
cbind(tabla,Acierto_test=round(aciertos,1))



#UNA ALTERNATIVA: Usar nnet con menos variables
#Reducir la dimensionalidad mediante ACP
#########################################
acp<- princomp(datos[indient,-1],cor=TRUE)
summary(acp)
varcp<- acp$sdev^2
porcen<-100*varcp/sum(varcp)
cbind(1:50,varcp[1:50],porcen[1:50],acum=cumsum(porcen)[1:50])
m<- 44  # componentes principales con var >1 (83.67% de la varianza total)
puntucp_ent<- acp$scores[,1:m]
puntucp_test<- predict(acp,datos[inditest,-1])[,1:m]
puntucp=rbind(puntucp_ent,puntucp_test)
digito=c(datos[indient,1],datos[inditest,1])
digi_cod=class.ind(digito)
head(digi_cod)
#Una red con nnet, sin configurar el tamaño
red=nnet(puntucp[1:nent,],digi_cod[1:nent,],
         size=15,linout=TRUE)

summary(red)
str(predict(red))
pred_ent=apply(predict(red),1,which.max)-1
pred_test=apply(predict(red,puntucp[-c(1:nent),]),1,which.max)-1
tablaent=table(digito[1:nent],pred_ent)
tablaent
aciertos_ent=100*diag(prop.table(tablaent,1))
acierto_ent=100*sum(diag(tablaent))/sum(tablaent)
acierto_ent
cbind(tablaent,Acierto_ent=round(aciertos_ent,1))

tablatest=table(digito[-c(1:nent)],pred_test)
tablatest
aciertos_test=100*diag(prop.table(tablatest,1))
acierto_test=100*sum(diag(tablatest))/sum(tablatest)
acierto_test
cbind(tablatest,Acierto_test=round(aciertos_test,1))
