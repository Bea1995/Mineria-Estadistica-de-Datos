#########################################
#MINERIA ESTADISTICA DE DATOS           #
#MASTER UNIVERSITARIO EN MATEMATICAS    #
#UN EJEMPLO DE CLASIFICACIÓN CON        #
#   DATOS NO BALANCEADOS                #
#########################################

######################################
#a). LECTURA Y PREPARACION DE LOS DATOS
######################################

load("Insolvencia.RData")
dim(datos)
str(datos)
###############################################
#Variable dependiente: failed_insolvent (16) factor 
#con dos niveles relativos a la insolvencia de 
#empresas, que son "No" y "Yes"
###############################################
names(datos)
summary(datos)
table(datos$failed_insolvent)  #DATOS NO BALANCEADOS
prop.table(table(datos$failed_insolvent))
barplot(table(datos$failed_insolvent))

#Para que la primer clase sea la minoritaria
#y usar "Sens" en los ajustes
datos$failed_insolvent = factor(as.character(datos$failed_insolvent),
           levels = rev(levels(datos$failed_insolvent)))
table(datos$failed_insolvent)

### PARTICION DE LOS DATOS EN LOS SUBCONJUNTOS
### ENTRENAMIENTO, VALIDACION Y TEST
### EL SUBCONJUNTO DE VALIDACION SE VA A UTILIZAR
### PARA CONFIGURAR EL PUNTO DE CORTE Y PARA 
### ELEGIR LOS PARAMETROS DE SVM

library(caret)

set.seed(156)
#60%, 15%, 25%
n=nrow(datos)
indices=1:n
ient=sample(indices,floor(n*0.6))
ival=sample(setdiff(indices,ient),floor(n*0.2))
itest=setdiff(indices,union(ient,ival))

training  = datos[ient,]
validation  = datos[ival,]
testing     = datos[itest,]
training_valid=rbind(training,validation)
#POSICIONES DEL CONJ. ENTREN. EN training_valid:
Index= 1:nrow(training)

dim(training)
dim(validation)
dim(training_valid)
dim(testing)  

#En los siguientes objetos se van a guardar
#las predicciones mediante los distintos métodos
#en los conjuntos de validación y test:
testResults = data.frame(failed_insolvent = testing$failed_insolvent)
validResults = data.frame(failed_insolvent = validation$failed_insolvent)


#########################################
### b) FUNCIONES PARA MEDIR EL RENDIMIENTO 
### Y OPCIONES DE CONTROL
### PARA EL ENTRENAMIENTO MEDIANTE caret
#########################################
#fiveStats devuelve las medidas de 
#twoClassSummary y defaultSummary: 
#Accuracy, Kappa, AUC ROC, Sensitivity y Specificity
fiveStats = function(...) 
  c(twoClassSummary(...), defaultSummary(...))
#Estos 5 indicadores se calcularán y se guardarán
#al ejecutar train

#OPCIONES DE CONTROL A USAR EN EL PAQUETE caret
#train: SE LE DARÁ training_valid
#ctrlval REALIZA VALIDACION CRUZADA CON UNA SOLA
#DIVISION, EN LA CUAL index DEFINE LAS POSICIONES
#DE ENTRENAMIENTO, EL RESTO SON LAS QUE DEFINEN
#EL CONJUNTO DE VALIDACIÓN
#indexFinal: DEFINE LAS POSICIONES DEL CONJUNTO 
#DONDE SE AJUSTA EL MODELO FINAL CON LA CONFIGURACIÓN
#ENCONTRADA PREVIAMENTE
ctrlval = trainControl(method = "cv",number=1,
                       index=list(Index), 
                       indexFinal=Index,
                       classProbs = TRUE,
                       summaryFunction = fiveStats,
                       verboseIter=TRUE)


#############################
### c) AJUSTE DEL MODELO SVM 
#############################
#Con tuneLength = total de valores de mtry
#a explorar 
set.seed(1410)
SVMFit = train(failed_insolvent ~ ., 
               data = training_valid,
              method = "svmRadial", 
              trControl = ctrlval,
              tuneLength=10,
              metric = "Sens") #Sensitividad
#Debido al problema de datos no balanceados,
#buscamos elevar la sensitividad
#También se recomienda en estos casos
#metric="Kappa"
SVMFit
SVMFit$results #Cada medida, con su desv. tip.


#probabilidades estimadas de "Yes"
validResults$SVM = predict(SVMFit, validation, 
                          type = "prob")[,1]
testResults$SVM = predict(SVMFit, testing, 
                         type = "prob")[,1]

#Vamos a calcular las medidas de rendimiento en el conjunto test
library(pROC)
SVMTestROC = roc(testResults$failed_insolvent, testResults$SVM,
                levels = rev(levels(testResults$failed_insolvent)))
SVMTestROC

SVMTestCM = confusionMatrix(predict(SVMFit, testing), 
                           testResults$failed_insolvent)
SVMTestCM
#Sensitividad muy baja!!
plot(SVMTestROC)


################################################
### d) APLICACION DE ESTRATEGIAS PARA EL
### TRATAMIENTO DE LOS DATOS NO BALANCEADOS
###############################################

###############################################
### 1.PUNTOS DE CORTE ALTERNATIVOS
### SE COMPARA LA PROBABILIDAD ESTIMADA
### CON MEJOR UMBRAL EN EL CONJUNTO DE
### VALIDACIÓN
### NECESITAMOS LA CURVA COR EN ESE CONJUNTO
###############################################

SVMvalidROC = roc(validResults$failed_insolvent, validResults$SVM,
                 levels = rev(levels(validResults$failed_insolvent)))
SVMvalidROC

plot(SVMvalidROC, type = "S", legacy.axes = TRUE, 
     xlab="1 - Especificidad",ylab="Sensitividad")
#LA FUNCION coords DETERMINA EL MEJOR UMBRAL
#best.method="closest.topleft" BUSCA EL UMBRAL 
#CON RENDIMIENTO MAS CERCANO A LA ESQUINA 
#SUP. IZQUIERDA 
#(MAYOR SENSITIVIDAD Y ESPECIFICIDAD) 
#PROBLEMA DE OPTIMIZACION: 
#min((1 - sensitivities)^2 + (1- specificities)^2)

#best.method="youden" BUSCA EL UMBRAL CON MAYOR VALOR
#PARA EL INDICE J DE YOUDEN, MAXIMIZA LA DISTANCIA
#A LA DIAGONAL
#PROBLEMA DE OPTIMIZACION: 
#max(sensitivities + specificities)

SVMThresh = coords(SVMvalidROC, x = "best", ret="threshold",
                  best.method="closest.topleft")

SVMThreshY = coords(SVMvalidROC, x = "best", ret="threshold",
                   best.method="youden")

SVMThresh
SVMThreshY #Comprobar si coinciden
#A continuación se aplica el primero,
#similarmente se aplicaría el segundo criterio

#CLASIFICACION CON UMBRALES ALTERNATIVOS
testResults$SVMAlt = factor(ifelse(testResults$SVM > SVMThresh,
                                  "Yes", "No"))

###SI AL CALCULAR LA MATRIZ DE CONFUSION A CONTINUACION
#APARECE EL MENSAJE WARNING INFORMANDO "LEVELS ARE NOT IN 
###THE SAME ORDER.." SE DEBE A QUE EN LOS DATOS LAS CATEGORIAS 
###SON "YES" Y "NO" EN ESE ORDEN
###EN LAS PREDICCIONES, YES/NO LO TOMA EN ORDEN ALFABETICO
SVMAltTestCM = confusionMatrix(testResults$SVMAlt, 
                               testResults$failed_insolvent)
SVMAltTestCM  #AUMENTO DE LA SENSITIVIDAd

SVMTestCM #RECORDEMOS EL RENDIMIENTO CON PUNTO DE CORTE 0.5

#SITUAR EN LA CURVA ROC TEST LOS RENDIMIENTOS
#PARA VARIOS UMBRALES
plot(SVMTestROC, print.thres = c(.5, .3, .10, SVMThresh), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, legacy.axes = TRUE)

###############################################
### 2. ESTRATEGIAS PARA DATOS NO BALANCEADOS 
### METODOS DE MUESTREO: 
### DOWNSAMPLING, UPSAMPLING Y SMOTE
###############################################

set.seed(1237)
library(caret)

#DOWNSAMPLE: SE MANTIENEN TODOS LOS CASOS DE LA CLASE MINORITARIA
#SE ELIGE ALEATORIAMENTE ESE NUMERO EN LA OTRA CLASE
downSampled_train = downSample(training[, -ncol(training)], 
                         training$failed_insolvent)
dim(downSampled_train)
table(downSampled_train$Class) 

downSampled_valid = downSample(validation[, -ncol(validation)], 
                               validation$failed_insolvent)
dim(downSampled_valid)
table(downSampled_valid$Class) 

#NOMBRE ORIGINAL DE LA VARIABLE OBJETIVO
names(downSampled_train)[16]=
    names(downSampled_valid)[16]="failed_insolvent"
training_valid_Down=rbind(downSampled_train,downSampled_valid)
Index_down= 1:nrow(downSampled_train)
ctrlval_down=ctrlval
ctrlval_down$index=list(Index_down)
ctrlval_down$indexFinal=Index_down

#UPSAMPLE: MUESTREO CON REEMPLAZAMIENTO
#EN LA CLASE MINORITORIA 
set.seed(1237)
upSampled_train = upSample(training[, -ncol(training)], 
                     training$failed_insolvent)
dim(upSampled_train)
table(upSampled_train$Class)

upSampled_valid = upSample(validation[, -ncol(validation)], 
                           validation$failed_insolvent)
dim(upSampled_valid)
table(upSampled_valid$Class) 


#NOMBRE ORIGINAL DE LA VARIABLE OBJETIVO
names(upSampled_train)[16]=
  names(upSampled_valid)[16]="failed_insolvent"

training_valid_Up=rbind(upSampled_train,upSampled_valid)
Index_up= 1:nrow(upSampled_train)
ctrlval_up=ctrlval
ctrlval_up$index=list(Index_up)
ctrlval_up$indexFinal=Index_up

#SMOTE: Synthetic Minority Over-sampling TEchnique:
#usa down y up-sampling. Depende de:
#Cantidad de up-sampling 
#Cantidad de down-sampling
#Numero de vecinos para definir nuevos casos 
#En el up-sampling se sintetizan nuevos casos mediante combinacion
#lineal de los vecinos de un caso elegido aleatoriamente
#SMOTE(form, data, perc.over = 200, k = 5, perc.under = 200,
#      learner = NULL, ...)
library(DMwR)
set.seed(1237)
smoted_train = SMOTE(failed_insolvent ~ ., data = training)
dim(smoted_train)
table(smoted_train$failed_insolvent)

smoted_valid = SMOTE(failed_insolvent ~ ., data = validation)
dim(smoted_valid)
table(smoted_valid$failed_insolvent) 

training_valid_Smote=rbind(smoted_train,smoted_valid)
Index_smote= 1:nrow(smoted_train)
ctrlval_smote=ctrlval
ctrlval_smote$index=list(Index_smote)
ctrlval_smote$indexFinal=Index_smote

#A CONTINUACION SE VA A CONSTRUIR EL MODELO SVM
#SOBRE CADA UNO DE ESTOS CONJUNTOS
###############################################


#MODELO SVM CON DATOS down-sampling
##################################
set.seed(1410)
SVMDown=train(failed_insolvent ~ ., 
      data = training_valid_Down,
      method = "svmRadial", 
      trControl = ctrlval_down,
      tuneLength=10,
      metric = "Sens")
SVMDown

#GUARDAMOS ROC DE VALIDACION PARA LUEGO BUSCAR EL MEJOR UMBRAL
validResults$SVMdown = predict(SVMDown, validation, type = "prob")[,1]
SVMDownROC = roc(validResults$failed_insolvent, validResults$SVMdown,
                levels = rev(levels(validResults$failed_insolvent)))
SVMDownROC

#Probabilidades estimadas de "Yes" en el conjunto test
testResults$SVMdown = predict(SVMDown, testing, 
                              type = "prob")[,1]


#MODELO SVM CON DATOS up-sampling
##################################

set.seed(1410)
SVMUp=train(failed_insolvent ~ ., 
              data = training_valid_Up,
              method = "svmRadial", 
              trControl = ctrlval_up,
              tuneLength=10,
              metric = "Sens")
SVMUp

#GUARDAMOS ROC DE VALIDACION PARA LUEGO BUSCAR EL MEJOR UMBRAL
validResults$SVMUp = predict(SVMUp, validation, 
                             type = "prob")[,1]
SVMUpROC = roc(validResults$failed_insolvent, validResults$SVMUp,
              levels = rev(levels(validResults$failed_insolvent)))
SVMUpROC

#Probabilidades estimadas de "Yes" en el conjunto test
testResults$SVMUp = predict(SVMUp, testing, 
                            type = "prob")[,1]

#MODELO SVM CON DATOS SMOTE
##########################
set.seed(1410)
SVMSmote =train(failed_insolvent ~ ., 
                  data = training_valid_Smote,
                  method = "svmRadial", 
                  trControl = ctrlval_smote,
                  tuneLength=10,
                  metric = "Sens")
SVMSmote

#GUARDAMOS ROC DE VALIDACION PARA LUEGO BUSCAR EL MEJOR UMBRAL
validResults$SVMsmote = predict(SVMSmote, validation, 
                                type = "prob")[,1]
SVMSmoteROC = roc(validResults$failed_insolvent, validResults$SVMsmote,
                 levels = rev(levels(validResults$failed_insolvent)))
SVMSmoteROC


#Probabilidades estimadas de "Yes" en el conjunto test
testResults$SVMsmote = predict(SVMSmote, testing,
                               type = "prob")[,1]


#LA SIGUIENTE FUNCION OBTIENE UN RESUMEN DE LOS 
#DISTINTOS MODELOS CONSTRUIDOS
#PARAMETROS:
#x: MODELO
#evl: CONJUNTO DE VALIDACION
#tst: CONJUNTO TEST
#DETERMINA EL MEJOR UMBRAL SEGUN 
#best.method="closest.topleft" EN VALIDACION
#SALIDA: AUC_VAL,AUC_TEST,SENS_TEST,ESPECIF_TEST 
###############################################

samplingSummary = function(x, evl, tst)
{
  lvl = rev(levels(tst$failed_insolvent))
  evlROC = roc(evl$failed_insolvent,
               predict(x, evl, type = "prob")[,1],
               levels = lvl)
  tstROC= roc(tst$failed_insolvent,
              predict(x, tst, type = "prob")[,1],
              levels = lvl)
  rocs = c(auc(evlROC),auc(tstROC))
  cut = coords(evlROC, x = "best", ret="threshold",
               best.method="closest.topleft")
  bestVals = coords(tstROC, cut, ret=c("sensitivity", "specificity"))
  out = c(rocs, bestVals*100)
  names(out) = c("valROC", "testROC", "testSens", "testSpec")
  out
}
SVMResults = rbind(samplingSummary(SVMFit, validation, testing),
                  samplingSummary(SVMDown, validation, testing),
                  samplingSummary(SVMUp, validation, testing),
                  samplingSummary(SVMSmote, validation, testing))
rownames(SVMResults) = c("Original", "Down-Sampling",
                        "Up-Sampling", "SMOTE")

SVMResults


#CON UMBRAL 0.5 (NO SE LE DA CONJ. VALID.):

samplingSummary05= function(x, tst)
{
  lvl = rev(levels(tst$failed_insolvent))
  tstROC= roc(tst$failed_insolvent,
              predict(x, tst, type = "prob")[,1],
              levels = lvl)
  rocs = auc(tstROC)
  bestVals = coords(tstROC, 0.5, ret=c("sensitivity", "specificity"))
  out = c(rocs, bestVals*100)
  names(out) = c( "testROC", "testSens", "testSpec")
  out
}

SVMResults05 = rbind(samplingSummary05(SVMFit, testing),
                    samplingSummary05(SVMDown, testing),
                    samplingSummary05(SVMUp, testing),
                    samplingSummary05(SVMSmote,testing))
rownames(SVMResults05) = c("Original", "Down-Sampling", 
                          "Up-Sampling", "SMOTE")

SVMResults05  #Con Down-Sampling poca variación


#REPRESENTAR LAS CURVAS ROC
rocCols = c("black", "green", "red","blue")

plot(roc(testResults$failed_insolvent, testResults$SVM, 
         levels = rev(levels(testResults$failed_insolvent))),
     type = "l", col = rocCols[1], legacy.axes = TRUE)
plot(roc(testResults$failed_insolvent, testResults$SVMdown, 
         levels = rev(levels(testResults$failed_insolvent))),
     type = "l", col = rocCols[2],add = TRUE, legacy.axes = TRUE)
plot(roc(testResults$failed_insolvent, testResults$SVMUp, 
         levels = rev(levels(testResults$failed_insolvent))),
     type = "l", col = rocCols[3], add = TRUE, legacy.axes = TRUE)
plot(roc(testResults$failed_insolvent, testResults$SVMsmote, 
         levels = rev(levels(testResults$failed_insolvent))),
     type = "l", col = rocCols[4], add = TRUE, legacy.axes = TRUE)

legend("bottomright",
       legend=c("Original", "Down-Sampling", "Up-Sampling","SMOTE"),
       lty = rep(1, 4),
       lwd = rep(2, 4),
       cex = .8,
       col = rocCols)

