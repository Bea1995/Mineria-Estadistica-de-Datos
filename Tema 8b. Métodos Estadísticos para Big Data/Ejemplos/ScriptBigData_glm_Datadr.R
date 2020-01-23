##########################################
#MASTER UNIVERSITARIO EN MATEMÁTICAS     #
#MINERÍA ESTADÍSTICA DE DATOS            #
#METODOS ESTADISTICOS PARA BIG DATA      # 
#REGRESION LOGISTICA CON                 #
#DIVISION Y RECOMBINACION Y SUBMUESTREO  #
#RAFAEL PINO MEJIAS                      # 
##########################################

##########################################
#Paquetes necesarios
#install.packages(c("datadr", "tibble"))
#datadr: PAQUETE QUE IMPLEMENTA 
#TECNICAS DE DIVISION Y RECOMBINACION
##########################################

library(datadr)  
data(adult)
help(adult)
#VARIABLE RESPUESTA: incomebin
#incomebin: 0 if income<=50K, 
#           1 if income>50K
dim(adult)
summary(adult)

#PARTICION ENTRENAMIENTO/TEST
set.seed(12345)
n=nrow(adult)
ient=sample(1:n,floor(0.75*n))
itest=setdiff(1:n,ient)

#######################################
#MODELO DE REGRESION LOGISTICA CON glm
#######################################
t0=proc.time()
rglm <- glm(incomebin ~ educationnum + hoursperweek 
                       + sex,data = adult[ient,],
                       family = binomial())
tglm=proc.time()-t0
tglm

summary(rglm)
#Coeficientes del modelo
( Coefic=coef(rglm) )

#################################
#APLICANDO TECNICAS DE DIVISION
#Y RECOMBINACION CON datadr
#################################

#Un tutorial sobre datadr:
#http://deltarho.org/docs-datadr/
#################################

#1. INICIALIZAR UN "DISTRIBUTED DATA FRAME" (ddf)
#################################################
#también existen los ddo (distributed data objects)
adultDdf <- ddf(adult[ient,], update = TRUE)
#"*Running map/reduce to get missing attributes...":
#MapReduce es un modelo de programación que 
#particiona una tarea en subtareas, trabajando 
#sobre cada una de ellas en paralelo 
#(Google, Dean & Ghemawat).
#MapReduce es la base de las operaciones de datadr.
#Si bien datadr ofrece las funciones divide() 
#y recombine(), el usuario también puede escribir
#directamente código propio de MapReduce

adultDdf
names(adultDdf)  #Los nombres de las variables
summary(adultDdf)
#mean=0.239 indica que incomebin=1 se
#presenta en un 23.9% de los casos
#incomebin está definido como numérico,
#income como factor
#(datos no balanceados)
prop.table(table(adult$income[ient]))



#2. DIVIDIR EL CONJUNTO DE DATOS adultDdf
#CON LA FUNCION divide()
#########################################

rrAdult <- divide(adultDdf, 
                  by = rrDiv(1000), 
                  update = TRUE,
                  postTransFn = function(x)
                    x[,c("incomebin", "educationnum", 
                         "hoursperweek", "sex")])

#rrDiv(1000): NUMERO DE CASOS APROXIMADO EN 
#             CADA SUBCONJUNTO
#postTransFn: QUE HACER CON CADA SUBCONJUNTO, 
#             SELECCIONAR LAS VARIABLES DE INTERES
#update=TRUE: pone en marcha un trabajo "MapReduce" 
#             para obtener información adicional, 
#             por ejemplo detectar valores perdidos 
#             o bien posibilitar la obtención de 
#             medidas descriptivas sobre el 
#             conjunto de datos completo
length(ient)/1000
rrAdult  #GENERA 24 SUBCONJUNTOS, CADA UNO 
         #ES UN PAR (key,value)
         #key: ETIQUETA QUE IDENTIFICA EL SUBCONJUNTO
         #value: EL SUBCONJUNTO DE DATOS
getKeys(rrAdult)  #TODOS LOS VALORES DE key
summary(rrAdult)  

#Existen algunas funciones que permiten obtener
#resúmenes sobre todo los datos en el ddf
#(suponiendo update=TRUE)
drAggregate(rrAdult,~incomebin)
tabla=drAggregate(rrAdult,~incomebin+sex)
tabla
xtabs(Freq~incomebin+sex,tabla) #En formato tabla
 
#CUANTILES DE LA DISTRIBUCION DEL
#NUMERO DE CASOS:
cbind(splitRowDistn(rrAdult)) 
cbind(splitRowDistn(rrAdult)[c(1,26,51,76,101)]) 

#COMO ACCEDER AL TAMAÑO DE CADA SUBCONJUNTO
NSubconj=length(getKeys(rrAdult) )
tama=numeric(NSubconj)
for (i in 1:NSubconj)
  tama[i]=nrow(rrAdult[[i]]$value)
tama
cbind(quantile(tama,prob=0:100/100),
     splitRowDistn(rrAdult)) #Es lo mismo
plot(tama,type="h",ylim=c(0,1100))
abline(h=mean(tama),lty=2,col="blue")

#ACCEDER A UN SUBCONJUNTO:
rrAdult[[1]]  #Primer subconjunto, 
              #un par (key,value)
dim(rrAdult[[1]]$value)
summary(rrAdult[[1]]$value)
head(rrAdult[[1]]$value)
tail(rrAdult[[1]]$value)



##3. MODELO DE REGRESIÓN LOGÍSTICA SOBRE
##EL ddf rrAdult:
##PRIMERA OPCION: 
#APLICARC glm A CADA SUBCONJUNTO Y 
#APLICAR LUEGO recombine
#SE TRATA DE LA TÉCNICA DIVIDIR Y CONQUISTAR
#########################################

#addTransform APLICA UNA FUNCIÓN 
#SOBRE CADA SUBCONJUNTO
#EN ESTE CASO LA FUNCION drGLM, QUE 
#DEVUELVE LOS COEFICIENTES PARA EL 
#MODELO AJUSTADO EN ESE SUBCONJUNTO
#LUEGO SE APLICA recombine
#combMeanCoef: CALCULA LA MEDIA DE LOS COEFICIENTES
#              SOBRE LOS DISTINTOS SUBCONJUNTOS
t0=proc.time()
adultGlm <- addTransform(rrAdult, function(x)
  drGLM(incomebin ~ educationnum + 
        hoursperweek + sex,
        data = x, family = binomial()))
CoefsdrGLM=recombine(adultGlm, combMeanCoef)
tdrGLM=proc.time()-t0
tdrGLM

adultGlm
#COEFICIENTES EN EL PRIMER SUBCONJUNTO:
adultGlm[[1]] 

#Resultados:
cbind(Real=Coefic,drGLM=CoefsdrGLM)
## INCONVENIENTE: drGLM NO PROPORCIONA 
## HERRAMIENTAS INFERENCIALES

##4. MODELO DE REGRESIÓN LOGÍSTICA SOBRE
##EL ddf rrAdult:
##SEGUNDA OPCION: drBLB()
##LAS SUBMUESTRAS SON LOS SUBCONJUNTOS
#YA EXISTENTES EN rrAdult
#######################################
##"Bags of Little Bootstrap"
##addTransform: AÑADE LA FUNCIÓN drBLB
##QUE SERÁ APLICADA A CADA UNO DE LOS 
##24 SUBCONJUNTOS
##SE GENERAN R MUESTRAS BOOTSTRAP 
##CADA UNA CON TAMAÑO EL DEL CONJUNTO
##DE ENTRENAMIENTO
##VAMOS A CALCULAR LA MEDIA, SD Y LOS CUANTILES
##APROPIADOS DE LOS R VALORES, OBTENIENDO
##UN IC AL 95% PARA LOS COEFICIENTES
##PARA CADA SUBCONJUNTO (MÉTODO PERCENTIL)
##TAMBIEN SE INCLUYE UN IC BASADO EN
##MEDIA-+CUANTIL_NORMAL*ES (MÉTODO NORMAL)
##SOLO PUEDEN UTILIZARSE METODOS QUE ADMITAN
##PESOS (weights)
t0=proc.time()
adultBlb <- addTransform(rrAdult, function(x) {
    drBLB(x,
    statistic = function(x, weights)
    coef(glm(incomebin ~ educationnum + hoursperweek + sex,
     data = x, weights = weights, family = binomial())),
    metric = function(T)
    c(mean(T),sd(T), mean(T)-qnorm(0.975)*sd(T),
      mean(T)+qnorm(0.975)*sd(T),
      quantile(T, c(0.025, 0.975))),
    R = 200,
    n = nrow(rrAdult)
  )
 })

## CALCULAR LA MEDIA DE LOS ESTIMADORES Y LOS IC
coefsBlb <- recombine(adultBlb, combMean)
tBLB=proc.time()-t0
tBLB

adultBlb
adultBlb[[1]]  #Los coeficientes e IC 
#para el primer subconjunto
round(matrix(adultBlb[[1]]$value,ncol=6,
             byrow=TRUE),3) 

#COEFICIENTES, SD Y LOS IC FINALES
coefsBlb
ICBLB=matrix(coefsBlb, ncol = 6, byrow = TRUE)
ICBLB
colnames(ICBLB)=c("CoefBLB","SDBLB",
                  "ICBLB_N_L","ICBLB_N_U",
                  "ICBLB_P_L","ICBLB_P_U")
ICBLB

round(cbind(Real=Coefic,drGLM=CoefsdrGLM,ICBLB),2)
#(LOS IC COINCIDEN EN LOS DOS PRIMEROS DECIMALES)

#IC 95% CON EL MODELO SOBRE TODOS LOS DATOS
tabla=summary(rglm)$coefficients
alfa=0.05
IC=cbind(IC_L=tabla[,1]-qnorm(1-alfa/2)*tabla[,2],
         IC_U=tabla[,1]+qnorm(1-alfa/2)*tabla[,2])

Resumen=cbind(Real=Coefic,IC,drGLM=CoefsdrGLM,ICBLB)
round(Resumen,2)
#Tomar exp para los odds-ratio
exp(Resumen[-1,])

#APLICAR EL MODELO A LOS DATOS TEST
#SE DEFINE SEGUIDAMENTE UNA FUNCION 
#PARA CALCULAR LAS
#ESTIMACIONES CON UN MODELO DE REG. 
#LOGISTICA DEL QUE SABEMOS LOS COEFICIENTES
#(NO EXISTE UNA FUNCIÓN predict PARA UN OBJETO
#RESULTANTE DEL PROCESO ANTERIOR)
#SI EL CONJUNTO TEST FUERA MUY GRANDE
#SE PUEDE CONVERTIR EN DDF Y APLICAR
#PROCEDIMIENTOS COMO LOS DE EJEMPLOS
#POSTERIORES

probrlog=function(x,coef)
{
  comblin=sum(coef*c(1,x))
  exp(comblin/(1+exp(comblin)))
}

#sex ES FACTOR female/male
test=data.frame(adult[itest,c("educationnum", "hoursperweek")],
                sex=adult[itest,"sex"]=="male")
head(test)
probtest=apply(test,1, probrlog,coef=ICBLB[,1])

tabla=table(adult[itest,"incomebin"],
            c(0,1)[(probtest>0.5)+1])
100*diag(prop.table(tabla,1))  
sum(diag(prop.table(tabla)))

#DATOS NO BALANCEADOS, HABRÍA QUE INCLUIR 
#PROCEDIMIENTOS APROPIADOS

library(ROCR)
prediobj<-prediction(probtest,adult[itest,"incomebin"])
plot(performance(prediobj, "tpr","fpr"),
     main="CURVA COR TEST, RF")
abline(a=0,b=1,col="blue",lty=2)
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc ,"\n")


