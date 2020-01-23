#########################################
## MASTER PROPIO UNIVERSIDAD DE SEVILLA #
## DATA SCIENCE & BIG DATA              #
## MACHINE LEARNING II                  #
## APRENDIZAJE NO SUPERVISADO           #
#########################################


##########################
#EJEMPLO 1. UNA RED SOM
##########################
#install.packages("kohonen")
library(kohonen)
data("wines") 
help(wines)
str(wines)
#También se carga la variable vintages
table(vintages)
#Conviene tipificar las variables 
wines.sc <- scale(wines)
#el algoritmo comienza con una inicialización 
#aleatoria de los nodos
set.seed(7) 
wine.som <- som(X = wines.sc, 
                grid = somgrid(xdim=5, 
                               ydim=4, 
                               topo="hexagonal",
                               neighbourhood.fct="gaussian") )
#neighbourhood.fct="bubble" es la otra opción
#Si ponemos radius=0 no se actualizan
#vecinos, (aprendizaje competitivo)
#Por defecto, va disminuyendo desde
#un valor inicial que es el cuantil 2/3
#del conjunto de todas las distancias
#entre pares de elementos

help(som)  #Ver las opciones principales
wine.som
summary(wine.som)
plot(wine.som,type="changes")
#Si se observa que no parece converger, 
#se puede aumentar rlen (100 por defecto)

plot(wine.som, type="codes")  #Por defecto, codes
#Tamaño de cada conglomerado:
plot(wine.som, "counts")
#Distancia media al representante correspondiente:
plot(wine.som, "quality")


###############################################
#EJEMPLO 2. UNA RED SOM APRENDIZAJE SUPERVISADO
#PROBLEMA DE CLASIFICACIÓN SEGÚN vintage
##############################################
set.seed(1)
ient=sample(1:nrow(wines),ceiling(nrow(wines)*0.7))
Xtraining <- scale(wines[ient, ])
Xtest <- scale(wines[-ient, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))
trainingdata <- list(measurements = Xtraining,
                     vintages = vintages[ient])
testdata <- list(measurements = Xtest, 
                 vintages = vintages[-ient])

mygrid = somgrid(4, 4, "hexagonal")
wines.xyf <- supersom(trainingdata, grid = mygrid,
                      rlen=200)

wines.xyf
summary(wines.xyf)
plot(wines.xyf,type="changes")
plot(wines.xyf, type = "counts")
plot(wines.xyf, type = "quality")

#Dibujo de los representantes,
#distinguiendo la X y la Y
par(mfrow = c(1, 2))
plot(wines.xyf, "codes")
par(mfrow = c(1, 1))

#Obtener las predicciones usando los valores X 
#del conjunto test: cada caso se asigna al 
#representante más cercano. La predicción se
#obtiene mediante la agregación de los valores 
#de la variable respuesta en ese conglomerado
som.prediction <- predict(wines.xyf, 
                          newdata = testdata[1])
tabla=table(vintages[-ient], 
            som.prediction$predictions[["vintages"]])
tabla
100*diag(prop.table(tabla,1))
100*sum(diag(tabla))/sum(tabla)

#predict guarda más información
str(som.prediction)

###########################
#EJEMPLO 3. UNA RED LVQ1
###########################
library(MASS)
library(class)
data(iris)

n<-nrow(iris)
indin<-c(1:n)
indient<-sort(sample(indin,100))
inditest<-setdiff(indin,indient)

xentre<-iris[indient,1:4]
xtest<-iris[inditest,1:4]
espentre<-iris[indient,5]
espetest<-iris[inditest,5]

cd0 <- lvqinit(xentre,espentre,10)
print("Datos de entrenamiento tras inicializar LVQ1:")
#lvqtest(cd0,xentre)
table(espentre,lvqtest(cd0,xentre))
print("Datos de test tras inicializar LVQ1:")
#lvqtest(cd0,xtest)
table(espetest,lvqtest(cd0,xtest))

cd1 <-lvq1(xentre, espentre, cd0,niter=1000)
print("Datos de entrenamiento tras finalizar LVQ1:")
table(espentre,lvqtest(cd1,xentre))
print("Datos de test tras finalizar LVQ1:")
table(espetest,lvqtest(cd1,xtest))

detach("package:class")

