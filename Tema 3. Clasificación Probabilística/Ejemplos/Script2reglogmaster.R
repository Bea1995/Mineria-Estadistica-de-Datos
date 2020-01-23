################################################################################
##   
##  Análisis Regresión Logística
##  Selección de variables
##   
################################################################################

### DIRECTORIO DE TRABAJO ####################################################
#setwd("C:/aMaster2015/RegresionLogistica/script2")

####################################################

### ANÁLISIS DE REGRESIÓN LOGÍSTICA
###
###      Funciones:  "glm"   
###        
####################################################################

#### Datos incendios.csv   ###############################################
# 
#####################################################################
# Descripción de los datos:
# 300 observaciones correspondientes a 300 municipios sobre los que, teniendo
# en cuenta datos históricos, se clasifican en :
#   - Municipios con alta incidencia de fuego (Y=1)
#   - Municipios con baja incidencia de fuego (Y=0)
#
# Se consideran las siguientes variables:
# 
# maquin_d  Densidad de maquinaria agrícola sobre superficie municipal
# gan_for	  Densidad de ganado en régimen extensivo sobre la sup. forestal (GAN_FOR)
# paro	    Tasa de paro (PARO)
# roadmu_d	Densidad de carreteras por superficie municipal (ROADMU_D)
# frag7x7	  Media municipal del índice de fragmentación. (FRAG7X7)
# prestur   Presión turística Si=1; No=0
######################################################################

# Lectura de datos
datos <- read.csv("incendios.csv", header=TRUE ,sep=";")
names(datos) # nombre de las variables
dim(datos)   # dimensiones del conjunto de datos
datos[1:6,]  # Primeros 6 datos
summary(datos) #resumen de datos

# Determinación como factor de la variable dependiente u objetivo
# y de la variable prestur
datos$Y = as.factor(datos$Y)
datos$prestur =as.factor(datos$prestur)
summary(datos$Y)
summary(datos$prestur)

# Realización del modelo de regresión logística
#
# Orden 
# glm(formula, family = binomial, data, weights, subset, ...)
#
# El argumento "family" sirve para indicar la componente aleatoria del 
# modelo así como la función de enlace (link) que se utiliza. 
# Si se especifica family=binomial, la función glm utiliza la función 
# logit como función de enlace. 
# Para elegir otro enlace se especifica mediante el argumento "link",
# por ejemplo, para ajustar un modelo "probit": "family=binomial(link=probit)"
# 
Yobj.rl2=glm(Y~.,data=datos,family=binomial)
summary(Yobj.rl2)

names(Yobj.rl2)
Yobj.rl2$coefficients        # estimación de los coeficientes "beta"
Yobj.rl2$fitted.values[1:6]  # Valores ajustados (de la probabilidad)
Yobj.rl2$family              # Familia seleccionada
Yobj.rl2$linear.predictor[1:6] # Estimaciones del predictor lineal, por tanto,
                               # de log(prob/(1-prob.))

# Observar que: exp(linear.predictor)/[1+exp(linear.predictor)] es
# igual a la probabilidad ajustada:
exp(Yobj.rl2$linear.predictor[1:6])/(1+exp(Yobj.rl2$linear.predictor[1:6]))
Yobj.rl2$fitted.values[1:6]  # Valores ajustados (de la probabilidad)
datos$Y[1:6]

# Sobre los residuos
# Las aportaciones al estadístico Desviación, es decir, las desviaciones 
# aportadas por cada observación (desviación residual) se obtienen como
# sigue:
residuals(Yobj.rl2)[1:6] # desviaciones residuales
Yobj.rl2$deviance        # estadístico desviación D

# D es la suma de los cuadrados de los residuos desviación
sum(residuals(Yobj.rl2)^2)

# Significación global del modelo:
Estad.G = Yobj.rl2$null.deviance - Yobj.rl2$deviance
gl.G = Yobj.rl2$df.null - Yobj.rl2$df.residual
pvalor = 1- pchisq(Estad.G,gl.G)

Estad.G # Estadístico G
gl.G    # Grados de Libertad asociados al Estadístico
pvalor  # p-valor asociado al contraste de significación global del modelo

# Intervalos de confianza para los parámetros.
library(MASS)
coef(Yobj.rl2)
confint(Yobj.rl2)

# Estimación de las probabilidades de que la respuesta sea igual a 1
Yobj.rl2.pred = predict(Yobj.rl2 , type="response", se =T)
names(Yobj.rl2.pred)
Yobj.rl2.pred$fit[1:6]
Yobj.rl2.pred$se[1:6]

# Cálculo de las odd ratios
# Odd Ratio = exp(beta)
# Intervalo de confianza exp{intervalo de confianza de beta}
exp(Yobj.rl2$coefficients)
exp(confint(Yobj.rl2)) #intervalo de confianza para las odds ratio

t=cbind(exp(Yobj.rl2$coefficients), exp(confint(Yobj.rl2)))
colnames(t)=c("Odds ratio", "Lim Inf (2.5%)","Lim Sup (97.5%)")
t
 