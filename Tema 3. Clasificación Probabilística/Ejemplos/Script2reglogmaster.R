################################################################################
##   
##  An�lisis Regresi�n Log�stica
##  Selecci�n de variables
##   
################################################################################

### DIRECTORIO DE TRABAJO ####################################################
#setwd("C:/aMaster2015/RegresionLogistica/script2")

####################################################

### AN�LISIS DE REGRESI�N LOG�STICA
###
###      Funciones:  "glm"   
###        
####################################################################

#### Datos incendios.csv   ###############################################
# 
#####################################################################
# Descripci�n de los datos:
# 300 observaciones correspondientes a 300 municipios sobre los que, teniendo
# en cuenta datos hist�ricos, se clasifican en :
#   - Municipios con alta incidencia de fuego (Y=1)
#   - Municipios con baja incidencia de fuego (Y=0)
#
# Se consideran las siguientes variables:
# 
# maquin_d  Densidad de maquinaria agr�cola sobre superficie municipal
# gan_for	  Densidad de ganado en r�gimen extensivo sobre la sup. forestal (GAN_FOR)
# paro	    Tasa de paro (PARO)
# roadmu_d	Densidad de carreteras por superficie municipal (ROADMU_D)
# frag7x7	  Media municipal del �ndice de fragmentaci�n. (FRAG7X7)
# prestur   Presi�n tur�stica Si=1; No=0
######################################################################

# Lectura de datos
datos <- read.csv("incendios.csv", header=TRUE ,sep=";")
names(datos) # nombre de las variables
dim(datos)   # dimensiones del conjunto de datos
datos[1:6,]  # Primeros 6 datos
summary(datos) #resumen de datos

# Determinaci�n como factor de la variable dependiente u objetivo
# y de la variable prestur
datos$Y = as.factor(datos$Y)
datos$prestur =as.factor(datos$prestur)
summary(datos$Y)
summary(datos$prestur)

# Realizaci�n del modelo de regresi�n log�stica
#
# Orden 
# glm(formula, family = binomial, data, weights, subset, ...)
#
# El argumento "family" sirve para indicar la componente aleatoria del 
# modelo as� como la funci�n de enlace (link) que se utiliza. 
# Si se especifica family=binomial, la funci�n glm utiliza la funci�n 
# logit como funci�n de enlace. 
# Para elegir otro enlace se especifica mediante el argumento "link",
# por ejemplo, para ajustar un modelo "probit": "family=binomial(link=probit)"
# 
Yobj.rl2=glm(Y~.,data=datos,family=binomial)
summary(Yobj.rl2)

names(Yobj.rl2)
Yobj.rl2$coefficients        # estimaci�n de los coeficientes "beta"
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
# Las aportaciones al estad�stico Desviaci�n, es decir, las desviaciones 
# aportadas por cada observaci�n (desviaci�n residual) se obtienen como
# sigue:
residuals(Yobj.rl2)[1:6] # desviaciones residuales
Yobj.rl2$deviance        # estad�stico desviaci�n D

# D es la suma de los cuadrados de los residuos desviaci�n
sum(residuals(Yobj.rl2)^2)

# Significaci�n global del modelo:
Estad.G = Yobj.rl2$null.deviance - Yobj.rl2$deviance
gl.G = Yobj.rl2$df.null - Yobj.rl2$df.residual
pvalor = 1- pchisq(Estad.G,gl.G)

Estad.G # Estad�stico G
gl.G    # Grados de Libertad asociados al Estad�stico
pvalor  # p-valor asociado al contraste de significaci�n global del modelo

# Intervalos de confianza para los par�metros.
library(MASS)
coef(Yobj.rl2)
confint(Yobj.rl2)

# Estimaci�n de las probabilidades de que la respuesta sea igual a 1
Yobj.rl2.pred = predict(Yobj.rl2 , type="response", se =T)
names(Yobj.rl2.pred)
Yobj.rl2.pred$fit[1:6]
Yobj.rl2.pred$se[1:6]

# C�lculo de las odd ratios
# Odd Ratio = exp(beta)
# Intervalo de confianza exp{intervalo de confianza de beta}
exp(Yobj.rl2$coefficients)
exp(confint(Yobj.rl2)) #intervalo de confianza para las odds ratio

t=cbind(exp(Yobj.rl2$coefficients), exp(confint(Yobj.rl2)))
colnames(t)=c("Odds ratio", "Lim Inf (2.5%)","Lim Sup (97.5%)")
t
 