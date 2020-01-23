################################################################################
## 
## ANÁLISIS DE REGRESIÓN LOGÍSTICA
##   
################################################################################

######################################################
### 
###      Función :  "glm"   
###      indicamos logistica con family="binomial"  
###
####################################################################
######################################################################
## Ejemplo de muerte por cardiopatía isquémica ################
###############################################################

# Datos de entrada
edad <- rep(c(25, 32.5, 37.5, 42.5,47.5, 52.5, 57.5, 65), c(10,15,12,15,13,8,17,10))
CI <- rep(c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0), c(1,9,2,13,3,9,5,10,6,7,5,3,13,4,8,2))

datos <- data.frame(edad, CI) 
head(datos) # cabecera del dataframe

names(datos) # nombre de las variables
dim(datos)   # dimensiones del conjunto de datos
datos[1:6,]  # Primeros 6 datos
summary(datos) #resumen de datos

## Para hacer regresión logística en R, las variables categóricas
## hemos de indicar que lo son, es decir que su tipo es factor

###########################################################
# Determinación como factor de la variable dependiente u objetivo
# CI: presencia (1) o ausencia (0) de cardiopatía isquémica
datos$CI = as.factor(datos$CI)
summary(datos$CI)

# Ajuste del modelo de regresión logística
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
Yobj.rl1 <- glm(datos$CI~.,data=datos,family=binomial)
summary(Yobj.rl1)

names(Yobj.rl1)
## str(Yobj.rl1)
Yobj.rl1$coefficients        # estimación de los coeficientes "beta"
Yobj.rl1$fitted.values[1:6]  # Valores ajustados (de la probabilidad)
Yobj.rl1$fitted.values[88:92]
Yobj.rl1$family              # Familia seleccionada
Yobj.rl1$linear.predictor[1:6] # Estimaciones del predictor lineal, por tanto,
                               # de log(prob/(1-prob.))
Yobj.rl1$linear.predictor[88:92]

# Observar que: exp(linear.predictor)/[1+exp(linear.predictor)] es
# igual a la probabilidad ajustada:
exp(Yobj.rl1$linear.predictor[1:6])/(1+exp(Yobj.rl1$linear.predictor[1:6]))
Yobj.rl1$fitted.values[1:6]  # Valores ajustados (de la probabilidad)
datos$CI[1:6]

# Sobre los residuos
Yobj.rl1$residuals[1:6]      # residuos (**)

# (**) Estos residuos no son los residuos de Pearson ni los asociados 
#      al estadístico desviación D. Son son los últimos obtenidos en el 
#      algoritmo de reponderación por mínimos cuadrados utilizado en el 
#      ajuste.

# Las aportaciones al estadístico Desviación, es decir, las desviaciones 
# aportadas por cada observación (desviación residual) se obtienen como
# sigue:

residuals(Yobj.rl1)[1:6] # desviaciones residuales
Yobj.rl1$deviance        # estadístico desviación D

# D es la suma de los cuadrados de los residuos desviación
sum(residuals(Yobj.rl1)^2)

# Significación global del modelo:
Estad.G = Yobj.rl1$null.deviance - Yobj.rl1$deviance
gl.G = Yobj.rl1$df.null - Yobj.rl1$df.residual
gl.G
pvalor = 1- pchisq(Estad.G,gl.G)

Estad.G # Estadístico G
gl.G    # Grados de Libertad asociados al Estadístico
pvalor  # p-valor asociado al contraste de significación global del modelo

# Intervalos de confianza para los parámetros.
library(MASS)
coef(Yobj.rl1)
confint(Yobj.rl1)

# Cálculo de las odds ratio
# Odd Ratio = exp(beta)
# Intervalo de confianza exp{intervalo de confianza de beta}
exp(Yobj.rl1$coefficients)
exp(confint(Yobj.rl1)) #intervalo de confianza para las odds ratio

t=cbind(exp(Yobj.rl1$coefficients), exp(confint(Yobj.rl1)))
colnames(t)=c("Odds ratio", "Lim Inf (2.5%)","Lim Sup (97.5%)")
t
 