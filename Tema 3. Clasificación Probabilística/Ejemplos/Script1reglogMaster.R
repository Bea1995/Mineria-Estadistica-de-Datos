################################################################################
## 
## AN�LISIS DE REGRESI�N LOG�STICA
##   
################################################################################

######################################################
### 
###      Funci�n :  "glm"   
###      indicamos logistica con family="binomial"  
###
####################################################################
######################################################################
## Ejemplo de muerte por cardiopat�a isqu�mica ################
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

## Para hacer regresi�n log�stica en R, las variables categ�ricas
## hemos de indicar que lo son, es decir que su tipo es factor

###########################################################
# Determinaci�n como factor de la variable dependiente u objetivo
# CI: presencia (1) o ausencia (0) de cardiopat�a isqu�mica
datos$CI = as.factor(datos$CI)
summary(datos$CI)

# Ajuste del modelo de regresi�n log�stica
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
Yobj.rl1 <- glm(datos$CI~.,data=datos,family=binomial)
summary(Yobj.rl1)

names(Yobj.rl1)
## str(Yobj.rl1)
Yobj.rl1$coefficients        # estimaci�n de los coeficientes "beta"
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
#      al estad�stico desviaci�n D. Son son los �ltimos obtenidos en el 
#      algoritmo de reponderaci�n por m�nimos cuadrados utilizado en el 
#      ajuste.

# Las aportaciones al estad�stico Desviaci�n, es decir, las desviaciones 
# aportadas por cada observaci�n (desviaci�n residual) se obtienen como
# sigue:

residuals(Yobj.rl1)[1:6] # desviaciones residuales
Yobj.rl1$deviance        # estad�stico desviaci�n D

# D es la suma de los cuadrados de los residuos desviaci�n
sum(residuals(Yobj.rl1)^2)

# Significaci�n global del modelo:
Estad.G = Yobj.rl1$null.deviance - Yobj.rl1$deviance
gl.G = Yobj.rl1$df.null - Yobj.rl1$df.residual
gl.G
pvalor = 1- pchisq(Estad.G,gl.G)

Estad.G # Estad�stico G
gl.G    # Grados de Libertad asociados al Estad�stico
pvalor  # p-valor asociado al contraste de significaci�n global del modelo

# Intervalos de confianza para los par�metros.
library(MASS)
coef(Yobj.rl1)
confint(Yobj.rl1)

# C�lculo de las odds ratio
# Odd Ratio = exp(beta)
# Intervalo de confianza exp{intervalo de confianza de beta}
exp(Yobj.rl1$coefficients)
exp(confint(Yobj.rl1)) #intervalo de confianza para las odds ratio

t=cbind(exp(Yobj.rl1$coefficients), exp(confint(Yobj.rl1)))
colnames(t)=c("Odds ratio", "Lim Inf (2.5%)","Lim Sup (97.5%)")
t
 