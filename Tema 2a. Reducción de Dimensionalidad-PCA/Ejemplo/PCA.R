## ACP

##lectura de datos (sep blanco, decimal punto)
auxaire.dat <- read.table("datosaire.txt", header=T, sep=" ", dec=".")

##salvar con fichero R
## quito la columna 1 (-1 en la parte de columnas)
aire.dat=auxaire.dat[,-1]
attach(aire.dat)

##nombres como nombres de las columnas
#dimnames

dimnames(aire.dat)[[1]] <- c("Phoenix", "Little Rock", "San Francisco",
                             "Denver", "Hartford", "Wilmington", "Washington", "Jacksonville", "Miami",
                             "Atlanta", "Chicago", "Indianapolis", "Des Moines", "Wichita", "Louisville",
                             "New Orleans", "Baltimore", "Detroit", "Minneapolis-St. Paul", "Kansas City",
                             "St. Louis", "Omaha", "Alburquerque", "Albany", "Buffalo", "Cincinnati",
                             "Cleveland", "Columbus", "Philadelphia", "Pittsburgh", "Providence",
                             "Memphis", "Nashville", "Dallas", "Houston", "Salt Lake City", "Norfolk",
                             "Richmond", "Seattle", "Charleston", "Milwaukee")
## Descriptiva
summary(aire.dat)

##nube de puntos cada dos variables 
##Pares de observaciones cada dos variables, cada punto corresponde a una ciudad
pairs(aire.dat)

#Con histogramas
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  2
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="blue", ...)
}
pairs(aire.dat,diag.panel=panel.hist)

##bloxplot
boxplot(aire.dat)

## Pcs tiene sentido para variables correladas
## ¿las nuestras lo están?
R=cor(aire.dat)
round(R,2)
det(R)
## proximo a cero, entonces alta correlacion
##tiene sentido hallar Cps

##Cálculo de Cps
?princomp
pca<-princomp(aire.dat,cor=T)
str(pca)

pca$sdev
pca$loadings[,1:2]

summary(pca)

##sumario más elaborado
mysummary<- matrix(NA,nrow=length(pca$sdev),ncol=3)
mysummary[,1]<-  pca$sdev^2
mysummary[,2]<- 100*mysummary[,1]/sum(mysummary[,1])
mysummary[,3]<- cumsum(mysummary[,2])
colnames(mysummary)<- c("Eigenvalue","Percentage","Cumulative percent.")
round(mysummary,2)

##compruebo algebra
eigen(R)$values
#numero de componentes
plot(pca,col="blue",main="Plot")
abline(h=1,lwd=2,lty=2,col="red")

#loadings: cargas
loadings(pca)
## las dos primeras
round(pca$loadings[,1:2],2)

#Medimos la importancia de las Xi en Cps
correlations<-loadings(pca)%*%diag(pca$sdev)
## correlations
correlations[,1:2]
