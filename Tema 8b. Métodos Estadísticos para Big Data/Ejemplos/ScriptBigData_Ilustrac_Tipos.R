##########################################
#MASTER UNIVERSITARIO EN MATEMÁTICAS     #
#MINERÍA ESTADÍSTICA DE DATOS            #
#METODOS ESTADISTICOS PARA BIG DATA      # 
#ILUSTRACION DEL EFECTO DEL TIPO DE DATOS#
#SOBRE EL TAMAÑO DE UN OBJETO R          #
#RAFAEL PINO MEJIAS                      # 
##########################################
x=matrix(0,1e+08,3)
object.size(x)
tama=function(x) #EN GB
{
  tama=object.size(x)/(1024^3)
  tama=unclass(tama)
  names(tama)="GigaBytes"
  tama
}
round(tama(x),2)

#Si x va a contener enteros:
x=matrix(as.integer(0),1e+08,3)
round(tama(x),2)

#Si se realizan cálculos
x=x+1
round(tama(x),2)

#Mejor:
x=matrix(as.integer(0),1e+08,3)
x=x+as.integer(1)
round(tama(x),2)

rm(x) #Eliminar x
