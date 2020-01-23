################################################
#### Máster en Matemáticas. Curso 2018/2019 ####
## Minería Estadística de Datos ################
## Datos de RNASeq con DESeq2 ##################
## Inmaculada Barranco #########################
## 23 enero de 2019 ############################
################################################

## Instalación de librerías.
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("Biobase")
biocLite("DESeq")
biocLite("DESeq2")
biocLite("pasilla")
biocLite("GenomicRanges")

## Carga de librerías.
library(Biobase)
library(DESeq)
library(DESeq2)

## Cargamos los datos.
library("pasilla")

#Nos quedamos con los datos pertenecientes a los genes.
data("pasillaGenes")
head(pasillaGenes)

#Tabla de conteos
countData <- counts(pasillaGenes)
dim(countData)

#Vemos la cabecera de la tabla
head(countData)

#Datos en formato adecuado DESeqDataSet
#para ello debemos tener la información de las columnas
colData <- pData(pasillaGenes)[, c("condition", "type")]
colData

#Podemos construir un DESeqDataSet 
dds <- DESeqDataSetFromMatrix(countData=countData,
                              colData=colData,
                              design=~condition)
colData(dds)$condition <- factor(colData(dds)$condition, 
                                 levels=c("untreated", "treated"))
dds
 
#Análisis de expresión diferencial
analisisdds <- DESeq(dds)
respordefecto <- results(analisisdds)
respordefecto
res <- respordefecto[order(respordefecto$padj),]
head(res)

#Tabla con los genes DE
resDE <- as.data.frame(res)
difexpDE <- resDE[resDE$padj<0.01,]
dim(difexpDE)

#Grafico: MA-Plot
plotMA(res, ylim=c(-2,2), main="Gráfico MA")

#Vamos a tener el cuenta el tipo de secuenciación
design(dds)<-formula(~ type + condition)
dds<-DESeq(dds)
res<-results(dds)
head(res)

#Color key e histograma
library("RColorBrewer")
library("gplots")
select<-order(rowMeans(counts(dds,normalized=TRUE)),
              decreasing=TRUE)[1:30]
hmcol<-colorRampPalette(brewer.pal(9,"GnBu"))(100)
heatmap.2(counts(dds,normalized=TRUE)[select,],col=hmcol,
          Rowv=FALSE,Colv=FALSE,scale="none",
          dendogram="none", trace="none", margin=c(10,6))

#Transformacion de los datos
rld<-rlogTransformation(dds,blind=TRUE)

#Mapa de calor de los datos transformados
heatmap.2(assay(rld)[select,],col=hmcol,
          Rowv=FALSE,Colv=FALSE,scale="none",
          dendrogram="none",trace="none",margin=c(10,6))

#Mapa de calor de las distancias entre muestras
#Distancias
distsRL<-dist(t(assay(rld)))
#Matriz de distancias
mat<-as.matrix(distsRL)
mat

#Mapa de calor
rownames(mat)<-colnames(mat)<-with(colData(dds),
                    paste(condition,type,sep=":"))
heatmap.2(mat,trace="none",col=rev(hmcol),margin=c(13,13))

#Componentes principales de las muestras
print(plotPCA(rld,intgroup=c("condition","type")))
