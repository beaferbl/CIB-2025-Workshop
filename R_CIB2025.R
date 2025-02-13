### CIB2025: INTRODUCCIÓN AL ANALISIS DE DATOS CON R ###

# 1. R COMO CALCULADORA
3+2 # suma
3-2 # resta
3*2 # multiplicación
3/2 # división con decimales
3%/%2 # división entera
3**2 # potencia
3^2 # potencia

# 2. OBJETOS EN R
## Definir Variables
a <- 3+2
a=3+2 
a < -3 # ¡OJO!

### Las variables no tienen por qué ser números, también pueden ser palabras.
b <- "Hola"
c <- "Adios"

### Las variables se pueden sobreescribir y reasignarles otro valor.
c<-4

## Funciones Rbase y objetos
sqrt(2)
class(sqrt)
help(sqrt)

## Vectores (*numeric*, *character*)
numeros <- c(3,2,5,10,1,12)
arboles<-c("pino","castaño", "manzano", "roble")

numeros.caracteres<-c(1,2,"pino","castaño")
class(numeros.caracteres)

### Acceder a posiciones
arboles[2]
arboles[c(3,1,2)]

### Ordenar vectores
sort(numeros)
sort(numeros, decreasing = TRUE)

order(numeros)
order(numeros, decreasing = TRUE)

sort(arboles)
order(arboles)

###  Conjuntos de datos (*data.frame*)
df<-data.frame(meses=c("enero","febrero","marzo","abril","mayo","junio",
                       "julio","agosto","septiembre","octubre",
                       "noviembre","diciembre"),
               dias=c(31,28,31,30,31,30,31,31,30,31,30,31))

df$meses
df$dias

df[,] # nos devuelve todas las filas y todas las columnas, es decir, el data.frame entero
df[,1] # nos devuelve todas las filas y la primera columna
df[1,] # nos devuelve la primera fila y todas las columnas
df[,"dias"] # nos devuelve todas las filas y la columna "dias"

# 3. Rutas y directorios para importar datos
getwd()
setwd("/media/adminiis/HematoLaFe/MGC/CIB2025_tallerR/")
list.files()

# 4. Operaciones con conjuntos de datos
## 4.1 Importar datos
### Cargar un fichero de excel
# install.packages("readxl") # Descomentar esta línea para instalarlo
library(readxl)
bca<-read_xlsx("BCA_assay.xlsx",sheet=1,skip = 1)

### Cargar ficheros .txt /.csv /.tsv
tmb<-read.delim("tmb_mskcc_2018_clinical_data.tsv")
tmb<-read.table("tmb_mskcc_2018_clinical_data.tsv",header=TRUE,sep="\t") 
help(read.csv)

## 4.2 Conocer la estructura del conjunto de datos (Analisis Exploratorio/Descriptivo)
nrow(bca) # Nº de columnas
ncol(bca) # Nº de filas
colnames(bca) # Nombre de las columnas

head(bca) # Acceder a la cabecera
tail(bca) # Acceder a las ultimas filas

### Funciones de resumen:
str(bca)  
summary(bca)

### Acceder a valores concretos dentro del conjunto de datos (data.frame[fila, columna])
# Acceder a las columnas:
bca$nombre
bca$medida1
bca$medida2

bca[6,] # Acceder a una fila concreta:
bca[3,2] # Acceder a un valor específico
bca[1:3,1:3] # Acceder a un subconjunto de datos
bca[1:3, c(1,4)] # acceder a columnas concretas por posición
bca[1:3, c("nombre", "concentracion")] # Acceder por nombre
bca[1:3, c("concentracion", "nombre")] # Reordenar columnas

# Conocer las variables categoricas
colnames(tmb)

str(tmb) #1661 observaciones, 24 variables
summary(tmb)

table(tmb$Sex) # Distribución de sexos 
#A priory viendo la tabla no sabemos de qué tipos de cancer/tipos de muestras tenemos información:
table(tmb$Cancer.Type) # Tipos de cancer recogidos
table(tmb$Sample.Type) # Tipos de muestras: al diagnostico (primary) o metastásicas
table(tmb$Drug.Type) # Tipos de farmaco

# Crear/Añadir columnas
bca$factor_correccion <- 0.5 #  todas las filas tendrán el mismo valor asignado
View(bca)
bca$fecha <- "12/02/2025"

valores <- c(0.1,0.5,1.6,1.80,1.76,0.90, 1.60, 1.81,1.74, 2.4, 0.9) # especificar el valor de cada fila
bca$factor_correccion <- valores

## 4.3 Filtrar un conjunto de datos
### Operadores logicos:
# IMPORTANTE ESCRIBIR DOS VECES "=", si escribimos un "=" estamos asignando
1==2 ## ¿1 es igual a 2?
1!=2 ## ¿1 es diferente de 2?
1<0 ## ¿1 es menor que 0?
1>0 ## ¿1 es mayor que 0?

bca[bca$nombre == "muestra1",]
bca[bca$nombre == "muestra2",]
bca[bca$medida1 > 0.5,] # Importante: la "," indica que queremos todas las columnas.
# Obtener valores de una columna
bca$nombre[bca$medida1 >0.5] # Nota: Ya seleccionamos una columna, no hace falta la ",".

tmb[tmb$Cancer.Type == "Breast Cancer",] 
# Selección de columnas concretas:
tmb[tmb$Cancer.Type == "Breast Cancer", c("Sample.ID", "Patient.ID", "Sample.Type", "Mutation.Count")] 
# Muestras metastásicas y de cancer de mama:
tmb[tmb$Cancer.Type == "Breast Cancer" & tmb$Sample.Type == "Metastasis",] # &: AND

table(tmb$Cancer.Type) # Tipos de cancer recogidos
tmb[tmb$Cancer.Type %in%  c("Breast Cancer", "Melanoma", "Skin Cancer, Non-Melanoma"),] # dataframe
tmb$Sample.ID[tmb$Cancer.Type %in%  c("Breast Cancer", "Melanoma", "Skin Cancer, Non-Melanoma")] # listado

table(tmb$Age.Group.at.Diagnosis.in.Years)
tmb$Sample.ID[tmb$Age.Group.at.Diagnosis.in.Years %in% c("31-50", "50-60")]

subset(tmb, Cancer.Type == "Breast Cancer")

## 4.4 Ordenar un conjunto de datos
tmb_ordenado <- tmb[order(tmb$Mutation.Count), ] # Orden ascendente

tmb_ordenado <- tmb[order(tmb$Mutation.Count, decreasing=TRUE),] # Orden descendente
head(tmb_ordenado$Mutation.Count)

## 4.5 Operaciones sencillas con columnas
bca$Conteo <- bca$medida1 + 10
bca$Conteo <- bca$medida1 -10 
bca$Conteo <- bca$medida1/100
bca$Conteo <- bca$medida1 * bca$factor_correccion

### Valores nulos
any(is.na(bca))
colSums(is.na(bca)) # Nulos en la columna de concentracion

bca_aux <- bca

bca[is.na(bca)] <- 0 # Asignar un valor
any(is.na(bca))

bca_aux <- na.omit(bca_aux) # Eliminar estos registros

## 4.6 Buscar valores y reemplazarlos
### gsub: substituir/reemplazar valores
table(tmb$Cancer.Type)
tmb$Cancer.Type <- gsub(pattern = "Cancer",
                        replacement = "Tumor",
                        x = tmb$Cancer.Type) 
table(tmb$Cancer.Type)

tmb$Patient.ID[1:5]
tmb$Patient.ID <- gsub(pattern = "-",
                       replacement = ".",
                       x = tmb$Patient.ID) 
tmb$Patient.ID[1:5]


### Buscar valores:
### Cuando queremos saber la posición de un registro al trabajar con grandes volumenes de información:
### grep: devuelve indices
### grepl: devuelve verdadero/falso
grep(pattern= "Glioma",
     x = tmb$Cancer.Type)

glioma.df <- tmb[grep(pattern= "Glioma",x = tmb$Cancer.Type),]
head(glioma.df)

grepl(pattern= "Glioma",
      x = tmb$Cancer.Type)

glioma.df <- tmb[grepl(pattern= "Glioma",x = tmb$Cancer.Type),]

## 4.7 Instalación de paquetes y librerias de conjuntos de datos

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("heplots")
# install.packages("RColorBrewer")

library(dplyr)
library(ggplot2)
library(heplots)
library(RColorBrewer)

### El paquete dplyr
# Cheat Sheet: https://nyu-cdsc.github.io/learningr/assets/data-transformation.pdf

tmb %>% 
  group_by(Sex) %>%
  count() # Similar a la función table

tmb %>% 
  group_by(Cancer.Type) %>%
  summarise(Promedio_tiempo = mean(Age.at.Which.Sequencing.was.Reported..Days.)) # Aparece un NA

tmb %>% 
  group_by(Cancer.Type) %>%
  summarise(Promedio_tiempo = mean(Age.at.Which.Sequencing.was.Reported..Days., na.rm = TRUE)) #na.rm: no tener en cuenta los valores nulos en el calculo

tmb %>% 
  filter(Cancer.Type == "Melanoma") %>%
  group_by(Sex) %>%
  summarise(Promedio_tiempo = mean(Age.at.Which.Sequencing.was.Reported..Days., na.rm = TRUE))

# 5. Analisis estadistico:
tmb<-read.delim("tmb_mskcc_2018_clinical_data.tsv")
str(tmb)

id.duplicados<-duplicated(tmb$Patient.ID)
sum(id.duplicados)

na.count<-rowSums(is.na(tmb))
tmb.complete<-tmb[na.count==0,]

## 5.1 Estadistica descriptiva
summary(tmb.complete)

mean(tmb$Mutation.Count)
mean(tmb$Mutation.Count, na.rm = TRUE)
mean(tmb.complete$Mutation.Count)

min(tmb.complete$Mutation.Count)
max(tmb.complete$Mutation.Count)

table(tmb.complete$Cancer.Type)

### Gráfico de dispersión.
plot(tmb.complete$Mutation.Count, tmb.complete$Age.at.Which.Sequencing.was.Reported..Days.,
     xlab="número de mutaciones",
     ylab = "edad (días)")

### Grafico de cajas y bigotes
boxplot(tmb.complete$Mutation.Count)

boxplot(tmb.complete$Mutation.Count ~tmb.complete$Cancer.Type,
        xlab="Tipo de cáncer",
        ylab="Número de mutaciones")

### Grafico de barras
library(RColorBrewer)
age.freq<-table(tmb.complete$Age.Group.at.Diagnosis.in.Years)[c(1,3,4,5,2)]
age.bar<-barplot(age.freq,
                 col=brewer.pal(5, "Set3"),
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30),
                 cex.names=0.6
)
## 5.2 Estadística inferencial
### Test de normalidad
shapiro.test(tmb.complete$Mutation.Count)
qqnorm(tmb.complete$Mutation.Count)
qqline(tmb.complete$Mutation.Count)
### Test de igualdad de varianzas
tmb.complete$Mutation.Count <- as.numeric(tmb.complete$Mutation.Count)
leveneTests(tmb.complete[,"Mutation.Count", drop=FALSE],
            tmb.complete$Sex)
### Comparación de medias para distribuciones normales (pruebas paramétricas)
#### * Comparación de dos grupos: T test.
plot(extra ~ group, data = sleep)
leveneTests(sleep[,1, drop=FALSE], sleep$group)
t.test(extra ~ group, data = sleep, var.equal=TRUE)
#### * Comparación de más de dos grupos: ANOVA.
anova(lm(Mutation.Count ~ Cancer.Type, data = tmb.complete))

### Comparación de más de dos grupos: test de Kruskal Wallis.
kruskal.test(Mutation.Count ~ Cancer.Type, data = tmb.complete)

### Correlación entre variables.
plot(tmb.complete$Age.at.Which.Sequencing.was.Reported..Days., tmb.complete$Mutation.Count,
     xlab="Edad (días)",
     ylab = "Número de mutaciones")
cor(tmb.complete$Age.at.Which.Sequencing.was.Reported..Days., tmb.complete$Mutation.Count)

#### Función Pairs
pairs(tmb.complete[,c("Age.at.Which.Sequencing.was.Reported..Days.",
                      "Mutation.Count",
                      "Sample.coverage",
                      "TMB..nonsynonymous.")])

### Test para variables categóricas
table(tmb.complete$Cancer.Type,tmb.complete$Sex)
chisq.test(table(tmb.complete$Sex[tmb.complete$Cancer.Type=="Bladder Cancer"]))

## 5.3 Edición de figuras
# install.packages("ggplot2")
library(ggplot2)
# Añadir colores manualmente: https://r-charts.com/es/colores/
ggplot(data = tmb, aes(x = Overall.Survival.Status, y = Overall.Survival..Months., fill = factor(Overall.Survival.Status))) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0:LIVING" = "lightblue", "1:DECEASED" = "grey")) +
  labs(title = "Overall Survival by Status", 
       x = "Survival Status", 
       y = "Overall Survival (Months)")

# Paletas de colores
display.brewer.all(colorblindFriendly = TRUE)
age.freq<-table(tmb$Age.Group.at.Diagnosis.in.Years)[c(1,3,4,5,2)]
age.bar<-barplot(age.freq,
                 col=brewer.pal(5, "Blues"), # brewer.pal(5, "RdBu"); brewer.pal(5, "Set3"); brewer.pal(5, "Pastel2")
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30),
                 cex.names=0.6
)

# Posiciones de la leyenda
ggplot(data=tmb[1:20,], 
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) + # capa de puntos tamaño 3
  theme(text=element_text(size=10))+
  labs(y="Cobertura",x="Pureza Tumoral")

# Posiciones de la leyenda 
ggplot(data=tmb[1:20,],
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) +
  theme(text=element_text(size=10), legend.position = "bottom")+
  labs(y="Cobertura",x="Pureza Tumoral")

ggplot(data=tmb[1:20,],
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) +
  theme(text=element_text(size=10), legend.position = "top")+
  labs(y="Cobertura",x="Pureza Tumoral")

cols <- c("#55AD89", "#EF6F6A") ## cambiamos el color
ggplot(data=tmb[1:20,],
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) +
  scale_color_manual(values = cols) +
  theme(text=element_text(size=10), legend.position = c(0.1, 0.9))+ # coordenadas de leyenda
  labs(y="Cobertura",x="Pureza Tumoral")

# Eliminar outliers
ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.))+
  geom_boxplot() +
  labs(y="Supervivencia global (meses)",x=" ") +
  theme(text=element_text(size=10))

ggplot(data=tmb,
       aes(x=Overall.Survival.Status,y=Overall.Survival..Months.))+
  geom_boxplot(outlier.shape = NA) +
  labs(y="Supervivencia global (meses)",x=" ") +
  theme(text=element_text(size=10))

# 6. Guardar resultados
write.table()
help(ggsave)

# ruta_file_name = ""
# ggsave(ruta_file_name,
#        plot=last_plot(),
#        device = "png")

