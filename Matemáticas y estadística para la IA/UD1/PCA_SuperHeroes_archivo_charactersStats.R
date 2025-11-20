# =========================
# 0) Cargando dados
# =========================
library(readxl)
base_datos <- read_excel("/home/marco/Master/Matemáticas y estadística para la IA/UD1/charactersStats.xls")
View(base_datos)

# =========================
# 1) Paquetes
# =========================
# Descomenta la siguiente línea la primera vez:
#install.packages(c("tidyverse","skimr","DataExplorer","GGally","corrplot"))
library(tidyverse)
library(skimr)
library(DataExplorer)
library(GGally)
library(corrplot)


# Missing values por columna (valores faltantes)
# Conteo NA + "" por columna
anyNA(base_datos)                   # True si hay algún valor faltante
colSums(is.na(base_datos))          # Cuenta de NA por columna
sum(is.na(base_datos))              # Total de NA en todo el dataset
# -----------------------------------------------------------------------------------------------------------------------
# se podrian hacer 3 procesos: 
# 1)Imputación con la moda (dato categorico); 2)Creación de categoría "Desconocido" o "Faltante"; 3)Eliminar filas con NA
# 1) 
#moda <- names(sort(table(base_datos$alignment), decreasing = TRUE))[1]
#base_datos$alignment[is.na(base_datos$alignment)] <- moda
# 2)
#base_datos$alignment <- addNA(base_datos$alignment)
#levels(base_datos$alignment)[is.na(levels(base_datos$alignment))] <- "Desconocido"
# 3)Eliminando filas con NA
base_datos$Alignment[base_datos$Alignment == ""] <- NA    # Cambia las "" por NA
base_datos <- base_datos[!is.na(base_datos$Alignment), ]  # Elimina las filas ya limpias
unique(base_datos$Alignment)    # Debe mostrar solo "good", "bad", "neutral"
View(base_datos)

#------------------------------------------------------------------------------------------------------------------------

# Duplicados

col_clave <- "Name"
sum(duplicated(base_datos[[col_clave]]))
base_datos <- base_datos[!duplicated(base_datos$Name), ]
View(base_datos)

# Tipos de variables (numéricas vs categóricas)
# Estructura y tipos de cada columna
str(base_datos)                # Muestra estructura y tipo de cada columna
sapply(base_datos, class)      # Devuelve clase de cada variable (numeric, factor, character, etc.)
summary(base_datos)            # Resumen estadístico y categórico

# Identificar nombres de variables numéricas y categóricas
num_vars <- base_datos %>% select(where(is.numeric)) %>% names()
cat_vars <- base_datos %>% select(where(~!is.numeric(.))) %>% names()
list(numericas = num_vars, categoricas = cat_vars)

# Perfilado general rápido
skim(base_datos)

# =========================
# 3) Univariado (distribuciones)
# =========================
# Histograma/densidad para todas las numéricas 
ggplot(base_datos, aes(x = Intelligence)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Intelligence")

ggplot(base_datos, aes(x = Speed)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Intelligence")

ggplot(base_datos, aes(x = Intelligence)) +
  geom_density(fill = "lightgreen") +
  labs(title = "Densidad de Intelligence")

#Frecuencias para categorica
library(ggplot2)

ggplot(base_datos, aes(x = Alignment)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Frecuencias de Alignment", x = "Alignment", y = "Frecuencia")

#Boxplot
ggplot(base_datos, aes(y = Intelligence)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot de Intelligence")

ggplot(base_datos, aes(y = Speed)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot de Intelligence")

ggplot(base_datos, aes(x = Alignment, y = Intelligence, fill = Alignment)) +
  geom_boxplot() +
  labs(title = "Boxplot de Intelligence por Alignment")

#Diagrama de correlacion entre variables
library(corrplot)
num_vars <- base_datos %>% select(where(is.numeric)) # Selecciona solo las variables numéricas
mat_cor <- cor(num_vars, use = "complete.obs") # Calcula matriz de correlación
corrplot(mat_cor, method = "circle", type = "upper", tl.cex = 0.8) # Diagrama visual
#=============================================================================================================================
# Analysis PCA
#=============================================================================================================================
respca <- prcomp(base_datos, scale = TRUE) #error!!!! hay variable "con letras"...
View(base_datos) #la columna Name se debe pasar a numerico, lo mejor es intercambiarla por columna numerica de la izquierda
install.packages("textshape")
library(textshape)
base_datos <- textshape::column_to_rownames(base_datos, loc=1)
View(base_datos)

df <- as.data.frame(base_datos) #paso mi .xls a data frame
df <- subset(df, select = -c(Alignment,Total)) #quito la categorica Alignment
view(df)

library(stats) #prcomp() forma rapida de PCA sobre matriz de datos
respca <- prcomp(df, scale = TRUE) #Se crea un objeto llamado respca
names(respca)

head(respca$rotation) [, 1:5] #se ejecuta rotation con 5 componentes principales, entrega coordenadas de los datos en nuevo sistema rotado de cooredenadas
dim(respca$rotation) #numero de los distintos componentes principales
head(respca$x)[,1:5] #muestra los vectores de los scores
respca$sdev #muestra desviaciones de cada CP
respca$sdev^2 #Varianza explicada por cada componente
summary(respca)

#Comprobando la importancia del componente 1
xx<-respca$x
xx<-as.data.frame(xx)
base_datos$PC1 <- xx$PC1
base_datos$PC2 <- xx$PC2
base_datoscor <- subset(base_datos, select = -c(Alignment))
head(base_datoscor)
view(base_datoscor)
cor(base_datoscor)

#otra funcion de PCA
library(FactoMineR)
respca2 <- PCA(X=df, scale.unit = FALSE, ncp = 6, graph = TRUE)
print(respca2)
head(respca2$eig)

#Otras Visualizaciones
install.packages("factoextra") #visualización eigenvalores
library(factoextra)
fviz_eig(respca2)

fviz_screeplot(respca2) #otra de lo mismo

fviz_pca_ind(respca2) #representación de observaciones sobre componentes principales

fviz_pca_var(respca2) #representación de variables sobre componentes principales

#ejemplos de como visualizar aquí:
#http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining

fviz_contrib(respca2,choice = "var") #Representa la contribución de filas/columnas de los resultados de un pca.
fviz_contrib(respca2,choice = "ind")
