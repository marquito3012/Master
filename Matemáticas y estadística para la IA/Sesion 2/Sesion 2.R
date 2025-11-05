# =========================================
# 0) Cargando datos
# =========================================

#install.packages("readxl")
library(readxl)

base_datos <- read_excel("/home/marco/Master/Matemáticas y estadística para la IA/Sesion 2/charactersStats.xls")
View(base_datos)

# =========================================
# 1) Paquetes
# =========================================

#install.packages(c("tidyverse", "skimr", "DataExplorer", "GGally", "corrplot"))
library(tidyverse)
library(skimr)
library(DataExplorer)
library(GGally)
library(corrplot)

# =========================================
# 2) Calidad de datos
# =========================================

anyNA(base_datos)
colSums(is.na(base_datos))

# eliminar filas
base_datos$Alignment[base_datos$Alignment == ""] <- NA
base_datos <- base_datos[!is.na(base_datos$Alignment), ]
unique(base_datos$Alignment)
View(base_datos)

# tratamiento de duplicados
col_clave <- "Name"
sum(duplicated(base_datos[[col_clave]]))
base_datos <- base_datos[!duplicated(base_datos$Name), ]
View(base_datos)

# Tipos de variables (numéricas vs categóricas)
# Estructura y tipos de cada columna
str(base_datos)
sapply(base_datos, class)
summary(base_datos)

# Identificar nombres de variables numéricas y categóricas
num_vars <- base_datos %>% select(where(is.numeric)) %>% names()
cat_vars <- base_datos %>% select(where(~! is.numeric(.))) %>% names()
list(numericas = num_vars, categoricas = cat_vars)

# Perfilado general rápido
skim(base_datos)


# =========================================
# 3) Univariado (distribuciones)
# =========================================

# Histograma/densidad para todas las numéricas
ggplot(base_datos, aes(x = Intelligence)) +
  geom_histogram(binwidth = 30, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Intelligence")

ggplot(base_datos, aes(x = Speed)) +
  geom_histogram(binwidth = 30, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Speed")

ggplot(base_datos, aes(x = Intelligence)) +
  geom_density(fill = "lightgreen") +
  labs(title = "Densidad de Intelligence")

# Frecuencias para categórica
library(ggplot2)

ggplot(base_datos, aes(x = Alignment)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Frecuencias de Alignment", x = "Alignment", y = "Frecuencia")

# Boxplot
ggplot(base_datos, aes(y = Intelligence)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot de Intelligence")

ggplot(base_datos, aes(x = Alignment, y = Intelligence, fill = Alignment)) +
  geom_boxplot() +
  labs(title = "Boxplot de Intelligence por Alignment")

# Diagrama de correlación entre variables
num_vars <- base_datos %>% select(where(is.numeric)) # Selecciona solo las variables numéricas
mat_cor <- cor(num_vars, use = "complete.obs") # Calcula la matriz de correlacción
corrplot(mat_cor, method = "circle", type = "upper", tl.cex = 0.8) # Diagrama visual

corrplot(mat_cor, method = "color", type = "full", tl.cex = 0.8) # OTRO DIAGRAMA VISUAL
print(mat_cor)
cor_table <- as.data.frame(as.table(mat_cor)) # pasar matriz a tabla
print(cor_table)

# =========================================
# 4) Análisis PCA
# =========================================

#respca <- prcomp (base_datos, scale = TRUE) # ERROR!!!!!!! hay variables no numéricas
view(base_datos)

base_datos <- textshape::column_to_rownames(base_datos, loc=1)
view(base_datos)


df <- as.data.frame(base_datos) # paso mi .xls a data frame
df <- subset(df, select = -c(Alignment, Total)) # Quito la categórica Alignment
view(df)

mat_cor <- cor(df, use = "complete.obs")
print(mat_cor)
cor_table <- as.data.frame(as.table(mat_cor))
print(cor_table)
corrplot(mat_cor, method = "color", type = "full", tl.cex = 0.8)

respca <- prcomp(df, scale = TRUE) # Se crea un objeto llamado respca
names(respca)

head(respca$rotation) [,1:5] # se ejecuta rotation con 5 componentes principales, entrega coordenadas de los datos en nuevo sistema rotado de 
dim(respca$rotation) # numero de los distintos componentes principales
head(respca$x) [,1:5] # muestra los vectores de los scores
respca$sdev # muestra desviaciones de cada CP
respca$sdev^2 # varianza explicada por cada componente
summary(respca)
