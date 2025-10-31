# =========================================
# 0) Cargando datos
# =========================================

install.packages("readxl")
library(readxl)

base_datos <- read_excel("/home/marco/Matemáticas y estadística para la IA/Sesion 2/charactersStats.xls")
View(base_datos)

# =========================================
# 1) Paquetes
# =========================================

install.packages(c("tidyverse", "skimr", "DataExplorer", "GGally", "corrplot"))
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

str(base_datos)
sapply(base_datos, class)
summary(base_datos)

# analisis PCA
respca <- prcomp(base_datos, scale=TRUE)
View(base_datos)

install.packages("textshape")
library(textshape)
base_datos <- textshape::column_to_rownames(base_datos, loc=1)
View(base_datos)

df <- as.data.frame(base_datos)
df <- subset(df, select = -c(Alignment, Total))
View(df)

install.packages("stats")
library(stats)
respca <- prcomp(df, scale = TRUE)