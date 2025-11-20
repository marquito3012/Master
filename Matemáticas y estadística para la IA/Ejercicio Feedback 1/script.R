library(AER)
library(readr)
library(tidyverse)
library(skimr)
library(GGally)
library(ggplot2)
library(corrplot)

# Cargamos los datos
datos <- read_csv("/home/marco/Master/Matemáticas y estadística para la IA/Ejercicio Feedback 1/train.csv", na = c("N/A", "", "NaN"))

# Conseguimos las dimensiones de los datos
dim(datos)

# Mostramos las primeras 6 entradas de los datos
head(datos)

###### Identificación de valores nulos
# Comprobación de datos nulos
any(is.na(datos))

# Comprobación de la existencia de datos "NA" como caracteres
any(datos == "NA", na.rm = TRUE)

# Se realiza un conteo de los valores "NA" por columna
na_count <- colSums(datos == "NA", na.rm = TRUE)

# Se expresa el valor en porcentaje, dejando 2 decimales
na_percentage <- round(na_count / nrow(datos) * 100, digits = 2)

# Se filtra las columnas en las que aparecen datos "NA"
na_percentage[na_percentage > 0]

# Al ser LotFrontage datos numéricos, asignamos la moda a los valores "NA"
datos$LotFrontage[datos$LotFrontage == "NA"] <- NA
moda <- names(sort(table(datos$LotFrontage), decreasing = TRUE))[1]
datos$LotFrontage[is.na(datos$LotFrontage)] <- moda
datos$LotFrontage <- as.numeric(datos$LotFrontage)

# Al ser MasVnrType datos numéricos, asignamos la moda a los valores "NA"
datos$MasVnrType[datos$MasVnrType == "NA"] <- NA
moda <- names(sort(table(datos$MasVnrType), decreasing = TRUE))[1]
datos$MasVnrType[is.na(datos$MasVnrType)] <- moda

# Al ser MasVnrArea datos numéricos, asignamos la moda a los valores "NA"
datos$MasVnrArea[datos$MasVnrArea == "NA"] <- NA
moda <- names(sort(table(datos$MasVnrArea), decreasing = TRUE))[1]
datos$MasVnrArea[is.na(datos$MasVnrArea)] <- moda
datos$MasVnrArea <- as.numeric(datos$MasVnrArea)

summary(datos)

# Eliminamos la columna Id
datos <- datos[, -1]

##### Tipos de variables
# Mostramos la estructura de los datos
str(datos)

# Mostramos la clase de cada variable
sapply(datos, class)

# Resumen estadístico y categórico
summary(datos)

# Identificar nombres de variables numéricas y categóricas
num_vars <- datos %>% select(where(is.numeric)) %>% names()
cat_vars <- datos %>% select(where(~!is.numeric(.))) %>% names()
list(numericas = num_vars, categoricas = cat_vars)

# Perfilado general rápido
skim(datos)

##### Mapeo de Valores a numéricos
calidad_map <- c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "NA" = 0)
acabado_map <- c("Fin" = 3, "RFn" = 2, "Unf" = 1, "NA" = 0)
exposicion_map <- c("Gd" = 4, "Av" = 3, "Mn" = 2, "No" = 1, "NA" = 0)
datos_procesados <- datos %>%
  mutate(
    # Variables con escala estándar (Estas estaban bien)
    ExterQual   = as.numeric(recode(ExterQual, !!!calidad_map, .default = 0)),
    HeatingQC   = as.numeric(recode(HeatingQC, !!!calidad_map, .default = 0)),
    KitchenQual = as.numeric(recode(KitchenQual, !!!calidad_map, .default = 0)),
    BsmtQual    = as.numeric(recode(BsmtQual, !!!calidad_map, .default = 0)),
    FireplaceQu = as.numeric(recode(FireplaceQu, !!!calidad_map, .default = 0)),
    
    # Variables con escalas propias (CORREGIDO)
    GarageFinish = as.numeric(recode(GarageFinish, !!!acabado_map, .default = 0)),
    BsmtExposure = as.numeric(recode(BsmtExposure, !!!exposicion_map, .default = 0)),
    
    # Binarias (Estaban bien) [cite: 217, 238]
    CentralAir  = ifelse(CentralAir == "Y", 1, 0),
    Street      = ifelse(Street == "Pave", 1, 0)
  )

##### Matriz de correlacción
num_vars <- datos_procesados %>% select(where(is.numeric))
mat_cor <- cor(num_vars, use = "complete.obs") # Calcula matriz de correlación
corrplot(mat_cor, method = "color", type = "full", tl.cex = 0.8) # Diagrama visual

str(datos_procesados)

