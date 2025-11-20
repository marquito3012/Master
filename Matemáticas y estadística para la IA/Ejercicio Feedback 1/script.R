library(AER)
library(readr)

# Cargamos los datos
datos <- read_csv("/home/marco/Master/Matemáticas y estadística para la IA/Ejercicio Feedback 1/train.csv", na = c("N/A", "", "NaN"))

# Conseguimos las dimensiones de los datos
dim(datos)

# Mostramos las primeras 6 entradas de los datos
head(datos)

###### Análisis exploratorio de la variable objetivo SalePrice
attach(datos)

# Hacemos un histograma
hist(SalePrice, freq=FALSE)

# Mostramos un summary
summary(SalePrice)

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
hist(datos$LotFrontage)

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