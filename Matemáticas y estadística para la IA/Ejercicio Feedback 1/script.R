library(AER)
library(readr)

# Cargamos los datos
datos <- read_csv("/home/marco/Master/Matemáticas y estadística para la IA/Ejercicio Feedback 1/train.csv", na = c("NA", "N/A", "", "NaN"))

# Eliminamos la columna "Id"
datos <- datos[, -1]

# Mostramos los datos
View(datos)

# Conseguimos las dimensiones de los datos
dim(datos)

# Mostramos la estructura de los datos
str(datos)

# Mostramos las primeras 6 entradas de los datos
head(datos)


###### Análisis exploratorio de la variable objetivo SalePrice
attach(datos)

# Hacemos un histograma
hist(SalePrice, freq=FALSE)

# Mostramos un summary
summary(SalePrice)

###### Identificación de valores nulos
anyNA(datos)
na_count <- colSums(is.na(datos))
na_percentage <- round(na_count / nrow(datos) * 100, digits = 2)
na_percentage[na_percentage > 0]
sum(is.na(datos))

# Eliminamos las columnas cuyo porcentaje de valores nulos es superior al 50%
columnas_a_eliminar <- names(na_percentage[na_percentage > 50])
datos <- datos[, !(names(datos) %in% columnas_a_eliminar)]

View(datos)
