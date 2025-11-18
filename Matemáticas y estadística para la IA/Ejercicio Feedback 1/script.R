library(AER)
library(readxl)

datos <- read_excel("/home/marco/Master/Matemáticas y estadística para la IA/Ejercicio Feedback 1/train.xlsx")

datos <- datos[, -1]

View(datos)

summary(datos)

attach(datos)

hist(MSSubClass)
hist()