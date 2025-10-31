# Instalar y cargar las librerías necesarias
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")
library(ggplot2)
library(reshape2)

# Generar un conjunto de datos inventado con 100 observaciones para X1, X2, y X3
set.seed(123)  # Fijar semilla para reproducibilidad
X1 <- rnorm(100, mean = 50, sd = 10) # X1: Se genera utilizando la función rnorm(), que crea 100 valores aleatorios
                                      #con una distribución normal (media = 50, desviación estándar

X2 <- X1 * 0.5 + rnorm(100, mean = 30, sd = 5)  # X2: Está correlacionada positivamente con X1. 
                                                #Utilizamos una combinación de X1 y un término aleatorio para 
                                                #simular esta relación.

X3 <- X1 * -0.3 + rnorm(100, mean = 10, sd = 2)  #X3: Está correlacionada negativamente con X1, simulada de manera
                                                  #similar a X2 pero con un coeficiente negativo.

# Combinar las variables en un data frame
data <- data.frame(X1 = X1, X2 = X2, X3 = X3)

# Calcular la matriz de correlación
corr_matrix <- cor(data)#Calcula la matriz de correlación para las tres variables generadas.

# Transformar la matriz de correlación en un formato largo para ggplot2
melted_corr_matrix <- melt(corr_matrix)

# Crear un heatmap de la matriz de correlación usando ggplot2
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Añadir etiquetas con los valores de correlación
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlación") +
  theme_minimal() + #Define la escala de colores en el gráfico, con rojo indicando correlación positiva fuerte, 
                    #azul para correlación negativa, y blanco para valores cercanos a 0 (sin correlación).
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +  # Rotar las etiquetas de las variables
  ggtitle("Matriz de Correlación - Datos Ficticios")

