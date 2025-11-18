# Instalar y cargar las librerías necesarias
if (!requireNamespace("Rtsne", quietly = TRUE)) {
  install.packages("Rtsne")
}
library(Rtsne)

# Cargar el dataset iris y eliminar la columna de especies (para que sólo queden las variables numéricas)
data(iris)
iris_data <- iris[, -5]  # Eliminar la columna Species

# Eliminar filas duplicadas del dataset
iris_data <- unique(iris_data)

# Aplicar t-SNE (reducción de dimensiones a 2D)
set.seed(42)  # Para asegurar la reproducibilidad
tsne_result <- Rtsne(iris_data, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)

# Extraer los resultados de t-SNE y convertirlos en un data frame
tsne_data <- as.data.frame(tsne_result$Y)  # Convertir la salida en un data frame
colnames(tsne_data) <- c("Dim1", "Dim2")  # Nombrar las columnas del resultado

# Agregar las especies al data frame
# Filtramos las especies para que correspondan a las filas no duplicadas
tsne_data$Species <- iris$Species[!duplicated(iris[, -5])]

# Instalar y cargar ggplot2 para la visualización
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Graficar el resultado de t-SNE
ggplot(tsne_data, aes(x = Dim1, y = Dim2, color = Species)) +
  geom_point(size = 3) +
  labs(title = "t-SNE con el dataset Iris (Sin duplicados)", x = "Dimensión 1", y = "Dimensión 2") +
  theme_minimal()

