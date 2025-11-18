# Cargar el dataset iris
data(iris)

# Ver las primeras filas del dataset
head(iris)

# Remover la columna de especies para hacer PCA solo con las variables numéricas
iris_data <- iris[, 1:4]

# Estandarizar los datos (centrar y escalar)
iris_scaled <- scale(iris_data)

# Aplicar PCA usando la función prcomp(), con centrado y escalado activado
pca_iris <- prcomp(iris_scaled, center = TRUE, scale. = TRUE)

# Mostrar el resumen de los resultados del PCA
summary(pca_iris)

# Mostrar los autovalores (varianza explicada por cada componente)
pca_iris$sdev^2

# Instalar y cargar ggbiplot si no está instalado
if (!require(ggbiplot)) install.packages("ggbiplot")
library(ggbiplot)

# Visualizar el biplot de los primeros dos componentes principales con colores para las especies
biplot_iris <- ggbiplot(pca_iris, 
                        ellipse = TRUE,           # Dibujar elipses alrededor de los grupos
                        circle = TRUE,            # Dibujar el círculo de confianza
                        var.axes = TRUE,          # Mostrar las flechas de las variables
                        groups = iris$Species) +  # Colorear por especie
  scale_color_manual(name = "Especie", values = c("red", "blue", "green")) +  # Colores personalizados para las especies
  ggtitle("Biplot de PCA - Iris Dataset") +  # Título del gráfico
  theme_minimal()  # Tema limpio y minimalista

# Mostrar el biplot
print(biplot_iris)

