# Generar datos de una distribución normal
set.seed(123)  # Fijar semilla para reproducibilidad
datos <- rnorm(1000, mean = 0, sd = 1)  # 1000 datos con media 0 y desviación estándar 1

# Graficar el histograma de la distribución normal generada
hist(datos, 
     breaks = 30,  # Número de barras en el histograma
     probability = TRUE,  # Para normalizar el histograma
     main = "Distribución Normal",
     xlab = "Valores",
     col = "lightgreen",
     border = "darkgreen")

# Superponer la curva de densidad teórica de la distribución normal
curve(dnorm(x, mean = 0, sd = 1),  # Media y desviación estándar de la curva
      col = "blue",  # Color de la curva
      lwd = 2,  # Grosor de la línea
      add = TRUE)  # Agregar la curva al histograma existente
