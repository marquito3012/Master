# ============================================================================
# REGRESIÓN LINEAL MÚLTIPLE CON PCA - BUENAS PRÁCTICAS
# ============================================================================
# Este código implementa correctamente la secuencia recomendada:
# 1. División de datos (train/val/test)
# 2. Preprocesamiento solo en train
# 3. Aplicación de transformaciones aprendidas a val/test
# 4. Entrenamiento y evaluación
# ============================================================================

# --- LIBRERÍAS ---
library(readxl)   # Para leer archivos Excel
library(caret)    # Para crear particiones estratificadas
library(stats)    # Para PCA y regresión lineal

# --- 1. CARGA DE DATOS ---
# Lee el archivo Excel con las características
df <- read_excel("C:/Users/34622/Desktop/Master Uax/SM142001_Matemáticas y Estadística para la IA/Codigos_2025_2026/Caracteristicas_Limp.xlsx")

# Separa variables predictoras (X) y variable objetivo (y)
X <- df[, c("Intelligence", "Strength", "Speed", "Durability", "Power")]
y <- df$Combat

cat("Dimensiones del dataset:", nrow(df), "filas,", ncol(X), "predictores\n\n")

# --- 2. DIVISIÓN DE DATOS (60% train, 20% val, 20% test) ---
set.seed(42)  # Semilla para reproducibilidad

# Paso 1: Separar 60% para entrenamiento
# createDataPartition hace división estratificada respetando distribución de y
trainIndex <- createDataPartition(y, p = 0.6, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]

# Paso 2: Del 40% restante, dividir en 50%-50% (validación y reserva/test)
remainingIndex <- setdiff(seq_len(nrow(df)), trainIndex)
valIndex <- createDataPartition(y[remainingIndex], p = 0.5, list = FALSE)

X_val <- X[remainingIndex[valIndex], ]
y_val <- y[remainingIndex[valIndex]]

X_test <- X[remainingIndex[-valIndex], ]  # Renombrado a "test" 
y_test <- y[remainingIndex[-valIndex]]

cat("Tamaños de conjuntos:\n")
cat("  - Entrenamiento:", nrow(X_train), "observaciones\n")
cat("  - Validación:", nrow(X_val), "observaciones\n")
cat("  - Test:", nrow(X_test), "observaciones\n\n")

# --- 3. ESCALADO DE DATOS ---
# CRÍTICO: Solo se calculan parámetros (media y desviación) del conjunto de ENTRENAMIENTO
# Esto evita "data leakage" (filtración de información del test al train)

# Escalar conjunto de entrenamiento
X_train_scaled <- scale(X_train)

# Guardar los parámetros de escalado aprendidos del entrenamiento
center_params <- attr(X_train_scaled, "scaled:center")  # Medias
scale_params <- attr(X_train_scaled, "scaled:scale")    # Desviaciones estándar

# Aplicar los MISMOS parámetros a validación y test
# NUNCA recalcular medias/desviaciones de val/test
X_val_scaled <- scale(X_val, center = center_params, scale = scale_params)
X_test_scaled <- scale(X_test, center = center_params, scale = scale_params)

cat("Escalado completado usando parámetros de entrenamiento\n")
cat("Ejemplo - Media Intelligence (train):", round(center_params["Intelligence"], 2), "\n\n")

# --- 4. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA) ---
# PCA se ajusta SOLO con datos de entrenamiento
# center=FALSE y scale.=FALSE porque ya escalamos manualmente antes
pca <- prcomp(X_train_scaled, center = FALSE, scale. = FALSE)

# Calcular varianza explicada acumulada por cada componente
var_explained <- summary(pca)$importance[3, ]  # Tercera fila = proporción acumulada

# Seleccionar componentes que expliquen al menos 95% de la varianza
n_comp <- min(which(var_explained >= 0.95))

cat("--- RESULTADO PCA ---\n")
cat("Componentes que explican ≥95% varianza:", n_comp, "de", ncol(X), "\n")
cat("Varianza explicada por componente:\n")
print(round(summary(pca)$importance, 3))
cat("\n")

# --- 5. PROYECCIÓN A ESPACIO PCA ---
# Transformar datos usando la rotación (loading matrix) aprendida del train

# Train: Ya transformado por prcomp
X_train_pca <- pca$x[, 1:n_comp, drop = FALSE]

# Val y Test: Proyectar usando la matriz de rotación del PCA entrenado
X_val_pca <- as.matrix(X_val_scaled) %*% pca$rotation[, 1:n_comp, drop = FALSE]
X_test_pca <- as.matrix(X_test_scaled) %*% pca$rotation[, 1:n_comp, drop = FALSE]

# --- 6. ENTRENAMIENTO DEL MODELO ---
# Ajustar regresión lineal múltiple con los componentes principales
modelo_lm_pca <- lm(y_train ~ ., data = as.data.frame(X_train_pca))

cat("--- MODELO AJUSTADO ---\n")
print(summary(modelo_lm_pca))
cat("\n")
#salidas: Los errores están ligeramente sesgados hacia subestimar Combat (Mediana ≈ -5.2)
#Outliers: Hay algunos casos con errores muy grandes (±50 puntos), probablemente personajes con habilidades únicas
#Todos los PC son estadisticamente significativos
#R² = 0.67 es bueno para datos reales



# --- 7. PREDICCIÓN Y EVALUACIÓN ---
# Predecir en validación y test usando las proyecciones PCA
pred_train_pca <- predict(modelo_lm_pca, newdata = as.data.frame(X_train_pca))
pred_val_pca <- predict(modelo_lm_pca, newdata = as.data.frame(X_val_pca))
pred_test_pca <- predict(modelo_lm_pca, newdata = as.data.frame(X_test_pca))

# Función para calcular métricas de error
rmse <- function(y_true, y_pred) sqrt(mean((y_true - y_pred)^2))
mae <- function(y_true, y_pred) mean(abs(y_true - y_pred))
r2 <- function(y_true, y_pred) cor(y_true, y_pred)^2

# --- 8. RESULTADOS ---
cat("========================================\n")
cat("      MÉTRICAS DE RENDIMIENTO (PCA)\n")
cat("========================================\n\n")

cat("ENTRENAMIENTO:\n")
cat("  RMSE:", round(rmse(y_train, pred_train_pca), 2), "\n") #promedio de los errores de predicción
cat("  MAE: ", round(mae(y_train, pred_train_pca), 2), "\n") #Error promedio sin penalizar outlaiers
cat("  R²:  ", round(r2(y_train, pred_train_pca), 4), "\n\n") #Coeficiente de determinación (varianza explicada)

cat("VALIDACIÓN:\n")
cat("  RMSE:", round(rmse(y_val, pred_val_pca), 2), "\n")
cat("  MAE: ", round(mae(y_val, pred_val_pca), 2), "\n")
cat("  R²:  ", round(r2(y_val, pred_val_pca), 4), "\n\n")

cat("TEST (conjunto de reserva):\n")
cat("  RMSE:", round(rmse(y_test, pred_test_pca), 2), "\n")
cat("  MAE: ", round(mae(y_test, pred_test_pca), 2), "\n")
cat("  R²:  ", round(r2(y_test, pred_test_pca), 4), "\n\n")
#salidas: Train y Test son casi idénticos (RMSE: 19.33 vs 19.16) generaliza perfectamente a datos nuevos



# --- 9. VISUALIZACIONES OPCIONALES ---
# Descomenta si quieres ver gráficos

# # Varianza explicada por componente
par(mfrow = c(1, 2))
 barplot(summary(pca)$importance[2, ], 
         main = "Varianza por Componente",
         xlab = "Componente Principal", 
         ylab = "Proporción de Varianza",
         col = "steelblue")
 
 plot(var_explained, type = "b", 
      main = "Varianza Acumulada",
      xlab = "Número de Componentes", 
      ylab = "Proporción Acumulada",
      col = "darkred", lwd = 2)
 abline(h = 0.95, col = "blue", lty = 2, lwd = 2)
 legend("bottomright", legend = "Umbral 95%", 
        col = "blue", lty = 2, lwd = 2)
 
# # Predicciones vs Valores Reales
 par(mfrow = c(1, 3))
 plot(y_train, pred_train_pca, main = "Train", 
      xlab = "Real", ylab = "Predicho", pch = 19, col = "blue")
 abline(0, 1, col = "red", lwd = 2)
 
 plot(y_val, pred_val_pca, main = "Validación", 
      xlab = "Real", ylab = "Predicho", pch = 19, col = "green")
 abline(0, 1, col = "red", lwd = 2)
 
 plot(y_test, pred_test_pca, main = "Test", 
      xlab = "Real", ylab = "Predicho", pch = 19, col = "orange")
 abline(0, 1, col = "red", lwd = 2)

# ============================================================================
# NOTAS IMPORTANTES:
# ============================================================================
# 1. NUNCA usar estadísticas de val/test para preprocesamiento
# 2. PCA se ajusta solo en train, luego se proyectan val/test
# 3. El conjunto de test NO debe tocarse hasta la evaluación final
# 4. Si RMSE(val) << RMSE(test) → posible overfitting
# 5. Usar validación para ajustar hiperparámetros (ej: número de componentes)
# ============================================================================